use std::any::Any;
use std::collections::HashMap;
use std::env;
use std::fmt;
use std::io;
use std::num::NonZeroUsize;
use std::panic;
use std::process::{self, Command};
use std::sync::{Arc, Mutex};
use std::sync::mpsc;
use std::thread::{self, ThreadId};
use std::time::{Duration, Instant};

use crate::thread_pool::{self, ThreadPool};

mod test {
    #![allow(unused_imports)]

    pub use ::test::*;
    pub use ::test::test::*;
}

#[derive(Clone)]
pub enum TestRunStrategy {
    InProcess(Option<ThreadPool>),
    InIsolatedChildProcess(Arc<dyn Fn(&mut process::Command) + Send + Sync>),
}

impl fmt::Debug for TestRunStrategy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InProcess(thread_pool) => {
                f.debug_tuple("InProcess")
                    .field(thread_pool).finish()
            }
            Self::InIsolatedChildProcess(_) => {
                f.debug_tuple("InIsolatedChildProcess")
                    .field(&format_args!("_")).finish()
            }
        }
    }
}

#[derive(Debug)]
pub enum ThreadHandle {
    StandaloneThread(thread::JoinHandle<()>),
    ThreadPoolThread(thread_pool::JobHandle),
}

impl ThreadHandle {
    pub fn thread_id(&self) -> ThreadId {
        match self {
            Self::StandaloneThread(join_handle) => join_handle.thread().id(),
            Self::ThreadPoolThread(job_handle) => job_handle.thread_id(),
        }
    }

    pub fn join(self) -> Result<(), Box<dyn Any + Send + 'static>> {
        match self {
            Self::StandaloneThread(join_handle) => join_handle.join(),
            Self::ThreadPoolThread(job_handle) => job_handle.join(),
        }
    }

    pub fn is_finished(&self) -> bool {
        match self {
            Self::StandaloneThread(join_handle) => join_handle.is_finished(),
            Self::ThreadPoolThread(job_handle) => job_handle.is_finished(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TestResult {
    Ok,
    Ignored,
    Failed,
    FailedMsg(String),
    CrashedMsg(String),
    TimedOut,
}

const TR_OK: i32 = 50;
const TR_FAILED: i32 = 51;

impl TestResult {
    pub fn from_task<'a>(
        test_should_panic: test::ShouldPanic,
        task_result: Result<(), &'a (dyn Any + Send + 'static)>,
        test_timeout: Option<Duration>,
        task_exec_time: Option<Duration>,
    ) -> Self {
        let result = match (test_should_panic, task_result) {
            | (test::ShouldPanic::No, Ok(_))
            | (test::ShouldPanic::Yes, Err(_)) => TestResult::Ok,

            | (test::ShouldPanic::Yes, Ok(_))
            | (test::ShouldPanic::YesWithMessage(_), Ok(_)) => {
                TestResult::FailedMsg("test did not panic as expected".to_string())
            }

            (test::ShouldPanic::YesWithMessage(msg), Err(ref err)) => {
                let maybe_panic_str = err
                    .downcast_ref::<String>()
                    .map(|e| &**e)
                    .or_else(|| err.downcast_ref::<&'static str>().copied());

                match maybe_panic_str {
                    Some(panic_str) if panic_str.contains(msg) => TestResult::Ok,
                    Some(panic_str) => {
                        TestResult::FailedMsg(format!(
                            r#"panic did not contain expected string
      panic message: `{panic_str:?}`,
 expected substring: `{msg:?}`"#
                        ))
                    }
                    _ => {
                        let panic_ty = (**err).type_id();
                        TestResult::FailedMsg(format!(
                            r#"expected panic with string value
 found non-string value: `{panic_ty:?}`
     expected substring: `{msg:?}`"#
                        ))
                    }
                }
            }

            _ => TestResult::Failed,
        };

        if result != TestResult::Ok { return result; }

        if let (Some(test_timeout), Some(task_time)) = (test_timeout, task_exec_time) {
            if task_time > test_timeout {
                return TestResult::TimedOut;
            }
        }

        result
    }

    pub fn from_exit_status(
        exit_status: process::ExitStatus,
        test_timeout: Option<Duration>,
        task_exec_time: Option<Duration>,
    ) -> Self {
        #[cfg(not(unix))]
        let exit_code = exit_status.code().expect("received no exit code");
        #[cfg(unix)]
        let Some(exit_code) = exit_status.code() else {
            use std::os::unix::process::ExitStatusExt;
            match exit_status.signal() {
                Some(signal) => return TestResult::CrashedMsg(format!("received signal {signal}")),
                None => return TestResult::CrashedMsg("received unknown signal".to_owned()),
            }
        };

        let result = match exit_code {
            TR_OK => TestResult::Ok,
            TR_FAILED => TestResult::Failed,
            _ => TestResult::CrashedMsg(format!("got unexpected exit code {exit_code}")),
        };

        if result != TestResult::Ok { return result; }

        if let (Some(test_timeout), Some(task_time)) = (test_timeout, task_exec_time) {
            if task_time > test_timeout {
                return TestResult::TimedOut;
            }
        }

        result
    }
}

#[derive(Debug)]
pub struct Test {
    pub desc: test::TestDesc,
    pub test_fn: test::TestFn,
    pub timeout: Option<Duration>,
}

#[derive(Debug)]
pub struct CompletedTest {
    pub id: test::TestId,
    pub desc: test::TestDesc,
    pub result: TestResult,
    pub exec_time: Option<Duration>,
    pub stdout: Vec<u8>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ControlMsg {
    KillChildProcess,
}

fn run_test_in_process(
    id: test::TestId,
    desc: test::TestDesc,
    test_fn: Box<dyn FnOnce() -> Result<(), String> + Send>,
    monitor_ch: mpsc::Sender<CompletedTest>,
    test_timeout: Option<Duration>,
    no_capture: bool,
) {
    let io_buffer = Arc::new(Mutex::new(Vec::new()));

    if !no_capture {
        io::set_output_capture(Some(io_buffer.clone()));
    }

    fn fold_err<T, E>(result: Result<Result<T, E>, Box<dyn Any + Send>>) -> Result<T, Box<dyn Any + Send>>
    where
        E: Send + 'static,
    {
        match result {
            Ok(Err(e)) => Err(Box::new(e)),
            Ok(Ok(v)) => Ok(v),
            Err(e) => Err(e),
        }
    }

    let start = Instant::now();
    let result = fold_err(panic::catch_unwind(panic::AssertUnwindSafe(test_fn)));
    let exec_time = start.elapsed();

    io::set_output_capture(None);

    let test_result = match result {
        Ok(_) => TestResult::from_task(desc.should_panic, Ok(()), test_timeout, Some(exec_time)),
        Err(e) => TestResult::from_task(desc.should_panic, Err(e.as_ref()), test_timeout, Some(exec_time)),
    };

    let stdout = io_buffer.lock().unwrap_or_else(|e| e.into_inner()).to_vec();
    let completed_test = CompletedTest { id, desc, result: test_result, exec_time: Some(exec_time), stdout };

    match monitor_ch.send(completed_test) {
        Ok(()) => {}
        // Send errors will only occur if the test execution outlives the test run, closing the receiver early. This
        // happens if the test runner was stopped early, leaving already running tests lingering until completion. This
        // behaviour is considered intended and so send errors are explicitly ignored.
        Err(mpsc::SendError(_)) => {}
    };
}

pub static TEST_SUBPROCESS_INVOCATION: &str = "__ISOLATED_TEST_CASE";

fn spawn_test_subprocess(
    id: test::TestId,
    desc: test::TestDesc,
    cmd_hook: Arc<dyn Fn(&mut process::Command) + Send + Sync>,
    control_ch: Option<mpsc::Receiver<ControlMsg>>,
    monitor_ch: mpsc::Sender<CompletedTest>,
    test_timeout: Option<Duration>,
    no_capture: bool,
) {
    let current_exe = env::current_exe().expect("cannot resolve test executable path");

    let mut cmd = Command::new(current_exe);
    cmd.env(TEST_SUBPROCESS_INVOCATION, desc.name.as_slice());

    if no_capture {
        cmd.stdout(process::Stdio::inherit());
        cmd.stderr(process::Stdio::inherit());
    } else {
        cmd.stdout(process::Stdio::piped());
        cmd.stderr(process::Stdio::piped());
    }

    // Allow caller to customize the test subprocess command.
    cmd_hook(&mut cmd);

    let (test_result, exec_time, output) = 'test_exec: {
        let mut child = cmd.spawn().expect("failed to spawn subprocess for test");

        let start = Instant::now();
        if let Some(test_timeout) = test_timeout {
            loop {
                if let Some(control_ch) = &control_ch {
                    match control_ch.try_recv() {
                        // Kill test subprocess if the test run is stopped early.
                        Ok(ControlMsg::KillChildProcess) => {
                            child.kill().expect("failed to kill test subprocess");
                            let output = child.wait_with_output().expect("failed to get output of killed test subprocess");
                            break 'test_exec (TestResult::Ignored, start.elapsed(), output);
                        }

                        Err(mpsc::TryRecvError::Disconnected) => panic!("test subprocess left dangling: control channel disconnected"),

                        // No control messages, continue normally.
                        Err(mpsc::TryRecvError::Empty) => {}
                    }
                }

                let exit_status = match child.try_wait() {
                    Ok(exit_status) => exit_status,
                    Err(e) => {
                        let err = format!("failed to poll test subprocess: {e:?}");
                        child.kill().expect("failed to kill test subprocess");
                        let output = child.wait_with_output().expect("failed to get output of killed test subprocess");
                        break 'test_exec (TestResult::FailedMsg(err), start.elapsed(), output);
                    }
                };

                if let Some(_exit_status) = exit_status { break; }

                if start.elapsed() > test_timeout {
                    child.kill().expect("failed to kill test subprocess");
                    let output = child.wait_with_output().expect("failed to get output of killed test subprocess");
                    break 'test_exec (TestResult::TimedOut, start.elapsed(), output);
                }
            }
        } else {
            child.wait().expect("failed to wait for test subprocess");
        }
        let exec_time = start.elapsed();

        let output = child.wait_with_output().expect("failed to get output of killed test subprocess");
        let test_result = TestResult::from_exit_status(output.status, test_timeout, Some(exec_time));
        break 'test_exec (test_result, exec_time, output);
    };

    let mut stdout = output.stdout;
    stdout.extend_from_slice(&output.stderr);

    let completed_test = CompletedTest { id, desc, result: test_result, exec_time: Some(exec_time), stdout };
    monitor_ch.send(completed_test).expect("test subprocess left dangling: monitor channel disconnected");
}

/// Fixed frame used to clean the backtrace with `RUST_BACKTRACE=1`.
#[inline(never)]
fn __rust_begin_short_backtrace<T, F: FnOnce() -> T>(f: F) -> T {
    let result = f();

    // Prevent this frame from being tail-call optimized away.
    test::black_box(result)
}

pub fn run_test_in_spawned_subprocess(test: test::TestDescAndFn) -> ! {
    let builtin_panic_hook = panic::take_hook();

    let exit_with_result = Arc::new(move |panic_info: Option<&panic::PanicHookInfo<'_>>| -> ! {
        let task_result = match panic_info {
            Some(info) => Err(info.payload()),
            None => Ok(()),
        };
        let test_result = TestResult::from_task(test.desc.should_panic, task_result, None, None);

        if let TestResult::FailedMsg(msg) = &test_result {
            eprintln!("{msg}");
        }
        if let Some(info) = panic_info {
            builtin_panic_hook(info);
        }

        match test_result {
            TestResult::Ok => process::exit(TR_OK),
            TestResult::Failed | TestResult::FailedMsg(_) => process::exit(TR_FAILED),
            TestResult::CrashedMsg(_) | TestResult::TimedOut | TestResult::Ignored => unreachable!(),
        }
    });

    panic::set_hook({
        let exit_with_result_panic = exit_with_result.clone();
        Box::new(move |panic_info| exit_with_result_panic(Some(panic_info)))
    });

    let result = match test.testfn {
        test::TestFn::StaticTestFn(f)
        => __rust_begin_short_backtrace(f),

        | test::TestFn::DynTestFn(_)
        | test::TestFn::StaticBenchFn(_)
        | test::TestFn::StaticBenchAsTestFn(_)
        | test::TestFn::DynBenchFn(_)
        | test::TestFn::DynBenchAsTestFn(_)
        => unreachable!(),
    };

    if let Err(e) = result { panic!("{e}"); }

    exit_with_result(None);
}

fn run_test(
    id: test::TestId,
    test: Test,
    control_ch: Option<mpsc::Receiver<ControlMsg>>,
    monitor_ch: mpsc::Sender<CompletedTest>,
    test_run_strategy: TestRunStrategy,
    no_capture: bool,
) -> Option<ThreadHandle> {
    let Test { desc, test_fn, timeout } = test;

    let ignore_because_no_process_support = match desc.should_panic {
        test::ShouldPanic::Yes | test::ShouldPanic::YesWithMessage(_) => {
            // Emscripten can catch panics but other WASM targets cannot.
            cfg!(target_family = "wasm") && !cfg!(target_os = "emscripten")
        }
        _ => false,
    };

    if desc.ignore || ignore_because_no_process_support {
        let message = CompletedTest { id, desc, result: TestResult::Ignored, exec_time: None, stdout: Vec::new() };
        monitor_ch.send(message).unwrap();
        return None;
    }

    fn run_test_impl(
        id: test::TestId,
        desc: test::TestDesc,
        test_fn: Box<dyn FnOnce() -> Result<(), String> + Send>,
        test_run_strategy: TestRunStrategy,
        control_ch: Option<mpsc::Receiver<ControlMsg>>,
        monitor_ch: mpsc::Sender<CompletedTest>,
        test_timeout: Option<Duration>,
        no_capture: bool,
    ) -> Option<ThreadHandle> {
        let thread_pool = match &test_run_strategy {
            TestRunStrategy::InProcess(thread_pool) => thread_pool.clone(),
            TestRunStrategy::InIsolatedChildProcess(_) => None,
        };

        let name = desc.name.clone();
        let run_test = move || {
            match test_run_strategy {
                TestRunStrategy::InProcess(_)
                => run_test_in_process(id, desc, test_fn, monitor_ch, test_timeout, no_capture),

                TestRunStrategy::InIsolatedChildProcess(cmd_hook)
                => spawn_test_subprocess(id, desc, cmd_hook, control_ch, monitor_ch, test_timeout, no_capture),
            }
        };

        let supports_threads = !cfg!(target_os = "emscripten") && !cfg!(target_family = "wasm");

        if supports_threads {
            let mut run_test = Arc::new(Mutex::new(Some(run_test)));
            let run_test_on_thread = run_test.clone();
            let job = move || run_test_on_thread.lock().unwrap().take().unwrap()();

            match thread_pool {
                Some(thread_pool) => {
                    let handle = thread_pool.execute(job);
                    Some(ThreadHandle::ThreadPoolThread(handle))
                }
                None => {
                    let thread = thread::Builder::new().name(name.as_slice().to_owned());
                    match thread.spawn(job) {
                        Ok(handle) => Some(ThreadHandle::StandaloneThread(handle)),
                        Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                            // `ErrorKind::WouldBlock` means hitting the thread limit on some platforms, so run the test
                            // synchronously on this thread instead.
                            Arc::get_mut(&mut run_test).unwrap().get_mut().unwrap().take().unwrap()();
                            None
                        }
                        Err(e) => panic!("failed to spawn thread for test run: {e}"),
                    }
                }
            }
        } else {
            run_test();
            None
        }
    }

    match test_fn {
        test::TestFn::StaticTestFn(f) => {
            let test_fn = Box::new(move || __rust_begin_short_backtrace(f));
            run_test_impl(id, desc, test_fn, test_run_strategy, control_ch, monitor_ch, timeout, no_capture)
        }

        test::TestFn::DynTestFn(_) => {
            panic!("dynamic tests are not supported");
        }

        | test::TestFn::StaticBenchFn(_)
        | test::TestFn::StaticBenchAsTestFn(_)
        | test::TestFn::DynBenchFn(_)
        | test::TestFn::DynBenchAsTestFn(_) => {
            panic!("benchmarks are not supported");
        }
    }
}

pub fn concurrency() -> usize {
    match env::var("RUST_TEST_THREADS").ok() {
        Some(value) => {
            value.parse::<NonZeroUsize>().ok().map(NonZeroUsize::get)
                .expect("RUST_TEST_THREADS must be a positive, non-zero integer")
        }
        None => thread::available_parallelism().map(NonZeroUsize::get).unwrap_or(1)
    }
}

#[derive(Debug)]
pub struct RunningTest {
    pub desc: test::TestDesc,
    pub timeout: Option<Duration>,
    pub start_time: Instant,
    pub control_tx: mpsc::Sender<ControlMsg>,
    pub join_handle: Option<ThreadHandle>,
}

#[derive(Debug)]
pub enum TestEvent {
    Queue(usize, usize),
    Wait(test::TestDesc, Option<ThreadId>),
    Result(CompletedTest),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Flow {
    Continue,
    Stop,
}

pub fn run_tests<E, F>(
    tests: Vec<Test>,
    mut on_test_event: F,
    test_run_strategy: TestRunStrategy,
    no_capture: bool,
) -> Result<(Vec<Test>, Vec<RunningTest>), E>
where
    F: FnMut(TestEvent, &mut Vec<(test::TestId, Test)>) -> Result<Flow, E>,
{
    let tests = tests.into_iter().enumerate()
        .map(|(i, test)| (test::TestId(i), test))
        .filter(|(_, test)| matches!(test.test_fn, test::TestFn::StaticTestFn(_) | test::TestFn::DynTestFn(_)))
        .collect::<Vec<_>>();

    let concurrency = match &test_run_strategy {
        TestRunStrategy::InProcess(Some(thread_pool)) => thread_pool.max_thread_count(),
        _ => concurrency(),
    };

    let mut remaining_tests = tests;
    // Reverse the list of remaining tests so that we can `pop` from the queue in order.
    remaining_tests.reverse();

    type RunningTestMap = HashMap<test::TestId, RunningTest>;
    let mut running_tests: RunningTestMap = Default::default();
    let mut lingering_tests: RunningTestMap = Default::default();

    let (test_tx, test_rx) = mpsc::channel::<CompletedTest>();

    if concurrency == 1 {
        macro event($event:expr) {
            if let Flow::Stop = on_test_event($event, &mut remaining_tests)? {
                let remaining_tests = remaining_tests.into_iter().map(|(_, test)| test).collect();
                return Ok((remaining_tests, vec![]));
            }
        }

        while let Some((id, test)) = remaining_tests.pop() {
            if let Some(_) = test.timeout {
                panic!("test timeout was requested but concurrency is unavailable");
            }

            event!(TestEvent::Queue(1, remaining_tests.len()));

            let desc = test.desc.clone();

            let join_handle = run_test(id, test, None, test_tx.clone(), test_run_strategy.clone(), no_capture);
            event!(TestEvent::Wait(desc, join_handle.as_ref().map(|h| h.thread_id())));
            let mut completed_test = test_rx.recv().unwrap();

            if let Some(join_handle) = join_handle {
                if let Err(_) = join_handle.join() {
                    if let TestResult::Ok = completed_test.result {
                        completed_test.result = TestResult::FailedMsg("panicked after reporting success".to_owned());
                    }
                }
            }

            event!(TestEvent::Result(completed_test));
        }

        event!(TestEvent::Queue(0, 0));

        let remaining_tests = remaining_tests.into_iter().map(|(_, test)| test).collect();
        return Ok((remaining_tests, vec![]));
    } else {
        fn cleanup_isolated_tests(running_tests: &mut RunningTestMap, test_rx: &mpsc::Receiver<CompletedTest>) {
            for (_, test) in &*running_tests {
                match test.control_tx.send(ControlMsg::KillChildProcess) {
                    Ok(()) => {}
                    // Send errors will only occur if the test execution completed during cleanup.
                    // This is fine, since the completed test will have been sent through the test monitor channel.
                    // As such, send errors are explicitly ignored here.
                    Err(mpsc::SendError(_)) => {}
                };
            }

            for (_, running_test) in running_tests.drain() {
                let mut completed_test = test_rx.recv().expect("test monitor channel disconnected");

                if let Some(join_handle) = running_test.join_handle {
                    if let Err(_) = join_handle.join() {
                        if let TestResult::Ok = completed_test.result {
                            completed_test.result = TestResult::FailedMsg("panicked after reporting success".to_owned());
                        }
                    }
                }
            }
        }

        macro event($event:expr) {
            if let Flow::Stop = on_test_event($event, &mut remaining_tests)? {
                if let TestRunStrategy::InIsolatedChildProcess(_) = &test_run_strategy {
                    cleanup_isolated_tests(&mut running_tests, &test_rx);
                }

                lingering_tests.extend(running_tests.drain());

                let remaining_tests = remaining_tests.into_iter().map(|(_, test)| test).collect();
                let lingering_tests = lingering_tests.into_values().collect();
                return Ok((remaining_tests, lingering_tests));
            }
        }

        while !running_tests.is_empty() || !remaining_tests.is_empty() {
            event!(TestEvent::Queue(running_tests.len(), remaining_tests.len()));

            while running_tests.len() < concurrency && let Some((id, test)) = remaining_tests.pop() {
                event!(TestEvent::Queue(running_tests.len(), remaining_tests.len()));

                let desc = test.desc.clone();
                let timeout = test.timeout;

                let (control_tx, control_rx) = mpsc::channel::<ControlMsg>();
                let join_handle = run_test(id, test, Some(control_rx), test_tx.clone(), test_run_strategy.clone(), no_capture);
                event!(TestEvent::Wait(desc.clone(), join_handle.as_ref().map(|h| h.thread_id())));
                running_tests.insert(id, RunningTest { desc, timeout, start_time: Instant::now(), control_tx, join_handle });
            }

            if let TestRunStrategy::InProcess(_) = &test_run_strategy {
                let running_test_ids = running_tests.keys().cloned().collect::<Vec<_>>();
                for test_id in running_test_ids {
                    let running_test = running_tests.get(&test_id).unwrap();
                    let Some(test_timeout) = running_test.timeout else { continue; };
                    let exec_time = running_test.start_time.elapsed();
                    if exec_time > test_timeout {
                        match &running_test.join_handle {
                            Some(join_handle) if !join_handle.is_finished() => {
                                let completed_test = CompletedTest {
                                    id: test_id,
                                    desc: running_test.desc.clone(),
                                    result: TestResult::TimedOut,
                                    exec_time: Some(exec_time),
                                    // TODO: Propagate stdout from `run_test`.
                                    stdout: vec![],
                                };

                                let running_test = running_tests.remove(&test_id).unwrap();
                                event!(TestEvent::Queue(running_tests.len(), remaining_tests.len()));
                                lingering_tests.insert(test_id, running_test);
                                event!(TestEvent::Result(completed_test));
                            }
                            None => eprintln!("test timed out but it cannot be halted as it is not running concurrently"),
                            _ => {}
                        }
                    }
                }
            }

            if running_tests.is_empty() { break; }

            let deadline = running_tests.values().filter_map(|test| test.timeout.map(|test_timeout| test.start_time + test_timeout)).reduce(Ord::min);
            let mut completed_test = match (deadline, &test_run_strategy) {
                (Some(deadline), TestRunStrategy::InProcess(_)) => {
                    match test_rx.recv_deadline(deadline) {
                        Err(mpsc::RecvTimeoutError::Timeout) => { continue; }
                        Err(mpsc::RecvTimeoutError::Disconnected) => panic!("test monitor channel disconnected"),
                        Ok(completed_test) => completed_test,
                    }
                }
                _ => {
                    match test_rx.recv() {
                        Err(mpsc::RecvError) => panic!("test monitor channel disconnected"),
                        Ok(completed_test) => completed_test,
                    }
                }
            };

            let Some(running_test) = running_tests.remove(&completed_test.id) else {
                // The test completion corresponds to a test that has been previously marked as timed out.
                // In this case, the completion was caused by changes in the active mutations and should be considered bogus.
                continue;
            };

            if let Some(join_handle) = running_test.join_handle {
                if let Err(_) = join_handle.join() {
                    if let TestResult::Ok = completed_test.result {
                        completed_test.result = TestResult::FailedMsg("panicked after reporting success".to_owned());
                    }
                }
            }

            event!(TestEvent::Queue(running_tests.len(), remaining_tests.len()));
            event!(TestEvent::Result(completed_test));
        }

        let remaining_tests = remaining_tests.into_iter().map(|(_, test)| test).collect();
        let lingering_tests = lingering_tests.into_values().collect();
        return Ok((remaining_tests, lingering_tests));
    }
}
