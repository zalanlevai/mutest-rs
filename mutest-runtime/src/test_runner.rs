use std::any::Any;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::env;
use std::hash::BuildHasherDefault;
use std::io;
use std::num::NonZeroUsize;
use std::panic;
use std::sync::{Arc, Mutex};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

mod test {
    pub use ::test::*;
    pub use ::test::test::*;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TestResult {
    Ok,
    Ignored,
    Failed,
    FailedMsg(String),
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

    pub fn from_exit_code(
        exit_code: i32,
        test_timeout: Option<Duration>,
        task_exec_time: Option<Duration>,
    ) -> Self {
        let result = match exit_code {
            TR_OK => TestResult::Ok,
            TR_FAILED => TestResult::Failed,
            _ => TestResult::FailedMsg(format!("got unexpected exit code {exit_code}")),
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
pub struct CompletedTest {
    pub id: test::TestId,
    pub desc: test::TestDesc,
    pub result: TestResult,
    pub exec_time: Option<Duration>,
    pub stdout: Vec<u8>,
}

fn run_test_in_process(
    id: test::TestId,
    desc: test::TestDesc,
    test_fn: Box<dyn FnOnce() + Send>,
    monitor_ch: mpsc::Sender<CompletedTest>,
    test_timeout: Option<Duration>,
    no_capture: bool,
) {
    let io_buffer = Arc::new(Mutex::new(Vec::new()));

    if !no_capture {
        io::set_output_capture(Some(io_buffer.clone()));
    }

    let start = Instant::now();
    let result = panic::catch_unwind(panic::AssertUnwindSafe(test_fn));
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

/// Fixed frame used to clean the backtrace with `RUST_BACKTRACE=1`.
#[inline(never)]
fn __rust_begin_short_backtrace<F: FnOnce()>(f: F) {
    f();

    // Prevent this frame from being tail-call optimized away.
    test::black_box(());
}

fn run_test(
    id: test::TestId,
    test: test::TestDescAndFn,
    monitor_ch: mpsc::Sender<CompletedTest>,
    concurrency: test::Concurrent,
    test_timeout: Option<Duration>,
    no_capture: bool,
) -> Option<thread::JoinHandle<()>> {
    let test::TestDescAndFn { desc, testfn } = test;

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
        test_fn: Box<dyn FnOnce() + Send>,
        monitor_ch: mpsc::Sender<CompletedTest>,
        concurrency: test::Concurrent,
        test_timeout: Option<Duration>,
        no_capture: bool,
    ) -> Option<thread::JoinHandle<()>> {
        let name = desc.name.clone();
        let run_test = move || run_test_in_process(id, desc, test_fn, monitor_ch, test_timeout, no_capture);

        let supports_threads = !cfg!(target_os = "emscripten") && !cfg!(target_family = "wasm");

        match concurrency {
            test::Concurrent::Yes if supports_threads => {
                let thread = thread::Builder::new().name(name.as_slice().to_owned());

                let mut run_test = Arc::new(Mutex::new(Some(run_test)));
                let run_test_on_thread = run_test.clone();

                match thread.spawn(move || run_test_on_thread.lock().unwrap().take().unwrap()()) {
                    Ok(handle) => Some(handle),
                    Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                        // `ErrorKind::WouldBlock` means hitting the thread limit on some platforms, so run the test
                        // synchronously on this thread instead.
                        Arc::get_mut(&mut run_test).unwrap().get_mut().unwrap().take().unwrap()();
                        None
                    }
                    Err(e) => panic!("failed to spawn thread for test run: {e}"),
                }
            }

            test::Concurrent::Yes => {
                panic!("concurrent test execution was requested but thread support is not available");
            }

            test::Concurrent::No => {
                run_test();
                None
            }
        }
    }

    match testfn {
        test::TestFn::StaticTestFn(f) => {
            let test_fn = Box::new(move || __rust_begin_short_backtrace(f));
            run_test_impl(id, desc, test_fn, monitor_ch, concurrency, test_timeout, no_capture)
        }

        test::TestFn::DynTestFn(_) => {
            panic!("dynamic tests are not supported");
        }
        test::TestFn::StaticBenchFn(_) | test::TestFn::DynBenchFn(_) => {
            panic!("benchmarks are not supported");
        }
    }
}

#[derive(Debug)]
pub struct RunningTest {
    pub desc: test::TestDesc,
    pub start_time: Instant,
    pub join_handle: Option<thread::JoinHandle<()>>,
}

#[derive(Debug)]
pub enum TestEvent {
    Queue(usize, usize),
    Wait(test::TestDesc),
    Result(CompletedTest),
    Done(Vec<test::TestDescAndFn>, Vec<RunningTest>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Flow {
    Continue,
    Stop,
}

pub fn run_tests<E, F>(
    tests: Vec<test::TestDescAndFn>,
    mut on_test_event: F,
    test_timeout: Option<Duration>,
    no_capture: bool,
) -> Result<(), E>
where
    F: FnMut(TestEvent) -> Result<Flow, E>,
{
    let tests = tests.into_iter().enumerate()
        .map(|(i, test)| (test::TestId(i), test))
        .filter(|(_, test)| matches!(test.testfn, test::TestFn::StaticTestFn(_) | test::TestFn::DynTestFn(_)))
        .collect::<Vec<_>>();

    let concurrency = match env::var("RUST_TEST_THREADS").ok() {
        Some(value) => {
            value.parse::<NonZeroUsize>().ok().map(NonZeroUsize::get)
                .expect("RUST_TEST_THREADS must be a positive, non-zero integer")
        }
        None => thread::available_parallelism().map(NonZeroUsize::get).unwrap_or(1)
    };

    let mut remaining_tests = tests;
    // Reverse the list of remaining tests so that we can `pop` from the queue in order.
    remaining_tests.reverse();

    let mut running_tests: HashMap<test::TestId, RunningTest, BuildHasherDefault<DefaultHasher>> = Default::default();
    let mut lingering_tests: HashMap<test::TestId, RunningTest, BuildHasherDefault<DefaultHasher>> = Default::default();

    let (tx, rx) = mpsc::channel::<CompletedTest>();

    if concurrency == 1 {
        macro event($event:expr) {
            if let Flow::Stop = on_test_event($event)? {
                let remaining_tests = remaining_tests.into_iter().map(|(_, test)| test).collect();
                on_test_event(TestEvent::Done(remaining_tests, vec![]))?;

                return Ok(());
            }
        }

        if let Some(_) = test_timeout {
            panic!("test timeouts were requested but concurrency is unavailable");
        }

        while let Some((id, test)) = remaining_tests.pop() {
            event!(TestEvent::Queue(1, remaining_tests.len()));
            event!(TestEvent::Wait(test.desc.clone()));

            let join_handle = run_test(id, test, tx.clone(), test::Concurrent::No, test_timeout, no_capture);
            assert!(join_handle.is_none());
            let completed_test = rx.recv().unwrap();

            event!(TestEvent::Result(completed_test));
        }

        event!(TestEvent::Queue(0, 0));

        let remaining_tests = remaining_tests.into_iter().map(|(_, test)| test).collect();
        on_test_event(TestEvent::Done(remaining_tests, vec![]))?;
    } else {
        macro event($event:expr) {
            if let Flow::Stop = on_test_event($event)? {
                lingering_tests.extend(running_tests.drain());

                let remaining_tests = remaining_tests.into_iter().map(|(_, test)| test).collect();
                let lingering_tests = lingering_tests.into_values().collect();
                on_test_event(TestEvent::Done(remaining_tests, lingering_tests))?;

                return Ok(());
            }
        }

        while !running_tests.is_empty() || !remaining_tests.is_empty() {
            event!(TestEvent::Queue(running_tests.len(), remaining_tests.len()));

            while running_tests.len() < concurrency && let Some((id, test)) = remaining_tests.pop() {
                event!(TestEvent::Queue(running_tests.len(), remaining_tests.len()));
                event!(TestEvent::Wait(test.desc.clone()));

                let desc = test.desc.clone();

                let join_handle = run_test(id, test, tx.clone(), test::Concurrent::Yes, test_timeout, no_capture);
                running_tests.insert(id, RunningTest { desc, start_time: Instant::now(), join_handle });
            }

            if let Some(test_timeout) = test_timeout {
                let running_test_ids = running_tests.keys().cloned().collect::<Vec<_>>();
                for test_id in running_test_ids {
                    let running_test = running_tests.get(&test_id).unwrap();
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

            let deadline = test_timeout.and_then(|test_timeout| running_tests.values().map(|test| test.start_time + test_timeout).reduce(Ord::min));
            let mut completed_test = match deadline {
                Some(deadline) => {
                    match rx.recv_deadline(deadline) {
                        Err(mpsc::RecvTimeoutError::Timeout) => { continue; }
                        Err(mpsc::RecvTimeoutError::Disconnected) => panic!("test monitor channel disconnected"),
                        Ok(completed_test) => completed_test,
                    }
                }
                None => {
                    match rx.recv() {
                        Err(mpsc::RecvError) => panic!("test monitor channel disconnected"),
                        Ok(completed_test) => completed_test,
                    }
                }
            };

            let running_test = running_tests.remove(&completed_test.id).expect("just completed test not found amongst running tests");

            if let Some(join_handle) = running_test.join_handle {
                if let Err(_) = join_handle.join() {
                    if let TestResult::Ok = completed_test.result {
                        completed_test.result = TestResult::FailedMsg("panicked after reporting success".to_string());
                    }
                }
            }

            event!(TestEvent::Queue(running_tests.len(), remaining_tests.len()));
            event!(TestEvent::Result(completed_test));
        }

        let remaining_tests = remaining_tests.into_iter().map(|(_, test)| test).collect();
        let lingering_tests = lingering_tests.into_values().collect();
        on_test_event(TestEvent::Done(remaining_tests, lingering_tests))?;
    }

    Ok(())
}
