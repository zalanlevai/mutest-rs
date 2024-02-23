use std::any::Any;
use std::cell::UnsafeCell;
use std::fmt;
use std::panic;
use std::sync::atomic::{self, AtomicU32, AtomicUsize};
use std::sync::{Arc, Mutex};
use std::sync::mpsc;
use std::thread;

/// Fixed frame used to clean the backtrace with `RUST_BACKTRACE=1`.
#[inline(never)]
fn __rust_begin_short_backtrace<T, F: FnOnce() -> T>(f: F) -> T {
    let result = f();

    // Prevent this frame from being tail-call optimized away.
    test::black_box(result)
}

pub type Thunk<'a> = Box<dyn FnOnce() + Send + 'a>;

struct Packet {
    result: UnsafeCell<Option<Result<(), Box<dyn Any + Send + 'static>>>>
}

unsafe impl Sync for Packet {}

impl Drop for Packet {
    fn drop(&mut self) {
        if let Err(_) = panic::catch_unwind(panic::AssertUnwindSafe(|| *self.result.get_mut() = None)) {
            println!("thread result panicked on drop");
            std::process::abort();
        }
    }
}

#[derive(Clone)]
struct AtomicSingleWait(Arc<AtomicU32>);

impl AtomicSingleWait {
    pub fn new() -> Self {
        Self(Arc::new(AtomicU32::new(0)))
    }

    pub fn wake_all(&self) {
        self.0.store(1, atomic::Ordering::Relaxed);
        atomic_wait::wake_all(self.0.as_ref());
    }

    pub fn wait(&self) {
        atomic_wait::wait(self.0.as_ref(), 0);
    }
}

pub struct JobHandle {
    finished: AtomicSingleWait,
    packet: Arc<Packet>,
}

impl fmt::Debug for JobHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("JobHandle").finish_non_exhaustive()
    }
}

impl JobHandle {
    pub fn join(mut self) -> Result<(), Box<dyn Any + Send + 'static>> {
        self.finished.wait();
        Arc::get_mut(&mut self.packet).unwrap().result.get_mut().take().unwrap()
    }

    pub fn is_finished(&self) -> bool {
        Arc::strong_count(&self.packet) == 1
    }
}

struct ThreadPoolData {
    name: Option<String>,
    stack_size: Option<usize>,
    max_thread_count: AtomicUsize,

    job_receiver: Mutex<mpsc::Receiver<(Thunk<'static>, Arc<Packet>, AtomicSingleWait)>>,
    active_threads_count: AtomicUsize,
    queued_count: AtomicUsize,
    panic_count: AtomicUsize,
}

struct Sentinel<'a> {
    data: &'a Arc<ThreadPoolData>,
    active: bool,
}

impl<'a> Sentinel<'a> {
    fn new(data: &'a Arc<ThreadPoolData>) -> Self {
        Sentinel { data, active: true }
    }

    fn cancel(mut self) {
        self.active = false;
    }
}

impl<'a> Drop for Sentinel<'a> {
    fn drop(&mut self) {
        if !self.active { return; }

        self.data.active_threads_count.fetch_sub(1, atomic::Ordering::SeqCst);
        if thread::panicking() {
            self.data.panic_count.fetch_add(1, atomic::Ordering::SeqCst);
        }

        spawn_in_pool(self.data.clone());
    }
}

fn spawn_in_pool(data: Arc<ThreadPoolData>) {
    let mut builder = thread::Builder::new();
    if let Some(name) = &data.name {
        builder = builder.name(name.clone());
    }
    if let Some(stack_size) = &data.stack_size {
        builder = builder.stack_size(*stack_size);
    }

    let _handle = builder.spawn(move || {
        // Will spawn a new thread on panic unless it is cancelled.
        let sentinel = Sentinel::new(&data);

        loop {
            let job_msg = data.job_receiver.lock().expect("worker thread unable to lock job receiver").recv();
            let (job, packet, finished) = match job_msg {
                Ok(job) => job,
                Err(_) => break,
            };
            data.queued_count.fetch_sub(1, atomic::Ordering::SeqCst);

            data.active_threads_count.fetch_add(1, atomic::Ordering::SeqCst);

            let result = panic::catch_unwind(panic::AssertUnwindSafe(|| __rust_begin_short_backtrace(job)));
            unsafe { *packet.result.get() = Some(result) };
            drop(packet);
            finished.wake_all();

            data.active_threads_count.fetch_sub(1, atomic::Ordering::SeqCst);
        }

        sentinel.cancel();
    }).unwrap();
}

pub struct ThreadPool {
    data: Arc<ThreadPoolData>,
    job_sender: mpsc::Sender<(Thunk<'static>, Arc<Packet>, AtomicSingleWait)>,
}

impl ThreadPool {
    pub fn new(size: usize, name: Option<String>, stack_size: Option<usize>) -> Self {
        let (tx, rx) = mpsc::channel::<(Thunk<'static>, Arc<Packet>, AtomicSingleWait)>();

        let data = Arc::new(ThreadPoolData {
            name,
            stack_size,
            max_thread_count: AtomicUsize::new(size),
            job_receiver: Mutex::new(rx),
            active_threads_count: AtomicUsize::new(0),
            queued_count: AtomicUsize::new(0),
            panic_count: AtomicUsize::new(0),
        });

        for _ in 0..size {
            spawn_in_pool(data.clone());
        }

        ThreadPool { data, job_sender: tx }
    }

    pub fn execute<F>(&self, job: F) -> JobHandle
    where
        F: FnOnce() + Send + 'static,
    {
        let packet = Arc::new(Packet {
            result: UnsafeCell::new(None),
        });

        let finished = AtomicSingleWait::new();

        self.data.queued_count.fetch_add(1, atomic::Ordering::SeqCst);
        self.job_sender.send((Box::new(job), packet.clone(), finished.clone())).expect("cannot send job into queue");

        JobHandle { finished, packet }
    }

    pub fn max_thread_count(&self) -> usize {
        self.data.max_thread_count.load(atomic::Ordering::Relaxed)
    }

    pub fn active_count(&self) -> usize {
        self.data.active_threads_count.load(atomic::Ordering::SeqCst)
    }

    pub fn queued_count(&self) -> usize {
        self.data.queued_count.load(atomic::Ordering::Relaxed)
    }

    pub fn panic_count(&self) -> usize {
        self.data.panic_count.load(atomic::Ordering::Relaxed)
    }
}

impl Clone for ThreadPool {
    fn clone(&self) -> Self {
        ThreadPool {
            data: self.data.clone(),
            job_sender: self.job_sender.clone(),
        }
    }
}

impl fmt::Debug for ThreadPool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("ThreadPool")
            .field("name", &self.data.name)
            .field("max_thread_count", &self.max_thread_count())
            .field("active_count", &self.active_count())
            .field("queued_count", &self.queued_count())
            .field("panic_count", &self.panic_count())
            .finish()
    }
}
