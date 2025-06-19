//@ build
//@ stderr: empty

#[cfg(test)]
mod tests {
    thread_local! {
        static TLS: () = Default::default();
    }

    #[test]
    fn test_builtins() {
        println!("{:?}, {:?}, {:?}", 3, 2, Some(1));
        let _ = format!("abc.{}", 0);
        assert_eq!(1, 1);
        assert!(true, "assert macro");
        if false { unreachable!("unreachable macro"); }
        if false { panic!("panic macro"); }
    }
}
