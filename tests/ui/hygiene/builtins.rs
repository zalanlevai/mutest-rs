//@ build
//@ stderr: empty

#[cfg(test)]
mod tests {
    #[test]
    fn test_builtins() {
        println!("{:?}, {:?}, {:?}", 3, 2, Some(1));
        assert_eq!(1, 1);
        assert!(true, "assert macro");
        if false { unreachable!("unreachable macro"); }
        if false { panic!("panic macro"); }
    }
}
