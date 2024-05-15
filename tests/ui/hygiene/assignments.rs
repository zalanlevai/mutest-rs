//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

#[cfg(test)]
mod tests {
    #[test]
    fn test_assignments() {
        macro assignment_hygiene($x:ident) {
            $x = 1;
            let x = 2;
            println!("{x}");
        }

        let mut x = 0;
        assignment_hygiene!(x);
        println!("{x}");
    }
}
