#[macro_export]
macro_rules! expr {
    ($id:ident, $expr:expr) => {
        {
            {
                let header_decor = " \u{001b}[34m-->\u{001b}[0m ";
                let snippet_decor = "  \u{001b}[34m|\u{001b}[0m  ";

                let id = stringify!($id);
                let expr = stringify!($expr).lines().map(|str| format!("{}{}\n", snippet_decor, str)).collect::<String>();
                let expr_with_padding = format!("{}\n{}{}", snippet_decor, expr, snippet_decor);

                println!("\n{}mutest_runtime::expr! {}:\n{}", header_decor, id, expr_with_padding);
            }

            $expr
        }
    };
}

#[macro_export]
macro_rules! stmt {
    ($id:ident, $expr:expr) => {
        {
            {
                let header_decor = " \u{001b}[34m-->\u{001b}[0m ";
                let snippet_decor = "  \u{001b}[34m|\u{001b}[0m  ";

                let id = stringify!($id);
                let expr = stringify!($expr).lines().map(|str| format!("{}{}\n", snippet_decor, str)).collect::<String>();
                let expr_with_padding = format!("{}\n{}{}", snippet_decor, expr, snippet_decor);

                println!("\n{}mutest_runtime::stmt! {}:\n{}", header_decor, id, expr_with_padding);
            }

            $expr;
        }
    };
}

#[macro_export]
macro_rules! local {
    ($id:ident, $pat:pat $(, $ty:ty)?) => {
        {
            let header_decor = " \u{001b}[34m-->\u{001b}[0m ";
            let snippet_decor = "  \u{001b}[34m|\u{001b}[0m  ";

            let id = stringify!($id);
            let snippet = format!("{}let {} = {{...}};", snippet_decor, stringify!($pat));
            let snippet_with_padding = format!("{}\n{}\n{}", snippet_decor, snippet, snippet_decor);

            println!("\n{}mutest_runtime::local! {}:\n{}", header_decor, id, snippet_with_padding);
        }

        let $pat$(: $ty)? = Default::default();
    };
}
