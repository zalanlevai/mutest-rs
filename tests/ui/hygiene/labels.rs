//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#[cfg(test)]
mod tests {
    #[test]
    fn test_label_hygiene() {
        macro label_hygiene($label:tt) {
            'label: loop {
                if false { continue 'label; }
                break $label;
            }
        }

        'label: loop {
            label_hygiene!('label);
        }
    }
}
