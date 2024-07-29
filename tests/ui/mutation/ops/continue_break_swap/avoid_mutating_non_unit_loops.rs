//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: continue_break_swap

#![allow(unreachable_code)]

fn f() {
    loop {
        break;
    }

    let _ = loop {
        continue;
        break 1;
    };

    loop {
        let _ = 'a: loop {
            loop {
                continue;
                continue 'a;
                break 'a 1;
            }

            break 0;
        };

        continue;
        break;
    }
}

#[test]
fn test() {
    f();
}
