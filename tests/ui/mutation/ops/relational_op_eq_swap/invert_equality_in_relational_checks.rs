//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: relational_op_eq_swap

fn f() {
    let not_a_bool = 5 + 7 - 3;

    let mut tainted = false;
    for i in 0..10 {
        if i > 9 {
            tainted = true;
        }
    }

    let complex_bool = (not_a_bool + 1) == 10 || not_a_bool * 0 >= 1;

    if tainted && not_a_bool <= 9 {
        println!("tainted && not_a_bool <= 9");
    } else if tainted && not_a_bool > 9 {
        println!("tainted && not_a_bool > 9");
    } else if !(complex_bool as u8 >= 1) {
        println!("!complex_bool");
    } else {
        println!("something else");
    }

    match () {
        _ if complex_bool => { println!("complex_bool"); }
        () if (not_a_bool <= 9 && not_a_bool >= 9) || tainted => { println!("not_a_bool == 9 || tainted"); }
        _ => { println!("something else"); }
    }
}

#[test]
fn test() {
    f();
}
