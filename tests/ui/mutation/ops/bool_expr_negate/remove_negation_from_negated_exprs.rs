//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: bool_expr_negate

fn f() {
    let not_a_bool = 5 + 7 - 3;

    let mut tainted = !true;
    for i in 0..10 {
        if !(i <= 9) {
            tainted = !false;
        }
    }

    let complex_bool = !((not_a_bool + 1) != 10) || !(not_a_bool * 0 < 1);

    if tainted && !(not_a_bool != 9) {
        println!("tainted && not_a_bool == 9");
    } else if tainted && !(not_a_bool <= 9) {
        println!("tainted && not_a_bool > 9");
    } else if !!!complex_bool {
        println!("!complex_bool");
    } else {
        println!("something else");
    }

    match () {
        _ if !!complex_bool => { println!("complex_bool"); }
        () if !(not_a_bool != 9) || tainted => { println!("not_a_bool == 9 || tainted"); }
        _ => { println!("something else"); }
    }

    let nested_ident_negation = !!!complex_bool;
    let nested_expr_negation = !!!!(!(not_a_bool == 9));

    let _ = nested_ident_negation || nested_expr_negation;
}

#[test]
fn test() {
    f();
}
