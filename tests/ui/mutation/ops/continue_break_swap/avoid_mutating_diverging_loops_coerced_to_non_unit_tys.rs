//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: continue_break_swap

#![allow(unreachable_code)]

fn first_even(xs: &[u32]) -> Option<u32> {
    let mut it = xs.iter();
    loop {
        let x = *it.next()?;
        if x % 2 != 0 {
            continue;
        }
        return Some(x);
    }
}

fn first_odd(xs: &[u32]) -> Option<u32> {
    let mut it = xs.iter();
    let odd = loop {
        let x = *it.next()?;
        if x % 2 == 0 {
            continue;
        }
        return Some(x);
    };
    odd
}

fn skip_evens(xs: &[u32]) {
    let mut it = xs.iter();
    loop {
        let Some(&x) = it.next() else { return; };
        if x % 2 == 0 {
            continue;
        }
    };
}

fn skip_odds(xs: &[u32]) {
    let mut it = xs.iter();
    loop {
        let Some(&x) = it.next() else { return; };
        if x % 2 != 0 {
            continue;
        }
    }
}

#[test]
fn test() {
    assert_eq!(first_even(&[1, 2]), Some(2));
    assert_eq!(first_odd(&[2, 1]), Some(1));
    skip_evens(&[1, 2]);
    skip_odds(&[1, 2]);
}
