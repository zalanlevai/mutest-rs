//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr
//@ mutest-flags: -v --call-graph-depth-limit=3

fn depth_5() {}

fn depth_4() {
    depth_5();
}

fn depth_3() {
    depth_4();
}

fn depth_2() {
    depth_3();
}

fn depth_1() {
    depth_2();
}

#[test]
fn test() {
    depth_1();
}
