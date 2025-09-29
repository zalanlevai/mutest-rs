//@ build
//@ stderr: empty

#![feature(test)]
#![feature(custom_test_frameworks)]
#![test_runner(dummy_test_runner)]

extern crate test;

fn dummy_test_runner(_tests: &[test::TestDescAndFn]) {}
