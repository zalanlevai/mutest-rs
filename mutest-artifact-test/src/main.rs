#![allow(unused_features)]
#![allow(unused_imports)]

#![allow(dead_code)]

#![feature(test)]
#![feature(custom_test_frameworks)]
#![test_runner(mutest_generated::harness)]

pub mod mutest_generated {
    use std::borrow::Cow;

    extern crate test;
    extern crate mutest_runtime;
    extern crate mutest_mutations;

    pub(crate) struct SubstMap {
        pub r#iuwegjfnjkndbiowjfoijewofij: Option<mutest_runtime::SubstMeta<'static>>,
        pub r#klnvfjkdnvjknskjvnkjdnsvjks: Option<mutest_runtime::SubstMeta<'static>>,
    }

    pub(crate) const MUTATIONS: &[&mutest_runtime::MutationMeta] = &[
        // &mutest_mutations::ReplaceArgWithDefault {
        //     param_ident: Cow::Borrowed("a"),
        // },
        &mutest_runtime::MutationMeta {},
    ];

    const MUTANTS: &[mutest_runtime::MutantMeta<SubstMap>] = &[
        mutest_runtime::MutantMeta {
            mutations: &[],
            substitutions: SubstMap {
                r#iuwegjfnjkndbiowjfoijewofij: None,
                r#klnvfjkdnvjknskjvnkjdnsvjks: None,
            },
        },
        mutest_runtime::MutantMeta {
            // mutations: [
            //     &mutest_mutations::ReplaceArgWithDefault {
            //         arg_ident: "a".to_owned(),
            //     },
            //     &mutest_mutations::InsertDebugPrint {
            //         path: "mutest_artifact_test::foo".to_owned(),
            //     },
            // ],
            mutations: &[],
            substitutions: SubstMap {
                r#iuwegjfnjkndbiowjfoijewofij: None,
                // r#klnvfjkdnvjknskjvnkjdnsvjks: None,
                // r#klnvfjkdnvjknskjvnkjdnsvjks: Some(mutest_bridge::Substitute {
                //     mutation: MUTATIONS[1],
                //     kind: mutest_bridge::SubstituteKind::Fn(&TestSubstitution { value: 5 }),
                // }),
                r#klnvfjkdnvjknskjvnkjdnsvjks: Some(mutest_runtime::SubstMeta {
                    mutation: MUTATIONS[0],
                }),
            },
        },
    ];

    // #[derive(Debug)]
    // struct TestSubstitution {
    //     value: usize,
    // }
    //
    // impl mutest_bridge::FnSubstitute for TestSubstitution {
    //     type Output = usize;
    //
    //     fn apply(&self) -> Self::Output {
    //         println!("Hello from mutest_generated::TestSubstituion");
    //         self.value
    //     }
    // }

    pub(crate) static ACTIVE_MUTANT_HANDLE: mutest_runtime::ActiveMutantHandle<SubstMap> = mutest_runtime::ActiveMutantHandle::empty();

    pub(crate) fn harness(tests: &[&test::TestDescAndFn]) {
        println!("Hello from a generated mutest_harness!");
        mutest_runtime::mutest_main_static(tests, MUTANTS, &ACTIVE_MUTANT_HANDLE);
    }
}

fn main() {}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works_1() {
        println!("mutest_artifact_test::tests::it_works_1");
        // println!("{:#?}", crate::mutest_generated::ACTIVE_MUTANT_HANDLE.borrow().unwrap().substitutions.r#iuwegjfnjkndbiowjfoijewofij);

        // assert_eq!(mutest_runtime::substitute!(crate::mutest_generated::ACTIVE_MUTANT_HANDLE, r#iuwegjfnjkndbiowjfoijewofij, true), true);
        // crate::mutest_generated::ACTIVE_MUTANT_HANDLE.borrow().unwrap().substitutions.r#iuwegjfnjkndbiowjfoijewofij.unwrap().mutation

        {
            // let subst = match crate::mutest_generated::ACTIVE_MUTANT_HANDLE.borrow() {
            //     Some(mutant) => mutant.substitutions.r#iuwegjfnjkndbiowjfoijewofij,
            //     None => None,
            // };

            match crate::mutest_generated::ACTIVE_MUTANT_HANDLE.borrow().and_then(|m| m.substitutions.r#iuwegjfnjkndbiowjfoijewofij.as_ref()) {
                Some(subst) if std::ptr::eq(subst.mutation, crate::mutest_generated::MUTATIONS[0]) => {}
                Some(subst) if std::ptr::eq(subst.mutation, crate::mutest_generated::MUTATIONS[1]) => {}
                Some(_) => { unreachable!() }
                None => { /* <ORIGINAL_EXPR> */ }
            }
        }
    }

    #[test]
    fn it_works_2() {
        println!("mutest_artifact_test::tests::it_works_2");
        // println!("{:#?}", crate::mutest_generated::ACTIVE_MUTANT_HANDLE.borrow().unwrap().substitutions.r#klnvfjkdnvjknskjvnkjdnsvjks);

        // assert_eq!(mutest_runtime::substitute!(crate::mutest_generated::ACTIVE_MUTANT_HANDLE, r#klnvfjkdnvjknskjvnkjdnsvjks, 8), 8);
    }
}
