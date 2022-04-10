pub macro lookup_substitution($handle:path, $subst:ident) {
    // match crate::mutest_generated::ACTIVE_MUTANT_HANDLE.borrow() {
    match $handle.borrow() {
        Some(mutant) => match &mutant.substitutions.$subst {
            Some(substitution) => Some(substitution),
            None => None
        }
        None => None
    }
}

pub macro substitute($handle:path, $subst:ident, $original:tt) {
    match lookup_substitution!($handle, $subst) {
        Some(substitution) => substitution.apply(),
        None => { $original }
    }
}
