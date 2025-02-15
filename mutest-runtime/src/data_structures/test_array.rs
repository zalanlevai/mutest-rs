use std::iter;

pub struct TestArray<'t, T> {
    index: &'t [&'t test::TestName],
    data: Box<[Option<T>]>,
}

impl<'t, T> TestArray<'t, T> {
    pub fn new(index: &'t [&'t test::TestName]) -> Self {
        let index = index;
        let data = iter::repeat_with(|| None).take(index.len()).collect();
        Self { index, data }
    }

    pub fn iter_data<'a>(&'a self) -> impl Iterator<Item = &'a Option<T>> {
        self.data.iter()
    }

    pub fn fill<F>(&mut self, mut f: F)
    where
        F: FnMut(&'t test::TestName) -> Option<T>,
    {
        assert_eq!(self.index.len(), self.data.len());
        for (idx, test_name) in self.index.iter().enumerate() {
            self.data[idx] = f(test_name);
        }
    }
}
