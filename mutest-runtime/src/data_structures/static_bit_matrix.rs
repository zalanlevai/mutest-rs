#![expect(private_bounds)]

type Word = u64;
const WORD_BYTES: usize = size_of::<Word>();
const WORD_BITS: usize = WORD_BYTES * 8;

#[inline]
const fn words_per_row_count(n: u32) -> usize {
    (n as usize).div_ceil(WORD_BITS)
}

#[inline]
pub(crate) const fn words_count(n: u32) -> usize {
    n as usize * words_per_row_count(n)
}

// HACK: Clone bound added because of an issue with `feature(generic_const_exprs)`
//       when the suggested empty bounds are evaluated across crate-boundaries,
//       see https://github.com/rust-lang/rust/issues/145069#issuecomment-3754163634.
pub struct StaticBitMatrix<const N: u32>
where
    [(); words_count(N)]: Clone,
{
    words: [Word; words_count(N)],
}

impl<const N: u32> StaticBitMatrix<N>
where
    [(); words_count(N)]: Clone,
{
    pub const fn from_symmetric_pairs(pairs: &[(u32, u32)]) -> Self {
        let mut matrix = Self { words: [0; words_count(N)] };

        // NOTE: We must use a manual index-based loop, because
        //       for-loops and iterators are not yet supported in const contexts.
        let mut i = 0;
        while i < pairs.len() {
            let (a, b) = pairs[i];
            matrix.symmetric_set(a, b);
            i += 1;
        }

        matrix
    }

    pub const fn from_raw_words(words: &[Word]) -> Self {
        Self { words: *words.as_array().unwrap() }
    }

    pub const fn set(&mut self, row: u32, column: u32) {
        assert!(row < N && column < N);
        let row = row as usize;
        let column = column as usize;

        let words_per_row = words_per_row_count(N);
        let row_first_word_idx = row * words_per_row;
        let word_idx_offset = column / WORD_BITS;
        let word_mask = 1 << (column % WORD_BITS);
        let word = &mut self.words[row_first_word_idx + word_idx_offset];
        *word |= word_mask;
    }

    #[inline]
    pub const fn symmetric_set(&mut self, a: u32, b: u32) {
        self.set(a, b);
        self.set(b, a);
    }

    pub const fn contains(&self, row: u32, column: u32) -> bool {
        assert!(row < N && column < N);
        let row = row as usize;
        let column = column as usize;

        let words_per_row = words_per_row_count(N);
        let row_first_word_idx = row * words_per_row;
        let word_idx_offset = column / WORD_BITS;
        let word_mask = 1 << (column % WORD_BITS);
        (self.words[row_first_word_idx + word_idx_offset] & word_mask) != 0
    }

    pub const fn as_ref(&self) -> StaticBitMatrixRef<'_> {
        StaticBitMatrixRef::from_raw(N, &self.words)
    }
}

#[derive(Debug)]
pub struct StaticBitMatrixRef<'a> {
    n: u32,
    words: &'a [Word],
}

impl<'a> StaticBitMatrixRef<'a> {
    const fn from_raw(n: u32, words: &'a [Word]) -> Self {
        Self { n, words }
    }

    pub const fn contains(&self, row: u32, column: u32) -> bool {
        assert!(row < self.n && column < self.n);
        let row = row as usize;
        let column = column as usize;

        let words_per_row = words_per_row_count(self.n);
        let row_first_word_idx = row * words_per_row;
        let word_idx_offset = column / WORD_BITS;
        let word_mask = 1 << (column % WORD_BITS);
        (self.words[row_first_word_idx + word_idx_offset] & word_mask) != 0
    }
}
