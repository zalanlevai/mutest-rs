use std::fmt::{self, Write};

use similar::{ChangeTag, TextDiff};

struct LineNumber(Option<usize>);
impl fmt::Display for LineNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            None => write!(f, "    "),
            Some(idx) => write!(f, "{:<4}", idx + 1),
        }
    }
}

pub fn display_diff(expected: &str, actual: &str) -> Result<String, fmt::Error> {
    let diff = TextDiff::from_lines(expected, actual);

    let mut out = String::new();
    for (idx, group) in diff.grouped_ops(3).iter().enumerate() {
        if idx > 0 {
            writeln!(&mut out, "{:-^1$}", "-", 80)?;
        }

        for op in group {
            for change in diff.iter_inline_changes(op) {
                let (sign, ctrl, ctrl_emph) = match change.tag() {
                    ChangeTag::Delete => ("-", "\x1b[0;31m", "\x1b[4;31m"),
                    ChangeTag::Insert => ("+", "\x1b[0;32m", "\x1b[4;31m"),
                    ChangeTag::Equal => (" ", "\x1b[2m", "\x1b[2;4m"),
                };

                write!(&mut out, "\x1b[2m{ln_old}{ln_new}\x1b[0m |{sign}",
                    ln_old = LineNumber(change.old_index()),
                    ln_new = LineNumber(change.new_index()),
                )?;

                for (emphasized, value) in change.iter_strings_lossy() {
                    if emphasized {
                        write!(&mut out, "{ctrl_emph}{value}\x1b[0m")?;
                    } else {
                        write!(&mut out, "{ctrl}{value}\x1b[0m")?;
                    }
                }

                if change.missing_newline() {
                    writeln!(&mut out)?;
                }
            }
        }
    }

    Ok(out)
}
