use std::{fmt::Display, fs, io::Write, path::Path, thread};

use mtsyntax_plus::{build, parser, BuildContext, OutputContext};

struct Guard<S: Display>(S);
impl<S: Display> Drop for Guard<S> {
    fn drop(&mut self) {
        if thread::panicking() {
            eprintln!("{} panic!", self.0)
        }
    }
}

#[test]
fn main() {
    let this_file = file!();
    let path = Path::new(this_file);
    let dir = path.parent().unwrap();
    let files = fs::read_dir(dir).unwrap()
        .filter(|it| it.as_ref()
            .map_or(true, |entry| {
                "mtsx" == entry.path().extension().unwrap()
            }))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    for path in files {
        let path = path.path();
        let _guard = Guard(path.to_string_lossy());
        let s = fs::read_to_string(&path).unwrap();
        let (input, expected) = s.split_once("\n-- end --\n")
            .expect("cannot find split line");
        let rules = parser::rule_list(input).unwrap();

        let mut out = vec![];

        let mut ctx = BuildContext::default();
        let mut octx = OutputContext::new(|args| {
            out.write_fmt(args)
        });

        build(rules, &mut octx, &mut ctx).unwrap();

        let out = String::from_utf8(out).unwrap();

        if out.trim_end() == expected.trim_end() {
            eprintln!("{:<40} passed", path.to_string_lossy());
        } else {
            let diff = dissimilar::diff(expected, &out);
            eprintln!("-- input --\n{}", input.trim_end());
            eprintln!("-- output --\n{}", out.trim_end());
            eprintln!("-- expected --\n{}", expected.trim_end());
            eprintln!("-- diff --");
            for diff_chunk in diff {
                match diff_chunk {
                    dissimilar::Chunk::Equal(s) => print!("{s}"),
                    dissimilar::Chunk::Delete(s) => print!("\x1b[41m{s}\x1b[m\x1b[K"),
                    dissimilar::Chunk::Insert(s) => print!("\x1b[42m{s}\x1b[m\x1b[K"),
                }
            }
            panic!("{:<40} failed", path.to_string_lossy());
        }
    }
}
