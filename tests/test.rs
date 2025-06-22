use std::{io::Write, fs, path::Path};

use mtsyntax_plus::{build, parser, BuildContext, OutputContext};

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
        let s = fs::read_to_string(path.path()).unwrap();
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
            eprintln!("{:<40} passed", path.path().to_string_lossy());
        } else {
            eprintln!("-- input --\n{}", input.trim_end());
            eprintln!("-- output --\n{}", out.trim_end());
            eprintln!("-- expected --\n{}", expected.trim_end());
            panic!("{:<40} failed", path.path().to_string_lossy());
        }
    }
}
