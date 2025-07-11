use char_classes::any;
use getopts_macro::getopts::Matches;
use line_column::line_column;
use mtsyntax_plus::{build, parser::{self, Context}, BuildContext, Error, OutputContext, Rule};
use std::{
    convert::Infallible,
    env::args,
    fmt::{Debug, Display},
    fs::File,
    io::{self, stdin, stdout, Read, Write},
    num::NonZeroU32,
    process::exit,
    str::FromStr,
};

#[derive(Debug)]
struct Config {
    spaces: NonZeroU32,
    newline: String,
    rules_only: bool,
    compact: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            spaces: NonZeroU32::new(4).unwrap(),
            newline: "\n".into(),
            rules_only: false,
            compact: false,
        }
    }
}

fn error_exit(source: &str, e: Error) -> Result<Infallible, io::Error> {
    match e {
        Error::UndefinedRef(name) => {
            eprintln!("BuildError: undefined reference `{name}`");
        },
        Error::DuplicateDefine(name) => {
            eprintln!("BuildError: duplicate define `{name}`");
        },
        Error::RefNotARegexp(name) => {
            eprintln!("BuildError: reference `{name}` is not a regexp define");
        },
        Error::InvalidGroupId(loc, id) => {
            let (line, col) = line_column(source, loc);
            eprintln!("BuildError: invalid group id({id}) at {line}:{col}");
        },
        Error::IOError(e) => Err(e)?,
    }
    exit(3)
}

fn loc_prompt(s: &str) -> String {
    if s.is_empty() {
        return " at eof".into();
    }

    let word = s.split_once(any!("\t\r\n"))
        .map_or(s, |it| it.0);

    word.is_empty()
        .then_some(String::new())
        .unwrap_or_else(|| format!(" near `{word}`"))
}

fn unwrap_parsed<T>(
    s: &str,
    result: Result<T, peg::error::ParseError<peg::str::LineCol>>,
) -> T {
    result.unwrap_or_else(|e| {
        let loc_prompt = loc_prompt(&s[e.location.offset..]);

        eprintln!("ParseError: at {}:{}{loc_prompt}",
            e.location.line,
            e.location.column,
        );
        eprintln!("expected {}", e.expected);
        exit(3)
    })
}

fn output(
    source: &str,
    rules: Vec<Rule<'_>>,
    cfg: &Config,
    indent: &str,
    mut out: impl Write,
) -> io::Result<String> {
    let mut ctx = BuildContext::default();
    let mut octx =
        OutputContext::new(|args| {
            write!(&mut out, "{args}")
        });

    octx.compact = cfg.compact;
    octx.newline_str = &cfg.newline;
    if let Some(' ') | None = indent.chars().next() {
        octx.indent_level = indent.len() as u32 / cfg.spaces;
        octx.indent_str = " ".repeat(cfg.spaces.get() as usize).leak();
    } else {
        octx.indent_level = indent.len() as u32;
        octx.indent_str = "\t";
    }

    if let Err(e) = build(rules, &mut octx, &mut ctx) {
        eprintln!();
        error_exit(source, e)?;
    }

    Ok(octx.warnings(source))
}

fn proc_it(mut io: impl Read, cfg: &Config) -> io::Result<()> {
    let mut input = String::new();
    io.read_to_string(&mut input)?;
    drop(io);

    let ctx = &mut Context::default();

    if cfg.rules_only {
        let rules
            = unwrap_parsed(&input, parser::rule_list(&input, ctx));
        let indent = input.split_once(any!(^" \t"))
            .map_or(&*input, |it| it.0);
        print!("{indent}");
        let warns = output(&input, rules, cfg, indent, stdout().lock())?;
        eprint!("{warns}");
        return Ok(());
    }

    let (begin, rules, end)
        = unwrap_parsed(&input, parser::script(&input, ctx));

    let mut out = stdout().lock();

    let indent = begin
        .rsplit_once(any!(^" \t"))
        .map_or(begin, |(_, ind)| ind);

    write!(&mut out, "{begin}// Generated by mtsyntax-plus begin\n{indent}")?;

    let warns = output(&input, rules, cfg, indent, &mut out)?;

    write!(&mut out, "// Generated by mtsyntax-plus end\n{end}")?;
    eprint!("{warns}");

    Ok(())
}

fn opt_parse<T>(matched: &Matches, nm: &str) -> Option<T>
where T: FromStr,
      T::Err: Display,
{
    Some(matched.opt_get(nm).transpose()?
                .unwrap_or_else(|e|
    {
        eprintln!("Parse option '{nm}' failed: {e}");
        exit(2)
    }))
}

fn main() {
    let options = getopts_macro::getopts_options! {
        -c  --compact       "Using compact output";
        -r  --rules         "Parse rules only";
        -s  --spaces=N      "Indent spaces";
        -n  --newline=S     "Newline string";
        -h  --help          "Show help message";
        -v  --version       "Show version";
        .parsing_style(getopts_macro::getopts::ParsingStyle::FloatingFrees)
    };
    let args = &mut args();
    let proc = args.next().unwrap();
    let matched = match options.parse(args) {
        Ok(matched) => matched,
        Err(e) => {
            eprintln!("{e}");
            exit(2)
        },
    };
    if matched.opt_present("help") {
        let biref = options.short_usage(&proc);
        let about = "将MT管理器语法进行强化, 使其正则定义可以携带颜色";
        let info = format!("{biref} [FILES..]\n{about}");
        let usage = options.usage(&info);
        println!("{usage}");
        println!("input from stdin, output to stdout");
        exit(0)
    }
    if matched.opt_present("version") {
        eprintln!("{}", env!("CARGO_PKG_VERSION"));
        exit(0)
    }

    let mut cfg = Config::default();
    macro_rules! parse_cfg {
        ($($field:ident),+ $(,)?) => {
            $(
                if let Some($field) = opt_parse(&matched, stringify!($field)) {
                    cfg.$field = $field;
                }
            )+
        };
    }
    parse_cfg! {
        spaces,
        newline,
    }
    cfg.compact    = matched.opt_present("compact");
    cfg.rules_only = matched.opt_present("rules");

    let files = if matched.free.is_empty() {
        vec!["-".into()]
    } else {
        matched.free
    };

    let mut err = false;

    for path in files {
        let res = (|| if path == "-" {
            proc_it(stdin(), &cfg)
        } else {
            proc_it(File::open(&path)?, &cfg)
        })();
        if let Err(e) = res {
            err = true;
            eprintln!("error ({path}): {e}")
        }
    }

    if err {
        exit(1)
    }
}
