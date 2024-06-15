pub use parser::*;

use crate::{Rule, Expr, RuleData, Pattern};
use std::borrow::Cow;

fn new_rule_data<'a>(
    regexp: bool,
    mut exprs: Vec<Expr<'a>>,
    attrs: Vec<(&'a str, &'a str)>,
    mut colors: Vec<Option<&'a str>>,
) -> RuleData<'a> {
    if let Some(first) = colors.first_mut()
        .map(|color| color.take())
        .flatten()
    {
        colors.insert(1, first.into());
        match &mut exprs[..] {
            [Expr::Literal(s, n), ..] => {
                s.to_mut().insert(1, '(');
                *n += 1;
            },
            _ => exprs.insert(0, Expr::Literal("/(/".into(), 1)),
        }
        match &mut exprs[..] {
            [.., Expr::Literal(s, _)] => {
                let tail = s.to_mut().pop().unwrap();
                s.to_mut().push(')');
                s.to_mut().push(tail);
            },
            _ => exprs.push(Expr::Literal("/)/".into(), 0)),
        }
    }
    RuleData {
        exprs,
        colors,
        group_count: None,
        regexp,
        attrs,
    }
}

peg::parser!(grammar parser() for str {
    rule newline()
        = "\r"? "\n"

    rule comment()
        = "//" !"!" [^'\r' | '\n']*

    rule _()
        = quiet!{
            (
                [' ' | '\t']+
                / comment()? newline()
            )* (comment()? ![_])?
        }

    pub rule ident() -> &'input str
        = quiet!{$(
            !['0'..='9' | '-']
            ['a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-']+
        )}
        / expected!("ident")

    rule eident() -> Cow<'input, str>
        = i:ident() { format!("\"{i}\"").into() }
        / s:string() { s.into() }

    pub rule string() -> &'input str
        = quiet!{
            $("\"" (
                "\\" [^'\r' | '\n']
                / [^'"' | '\r' | '\n']
            )* "\"")
        }
        / expected!("string")

    pub rule regex() -> &'input str
        = quiet!{
            $("/" ("\\/" / [^'/' | '\r' | '\n'])+ "/")
        }
        / expected!("regex")

    pub rule unum() -> u32
        = s:quiet!{$("0" / !"0" ['0'..='9']+)}
        {?
            s.parse().map_err(|_| {
                "invalid number"
            })
        }
        / expected!("number")

    rule color() -> (u32, &'input str)
        = "$" n:unum() _ ":" _ name:string()
        { (n, name) }

    pub rule colors() -> Vec<Option<&'input str>>
        = colors:color() ** _
        {
            let mut res = Vec::new();
            for (id, color) in colors {
                let id = id as usize;
                if res.len() <= id { res.resize(id+1, None) }
                res[id] = color.into();
            }
            res
        }

    rule attr_val() -> &'input str
        = string()
        / $("true")
        / $("false")

    rule attr() -> (&'input str, &'input str)
        = "$" name:ident() _ ":" _ val:attr_val()
        { (name, val) }

    pub rule attrs() -> Vec<(&'input str, &'input str)>
        = attrs:attr() ** _
        {
            attrs
        }

    rule group_count_regex_ig()
        = "\\" ("\\"? "/" / [^ '/'])
        / "(?" !("<" ['a'..='z' | 'A'..='Z'])
        / [^'(' | '/' | '\\']

    rule group_count_str_ig()
        = "\\\\" [^ '"']
        / "\\" [_]
        / "(?" !("<" ['a'..='z' | 'A'..='Z'])
        / [^'(' | '"' | '\\']

    /// group count, contains `()` and `(?<name>)`
    pub rule group_count() -> u32
        = "/" group_count_regex_ig()*
            s:("(" (&[^ '?'] / "?<" ['a'..='z' | 'A'..='Z']) group_count_regex_ig()*)*
            "/"
            { s.len() as u32 }
        / "\"" group_count_str_ig()*
            s:("(" (&[^ '?'] / "?<" ['a'..='z' | 'A'..='Z']) group_count_str_ig()*)*
            "\""
            { s.len() as u32 }

    pub rule expr() -> Expr<'input>
        = s:(regex() / string()) { Expr::Literal(s.into(), group_count(s).unwrap()) }
        / "keywordsToRegex" "(" _ s:string() ++ (_ ("," _)?) _ ("," _)? ")"
            {
                Expr::KwdsToRegex(s)
            }
        / "@" i:ident() { Expr::Ref(i) }
        / "&" n:eident()
              c:("(" n:unum() ")" { n })?
              { Expr::Include(n, c.unwrap_or(0)) }

    pub rule mt_rule() -> Rule<'input>
        = name:ident()
        _ pats:(
            ":="
            _ exprs:expr() ++ (_ ("+" _)?)
            _ attrs:attrs()
            _ colors:colors()
            { vec![new_rule_data(false, exprs, attrs, colors).into()] }
          / ":=" _ "{"
            pats:(
                _
                p:( ":"
                    _ exprs:expr() ++ (_ ("+" _)?)
                    _ attrs:attrs()
                    _ colors:colors()
                    { new_rule_data(false, exprs, attrs, colors).into() }
                / "::"
                    _ name:eident()
                    { Pattern::IncludePattern(name) }
                )
                { p }
            )+
            _ "}"
            { pats }
          / "="
            _ exprs:expr() ++ (_ ("+" _)?)
            _ colors:colors()
            { vec![new_rule_data(true, exprs, vec![], colors).into()] }
        )
        {
            Rule {
                name,
                pats,
            }
        }

    pub rule rule_list() -> Vec<Rule<'input>>
        = _ s:mt_rule() ++ _ _ { s }

    pub rule script() -> (&'input str, Vec<Rule<'input>>, &'input str)
        =
        begin:$(
            (
                [' ' | '\t']*
                !"//!includeBegin"
                [^'\r' | '\n']*
                newline()
            )*
            [' ' | '\t']*
        )
        "//!includeBegin" newline()
        rules:rule_list()
        [' ' | '\t']* "//!includeEnd" newline()
        end:$([_]*)
        {
            (begin, rules, end)
        }
});

#[cfg(test)]
mod tests {
    use crate::{build, BuildContext, OutputContext};

    use super::*;

    #[test]
    fn parse_test() {
        let src = r#"
        foo = /(abc)/ + "def" + /./
            $1: "red" // abc

        bar := @foo + /;|\// + @foo // ...
        "#;
        println!("{src}");

        let rules = parser::rule_list(src)
            .unwrap_or_else(|e| {
                eprintln!("line {} column {}", e.location.line, e.location.column);
                eprintln!("expected {}", e.expected);
                panic!()
            });
        dbg!(&rules);
        let mut ctx = BuildContext::default();
        let mut octx = OutputContext::default();
        println!("-- output --");
        build(rules, &mut octx, &mut ctx).unwrap();
        println!("-- finished --");
    }

    #[test]
    fn script_test() {
        let src = r#"
        some text of begin
        //!includeBegin
        foo = /(abc)/ + "def" + &extern(2)
            $1: "red" // abc

        bar := @foo + /;|\// + @foo // ...
        //!includeEnd
        some text of end
        "#;
        println!("{src}");

        dbg!(script(src).unwrap());
    }

    #[test]
    fn group_count_test() {
        let datas = [
            (r#"//"#, 0),
            (r#"/m/"#, 0),
            (r#"/mmmmmm/"#, 0),
            (r#"/(?:m)/"#, 0),
            (r#"/m(?:m)/"#, 0),
            (r#"/m(?:m)m/"#, 0),
            (r#"/()/"#, 1),
            (r#"/m()/"#, 1),
            (r#"/()m/"#, 1),
            (r#"/m()m/"#, 1),
            (r#"/m(m)m/"#, 1),
            (r#"/m(m)m/"#, 1),
            (r#"/m(?:m)m/"#, 0),
            (r#"/m(?<=m)m/"#, 0),
            (r#"/m\(m)m/"#, 0),
            (r#"/m\\(m)m/"#, 1),
            (r#"/m\m(m)m/"#, 1),
            (r#"/m\\/(m)m/"#, 1),
            (r#"/m\\\\/(m)m/"#, 1),
            (r#"/m(?<a>)m/"#, 1),
            (r#"/m\(?<a>)m/"#, 0),
            (r#"/m\\(?<a>)m/"#, 1),
            (r#"/m\\(?=a)m/"#, 0),
            (r#"/m(?=a)m/"#, 0),
            (r#"/(?=a)/"#, 0),
            (r#"/(=a)/"#, 1),
            (r#"/(a)/"#, 1),
            (r#"/()/"#, 1),
            (r#"/()()/"#, 2),
            (r#"/m()()/"#, 2),
            (r#"/m()m()/"#, 2),
            (r#"/m()()m/"#, 2),
            (r#"/(())/"#, 2),
            (r#"/((()))/"#, 3),
            (r#"/(()())/"#, 3),
            (r#"/m(())/"#, 2),
            (r#"/(m())/"#, 2),
            (r#"/((m))/"#, 2),
            (r#"/m(m())/"#, 2),
            (r#"/((?:))/"#, 1),
            (r#"/(?:())/"#, 1),
            (r#"/((?=))/"#, 1),
            (r#"/((?<=))/"#, 1),
            (r#"/((?<x>))/"#, 2),
            (r#"/(?<m>(?<x>))/"#, 2),

            (r#""""#, 0),
            (r#""m""#, 0),
            (r#""()m""#, 1),
            (r#""m()""#, 1),
            (r#""(m)""#, 1),
            (r#""m(m)""#, 1),
            (r#""(m)m""#, 1),
            (r#""m()m""#, 1),
            (r#""m(m)m""#, 1),
            (r#""m(())m""#, 2),
            (r#""m(()())m""#, 3),
            (r#""m((()))m""#, 3),
            (r#""m((\\()))m""#, 2),
            (r#""m(\\(\\()))m""#, 1),
            (r#""m(\(\()))m""#, 1),
            (r#""(?<m>(?<x>))""#, 2),
            (r#""\\(?<m>(?<x>))""#, 1),
            (r#""(?<m>\\(?<x>))""#, 1),
        ];

        for (src, count) in datas {
            let c = match parser::group_count(src) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("src: {src}");
                    eprintln!("err: {},{}\nexpected: {}",
                        e.location.line,
                        e.location.column,
                        e.expected,
                    );
                    panic!()
                }
            };
            assert_eq!(c, count, "src: {src}");
        }
    }
}
