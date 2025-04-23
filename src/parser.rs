pub use parser::*;

use crate::{Color, Expr, Pattern, Rule, RuleData};
use std::borrow::Cow;
use peg::RuleResult;

fn new_rule_data<'a>(
    regexp: bool,
    mut exprs: Vec<Expr<'a>>,
    attrs: Vec<(&'a str, &'a str)>,
    mut colors: Vec<Option<Vec<Color<'a>>>>,
) -> RuleData<'a> {
    if let Some(first) = colors.first_mut()
        .and_then(|color| color.take())
    {
        colors.insert(1, first.into());
        exprs.insert(0, Expr::Literal("/(/".into(), 1));
        exprs.push(Expr::Literal("/)/".into(), 0))
    }
    let len = exprs.len();
    let mut exprs = exprs.into_iter()
        .fold(Vec::with_capacity(len), |mut acc, expr| {
            let Some(Expr::Literal(last, lc))
                = acc.last_mut()
            else {
                acc.push(expr);
                return acc;
            };
            assert_ne!(last.len(), 0);
            match expr {
                Expr::Literal(lit, c)
                if lit.chars().next() == last.chars().next() => {
                    last.to_mut().pop().unwrap();
                    last.to_mut().push_str(&lit[1..]);
                    *lc += c;
                },
                _ => {
                    acc.push(expr);
                }
            }
            acc
        });
    exprs.shrink_to_fit();
    RuleData {
        exprs,
        colors,
        group_count: None,
        regexp,
        attrs,
    }
}

fn unicode_ident(s: &str, i: usize) -> RuleResult<&str> {
    macro_rules! err { () => { return RuleResult::Failed }; }
    let iter = &mut s[i..].char_indices();
    let Some((_, ch)) = iter.next() else { err!() };
    if ch != '_' && !unicode_ident::is_xid_start(ch) { err!() }
    loop {
        let (end, ch) = iter.next()
            .unwrap_or((s.len(), ' '));
        if ch != '-' && ch != '_' && !unicode_ident::is_xid_continue(ch) {
            return RuleResult::Matched(i+end, &s[i..][..end]);
        }
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
        = #{unicode_ident}
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
        = quiet!{
            s:$("0" / !"0" ['0'..='9']+)
            {?
                s.parse()
                    .ok()
                    .filter(|&n| n < 100000)
                    .ok_or("")
            }
        }
        / expected!("number(0..100000)")

    rule color() -> (u32, Color<'input>)
        = "$" n:unum() _ ":" _
        res:( pats:pattern_group()  { pats.into() }
            / name:eident()         { name.into() }
        ) { (n, res) }

    pub rule colors() -> Vec<Option<Vec<Color<'input>>>>
        = colors:color() ** _
        {
            let mut res = Vec::new();
            for (id, color) in colors {
                let id = id as usize;
                res.extend((res.len()..=id).map(|_| None));
                res[id].get_or_insert(vec![]).push(color);
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

    rule expr_sugar() -> Expr<'input>
        = "|" { Expr::Literal("/|/".into(), 0) }
        / "(?:" { Expr::Literal("/(?:/".into(), 0) }
        / "($" color:eident() { Expr::ColorGroup(color) }
        / "(" { Expr::Literal("/(/".into(), 1) }
        / ")"
            x:( "{" a:unum() _ "," _ b:unum() "}"
                    { format!("/){{{a},{b}}}/").into() }
              / "{" a:unum() _ "," _ "}"
                    { format!("/){{{a},}}/").into() }
              / "{" _ "," _ b:unum() "}"
                    { format!("/){{,{b}}}/").into() }
              / "{" n:unum() "}"
                    { format!("/){{{n}}}/").into() }
              / "*" { "/)*/".into() }
              / "+" { "/)+/".into() }
              / "?" { "/)?/".into() }
              /     { "/)/".into() }
            )
            min:"?"?
            {
                let mut x: Cow<_> = x;
                if min.is_some() {
                    let ch = x.to_mut().pop().unwrap();
                    x.to_mut().push('?');
                    x.to_mut().push(ch);
                }
                Expr::Literal(x, 0)
            }

    pub rule expr() -> Expr<'input>
        = s:(regex() / string()) { Expr::Literal(s.into(), group_count(s).unwrap()) }
        / expr_sugar()
        / "keywordsToRegex" "(" _ s:string() ++ (_ ("," _)?) _ ("," _)? ")"
            {
                Expr::KwdsToRegex(s)
            }
        / "@" i:ident() { Expr::Ref(i) }
        / "&" i:ident() "(@)" { Expr::IncludeRef(i) }
        / "&" n:eident()
              c:("(" n:unum() ")" { n })?
              { Expr::Include(n, c.unwrap_or(0)) }

    rule normal_ruledata() -> RuleData<'input>
        = exprs:expr() ++ (_ ("+" _)?)
        _ attrs:attrs()
        _ colors:colors()
        { new_rule_data(false, exprs, attrs, colors) }
    rule include_pattern() -> Pattern<'input>
        = name:eident()
        { Pattern::IncludePattern(name) }
    rule normal_pattern() -> Pattern<'input>
        = exprs:expr() ++ (_ ("+" _)?)
        _ colors:colors()
        { new_rule_data(true, exprs, vec![], colors).into() }

    rule pattern_group_atom() -> Pattern<'input>
        = ":"  _ rul:normal_ruledata() { rul.into() }
        / "::" _ pat:include_pattern() { pat }
    rule pattern_group() -> Vec<Pattern<'input>>
        = "{" _ pats:(
            pat:normal_ruledata() { vec![pat.into()] }
            / pattern_group_atom() ++ _
        ) _ "}" { pats }

    pub rule mt_rule() -> Rule<'input>
        = name:ident()
        _ pats:(
            ":=" _ rul:normal_ruledata() { vec![rul.into()] }
          / ":=" _ pats:pattern_group() { pats }
          / "=" _ pat:normal_pattern() { vec![pat] }
        ) {
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
            $1: {/red_sub/} // abc

        bar := @foo + /;|\// + @foo // ...
        sugar = (&a) | (?: &b ) | (&c){2} | (&c){2 , 3} | (&d){, 3} | (&d){3 , }
        x := ($red /a/) /(b)/
            $1: blue
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

        bar:= @foo + /;|\// + @foo // ...
        //!includeEnd
        some text of end
        "#;
        println!("{src}");

        dbg!(script(src).unwrap());
    }

    #[test]
    fn ident_test() {
        let datas = [
            "foo",
            "foo-bar",
            "foo_bar",
            "foo_bar-",
            "foo__bar-",
            "foo--bar-",
            "foo-bar-你好",
            "_foo-bar-你好",
            "你好",
            "你-好",
        ];
        for data in datas {
            assert_eq!(parser::ident(data), Ok(data));
        }

        let fails = [
            "0x",
            "-0x",
            "-x",
            "-x y",
            "-x.y",
            "。",
            "0a",
        ];

        for fail in fails {
            assert!(parser::ident(fail).is_err(), "{fail:?}");
        }
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
