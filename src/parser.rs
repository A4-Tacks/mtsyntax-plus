mod context;

pub use context::*;
use either::Either::{self, Left, Right};
pub use parser::*;

use crate::{
    Color, ColorDef, Expr, OffsetGroup, PGroup, Pattern, Rule, RuleData,
};
use char_classes::FirstElem;
use peg::RuleResult;
use std::borrow::Cow;

fn new_rule_data<'a>(
    regexp: bool,
    mut exprs: Vec<Expr<'a>>,
    mut attrs: Vec<(&'a str, &'a str)>,
    mut colors: Vec<Option<ColorDef<'a>>>,
    ctx: &mut Context<'a>,
    templates: &[&str],
) -> RuleData<'a> {
    templates.iter()
        .rev()
        .copied()
        .for_each(|name|
    {
        ctx.apply_template(regexp, name, &mut exprs, &mut attrs, &mut colors);
    });

    if let Some(first) = colors.first_mut()
        .and_then(|color| color.take())
    {
        colors.insert(1, first.into());
        colors.offset_group(1);
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
            .unwrap_or((s.len()-i, ' '));
        if ch != '-' && ch != '_' && !unicode_ident::is_xid_continue(ch) {
            return RuleResult::Matched(i+end, &s[i..][..end]);
        }
    }
}

macro_rules! any {
    ($($pat:tt)*) => {
        |s, i| s[i..].first_elem()
            .filter(char_classes::any!($($pat)*))
            .map_or(RuleResult::Failed, |ch| {
                RuleResult::Matched(i+ch.len_utf8(), ())
            })
    };
}

peg::parser!(grammar parser(ctx: &mut Context<'input>) for str {
    use crate::utils::Assign;

    rule newline()
        = "\r"? "\n"

    rule comment()
        = "//" !"!" #{any!(^"\r\n")}*

    rule _()
        = quiet!{
            (
                #{any!(" \t")}+
                / comment()? newline()
            )* (comment()? ![_])?
        }

    pub rule ident() -> &'input str
        = #{unicode_ident}
        / expected!("ident")

    rule eident() -> Cow<'input, str>
        = i:ident() { format!("\"{i}\"").into() }
        / s:string() { s.into() }

    rule estr() -> Cow<'input, str>
        = s:ident() { s.into() }
        / "\"" s:str_body() "\"" { s.into() }

    rule str_body() -> &'input str
        = $(quiet!{("\\" #{any!(^"\r\n")} / #{any!(^"\"\r\n")})*})

    pub rule string() -> &'input str
        = quiet!{
            $("\"" str_body() "\"")
        }
        / expected!("string")

    pub rule regex() -> &'input str
        = quiet!{
            $("/" ("\\/" / #{any!(^"/\r\n")})+ "/")
        }
        / expected!("regex")

    pub rule unum() -> u32
        = quiet!{
            s:$("0" / !"0" #{any!("0-9")}+)
            {?
                s.parse()
                    .ok()
                    .filter(|&n| n < 100000)
                    .ok_or("")
            }
        }
        / expected!("number(0..100000)")

    rule raw_tt()
        = unum() / regex() / string() / ident()
        / #{any!("<:>@#$,+=")}
        / "(" _ (raw_tt() _)* ")"
        / "[" _ (raw_tt() _)* "]"
        / "{" _ (raw_tt() _)* "}"

    rule color() -> (u32, usize, Color<'input>)
        = "$" p:position!() n:unum() _ ":" _ res:color_content() { (n, p, res) }

    rule color_content() -> Color<'input>
        = templates:templates()
          pats:pattern_group(&templates) { pats.into() }
        / loc:position!() "parseColor" "("
            _ fg:parse_color_c()
            _ "," _ bg:parse_color_c()
            _ "," _ fmt:estr()
            _ "," _ base:estr()
            _ ")"
            { Color::ParseColor(loc, [fg, bg], [fmt, base]) }
        / name:eident()         { name.into() }

    rule parse_color_c() -> PGroup
        = "_"       { PGroup::Underline }
        / "auto"    { PGroup::Auto }
        / id:unum() { PGroup::Id(id) }

    pub rule colors() -> Vec<Option<ColorDef<'input>>>
        = colors:color() ** _
        {
            let mut res: Vec<Option<ColorDef>> = Vec::new();
            for (id, pos, color) in colors {
                res.assign(id as usize)
                    .get_or_insert_default()
                    .push((pos, color));
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
        / "(?" !("<" #{any!("a-zA-Z")})
        / #{any!(^"(/\\")}

    rule group_count_str_ig()
        = "\\\\" [^ '"']
        / "\\" [_]
        / "(?" !("<" #{any!("a-zA-Z")})
        / #{any!(^"(\"\\")}

    /// group count, contains `()` and `(?<name>)`
    pub rule group_count() -> u32
        = "/" group_count_regex_ig()*
            s:("(" (&[^ '?'] / "?<" #{any!("a-zA-Z")}) group_count_regex_ig()*)*
            "/"
            { s.len() as u32 }
        / "\"" group_count_str_ig()*
            s:("(" (&[^ '?'] / "?<" #{any!("a-zA-Z")}) group_count_str_ig()*)*
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
                    { format!("/){{0,{b}}}/").into() }
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
        = s:(regex() / string()) { Expr::Literal(s.into(), group_count(s, ctx).unwrap()) }
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
        / "include(" _ n:eident() _ c:("," _ c:unum() _ {c})? ")"
              { Expr::Include(n, c.unwrap_or(0)) }

    rule template_expr() -> Either<Expr<'input>, ()>
        = "[[" _ "]]" { Right(()) }
        / e:expr() { Left(e) }
    rule regex_template() -> RegexTemplate<'input>
        = texprs:template_expr() ++ (_ ("+" _)?)
        _ colors:colors()
        { RegexTemplate { texprs, colors } }
    rule pattern_template() -> PatternTemplate<'input>
        = texprs:template_expr() ++ (_ ("+" _)?)
        _ attrs:attrs()
        _ colors:colors()
        { PatternTemplate { base: RegexTemplate { texprs, colors }, attrs } }
    rule template_insert_regex(name: &'input str)
        = template:regex_template()
        { ctx.regex_templates.insert(name, template); }
    rule template_insert_pattern(name: &'input str)
        = template:pattern_template()
        { ctx.pattern_templates.insert(name, template); }
    #[cache]
    rule template_def()
        = "[[" _ name:ident() _ "]]" _ ":="
        ({ ctx.check_defined_name(name) })
        _ ("{" _ template_insert_pattern(name) _ "}"
               / template_insert_pattern(name))
        / "[[" _ name:ident() _ "]]" _ "="
        ({ ctx.check_defined_name(name) })
        _ ("{" _ template_insert_regex(name) _ "}"
               / template_insert_regex(name))
    rule templates() -> Vec<&'input str>
        = p:position!() t:("[[" _ nm:ident() _ "]]" _ {nm})*
        {
            if !t.is_empty() { ctx.last_template_use = p.into() }
            t
        }

    rule normal_ruledata(templates: &[&str]) -> RuleData<'input>
        = exprs:expr() ++ (_ ("+" _)?)
        _ attrs:attrs()
        _ colors:colors()
        { new_rule_data(false, exprs, attrs, colors, ctx, templates) }
    rule include_pattern() -> Pattern<'input>
        = name:eident()
        { Pattern::IncludePattern(name) }
    rule normal_pattern(templates: &[&str]) -> Pattern<'input>
        = exprs:expr() ++ (_ ("+" _)?)
        _ colors:colors()
        { new_rule_data(true, exprs, vec![], colors, ctx, templates).into() }
    rule raw_pattern() -> Pattern<'input>
        = s:$(&"{" raw_tt()) (_ ",")? { Pattern::Raw(s.into()) }

    rule pattern_group_atom(ext_templates: &[&str]) -> Pattern<'input>
        = "::" _ pat:include_pattern() { pat }
        / sub_templates:templates()
          ":"  _ rul:normal_ruledata(&ext_templates.iter().copied()
                .chain(sub_templates)
                .collect::<Vec<_>>())
          { rul.into() }
        / raw_pattern()
    rule pattern_group(templates: &[&str]) -> Vec<Pattern<'input>>
        = "{" _ pats:(
            pat:normal_ruledata(templates) { vec![pat.into()] }
            / pattern_group_atom(templates) ++ _
        ) _ "}" { pats }

    pub rule mt_rule() -> Rule<'input>
        = templates:templates()
          name:ident()
        _ pats:(
            ":=" _ rul:normal_ruledata(&templates) { vec![rul.into()] }
          / ":=" _ pats:pattern_group(&templates) { pats }
          / "=" _ pat:normal_pattern(&templates) { vec![pat] }
        )
        {
            Rule {
                name,
                pats,
            }
        }
        / template_def() _ rul:mt_rule() { rul }

    pub rule rule_list() -> Vec<Rule<'input>>
        = ({ctx.init()})
        _ s:mt_rule() ++ _ _ { s }

    pub rule script() -> (&'input str, Vec<Rule<'input>>, &'input str)
        =
        begin:$(
            (
                #{any!(" \t")}*
                !"//!includeBegin"
                #{any!(^"\r\n")}*
                newline()
            )*
            #{any!(" \t")}*
        )
        "//!includeBegin" newline()
        rules:rule_list()
        #{any!(" \t")}* "//!includeEnd" newline()
        end:$([_]*)
        {
            (begin, rules, end)
        }
});

#[cfg(test)]
mod tests {
    use crate::{build, BuildContext, OutputContext};

    use super::*;

    fn ctx() -> Context<'static> {
        Default::default()
    }

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

        let rules = parser::rule_list(src, &mut ctx())
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
        to = include(extern, 2) + include("foo bar", 3)
        to_z = include(extern) + include("foo bar")

        bar:= @foo + /;|\// + @foo // ...
        //!includeEnd
        some text of end
        "#;
        println!("{src}");

        dbg!(script(src, &mut ctx()).unwrap());
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
            assert_eq!(parser::ident(data, &mut ctx()), Ok(data));
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
            assert!(parser::ident(fail, &mut ctx()).is_err(), "{fail:?}");
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
            let c = match parser::group_count(src, &mut ctx()) {
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
