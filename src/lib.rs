use std::{
    borrow::Cow,
    cell::Cell,
    collections::{BTreeMap, HashMap},
    fmt::{self, Display},
    io,
    fmt::Write,
    ops::{Add, AddAssign},
};

mod utils;
pub mod parser;

macro_rules! fa {
    (;$($t:tt)*) => {
        ::std::format_args!("{}\n", ::std::format_args!($($t)*))
    };
    ($($t:tt)*) => {
        ::std::format_args!($($t)*)
    };
}
macro_rules! assert_match {
    ($e:expr, $pat:pat $(if $cond:expr)? $(,)?) => {
        match $e {
            $pat $(if $cond)? => {},
            ref val => {
                panic!(
                    "assert matches ({}) failed!\n left: {:?}\nright: {}",
                    ::core::stringify!($e),
                    val,
                    ::core::stringify!($pat $(if $cond)?),
                )
            },
        }
    };
    ($e:expr, $pat:pat $(if $cond:expr)?, $($f:tt)*) => {
        match $e {
            $pat $(if $cond)? => {},
            ref val => {
                panic!(
                    "assert matches ({}) failed, `{}`\n left: {:?}\nright: {}",
                    ::core::stringify!($e),
                    ::core::format_args!($($f)*),
                    val,
                    ::core::stringify!($pat $(if $cond)?),
                )
            },
        }
    };
}

type Loc = usize;

#[derive(Debug)]
pub enum Error {
    UndefinedRef(String),
    DuplicateDefine(String),
    RefNotARegexp(String),
    InvalidGroupId(Loc, u32),
    IOError(io::Error),
}
impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Self::IOError(e)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr<'a> {
    /// regexp or string literal, and group count.
    /// e.g /ab/ "abi"
    Literal(Cow<'a, str>, u32),
    /// like Literal, but not consume local color and output is capture group open
    ColorGroup(Cow<'a, str>),
    /// e.g keywordsToRegex("ab cd", "ef gh")
    KwdsToRegex(Vec<&'a str>),
    /// rule reference, e.g @foo
    Ref(&'a str),
    /// Include extern regexp, and group count
    /// e.g `&foo` `&foo(2)`
    Include(Cow<'a, str>, u32),
    /// Include ref, but not build colors
    /// e.g `&foo(@)`
    IncludeRef(&'a str),
}
impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(lit, _) => f.write_str(lit),
            Expr::ColorGroup(_) => f.write_str("/(/"),
            Expr::KwdsToRegex(kwds) => {
                write!(f, "keywordsToRegex(")?;
                if let Some(first) = kwds.first() {
                    write!(f, "{first}")?;
                    for &kwd in kwds[1..].iter() {
                        write!(f, " {kwd}")?;
                    }
                }
                write!(f, ")")
            },
            Expr::Ref(name) | Expr::IncludeRef(name)
                => write!(f, "include(\"{name}\")"),
            Expr::Include(name, _) => write!(f, "include({name})"),
        }
    }
}
impl Expr<'_> {
    /// 统计组数, 如果是未确定的通过给定的函数获取组数
    pub fn get_group_count<F>(&self, f: F) -> Result<u32>
    where F: FnOnce(&str) -> Option<u32>,
    {
        Ok(match self {
            | &Expr::ColorGroup(_) => 1,
            | &Expr::Include(_, c)
            | &Expr::Literal(_, c) => c,
            | &Expr::KwdsToRegex(_) => 0,
            | &Expr::Ref(name)
            | &Expr::IncludeRef(name) => f(name)
                .ok_or_else(|| Error::UndefinedRef(name.into()))?,
        })
    }

    /// 统计本地组数, 远程组或者无法本地访问的如 [`Expr::ColorGroup`] 均为 0
    pub fn hard_group_count(&self) -> u32 {
        match *self {
            Expr::Include(_, c) | Expr::Literal(_, c) => c,
            Expr::ColorGroup(_) | Expr::KwdsToRegex(_) => 0,
            Expr::Ref(_) | Expr::IncludeRef(_) => 0,
        }
    }

    fn build_colors_map<F>(
        &self,
        octx: &mut OutputContext<'_, F>,
        ctx: &BuildContext<'_>,
        map: &mut Vec<u32>,
        cur_color: &mut u32,
    ) -> Result<()>
    where F: FnMut(std::fmt::Arguments<'_>) -> io::Result<()>,
    {
        match self {
            &Expr::Include(_, c) |
            &Expr::Literal(_, c) => {
                for _ in 0..c {
                    map.push(*cur_color);
                    *cur_color += 1;
                }
            },
            &Expr::ColorGroup(_) => {
                *cur_color += 1;
            },
            &Expr::KwdsToRegex(_) => (),
            &Expr::Ref(name) => {
                let rule = ctx.get_rule(name)?;
                rule.build_colors_map(octx, ctx, &mut vec![], cur_color)?
            },
            &Expr::IncludeRef(name) => {
                let rule = ctx.get_rule(name)?;
                rule.build_colors_map(octx, ctx, map, cur_color)?
            },
        }
        Ok(())
    }

    pub fn build_colors<'a, F, C, CI>(
        &self,
        octx: &mut OutputContext<'_, F>,
        ctx: &BuildContext<'_>,
        mut color: C,
        map: &[u32],
    ) -> Result<()>
    where F: FnMut(std::fmt::Arguments<'_>) -> io::Result<()>,
          C: Iterator<Item = Option<CI>>,
          CI: Iterator<Item = &'a Color<'a>>,
    {
        match self {
            | &Expr::KwdsToRegex(_) => (),
            | &Expr::Ref(name) => {
                let rule = ctx.get_rule(name)?;
                rule.build_colors(octx, ctx, ctx.current_color.get())?
            },
            | &Expr::IncludeRef(name) => {
                let rule = ctx.get_rule(name)?;

                let cur_color = ctx.current_color.get();
                ctx.current_color.set(cur_color + rule.group_count.unwrap());
            },
            | &Expr::ColorGroup(ref color) => {
                let id = ctx.current_color.get();
                octx.sep()?;
                octx.output(fa!("{id}: {color}"))?;
                ctx.current_color.set(id + 1);
            },
            | &Expr::Literal(_, count)
            | &Expr::Include(_, count) => {
                let cur_color = ctx.current_color.get();

                for i in 0..count {
                    let id = i + cur_color;
                    if let Some(Some(subcolors)) = color.next() {
                        for color in subcolors {
                            octx.sep()?;
                            octx.output(fa!("{id}: "))?;
                            color.build(ctx, octx, map)?;
                        }
                    }
                }

                ctx.current_color.set(cur_color + count);
            },
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum PGroup {
    Underline,
    Auto,
    Id(u32),
}
impl PGroup {
    fn map_id<F>(self, f: F) -> Result<Self>
    where F: FnOnce(u32) -> Result<u32>,
    {
        Ok(match self {
            PGroup::Id(id) => Self::Id(f(id)?),
            _ => self,
        })
    }
}
impl Display for PGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PGroup::Underline => write!(f, "_"),
            PGroup::Auto => write!(f, "auto"),
            PGroup::Id(id) => write!(f, "{id}"),
        }
    }
}
impl Add<u32> for PGroup {
    type Output = Self;

    fn add(mut self, rhs: u32) -> Self::Output {
        self += rhs;
        self
    }
}
impl AddAssign<u32> for PGroup {
    fn add_assign(&mut self, rhs: u32) {
        if let PGroup::Id(id) = self { *id += rhs }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Color<'a> {
    Color(Cow<'a, str>),
    ParseColor(Loc, [PGroup; 2], [Cow<'a, str>; 2]),
    Pattern(Vec<Pattern<'a>>),
}

impl<'a> From<Cow<'a, str>> for Color<'a> {
    fn from(v: Cow<'a, str>) -> Self {
        Self::Color(v)
    }
}
impl<'a> From<Vec<Pattern<'a>>> for Color<'a> {
    fn from(v: Vec<Pattern<'a>>) -> Self {
        Self::Pattern(v)
    }
}
impl<'a> Color<'a> {
    fn build<F>(
        &self,
        ctx: &BuildContext<'a>,
        octx: &mut OutputContext<'_, F>,
        map: &[u32],
    ) -> Result<()>
    where F: FnMut(std::fmt::Arguments<'_>) -> io::Result<()>,
    {
        match self {
            Color::Color(color) => {
                octx.output(fa!("{color}"))?;
            },
            Color::ParseColor(loc, [fg, bg], [fmt, base]) => {
                let f = |id| {
                    map.get(id as usize)
                        .copied()
                        .ok_or(Error::InvalidGroupId(*loc, id))
                };
                let fg = fg.map_id(f)?;
                let bg = bg.map_id(f)?;
                octx.output(fa!("\"parseColor({fg},{bg},{fmt},{base})\""))?;
            },
            Color::Pattern(pats) => {
                match &pats[..] {
                    [] => unreachable!("pats is empty"),
                    [pat] => {
                        pat.build(ctx, octx)?;
                    },
                    pats => {
                        let mut fst = true;
                        octx.with_block(['[', ']'], |octx| {
                            for pat in pats {
                                if fst { octx.newline()?; fst = false }
                                pat.build(ctx, octx)?;
                            }
                            Result::Ok(())
                        })??;
                    },
                }
            },
        }
        Ok(())
    }

    pub fn as_color(&self) -> Option<&Cow<'a, str>> {
        if let Self::Color(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_pattern(&self) -> Option<&Vec<Pattern<'a>>> {
        if let Self::Pattern(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

type ColorDef<'a> = Vec<(Loc, Color<'a>)>;

trait OffsetGroup {
    fn offset_group(&mut self, rhs: u32);
}
impl OffsetGroup for Color<'_> {
    fn offset_group(&mut self, rhs: u32) {
        if let Color::ParseColor(_, [fg, bg], _) = self {
            *fg += rhs;
            *bg += rhs;
        }
    }
}
impl OffsetGroup for ColorDef<'_> {
    fn offset_group(&mut self, rhs: u32) {
        self.iter_mut()
            .for_each(|(_, color)| color.offset_group(rhs));
    }
}
impl<T: OffsetGroup> OffsetGroup for [T] {
    fn offset_group(&mut self, rhs: u32) {
        self.iter_mut()
            .for_each(|color| color.offset_group(rhs));
    }
}
impl<T: OffsetGroup> OffsetGroup for Option<T> {
    fn offset_group(&mut self, rhs: u32) {
        if let Some(color) = self {
            color.offset_group(rhs);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RuleData<'a> {
    exprs: Vec<Expr<'a>>,
    colors: Vec<Option<ColorDef<'a>>>,
    group_count: Option<u32>,
    regexp: bool,
    attrs: Vec<(&'a str, &'a str)>,
}

impl RuleData<'_> {
    pub fn update_group_count<F>(&mut self, mut f: F) -> Result<()>
    where F: FnMut(&str) -> Option<u32>
    {
        if self.group_count.is_none() {
            self.group_count = self.exprs.iter()
                .try_fold(0, |acc, exp| {
                    let count = exp.get_group_count(&mut f)?;
                    Ok::<_, Error>(acc + count)
                })?.into();
        }
        Ok(())
    }

    pub fn build_colors<F>(
        &self,
        octx: &mut OutputContext<'_, F>,
        ctx: &BuildContext<'_>,
        base_color: u32,
    ) -> Result<()>
    where F: FnMut(std::fmt::Arguments<'_>) -> io::Result<()>,
    {
        assert_match!(self.colors[..], [] | [None, ..]);
        assert_ne!(self.exprs.len(), 0);

        let mut color = self.colors
            .iter()
            .skip(1)
            .map(Option::as_deref);
        let mut sub_color = color.by_ref()
            .map(|it| it.map(|it| it
                    .iter().map(|it| &it.1)));

        let map = &mut vec![u32::MAX];
        self.build_colors_map(octx, ctx, map, &mut {base_color})?;
        for expr in &self.exprs {
            expr.build_colors(octx, ctx, sub_color.by_ref(), map)?;
        }

        if let Some(&(index, _)) = color.flatten().flatten().next() {
            octx.warn(index, "unused color");
        }

        Ok(())
    }

    pub fn build_colors_map<F>(
        &self,
        octx: &mut OutputContext<'_, F>,
        ctx: &BuildContext<'_>,
        map: &mut Vec<u32>,
        cur_color: &mut u32,
    ) -> Result<()>
    where F: FnMut(std::fmt::Arguments<'_>) -> io::Result<()>,
    {
        for ele in &self.exprs {
            ele.build_colors_map(octx, ctx, map, cur_color)?;
        }
        Ok(())
    }

    pub fn build<F>(
        &self,
        ctx: &BuildContext<'_>,
        octx: &mut OutputContext<'_, F>,
    ) -> Result<()>
    where F: FnMut(std::fmt::Arguments<'_>) -> io::Result<()>,
    {
        if self.regexp {
            out_exprs(&self.exprs, octx)?;
        } else {
            octx.with_sep_block(['{', '}'], |octx| {
                octx.output(fa!("match: "))?;
                out_exprs(&self.exprs, octx)?;

                for &(attr, val) in &self.attrs {
                    octx.sep()?;
                    octx.output(fa!("{attr}: {val}"))?;
                }

                ctx.current_color.set(1);

                self.build_colors(octx, ctx, 1)?;
                Result::Ok(())
            })??;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pattern<'a> {
    Normal(RuleData<'a>),
    IncludePattern(Cow<'a, str>),
    Raw(Cow<'a, str>),
}
impl<'a> From<RuleData<'a>> for Pattern<'a> {
    fn from(value: RuleData<'a>) -> Self {
        Self::Normal(value)
    }
}
impl Pattern<'_> {
    pub fn update_group_count<F>(&mut self, f: F) -> Result<()>
    where F: FnMut(&str) -> Option<u32>
    {
        match self {
            Pattern::Normal(data) => data.update_group_count(f),
            Pattern::IncludePattern(_) | Pattern::Raw(_) => Ok(()),
        }
    }

    pub fn build<F>(
        &self,
        ctx: &BuildContext<'_>,
        octx: &mut OutputContext<'_, F>,
    ) -> Result<()>
    where F: FnMut(std::fmt::Arguments<'_>) -> io::Result<()>,
    {
        match self {
            Pattern::Normal(data) => data.build(ctx, octx)?,
            Pattern::IncludePattern(name) => {
                octx.output(fa!("{{include: {name}}}"))?;
            },
            Pattern::Raw(raw) => {
                octx.output(fa!("{raw}"))?;
            },
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Rule<'a> {
    name: &'a str,
    pats: Vec<Pattern<'a>>,
}
impl Rule<'_> {
    pub fn update_group_count<F>(&mut self, mut f: F) -> Result<()>
    where F: FnMut(&str) -> Option<u32>
    {
        self.pats.iter_mut()
            .try_for_each(|pat| {
                pat.update_group_count(&mut f)
            })
    }

    pub fn build<F>(
        &self,
        ctx: &BuildContext<'_>,
        octx: &mut OutputContext<'_, F>,
    ) -> Result<()>
    where F: FnMut(std::fmt::Arguments<'_>) -> io::Result<()>,
    {
        octx.output(fa!("\"{}\": ", self.name))?;
        match &self.pats[..] {
            [one] => {
                one.build(ctx, octx)
            },
            pats => {
                octx.with_block(['[', ']'], |octx| {
                    if let Some(pat) = pats.first() {
                        pat.build(ctx, octx)?;
                    }
                    for pat in pats.iter().skip(1) {
                        octx.newline()?;
                        pat.build(ctx, octx)?;
                    }
                    Ok(())
                })?
            },
        }
    }
}

#[derive(Debug)]
pub struct BuildContext<'a> {
    current_color: Cell<u32>,
    rule_map: HashMap<String, RuleData<'a>>,
}

impl Default for BuildContext<'static> {
    fn default() -> Self {
        Self {
            current_color: 0.into(),
            rule_map: HashMap::new(),
        }
    }
}

impl BuildContext<'_> {
    fn get_rule(&self, name: &str) -> Result<&RuleData> {
        let rule = self.rule_map.get(name)
            .ok_or_else(|| Error::UndefinedRef(name.into()))
            .and_then(|data| {
                data.regexp.then_some(data)
                    .ok_or_else(|| Error::RefNotARegexp(name.into()))
            })?;
        Ok(rule)
    }
}

#[derive(Debug)]
pub struct OutputContext<'a, F = fn(fmt::Arguments<'_>) -> io::Result<()>> {
    pub newline_str: &'a str,
    pub indent_str: &'a str,
    pub indent_level: u32,
    pub compact: bool,
    messages: BTreeMap<Loc, Cow<'a, str>>,
    output: F,
}

impl<'a, F> OutputContext<'a, F>
where F: FnMut(fmt::Arguments<'_>) -> io::Result<()>,
{
    pub fn new(output: F) -> Self {
        Self {
            newline_str: "\n",
            indent_str: "    ",
            indent_level: 0,
            compact: false,
            messages: BTreeMap::new(),
            output,
        }
    }

    pub fn newline(&mut self) -> io::Result<()> {
        (self.output)(fa!("{}", self.newline_str))?;
        self.indent()
    }

    pub fn sep(&mut self) -> io::Result<()> {
        if self.compact {
            self.output(fa!(", "))
        } else {
            self.newline()
        }
    }

    pub fn splr(&mut self, s: impl Display) -> io::Result<()> {
        if self.compact {
            self.output(fa!("{s}"))
        } else {
            self.output(fa!(" {s} "))
        }
    }

    pub fn output(&mut self, args: fmt::Arguments<'_>) -> io::Result<()> {
        (self.output)(args)
    }

    pub fn outputln(&mut self, args: fmt::Arguments<'_>) -> io::Result<()> {
        (self.output)(args)?;
        self.newline()
    }

    pub fn indent(&mut self) -> io::Result<()> {
        for _ in 0..self.indent_level {
            (self.output)(fa!("{}", self.indent_str))?;
        }
        Ok(())
    }

    pub fn with_indent<F1, R>(&mut self, f: F1) -> R
    where F1: FnOnce(&mut Self) -> R,
    {
        self.indent_level += 1;
        let res = f(self);
        self.indent_level -= 1;
        res
    }

    pub fn with_block<F1, R>(&mut self, ch: [char; 2], f: F1) -> io::Result<R>
    where F1: FnOnce(&mut Self) -> R,
    {
        let res = self.with_indent(|this| {
            (this.output)(fa!("{}", ch[0]))?;
            this.newline()?;

            io::Result::Ok(f(this))
        })?;

        self.newline()?;
        (self.output)(fa!("{}", ch[1]))?;

        Ok(res)
    }

    pub fn with_sep_block<F1, R>(&mut self, ch: [char; 2], f: F1) -> io::Result<R>
    where F1: FnOnce(&mut Self) -> R,
    {
        let res = self.with_indent(|this| {
            (this.output)(fa!("{}", ch[0]))?;
            if !this.compact {
                this.newline()?;
            }

            io::Result::Ok(f(this))
        })?;

        if !self.compact {
            self.newline()?;
        }
        (self.output)(fa!("{}", ch[1]))?;

        Ok(res)
    }

    pub fn warn(&mut self, index: Loc, s: impl Into<Cow<'a, str>>) {
        self.messages.entry(index)
            .or_insert_with(|| s.into());
    }

    pub fn warnings(&mut self, source: &str) -> String {
        let mut warns = String::new();
        for (&index, s) in &self.messages {
            let (line, col) = line_column::line_column(source, index);
            writeln!(&mut warns, "Warn[{line}:{col}]: {s}").unwrap();
        }
        warns
    }
}
impl Default for OutputContext<'static> {
    fn default() -> Self {
        Self::new(|args| {
            print!("{args}");
            Ok(())
        })
    }
}

/// 融合相邻项并输出结果
fn out_exprs<'a, F, I>(exprs: I, octx: &mut OutputContext<'_, F>) -> io::Result<()>
where F: FnMut(std::fmt::Arguments<'_>) -> io::Result<()>,
      I: IntoIterator<Item = &'a Expr<'a>>,
{
    let exprs = exprs.into_iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>();

    let mut iter = exprs.into_iter();
    let first = iter.next().unwrap();
    let end = iter.try_fold(first, |mut a, b| {
        io::Result::Ok(if let Some('/' | '"') = a.chars()
            .next_back()
            .zip(b.chars().next())
            .and_then(|(a, b)| (a == b).then_some(a))
        {
            a.pop().unwrap();
            a.push_str(&b[1..]);
            a
        } else {
            octx.output(fa!("{a}"))?;
            octx.splr('+')?;
            b
        })
    })?;
    octx.output(fa!("{}", end))
}

pub fn build<'a, I, F>(
    rules: I,
    octx: &mut OutputContext<'_, F>,
    ctx: &mut BuildContext<'a>,
) -> Result<()>
where I: IntoIterator<Item = Rule<'a>>,
      F: FnMut(std::fmt::Arguments<'_>) -> io::Result<()>,
{
    for mut rule in rules {
        rule.update_group_count(|name| {
            ctx.rule_map.get(name)
                .map(|rule| rule.group_count.unwrap())
        })?;

        rule.build(ctx, octx)?;
        octx.newline()?;

        if let Some(Pattern::Normal(data))
            = rule.pats.into_iter().next()
        {
            if ctx.rule_map
                .insert(rule.name.into(), data)
                .is_some()
            {
                return Err(Error::DuplicateDefine(rule.name.into()));
            }
        }
    }
    Ok(())
}
