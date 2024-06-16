use std::{borrow::Cow, cell::Cell, collections::HashMap, fmt, io};

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

#[derive(Debug)]
pub enum Error {
    UndefineRef(String),
    RepeatDefineName(String),
    RefNotARegexp(String),
    IOError(io::Error),
}
impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Self::IOError(e)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Expr<'a> {
    /// regexp or string literal, and group count.
    /// e.g /ab/ "abi"
    Literal(Cow<'a, str>, u32),
    /// e.g keywordsToRegex("ab cd", "ef gh")
    KwdsToRegex(Vec<&'a str>),
    /// rule reference, e.g @foo
    Ref(&'a str),
    /// Include extern regexp, and group count
    /// e.g `&foo` `&foo(2)`
    Include(Cow<'a, str>, u32),
}
impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(lit, _) => f.write_str(lit),
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
            Expr::Ref(name) => write!(f, "include(\"{name}\")"),
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
            | &Expr::Include(_, c)
            | &Expr::Literal(_, c) => c,
            | &Expr::KwdsToRegex(_) => 0,
            | &Expr::Ref(name) => f(name)
                .ok_or_else(|| Error::UndefineRef(name.into()))?,
        })
    }

    pub fn build_colors<'a, F, C>(
        &self,
        octx: &mut OutputContext<'_, F>,
        ctx: &BuildContext<'_>,
        mut color: C,
    ) -> Result<()>
    where F: FnMut(std::fmt::Arguments<'_>) -> io::Result<()>,
          C: Iterator<Item = Option<&'a str>>,
    {
        match self {
            | &Expr::KwdsToRegex(_) => (),
            | &Expr::Ref(name) => {
                let rule = ctx.rule_map.get(name)
                    .ok_or_else(|| Error::UndefineRef(name.into()))
                    .and_then(|data| {
                        data.regexp.then_some(data)
                            .ok_or_else(|| Error::RefNotARegexp(name.into()))
                    })?;
                rule.build_colors(octx, ctx)?
            },
            | &Expr::Literal(_, count)
            | &Expr::Include(_, count) => {
                let cur_color = ctx.current_color.get();

                for i in 0..count {
                    let id = i + cur_color;
                    if let Some(Some(color)) = color.next() {
                        octx.newline()?;
                        octx.output(fa!("{id}: {color}"))?;
                    }
                }

                ctx.current_color.set(cur_color + count);
            },
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct RuleData<'a> {
    exprs: Vec<Expr<'a>>,
    colors: Vec<Option<&'a str>>,
    group_count: Option<u32>,
    regexp: bool,
    attrs: Vec<(&'a str, &'a str)>,
}

impl<'a> RuleData<'a> {
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
    ) -> Result<()>
    where F: FnMut(std::fmt::Arguments<'_>) -> io::Result<()>,
    {
        assert_match!(self.colors[..], [] | [None, ..]);
        assert_ne!(self.exprs.len(), 0);

        let mut color = self.colors
            .iter()
            .skip(1)
            .map(Option::as_deref);

        for expr in &self.exprs {
            expr.build_colors(octx, ctx, color.by_ref())?;
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
            for expr in self.exprs.iter().take(self.exprs.len() - 1) {
                octx.output(fa!("{expr} + "))?;
            }
            octx.output(fa!("{}", self.exprs.last().unwrap()))?;
        } else {
            octx.with_block(['{', '}'], |octx| {
                octx.output(fa!("match: "))?;
                for expr in self.exprs.iter().take(self.exprs.len() - 1) {
                    octx.output(fa!("{expr} + "))?;
                }
                octx.output(fa!("{}", self.exprs.last().unwrap()))?;

                for &(attr, val) in &self.attrs {
                    octx.newline()?;
                    octx.output(fa!("{attr}: {val}"))?;
                }

                ctx.current_color.set(1);

                self.build_colors(octx, ctx)?;
                Result::Ok(())
            })??;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Pattern<'a> {
    Normal(RuleData<'a>),
    IncludePattern(Cow<'a, str>),
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
            Pattern::IncludePattern(_) => Ok(()),
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
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Rule<'a> {
    name: &'a str,
    pats: Vec<Pattern<'a>>,
}
impl<'a> Rule<'a> {
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

#[derive(Debug)]
pub struct OutputContext<'a, F = fn(fmt::Arguments<'_>) -> io::Result<()>> {
    pub newline_str: &'a str,
    pub indent_str: &'a str,
    pub indent_level: u32,
    output: F,
}

impl<F> OutputContext<'_, F>
where F: FnMut(fmt::Arguments<'_>) -> io::Result<()>,
{
    pub fn new(output: F) -> Self {
        Self {
            newline_str: "\n",
            indent_str: "    ",
            indent_level: 0,
            output,
        }
    }

    pub fn newline(&mut self) -> io::Result<()> {
        (self.output)(fa!("{}", self.newline_str))?;
        self.indent()
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
}
impl Default for OutputContext<'static> {
    fn default() -> Self {
        Self::new(|args| {
            print!("{args}");
            Ok(())
        })
    }
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
                return Err(Error::RepeatDefineName(rule.name.into()));
            }
        }
    }
    Ok(())
}
