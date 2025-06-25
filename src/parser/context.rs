use std::{collections::HashMap, mem::take};

use either::Either;
use line_column::line_column;

use crate::{utils::Assign, ColorDef, Expr, OffsetGroup};

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegexTemplate<'a> {
    // FIXME: 未来可以支持多个模板命名参数, 可以把unit改成&str
    pub texprs: Vec<Either<Expr<'a>, ()>>,
    pub colors: Vec<Option<ColorDef<'a>>>,
}

impl<'a> RegexTemplate<'a> {
    pub fn apply(
        &self,
        exprs: &mut Vec<Expr<'a>>,
        colors: &mut Vec<Option<ColorDef<'a>>>,
    ) {
        let exprs_arg = take(exprs);
        let colors_arg = take(colors);
        let input_groupc = exprs_arg.iter()
            .map(|expr| expr.hard_group_count())
            .sum::<u32>();

        *exprs = self.gen_exprs(&exprs_arg);

        let groups_map = self.groups_map(input_groupc);

        if let Some(Some(first)) = colors_arg.first() {
            colors.assign(0)
                .get_or_insert_default()
                .extend(first.iter().cloned());
        }
        self.arg_offsets(input_groupc).for_each(|offset| {
            colors_arg.iter()
                .enumerate()
                .skip(1)
                .filter_map(|(i, ele)| Some((i, ele.as_ref()?)))
                .for_each(|(i, color)|
            {
                let mut color = color.clone();
                color.offset_group(offset);
                colors.assign(i+usize::try_from(offset).unwrap())
                    .get_or_insert_default()
                    .extend(color);
            });
        });

        self.colors.iter()
            .enumerate()
            .filter_map(|(i, ele)| Some((i, ele.as_ref()?)))
            .for_each(|(i, color)|
        {
            let input_i = groups_map[i];
            let mut color = color.clone();
            color.offset_group(input_i.checked_sub(i.try_into().unwrap()).unwrap());
            colors.assign(input_i.try_into().unwrap())
                .get_or_insert_default()
                .extend(color.iter().cloned());
        });
    }

    fn arg_offsets(&self, input_groupc: u32) -> impl Iterator<Item = u32> + '_ {
        let mut offset = 0;

        self.texprs.iter()
            .filter_map(move |texpr|
        {
            texpr.as_ref()
                .map_left(|expr| offset += expr.hard_group_count())
                .is_right()
                .then_some(offset)
                .inspect(|_| offset += input_groupc)
        })
    }

    fn gen_exprs(&self, exprs_arg: &[Expr<'a>]) -> Vec<Expr<'a>> {
        self.texprs.iter().flat_map(|it| {
            it.is_right().then(|| exprs_arg.iter().cloned())
                .into_iter()
                .flatten()
                .chain(it.as_ref().left().cloned())
        }).collect()
    }

    fn groups_map(&self, input_groupc: u32) -> Vec<u32> {
        let mut group_map = vec![0];
        let mut offset = u32::try_from(group_map.len()).unwrap();

        for expr in &self.texprs {
            match expr {
                Either::Left(expr) => {
                    let count = expr.hard_group_count();
                    group_map.extend((offset..).take(count.try_into().unwrap()));
                    offset += count;
                },
                Either::Right(()) => offset += input_groupc,
            }
        }

        group_map
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PatternTemplate<'a> {
    pub base: RegexTemplate<'a>,
    pub attrs: Vec<(&'a str, &'a str)>,
}

impl<'a> PatternTemplate<'a> {
    pub fn apply(
        &self,
        exprs: &mut Vec<Expr<'a>>,
        attrs: &mut Vec<(&'a str, &'a str)>,
        colors: &mut Vec<Option<ColorDef<'a>>>,
    ) {
        self.base.apply(exprs, colors);
        for attr in &self.attrs {
            if attrs.contains(attr) { continue }
            attrs.push(*attr);
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context<'a> {
    pub source: &'a str,
    pub last_template_use: Option<usize>,
    pub regex_templates: HashMap<&'a str, RegexTemplate<'a>>,
    pub pattern_templates: HashMap<&'a str, PatternTemplate<'a>>,
}

impl<'a> Context<'a> {
    pub fn init(&mut self) {
        self.regex_templates.clear();
        self.pattern_templates.clear();
    }

    pub fn check_defined_name(&self, name: &str) {
        if self.pattern_templates.contains_key(name)
            || self.regex_templates.contains_key(name)
        {
            panic!("Duplicate define template `{name}`")
        }
    }

    fn loc_hint(&self) -> Option<String> {
        let i = self.last_template_use?;
        self.source.get(i..)?;
        let (line, column) = line_column(self.source, i);
        Some(format!(" near {line}:{column}"))
    }

    pub fn get_regexp_template(&self, name: &str) -> &RegexTemplate<'a> {
        self.regex_templates.get(name).unwrap_or_else(|| {
            let hint = self.pattern_templates.contains_key(name).then(|| {
                format!(", but pattern template `{name}` exists")
            }).unwrap_or_default();
            let loc = self.loc_hint().unwrap_or_default();

            panic!("Undefined regexp template name `{}`{loc}{hint}", name)
        })
    }

    pub fn get_pattern_template(
        &self,
        name: &str,
    ) -> &PatternTemplate<'a> {
        self.pattern_templates.get(name).unwrap_or_else(|| {
            let hint = self.regex_templates.contains_key(name).then(|| {
                format!(", but regexp template `{name}` exists")
            }).unwrap_or_default();
            let loc = self.loc_hint().unwrap_or_default();

            panic!("Undefined pattern template name `{}`{loc}{hint}", name)
        })
    }

    pub fn apply_template(
        &mut self,
        regexp: bool,
        name: &str,
        exprs: &mut Vec<Expr<'a>>,
        attrs: &mut Vec<(&'a str, &'a str)>,
        colors: &mut Vec<Option<ColorDef<'a>>>,
    ) {
        if regexp {
            let template = self.get_regexp_template(name);
            template.apply(exprs, colors);
        } else {
            let template = self.get_pattern_template(name);
            template.apply(exprs, attrs, colors);
        }
    }
}
