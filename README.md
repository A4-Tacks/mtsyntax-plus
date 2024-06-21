MT管理器语法文件强化语法, 主要作用就是可以自动展开regexp include定义中的颜色组

例如如下代码

```
{
    name: ["TEST", ".test"],
    hide: false,

    defines: [
        //!includeBegin
        string-escape = /\\(?:[nrt'"\\]|(.))/
            $0: "strEscape"
            $1: "error"

        string = /"(?:[^"\\]|/ @string-escape /)*"/

        string-def := @string
            $recordAllGroups: true
            $0: "string"

        r-string-def := /r/ @string
            $recordAllGroups: true
            $0: "keyword2"

        num = /\b\d+\b/
            $0: "number"

        value-def := {
            ::string-def
            ::r-string-def
            : @num
        }
        //!includeEnd
    ]

    contains: [
        {include: "value-def"}
    ]
}
```

将被编译为

```
{
    name: ["TEST", ".test"],
    hide: false,

    defines: [
        // Generated by mtsyntax-plus begin
        "string-escape": /(\\(?:[nrt'"\\]|(.)))/
        "string": /"(?:[^"\\]|/ + include("string-escape") + /)*"/
        "string-def": {
            match: /(/ + include("string") + /)/
            recordAllGroups: true
            1: "string"
            2: "strEscape"
            3: "error"
        }
        "r-string-def": {
            match: /(r/ + include("string") + /)/
            recordAllGroups: true
            1: "keyword2"
            2: "strEscape"
            3: "error"
        }
        "num": /(\b\d+\b)/
        "value-def": [
            {include: "string-def"}
            {include: "r-string-def"}
            {
                match: include("num")
                1: "number"
            }
        ]
        // Generated by mtsyntax-plus end
    ]

    contains: [
        {include: "value-def"}
    ]
}
```

# 语法

使用`=`来定义正则, 使用`:=`来定义匹配器, 使用`:= { ... }`来定义匹配器组

使用`@name`来引用定义的正则, 使用`&name`来引用外部定义的正则,
并且可以使用`&name(count)`标注外部的正则拥有多少个高亮组, 以避免组匹配歪掉

可以使用`&name(@)`来include引用,
有时我们不需要具体的颜色但是又不想算定义的正则拥有多少个组时很有用

每个匹配通过可选的加号连接, 比如`@foo + @bar`和`@foo @bar`都可以

在定义了匹配后, 可以跟上属性和颜色组,
属性例如`$name: "value"`, 颜色例如`$2: "xxx"`, 属性定义在颜色前面

匹配器组里面使用`:`来定义match匹配器, 使用`::`来定义include匹配器

拥有几个语法糖, 使用`(`来表示`/(/`, 使用`(?:`来表示`/(?:/`,
使用`)`来表示`/)/`, 使用`){1, 2}`来表示`/){1,2}/`, 使用`|`来表示`/|/`

使用`($color`表示一个幽灵颜色组,
它的颜色定义来源于组声明处写的`color`而非后续本地定义如`$1: color`


# 如何使用
在拥有rust编译环境的情况下, 在项目目录下输入

```
cargo run
```

即可编译并运行, 要处理的文件从标准输入重定向给程序, 结果会输出到标准输出

一个现成的使用例子: [编译前](https://github.com/A4-Tacks/mindustry_logic_bang_lang/blob/f6428adf4bdecd8ba2245849ff184f54001df6f9/syntax/MT-Manager/MindustryLogic.mtsx)
[编译后](https://github.com/A4-Tacks/mindustry_logic_bang_lang/blob/f6428adf4bdecd8ba2245849ff184f54001df6f9/syntax/MT-Manager/MindustryLogic-compiled.mtsx)
