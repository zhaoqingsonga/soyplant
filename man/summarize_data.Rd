% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yield.R
\name{summarize_data}
\alias{summarize_data}
\title{根据字段类型按名称进行汇总}
\usage{
summarize_data(df, soy_traits)
}
\arguments{
\item{df}{数据框。包含需要汇总的数据。必须包含 "name" 列。}

\item{soy_traits}{数据框。包含字段类型信息。必须包含 "name_Lib" (字段名称) 和 "field_type" (字段类型) 列。}
}
\value{
汇总后的数据框，保持与原始数据框相同的列名、顺序和行顺序。
}
\description{
此函数根据提供的字段类型信息，按名称 (`name`) 对数据框进行分类汇总。
对于数字类型 (`N`) 的字段，计算平均值并保留两位小数；
对于其他类型（日期 `D`、字符 `C`、任意类型 `any` 或未定义类型），则保留每个 `name` 组中第一行的值。
汇总后的数据框将保持与原始数据框相同的列名、顺序和行顺序。
}
\details{
此函数首先将输入的数据框转换为 `data.table` 以提高性能。
然后，它根据 `soy_traits` 中定义的字段类型，按 `name` 列进行分组汇总。
}
\examples{
# 创建示例数据
df <- data.frame(
    name = rep(c("A", "B", "C"), each = 3),
    value_n = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    date_d = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-02-01", "2024-02-02", "2024-02-03", "2024-03-01", "2024-03-02", "2024-03-03")),
    color_c = rep(c("red", "green", "blue"), 3),
    other_col=1:9
)

soy_traits <- data.frame(
    name_Lib = c("value_n", "date_d", "color_c"),
    field_type = c("N", "D", "C")
)

# 执行汇总
summarized_df <- summarize_data(df, soy_traits)
print(summarized_df)
}
