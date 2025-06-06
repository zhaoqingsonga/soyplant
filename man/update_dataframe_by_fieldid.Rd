% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baiaoyun.R
\name{update_dataframe_by_fieldid}
\alias{update_dataframe_by_fieldid}
\title{根据唯一编号和fieldid字段更新数据框}
\usage{
update_dataframe_by_fieldid(reference_df, target_df)
}
\arguments{
\item{reference_df}{包含参考信息的数据框，其中必须包含名为“唯一编号”的列，此列用于与`target_df`中的“fieldid”列进行匹配。}

\item{target_df}{要更新的数据框，必须包含名为“fieldid”的列，此列用于与`reference_df`中的“唯一编号”列进行匹配。}
}
\value{
更新后的`target_df`数据框，其中相应列已按照匹配规则更新了值，数字类型的数据会以数字类型存储。
}
\description{
该函数通过匹配第一个数据框（`reference_df`）中的“唯一编号”字段与第二个数据框（`target_df`）中的“fieldid”字段，
将`reference_df`中对应行的相关列值更新到`target_df`中相应列上，并且在更新时会判断数据类型，若更新内容为数字则以数字类型存储。
}
\examples{
# 示例数据框（你可以替换为真实的数据框）
df1 <- data.frame(
    唯一编号 = c(1, 2, 3),
    value1 = c("a", "b", "c"),
    value2 = c(10, 20, 30)
)
df2 <- data.frame(
    fieldid = c(1, 2, 3),
    col1 = c("x", "y", "z"),
    col2 = c(5, 6, 7)
)
updated_df <- update_dataframe_by_fieldid(df1, df2)
print(updated_df)

}
