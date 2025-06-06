% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combination.R
\name{combine_slash_connector}
\alias{combine_slash_connector}
\title{合并母本和父本字符串，用重复的连接符连接}
\usage{
combine_slash_connector(mother_vec, father_vec, sep = "/")
}
\arguments{
\item{mother_vec}{字符向量，表示母本名称。}

\item{father_vec}{字符向量，表示父本名称。}

\item{sep}{字符串，用作连接符。默认值为 `"/"`，可以设定为其他字符（如 `"-"`）。}
}
\value{
字符向量，每个元素是母本和父本通过计算后的连接符拼接的结果。
}
\description{
此函数用于将母本和父本字符串对逐一合并。连接符（默认为 "/"）的重复次数，
是两者中出现最多的连续连接符数量加一。例如：
`"冀豆12/冀豆15//冀豆16"` 和 `"冀豆21/冀豆23"` 中最多连续为 `//`，
则合并结果使用 `///` 连接。
}
\examples{
combine_slash_connector("冀豆12/冀豆17//冀豆19", "2232/788")
# 返回: "冀豆12/冀豆17//冀豆19///2232/788"

combine_slash_connector("A-B--C", "D-E", sep = "-")
# 返回: "A-B--C---D-E"

}
