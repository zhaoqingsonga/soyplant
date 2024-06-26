% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/design_plot.r
\name{getWaterLaneProtected}
\alias{getWaterLaneProtected}
\title{getWaterLaneProtected}
\usage{
getWaterLaneProtected(blockStructure = "w/23/2r/w/2p/w/2p/23/w")
}
\arguments{
\item{blockStructure}{描述地块特征的字符串。默认值为 "w/23/2r/w/2p/w/2p/23/w"。
字符串可以包含以下元素：
  - 'w': 水沟
  - 'r': 道路
  - 'p': 保护行
  - 数字可跟在 'r' 或 'p' 后表示多个道路或保护行。}
}
\value{
一个包含以下内容的列表：
  - totalColumns: 总列数，包括水沟、道路和保护行。
  - waterColumns: 水沟列的索引。
  - laneColumns: 道路列的索引。
  - protectedColumns: 保护行列的索引。
}
\description{
解析描述水沟、道路和保护行序列的字符串。
}
\examples{
getWaterLaneProtected("w/23/2r/w/2p/w/2p/23/w")
getWaterLaneProtected("5/2r/3p/w")
}
