% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/field.R
\name{midu}
\alias{midu}
\title{计算不同农业参数}
\usage{
midu(亩密度_万株 = NULL, 行距_厘米 = NULL, 株距_厘米 = NULL)
}
\arguments{
\item{亩密度_万株}{亩密度，单位：万株（可选）。}

\item{行距_厘米}{行距，单位：厘米（可选）。}

\item{株距_厘米}{株距，单位：厘米（可选）。}
}
\value{
返回一个列表，包括以下字段：
\item{亩密度_万株}{计算出的亩密度，单位：万株。}
\item{行距_厘米}{计算出的行距，单位：厘米。}
\item{株距_厘米}{计算出的株距，单位：厘米。}
\item{米株数}{每米的株数。}
\item{亩用种量_万粒}{每亩用种量，单位：万粒，假设每株使用1.5粒种子。}
}
\description{
该函数用于根据已知的亩密度、株距和行距计算缺失的农业参数，确保只提供一个缺失值，其余两个必须为已知值。支持计算亩密度、株距、行距及米株数等。
}
\details{
该函数确保只有一个输入参数为空。如果三个参数中有两个以上为空，则会提示错误。计算公式如下：
\item{亩密度_万株 = 6666666 / (株距_厘米 * 行距_厘米) / 10000}{当亩密度为空时。}
\item{株距_厘米 = 6666666 / (亩密度_万株 * 10000 * 行距_厘米)}{当株距为空时。}
\item{行距_厘米 = 亩密度_万株 * 株距_厘米 / 6666666}{当行距为空时。}
}
\examples{
midu(亩密度_万株 = 30, 行距_厘米 = 20)
midu(株距_厘米 = 15, 行距_厘米 = 25)
midu(亩密度_万株 = 25, 株距_厘米 = 18)

}
