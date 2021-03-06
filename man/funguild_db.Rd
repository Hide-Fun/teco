% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funguild_db.R
\name{funguild_db}
\alias{funguild_db}
\title{\code{funguild_db}: Creating table for FUNGuild.}
\usage{
funguild_db(.otu_table, .identify_list, .tax_pat)
}
\arguments{
\item{.otu_table}{table (see example).}

\item{.identify_list}{identifying result (see example).}

\item{.tax_pat}{character, specify taxonomic levels containing .identify_list in same order.}
}
\description{
Creating table for analysis of FUNGuild.
}
\examples{

# make otu table.
otu_table <- data.frame(
   samplename = c("sample1", "sample2"),
   otu1 = c(1, 0),
   otu2 = c(0, 39)
)

# make identify list.
identify_list <- data.frame(
   otu = c("otu1", "otu2"),
   phylum = c("p1", "p2"),
   order = c("o1", "o2"),
   family = c("f1", "f2"),
   genus = c("g1", "g2")
)

funguild_db(.otu_table = otu_table,
            .identify_list = identify_list,
            .tax_pat = c("phylum", "order", "family", "genus"))
}
