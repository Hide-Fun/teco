% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_null_model.R
\name{make_null_model}
\alias{make_null_model}
\title{Make null model matrix.}
\usage{
make_null_model(.n, .comm, .tree, .model_type, .method, .weighted, ...)
}
\arguments{
\item{.n}{number of iteration.}

\item{.comm}{community data matrix.}

\item{.model_type}{randomization method, see vegan::randomizeMatrix}

\item{.method}{dissimilarity index. see vegan::vegdist.}

\item{.weighted}{convert `.comm` to presence/absence data.}

\item{...}{augument passed through `.f`.}
}
\description{
Make null model matrix.
}
