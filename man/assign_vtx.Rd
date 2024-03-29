% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_taxonomy.R
\name{assign_vtx}
\alias{assign_vtx}
\title{Assign VTX}
\usage{
assign_vtx(
  .blast_result,
  .labels = c(strict = "no", relax = "closest_to"),
  .param_strict = c(evalue = 1e-50, pident = 97, qcov = 80),
  .param_relax = c(evalue = 1e-50, pident = 90, qcov = 80),
  .headers = NULL,
  .print_data = FALSE
)
}
\arguments{
\item{.blast_result}{data.frame}

\item{.headers}{header names.}
}
\description{
Assign VTX
}
