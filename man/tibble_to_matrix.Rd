% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tibble_to_matrix.R
\name{tibble_to_matrix}
\alias{tibble_to_matrix}
\title{Convert tibble to matrix.}
\usage{
tibble_to_matrix(.tbl, .row = FALSE, .rowname = NULL)
}
\arguments{
\item{.tbl}{tibble}

\item{.row}{logical, Want rownames?}

\item{.rowname}{character, names of rownames column.}
}
\description{
Convert tibble to matrix.
}
\examples{
library(tibble)

tbl <-
   tibble(com = sprintf("com_\%d", 1:10),
          V1 = rnorm(10),
          V2 = rnorm(10))

# convert to matrix.
tibble_to_matrix(.tbl = tbl, .row = FALSE)
# move rownames to column.
tibble_to_matrix(.tbl = tbl, .row = TRUE, .rowname = "com")

}
