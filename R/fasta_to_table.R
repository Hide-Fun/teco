#' Convert fasta to table.
#'
#' converting fasta format file to table that have two rows including  OTU and sequence.
#' @param .fasta fasta format files.
#' @param .col_name name of column.
#' @export
#' @examples
#'
#' fasta <-
#'    data.frame(X1 = c(">otu_1", "ATGCGG", ">otu_2", "GCCGA"))
#'
#' fasta_to_table(.fasta = fasta, .col_name = X1)
fasta_to_table <- function(.fasta, .col_name = X1) {
  col_name <- rlang::enquo(.col_name)
  # rowname to column.
  df <- .fasta %>%
    tibble::rownames_to_column("rowname") %>%
    dplyr::mutate(rowname = readr::parse_integer(rowname))
  # select otu rows and sequence rows.
  otu <-  df %>%
    dplyr::filter(rowname %% 2 != 0 ) %>%
    dplyr::rename(otu := !!col_name) %>%
    dplyr::mutate(otu = stringr::str_remove(otu, ">")) %>%
    dplyr::select(otu)
  sequence <- df %>%
    dplyr::filter(rowname %% 2 == 0) %>%
    dplyr::rename(sequence := !!col_name) %>%
    dplyr::select(sequence)
  # bind cols of otu and sequence.
  rlt <- dplyr::bind_cols(otu, sequence)
  return(rlt)
}
