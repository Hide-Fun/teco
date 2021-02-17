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
  # select OTU rows and sequence rows.
  OTU <-  df %>%
    dplyr::filter(rowname %% 2 != 0 ) %>%
    dplyr::rename(OTU := !!col_name) %>%
    dplyr::mutate(OTU = stringr::str_remove(OTU, ">")) %>%
    dplyr::select(OTU)
  sequence <- df %>%
    dplyr::filter(rowname %% 2 == 0) %>%
    dplyr::rename(sequence := !!col_name) %>%
    dplyr::select(sequence)
  # bind cols of OTU and sequence.
  rlt <- dplyr::bind_cols(OTU, sequence)
  return(rlt)
}
