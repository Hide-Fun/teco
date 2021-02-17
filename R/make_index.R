#' Making Index primer fasta file.
#'
#' This function make index_1.fasta and index_2.fasta from SampleSheet.csv.
#' @param .path file path.
#' @param .skip integer, number of skipping row, default 18
#' @param .id id column
#' @param .index_1 index1 column
#' @param .index_2 index2 column
#' @export
make_index <- function(.path, .skip = 18, .id, .index_1, .index_2) {
  id <- rlang::enquo(.id)
  index_1 <- rlang::enquo(.index_1)
  index_2 <- rlang::enquo(.index_2)
  # load data.
  samplesheet <- readr::read_csv(.path, skip = .skip)
  #
  index <- samplesheet %>%
    dplyr::select(!!.id, !!.index_1, !!index_2) %>%
    dplyr::rename(ID := !!.id, index_1 := !!.index_1, index_2 := !!.index_2) %>%
    dplyr::mutate(ID = as.character(ID),
                  ID = stringr::str_c(">", ID)) %>%
    tibble::add_column(index_num = seq(length.out = nrow(samplesheet), by = 2) + 1,
                       ID_num = seq(length.out = nrow(samplesheet), by = 2))
  #
  index_arrange <- dplyr::tribble(
    ~num, ~index_1, ~index_2,
    index$index_num, index$index_1, index$index_2,
    index$ID_num, index$ID, index$ID) %>%
    tidyr::unnest(cols = c(num, index_1, index_2)) %>%
    dplyr::arrange(num)
  #
  readr::write_excel_csv(index_arrange, "index_arrange.csv")
  readr::write_tsv(index_arrange[,2], col_names = F, "index_1.fasta")
  readr::write_tsv(index_arrange[,3], col_names = F, "index_2.fasta")
  return(index_arrange)
}
