
#' @title Write to an R Markdown File
#'
#' @description This function takes header information and a listdown 
#' object and writes to a specified file.
#' @param rmd_header either a character or listdown_header with R Markdown
#' header information.
#' @param ld the listdown object that provides
#' information on how a presentation object should be displayed in the
#' output.
#' @param file_name the output file to write to.
#' @importFrom checkmate assert check_class check_character
#' @importFrom yaml as.yaml
#' @export
ld_write_file <- function(rmd_header, ld, file_name) {

  assert(
    check_class(rmd_header, "listdown_header"),
    check_character(rmd_header),
    check_list(rmd_header),
    combine = "or"
  )

  assert(
    check_class(ld, "listdown"),
    check_character(ld),
    combine = "or"
  )

  if (inherits(rmd_header, "listdown_header")) {
    rmd_header <- as.character(rmd_header)
  } else if (is.list(rmd_header)) {
    rmd_header <- as.yaml(rmd_header)
  }

  if (inherits(ld, "listdown")) {
    ld <- ld_make_chunks(ld, rmd_dir = dirname(file_name))
  }

  writeLines(c(rmd_header, ld), file_name)
}
