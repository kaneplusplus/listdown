# @examples
# if (require("ggplot2")) {
#
#   cc_list <- list(
#     Linear = ggplot(anscombe, aes(x = x1, y = y1)) + geom_point(),
#     `Non Linear` = ggplot(anscombe, aes(x = x2, y = y2)) + geom_point(),
#     `Outlier Vertical`= ggplot(anscombe, aes(x = x3, y = y3)) + 
#       geom_point(),
#     `Outlier Horizontal` =  ggplot(anscombe, aes(x = x4, y = y4)) +
#       geom_point())
#
#   rds_file <- file.path(tempdir(), "cc-list.rds")
#   saveRDS(cc_list, file = rds_file)
#
#   read_rds_str <- paste0('readRDS("', rds_file, '")')
#   ld <- listdown(read_rds_str,
#                  package = "ggplot2")
#
#   rmd_output <- file.path(tempdir(), "anscombe-quartet.rmd")
#   ld_write_file(ld_rmarkdown_header(title = "The Anscombe Quartet",
#                                     author = "Francis Anscombe",
#                                     date = "1973"),
#                 ld,
#                 rmd_output)
# 
# }

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
#' @export
ld_write_file <- function(rmd_header, ld, file_name) {

  assert(
    check_class(rmd_header, "listdown_header"),
    check_character(rmd_header),
    combine = "or"
  )

  assert(
    check_class(ld, "listdown"),
    check_character(ld),
    combine = "or"
  )

  if (inherits(rmd_header, "listdown_header")) {
    rmd_header <- as.character(rmd_header)
  } 

  if (inherits(ld, "listdown")) {
    ld <- ld_make_chunks(ld)
  }

  writeLines(c(rmd_header, ld), file_name)
}
