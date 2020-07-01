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
#' @importFrom crayon red
#' @export
ld_write_file <- function(rmd_header, ld, file_name) {
  if (inherits(rmd_header, "listdown_header")) {
    rmd_header <- as.character(rmd_header)
  } else if ( !is.character(rmd_header) ) {
    stop(red("Argument rmd_header should be of type listdown_header or",
             "character."))
  }
  if (inherits(ld, "listdown")) {
    ld <- ld_make_chunks(ld)
  } else if ( !is.character(ld) ) {
    stop(red("Argument ld should be of type listdown or character."))
  }
  writeLines(c(rmd_header, ld), file_name)
}
