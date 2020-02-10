#' @title Create a workflowr Header
#' @description Output a workflowr R Markdown header with specified title.
#' @param title the title of the page.
#' @param toc should the table of contents be generated? Default FALSE.
#' @export
ld_workflowr_header <- function(title, toc = FALSE) {
  c("---",
    sprintf('title: "%s"', title),
    "site: workflowr::wflow_site",
    "output:",
    "  workflowr::wflow_html:",
    sprintf("    toc: %s", tolower(as.character(toc))),
    "editor_options:",
    "chunk_output_type: console",
    "---")
}

#' @title Create an R Markdown Header
#' @description Output an R Markdown header with specified parameters.
#' @param title the title of the page.
#' @param author the author of the page. The default is NULL - no author.
#' @param date the date for the page. The default is NULL - no date.
#' @param output the output format of the page. If NULL then no output format.
#' The default is an html document.
#' @export
ld_rmarkdown_header <- function(title,
                                author = NULL,
                                date = NULL,
                                output = c("html_document",
                                           "pdf_document",
                                           "word_document")) {

  ret_str <- c("---", sprintf('title: "%s"', title))
  if (!is.null(author)) {
    ret_str <- c(ret_str, sprintf('author: "%s"', as.character(author)))
  }
  if (!is.null(date)) {
    ret_str <- c(ret_str, sprintf('date: "%s"', as.character(date)))
  }
  if (!is.null(output)) {
    ret_str <- c(ret_str, sprintf("output: %s", as.character(output[1])))
  }
  c(ret_str, "---")
}

#' @title Create an ioslides Presentation Header
#' @description Output an ioslides R Markdown header with specified parameters.
#' @param title the title of the page.
#' @param author the author of the page. The default is NULL - no author.
#' @param date the date for the page. The default is NULL - no date.
#' @param theme the ioslides theme for the document. The default is "flatly".
#' @export
ld_ioslides_header <- function(title, author = NULL, date = NULL,
                               theme = "flatly") {
  ret_str <- c("---", sprintf('title: "%s"', title))
  if (!is.null(author)) {
    ret_str <- c(ret_str, sprintf('author: "%s"', as.character(author)))
  }
  if (!is.null(date)) {
    ret_str <- c(ret_str, sprintf('date: "%s"', as.character(date)))
  }
  c(ret_str,
    "output:",
    "  ioslides_presentation:",
    sprintf("    theme: %s", theme),
    "---")
}
