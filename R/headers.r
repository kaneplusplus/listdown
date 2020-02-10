
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
    ret_str <- c(ret_str, sprintf("output: %s", as.character(output)))
  }
  c(ret_str, "---")
}

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
