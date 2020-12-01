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
  ret <- list(title = title)
  if (!is.null(author)) {
    ret$author <- author
  }
  if (!is.null(date)) {
    ret$date <- date
  }
  ret$output <- output[1]
  class(ret) <- c("listdown_header", "list")
  ret
}

#' @title Create a workflowr Header
#' @description Output a workflowr R Markdown header with specified title.
#' @param title the title of the page.
#' @param toc should the table of contents be generated? Default FALSE.
#' @export
ld_workflowr_header <- function(title, toc = FALSE) {
  ret <- list(title = title,
       site = "workflowr::wflow_site",
       output =
         list(`workflowr::wflow_html` =
              list(toc = toc)),
       editor_options = list(chunk_output_type = "console"))
  class(ret) <- c("listdown_header", "list")
  ret
}

#' @export
print.listdown_header <- function(x, ...) {
  ret <- as.character(x)
  cat(ret, "\n", sep = "\n")
  invisible(ret)
}

#' @export
#' @importFrom yaml as.yaml
as.character.listdown_header <- function(x, ...) {
  c("---",
    unlist(strsplit(as.yaml(x, ...), "\n")),
    "---")
}
