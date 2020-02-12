#' @title Create a listdown Object
#'
#' @description A listdown object provides information for how a presentation
#' list should be used to create an R Markdown document. It requires an
#' unquoted expression indicating how the presentation list will be loaded.
#' In addition, libraries required by the outputted document and other
#' paraeters can be specified.
#' @param load_ld_expr an unquoted expression to load the presentation list.
#' @param package a quoted list of package required by the outputted document.
#' @param decorator a named list mapping the potential types of list elements
#' to a decorator function.
#' @param init_expr an initial expression that will be added to the outputted
#' document after the libraries have been called.
#' @param ... default options sent to the chunks of the outputted document.
#' @export
listdown <- function(load_ld_expr,
                     package = NULL,
                     decorator = list(),
                     init_expr = NULL,
                     ...) {
  ret <- list(load_ld_expr = match.call()$load_ld_expr,
              decorator = as.list(match.call()$decorator)[-1],
              package = package,
              init_expr = match.call()$init_expr,
              dots = list(...))
  class(ret) <- "listdown"
  ret
}

#' @title Write a listdown Object to a String
#'
#' @description After a presentation list and listdown object have been
#' constructed the chunks can be rendered to a string, which can be appended
#' to a file, with appropriate headers, resulting in a compilable R Markdown
#' document.
#' @param pres_list the presentation list. A named list of object to be
#' displayed in the outputted R Markdown document.
#' @param ld the listdown object that provides
#' information on how a presentation object should be displayed in the
#' output.
#' @seealso \code{\link{listdown}}
#' @export
ld_make_chunks <- function(pres_list, ld) {
  UseMethod("ld_make_chunks", ld)
}

#' @importFrom crayon red
ld_make_chunks.default <- function(pres_list, ld) {
  stop(red("Don't know how to render an object of class ",
           paste(class(ld), collapse = ":"), ".", sep = ""))
}

#' @export
ld_make_chunks.listdown <- function(pres_list, ld) {

  ret_string <- ""
  if (length(ld$package) > 0) {
    ret_string <-
      c(ret_string,
        sprintf("```{r%s}", make_chunk_option_string(ld)),
        as.character(vapply(eval(ld$package),
                     function(x) sprintf("library(%s)", as.character(x)),
                     NA_character_)),
        "",
        sprintf("pres_list <- %s", deparse(ld$load_ld_expr)),
        "```")
  }
  if (length(ld$init_expr)) {
    ret_string <-
      c(ret_string,
        "",
        sprintf("```{r%s}", make_chunk_option_string(ld)),
        if (deparse(ld$init_expr[[1]]) == "{") {
          deparse(ld$init_expr[[-1]])
        } else {
          deparse(ld$init_expr)
        },
        "```")
  }
  ret_string <- c(ret_string,
    depth_first_concat(pres_list, ld))
  ret_string
}
