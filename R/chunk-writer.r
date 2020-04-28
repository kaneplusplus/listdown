#' @title Create a listdown Object
#'
#' @description A listdown object provides information for how a presentation
#' list should be used to create an R Markdown document. It requires an
#' unquoted expression indicating how the presentation list will be loaded.
#' In addition, libraries required by the outputted document and other
#' paraeters can be specified.
#' @param load_cc_expr an unquoted expression to load the presentation list.
#' @param package a quoted list of package required by the outputted document.
#' @param decorator a named list mapping the potential types of list elements
#' to a decorator function.
#' @param init_expr an initial expression that will be added to the outputted
#' document after the libraries have been called.
#' @param elem_chunk_opts a named list mapping the potential types of list
#' elements to chunk options that should be included for those types.
#' @param default_decorator the decorator to use for list elements whos type
#' is not inherited from the decorator list. If NULL then the those
#' elements will not be included when the chunks are written. By default
#' this is identity, meaning that the elements will be passed directly 
#' (through the identity() function).
#' @param ... default options sent to the chunks of the outputted document.
#' @param chunk_opts a named list of options sent to the chunks of outputted
#' documents. Note: takes priority over argument provided to ...
#' @importFrom crayon red
#' @export
listdown <- function(load_cc_expr,
                     package = NULL,
                     decorator = list(),
                     init_expr = NULL,
                     elem_chunk_opts = list(),
                     default_decorator = identity,
                     ...,
                     chunk_opts = NULL) {

  if ( !("default_decorator" %in% names(as.list(match.call))) ) {
    default_decorator = as.symbol("identity")
  } else {
    default_decorator = as.list(match.call()$default_decorator)
  }

  if (is.null(chunk_opts)) {
    chunk_opts <- list(...) 
  }
 
  not_r_chunk_opts <- not_r_chunk_opts(names(chunk_opts))
  if (length(not_r_chunk_opts) > 0) {
    stop(red("Unrecognized options:\n\t",
             paste(not_r_chunk_opts, collapse = "\n\t"),
             "\n", sep = ""))
  }

  # Check the chunk options of elem_chunk_opts.
  for (i in seq_along(elem_chunk_opts)) {
    not_r_chunk_opts <- not_r_chunk_opts(names(elem_chunk_opts[[i]]))
    if (length(not_r_chunk_opts) > 0) {
      stop(red("Unrecognized options for element type", 
               names(elem_chunk_opts)[i], ":\n\t",
               paste(not_r_chunk_opts, collapse = "\n\t"),
               "\n", sep = ""))
    }
  }
  ret <- list(load_cc_expr = match.call()$load_cc_expr,
              decorator = as.list(match.call()$decorator)[-1],
              package = package,
              init_expr = match.call()$init_expr,
              elem_chunk_opts = elem_chunk_opts,
              default_decorator = default_decorator,
              chunk_opts = chunk_opts)

  class(ret) <- "listdown"
  ret
}

#' @title Write a listdown Object to a String
#'
#' @description After a presentation list and listdown object have been
#' constructed the chunks can be rendered to a string, which can be appended
#' to a file, with appropriate headers, resulting in a compilable R Markdown
#' document.
#' @param ld the listdown object that provides
#' information on how a presentation object should be displayed in the
#' output.
#' @seealso \code{\link{listdown}}
#' @export
ld_make_chunks <- function(ld) {
  UseMethod("ld_make_chunks", ld)
}

#' @importFrom crayon red
ld_make_chunks.default <- function(ld) {
  stop(red("Don't know how to render an object of class ",
           paste(class(ld), collapse = ":"), ".", sep = ""))
}

#' @export
ld_make_chunks.listdown <- function(ld) {

  cc_list <- eval(ld$load_cc_expr)
  ret_string <- ""
  if (length(ld$package) > 0 || length(ld$init_expr)) {
    ret_string <-
      c(ret_string,
        sprintf("```{r%s}", make_chunk_option_string(ld$chunk_opts)))
    if (length(ld$package) > 0) {
      ret_string <- 
        c(ret_string,
          as.character(vapply(eval(ld$package),
                       function(x) sprintf("library(%s)", as.character(x)),
                       NA_character_)),
          "",
          sprintf("cc_list <- %s", deparse(ld$load_cc_expr)))
      if (length(ld$init_expr)) {
        ret_string <- c(ret_string, "")
      }
    }
    if (length(ld$init_expr)) {
      ret_string <-
        c(ret_string,
          if (deparse(ld$init_expr[[1]]) == "{") {
            unlist(lapply(ld$init_expr[-1], function(x) c("", deparse(x))))
          } else {
            deparse(ld$init_expr)
          })
    }
    ret_string <- c(ret_string, "```")
  }
  ret_string <- c(ret_string,
    depth_first_concat(cc_list, ld))
  ret_string
}
