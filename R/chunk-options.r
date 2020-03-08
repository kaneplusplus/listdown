
chunk_option_list <- c(
  "child",
  "code",
  "engine",
  "eval",
  "include",
  "purl",
  "collapse",
  "echo",
  "results",
  "error",
  "message",
  "warning",
  "comment",
  "highlight",
  "prompt",
  "strip.white",
  "tidy",
  "opts.label",
  "R.options",
  "ref.label",
  "autodep",
  "cache",
  "cache.comments",
  "cache.lazy",
  "cache.path",
  "cache.vars",
  "dependson",
  "anipots",
  "interval",
  "dev",
  "dev.args",
  "dpi",
  "external",
  "fig.align",
  "fig.cap",
  "fig.env",
  "fig.ext",
  "fig.height",
  "fig.width",
  "fig.keep",
  "fig.lp",
  "fig.path",
  "fig.pos",
  "fig.process",
  "fig.retina",
  "fig.scap",
  "fig.subcap",
  "fig.show",
  "fig.showtext",
  "out.extra",
  "out.height",
  "out.width",
  "resize.height",
  "resize.width",
  "sanitize"
  )

opt_not_r_chunk <- function(x) {
  x[!(vapply(x, function(opt) opt %in% chunk_option_list, FALSE))]
}

#' @title Apply Chunk Options to a Presentation Object
#' @description This function allows the user to set chunk options for 
#' individual elements of a presentation list.
#' @param pres_obj the presentation list element whose chunk options should
#' be modified.
#' @param chunk_name the name of the chunk. By default this is NULL, 
#' correpsonding to no chunk name.
#' @param ... named chunk options and their values.
#' @param arg_list the list of chunk options can be specified. If ... 
#' parameters are specified, then this option is ignored.
#' @export
ld_chunk_opts <- function(pres_obj, chunk_name = NULL, ..., arg_list = NULL) {
  a <- attributes(pres_obj)
  dots <- list(...)
  not_r_chunk_opts <- opt_not_r_chunk(names(dots))
  if (length(not_r_chunk_opts) > 0) {
    stop(red("Unrecognized options:\n\t",
             paste(not_r_chunk_opts, collapse = "\n\t"),
             "\n", sep = ""))
  }
  if (length(dots) > 0) {
    for (i in seq_along(dots)) {
      val <- dots[[i]]
      if (is.character(val)) {
        val <- sprintf('"%s"', val)
      } else if (is.null(val)) {
        val <- "NULL"
      } else {
        val <- as.character(val)
      }
      name <- names(dots)[i]
      a$listdown[name] <- list(val)
    }
    attributes(pres_obj) <- a
  } else if (!is.null(arg_list)) {
    a$listdown <- arg_list
    attributes(pres_obj) <- a
  }
  if (!is.null(chunk_name)) {
    a <- attributes(pres_obj)
    a$listdown$chunk_name <- chunk_name
    attributes(pres_obj) <- a
  }
  pres_obj
}


