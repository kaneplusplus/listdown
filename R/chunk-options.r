
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
  "sanitize",
  ""
  )

not_r_chunk_opts <- function(x) {
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
#' @param chunk_opts list of chunk options can be specified. Takes priority
#' over arguments provided to ...
#' @export
ld_chunk_opts <- function(pres_obj, chunk_name = NULL, ..., chunk_opts = NULL) {
  a <- attributes(pres_obj)
  if (is.null(chunk_opts)) {
    chunk_opts <- list(...)
  }
  not_r_chunk_opts <- not_r_chunk_opts(names(chunk_opts))
  if (length(not_r_chunk_opts) > 0) {
    stop(red("Unrecognized options:\n\t",
             paste(not_r_chunk_opts, collapse = "\n\t"),
             "\n", sep = ""))
  }
  if (length(chunk_opts) > 0) {
    for (i in seq_along(chunk_opts)) {
      val <- chunk_opts[[i]]
      if (is.character(val)) {
        val <- sprintf('"%s"', val)
      } else if (is.null(val)) {
        val <- "NULL"
      } else if (is.logical(val)) {
        val <- val
      } else {
        val <- as.character(val)
      }
      name <- names(chunk_opts)[i]
      a$listdown[name] <- list(val)
    }
    attributes(pres_obj) <- a
  } else if (!is.null(chunk_opts)) {
    a$listdown <- chunk_opts
    attributes(pres_obj) <- a
  }
  if (!is.null(chunk_name)) {
    a <- attributes(pres_obj)
    a$listdown$chunk_name <- chunk_name
    attributes(pres_obj) <- a
  }
  pres_obj
}


