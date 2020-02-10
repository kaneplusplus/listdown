
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

#' @export
ld_chunk_opts <- function(ld, chunk_name = NULL, ..., arg_list = NULL) {
  a <- attributes(ld)
  dots <- list(...)
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
    attributes(ld) <- a
  } else if (!is.null(arg_list)) {
    a$listdown <- arg_list
    attributes(ld) <- a
  }
  if (!is.null(chunk_name)) {
    a <- attributes(ld)
    a$listdown$chunk_name <- chunk_name
    attributes(ld) <- a
  }
  ld 
}


