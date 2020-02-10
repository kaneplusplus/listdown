
`%>>%` <- function(x, y) {
  UseMethod("%>>%", x)
}

`%>>%.character` <- function(x, y) {
  if (!inherits(y, "character")) {
    stop(red("Don't know how to append object of type", class(y),
             "to character"))
  }
  if (inherits(y, "character")) {
    c(x, y)
  } else if (inherits(y, "connection")) {
    writeLines(x, y)
    invisible(x)
  } else {
    stop(red("Don't know how to apply indirection operator to object of type ",
             class(y), ".", sep = ""))
  }
}
