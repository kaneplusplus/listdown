
#' @title Show the list of Computational Components as a Dendrogram
#'
#' @description This function creates text dendrograms from
#' a list of computational components. It is useful for
#' creating a dendrogram of the the computational components of a listdown
#' object allowing the user to view the components hierarchically.
#' @param x a named list of computational components
#' @importFrom crayon red
#' @examples
#' if (require("ggplot2")) {
#' 
#'   cc_list <- list(
#'     Linear = ggplot(anscombe, aes(x = x1, y = y1)) + geom_point(),
#'    `Non Linear` = ggplot(anscombe, aes(x = x2, y = y2)) + geom_point(),
#'    `Outlier Vertical`= ggplot(anscombe, aes(x = x3, y = y3)) + geom_point(),
#'    `Outlier Horizontal` =  ggplot(anscombe, aes(x = x4, y = y4)) +
#'      geom_point())
#'
#'   ld_cc_dendro(cc_list)
#' }
#' @export
ld_cc_dendro <- function(x) {
  if (!is.list(x)) {
    stop(red("Argument should be a list type."))
  }

  tdt_impl <- function(x, prefix_string) {
    for (i in seq_along(x)) {
      list_name <- names(x[i])
      if (is.null(list_name)) {
        list_name <- '""'
      }
      vert_sep <- ifelse(i == length(x), " o-- ", " |-- ")
      tree_str <<- c(tree_str, paste0(prefix_string, vert_sep, list_name))
      if (!class(x[[i]])[1] == "list") {
        in_sep <- ifelse(i == length(x), "  ", " |")
        tree_str <<- c(tree_str,
                       paste0(prefix_string, in_sep, "  o-- ",
                             "object of type(s):",
                             paste0(class(x[[i]]), collapse = " ")))
      } else {
        tdt_impl(x[[i]], paste0(prefix_string, " "))
      }
    }
  }
  tree_str <- as.character(as.list(match.call())$x)
  tdt_impl(x, " ")
  class(tree_str) <- "ld_cc_dendro"
  tree_str
}

#' @export
print.ld_cc_dendro <- function(x, ...) {
  cat("\n", paste(x, sep = "", collapse = "\n"), "\n", "\n", sep = "")
  invisible(x)
}
