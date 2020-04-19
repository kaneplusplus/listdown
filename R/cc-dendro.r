
#' @title Show the list of Computational Components as a Dendrogram
#' 
#' @description This function creates text dendrograms from 
#' a list of computational components. It is useful for
#' creating a dendrogram of the the computational components of a listdown
#' object allowing the user to view the components hierarchically.
#' @param x a named list of computational components
#' @importFrom crayon red
#' @examples
#' library(ggplot2)
#' 
#' cc_list <- list(
#'   Linear = ggplot(anscombe, aes(x = x1, y = y1)) + geom_point(),
#'  `Non Linear` = ggplot(anscombe, aes(x = x2, y = y2)) + geom_point(),
#'  `Outlier Vertical`= ggplot(anscombe, aes(x = x3, y = y3)) + geom_point(),
#'  `Outlier Horizontal` =  ggplot(anscombe, aes(x = x4, y = y4)) + 
#'    geom_point())
#'
#' ld_cc_dendro(cc_list)
#' @export
ld_cc_dendro <- function(x) {
  if (!is.list(x)) {
    stop(red("Argument should be a list type."))
  }
  tree_yml <- as_ld_yml(x)
  class(tree_str) <- c("ld_cc_dendro", class(tree_str))
  tree_str
}

#' importFom ymlthis draw_yml_tree
#' @export
print.ld_cc_dendro <- function(x, ...) {
  draw_yml_tree(x, ...)
}
