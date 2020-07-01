
# Helper function Eval Parse Paste (epp)
epp <- function(..., env = parent.frame(n = 2)) {
  eval(parse(text = paste0(...)), envir = env)
}

last_index_as_list <- function(loc) {
  re <- regexpr("\\[\\[[0-9]+\\]\\]$", loc)
  # Get the list location. Not the list element.
  paste0(substr(loc, 1, re[1] - 1),
         "[",
         gsub("\\]\\]", "", gsub("\\[\\[", "", substr(loc, re, nchar(loc)))),
         "]")
}

#' @title Turn a Computational Component List into YAML with Class Information
#'
#' @description Create an object of type yaml::yml from a list of
#' computational components. The function recursively descends into the list
#' and when an element type is not a list the class information substituted
#' for the object.
#' @param x a named list of computational components.
#' @importFrom yaml as.yaml
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
#'   as_ld_yml(cc_list)
#' }
#' @export
as_ld_yml <- function(x) {

  depth_first_copy <- function(loc = "") {
    elem <- epp("x", loc)
    locs <- vapply(seq_along(elem),
                  function(i) paste0(loc, "[[", i, "]]"), NA_character_)
    for (loc in locs) {
      elem <- epp("x", loc)

      if (inherits(elem, "list")) {
        depth_first_copy(loc)
      } else {
        loc_list <- last_index_as_list(loc)
        if (is.null(names(epp("x", loc_list)))) {
          eval(parse(text = paste("x", loc, " <<- paste(",
                     deparse(class(elem)), ", collapse = \":\")")))
        } else {
          # Wrap in a list for formatting.
          eval(parse(text = paste("x", loc, " <<- list(paste(",
                     deparse(class(elem)), ", collapse = \":\"))")))
        }
      }
    }
  }
  depth_first_copy()
  as.yaml(x)
}
