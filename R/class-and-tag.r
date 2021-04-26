#' Prepend Class Information and Add Attributes
#' 
#' @description listdown decorators map list element to functions. This
#' function is provided for convenience to prepend a class and attributes,
#' which can then be used by custom decorators to display those element.
#' @param .x an object to add class and attribute information to.
#' @param new_class the name of the class to be prepended to .x.
#' @param ... the attributes to attach to .x.
#' @export
class_and_tag <- function(.x, new_class, ...) {
  if (!is.character(new_class)) {
    stop("New class should be of type character.")
  }
  dots <- list(...)
  for (n in names(dots)) {
    attributes(.x)[[n]] <- dots[[n]]
  }
  class(.x) <- c(new_class, class(.x))
  .x
}
