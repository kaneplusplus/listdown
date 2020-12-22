
#' @title Create an expression to load a Computational Component
#' 
#' @description An expression to load a computational component can be either
#' a raw expression, a variable holding the expression, or a string. The
#' return is an unevaluated expression. 
#' @param load_cc_expr a string or expression that should be use to load 
#' the computational components.
#' @export
create_load_cc_expr <- function(load_cc_expr) {
  if (is.character(load_cc_expr)) {
    # If it's a string literal, then call str2lang on it.
    load_cc_expr <- str2lang(load_cc_expr)
  } else {
    load_cc_expr <- tryCatch( {
        lce <- eval(load_cc_expr)
        if (is.character(lce)) {
          # It's a variable holding a string. Call str2lang on it.
          str2lang(lce)
        } else {
          # It's a bare expression.
          load_cc_expr
        }
      },
      # It's a bare expression.
      finally = load_cc_expr)
  }
  load_cc_expr
}
