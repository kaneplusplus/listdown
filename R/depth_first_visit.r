
#' @export
listdown <- function(libs, rds_loc, decorators, 
                     echo = FALSE, message = FALSE, warning = FALSE) {
#  ret <- as.list(match.call())
  browser()
  ret <- list(libs = libs, rds_loc = rds_loc, 
              decorators = as.list(match.call()$decorators)[-1], 
              echo = echo, message = message, warning = warning)
  class(ret) <- "listdown"
  ret
}

#' Render a Listdown Object
ld_render <- function(print_list, ld) {
  UseMethod("ld_render", ld)
}


#' @importFrom crayon red
#' @export
ld_render.default <- function(print_list, ld) {
  stop(red("Don't know how to render an object of class ", 
           paste(class(ld), collapse = ":"), ".", sep = ""))
}

ld_render.listdown <- function(print_list, ld) {
  c("",
    sprintf("```{r, echo = %s, message = %s, warning = %s}",
            eval(ld$echo), eval(ld$message), eval(ld$warning)),
    as.character(vapply(eval(ld$libs), 
                 function(x) sprintf("library(%s)", as.character(x)),
                 NA_character_)),
    sprintf("print_list <- readRDS(%s)", ld$rds_loc),
    "```",
    "",
    depth_first_concat(print_list, ld))
}

list_loc_dive <- function(ll) {
  ll <- c(ll, 1)
  ll
}

list_loc_inc <- function(ll) {
  if (is.null(ll)) {
    ll <- 1
  } else {
    ll[length(ll)] <- ll[length(ll)] + 1
  }
  ll
}

list_loc_string <- function(ll) {
  if (is.null(ll)) {
    NULL
  } else {
    paste0("[[", paste0(ll, collapse = "]][["), "]]")
  }
}

list_loc_list_string <- function(ll) {
  if (length(ll) == 1) {
    paste0("[", ll, "]")
  } else {
    paste0(list_loc_string(ll[-length(ll)]), "[", ll[length(ll)], "]")
  }
}

depth_first_concat <- function(print_list, ld, heading = "#", 
                               list_location = c()) {
  ret_str <- ""

  depth_first_concat_impl <- function(print_list, ld, heading, list_location) {

    current_location <- paste0("print_list", list_loc_string(list_location))


     list_locs <- lapply(seq_along(eval(parse(text = current_location))),
                         function(x) c(list_location, x))
    
    for (list_loc in list_locs) {
      pll <- paste0("print_list", list_loc_list_string(list_loc))
      pl <- paste0("print_list", list_loc_string(list_loc))
      pll_name <- names(eval(parse(text = pll)))
      if (pll_name != "") {
        ret_str <<- c(ret_str, 
          "",
          sprintf(paste0(heading, " %s"), pll_name), 
          "")
      }

      # Check to see if we have a decorator for the element.
      if (any(
        vapply(names(ld$decorators), 
               function(x) inherits(eval(parse(text = pl)), x), NA))) {
        
        ind <- which(vapply(names(ld$decorators),
                     function(x) inherits(eval(parse(text = pl)), x), NA))

          ret_str <<- c(ret_str, 
            sprintf("```{r, echo = %s, message = %s, warning = %s}",
                    eval(ld$echo), eval(ld$message), eval(ld$warning)),
            "",
            paste0(as.character(ld$decorators[ind]), "( ", pl, " )"), 
            "",
            "```",
            "")

      } else if (inherits(eval(parse(text = pl)), "list")) {
        depth_first_concat_impl(print_list = print_list, ld = ld, 
                                heading = paste0(heading, "#"), 
                                list_location = list_loc)
      }
    }
  }
  depth_first_concat_impl(print_list = print_list, ld = ld, heading = heading,
                          list_location = list_location)
  ret_str
}
