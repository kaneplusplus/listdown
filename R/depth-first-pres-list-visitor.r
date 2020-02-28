
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

make_chunk_option_string <- function(ld, pres_obj = NULL) {
  chunk_option_string <- ""
  if (!is.null(pres_obj) && "listdown" %in% names(attributes(pres_obj))) {
    lda <- attributes(pres_obj)$listdown
    name_ind <- which(names(lda) == "chunk_name")
    chunk_name <- NULL
    if (length(name_ind)) {
      chunk_name <- lda[[name_ind]]
      lda <- lda[-name_ind]
    } 
    chunk_option_string <-
      if (length(lda) > 0 && !is.null(chunk_name)) {
        paste0(" ", paste(chunk_name,
               paste(names(lda), "=", as.character(lda), collapse = ", ")))
      } else if (length(lda) > 0) {
        paste(chunk_name,
              paste(names(lda), "=", as.character(lda), collapse = ", "))
    
      } else {
        paste0(" ", chunk_name) 
      }
  } else {
    # Use the default chunk options.
    if (length(ld$dots) > 0) {
      chunk_option_string <-
        paste0(" ", names(ld$dots), "=", as.character(ld$dots), 
               collapse = ", ")
    }
  }
  chunk_option_string
}

depth_first_concat <- function(pres_list, ld, heading = "#",
                               list_location = c()) {

  ret_str <- ""

  depth_first_concat_impl <- function(pres_list, ld, heading, list_location) {

    current_location <- paste0("pres_list", list_loc_string(list_location))


     list_locs <- lapply(seq_along(eval(parse(text = current_location))),
                         function(x) c(list_location, x))

    for (list_loc in list_locs) {
      pll <- paste0("pres_list", list_loc_list_string(list_loc))
      pl <- paste0("pres_list", list_loc_string(list_loc))
      pll_name <- names(eval(parse(text = pll)))
      if (pll_name != "") {
        ret_str <<- c(ret_str,
          sprintf(paste0(heading, " %s"), pll_name),
          "")
      }

      # Check to see if we have a decorator for the element.
      if (any(
        vapply(names(ld$decorator),
               function(x) inherits(eval(parse(text = pl)), x), NA))) {

        ind <- which(vapply(names(ld$decorator),
                     function(x) inherits(eval(parse(text = pl)), x), NA))

        ret_str <<- c(ret_str,
          sprintf("```{r%s}", 
                  make_chunk_option_string(ld, eval(parse(text = pl)))),
          paste0(as.character(ld$decorator[ind]), "(", pl, ")"),
          "```",
          "")

      } else if (inherits(eval(parse(text = pl)), "list")) {
        depth_first_concat_impl(pres_list = pres_list, ld = ld,
                                heading = paste0(heading, "#"),
                                list_location = list_loc)
      } else {
        # It's not one of the decorators, and it's not a list. Use the
        # default decorator.
        if (!is.null(ld$default_decorator)) {
          ret_str <<- c(ret_str,
            sprintf("```{r%s}", 
                    make_chunk_option_string(ld, eval(parse(text = pl)))),
            paste0(as.character(ld$default_decorator), "(", pl, ")"),
            "```",
            "")
        }
      }
    }
  }
  depth_first_concat_impl(pres_list = pres_list, ld = ld, heading = heading,
                          list_location = list_location)
  while(length(ret_str) > 0 && ret_str[length(ret_str)] == "") {
    ret_str <- ret_str[-length(ret_str)]
  }
  ret_str
}
