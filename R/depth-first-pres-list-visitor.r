
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

list_loc_ind_to_name <- function(pl, env = parent.frame(n = 2)) {
  ret <- gsub("\\[\\[[0-9]+\\]\\]", "", pl)
  re <- gregexpr("\\[\\[[0-9]+\\]\\]", pl)[[1]]
  re_len <- attr(re, "match.length")
  for (i in seq_along(re)) {
    idx <- substr(pl, re[i], re[i] + re_len[i] - 1)
    idx_name <- names(epp(ret, last_index_as_list(idx)))
    # Wrap in backticks if the names isn't a valid variable name.
    if (!is.null(idx_name) && idx_name != make.names(idx_name)) {
      idx_name <- paste0("`", idx_name, "`")
    }
    ret <- paste0(
      ret,
      ifelse(!is.null(idx_name),
             paste0("$", idx_name),
             idx))
  }
  ret
}

get_attribute_options <- function(ld, pres_obj = NULL) {
  ret <- list()
  if (!is.null(pres_obj) && "listdown" %in% names(attributes(pres_obj))) {
    ret <- attributes(pres_obj)$listdown
  }
  ret
}

make_chunk_option_string <- function(chunk_opts) {
  named_elements <- chunk_opts[names(chunk_opts) != ""]
  unnamed_element <- chunk_opts[names(chunk_opts) == ""]
  ret <- character()
  if (length(unnamed_element) > 0 && unnamed_element != "") {
    ret <- paste(ret, unnamed_element[[1]])
  }
  if (length(named_elements)) {
    opt_strings <- 
      paste(names(named_elements), 
            vapply(named_elements, deparse, NA_character_),
            sep = " = ")
    ret <- paste(opt_strings, collapse = ", ")
  }
  if (length(ret) > 0 && nchar(ret) > 0 && substr(ret, 1, 1) != " ") {
    ret <- paste0(" ", ret)
  }
  if (length(ret) == 0) {
    ret <- ""
  }
  ret
}

#' @importFrom crayon yellow
depth_first_concat <- function(cc_list, ld, heading = "#",
                               list_location = c()) {

  ret_str <- ""

  depth_first_concat_impl <- function(cc_list, ld, heading, list_location) {

    current_location <- paste0("cc_list", list_loc_string(list_location))


     list_locs <- lapply(seq_along(eval(parse(text = current_location))),
                         function(x) c(list_location, x))

    for (list_loc in list_locs) {
      pll <- paste0("cc_list", list_loc_list_string(list_loc))
      pl <- paste0("cc_list", list_loc_string(list_loc))
      pll_name <- names(eval(parse(text = pll)))
      if (length(pll_name) && pll_name != "") {
        ret_str <<- c(ret_str,
          sprintf(paste0(heading, " %s"), pll_name),
          "")
      }

      decorator_index <- which(
        vapply(names(ld$decorator),
               function(x) inherits(eval(parse(text = pl)), x), NA))

      if (length(decorator_index) > 1) {
        warning(yellow("Multiple matches to decorator of types:",
                       paste(names(decorator_index), collapse = " "),
                       ".\n\nUsing the first.", sep = " "))
      }

      chunk_opts <- ld$chunk_opts

      chunk_option_index <- which(
        vapply(names(ld$decorator_chunk_opts),
               function(x) inherits(eval(parse(text = pl)), x), NA))

      if (length(chunk_option_index) > 1) {
        warning(yellow("Multiple matches to chunk elemet option of types:",
                       paste(names(decorator_index), collapse = " "),
                       ".\n\nUsing the first.", sep = " "))
      }

      if (length(chunk_option_index) == 1) {
        new_chunk_opts <- ld$decorator_chunk_opts[[chunk_option_index]]
        for (i in seq_along(new_chunk_opts)) {
          chunk_opts[[names(new_chunk_opts)[i]]] <- new_chunk_opts[[i]]
        }
      }

      # Keep the last unnamed element (chunk name).
      unnamed_element <- which(names(chunk_opts) == "")
      if (length(unnamed_element) > 1) {
        chunk_opts <- chunk_opts[-unnamed_element[1]]
      }

      att_chunk_opts <- get_attribute_options(ld, eval(parse(text = pl)))

      for (i in seq_along(att_chunk_opts)) {
        chunk_opts[[names(att_chunk_opts)[i]]] <- att_chunk_opts[[i]]
      }

      # Keep the last unnamed element (chunk name).
      unnamed_element <- which(names(chunk_opts) == "")
      if (length(unnamed_element) > 1) {
        chunk_opts <- chunk_opts[-unnamed_element[1]]
      }

      chunk_opts_string <- make_chunk_option_string(chunk_opts)

      # Check to see if we have a decorator for the element.
      if (any(
        vapply(names(ld$decorator),
               function(x) inherits(eval(parse(text = pl)), x), NA))) {
        ret_str <<- c(
          ret_str,
          sprintf("```{r%s}", chunk_opts_string),
            ifelse(as.character(ld$decorator[[decorator_index]]) == "identity",
                   list_loc_ind_to_name(pl),
                   paste0(as.character(ld$decorator[decorator_index]),
                          "(", list_loc_ind_to_name(pl), ")")),
          "```",
          "")

      } else if (inherits(eval(parse(text = pl)), "list")) {
        depth_first_concat_impl(cc_list = cc_list, ld = ld,
                                heading = paste0(heading, "#"),
                                list_location = list_loc)
      } else {
        # It's not one of the decorators, and it's not a list. Use the
        # default decorator.
        if (!is.null(ld$default_decorator)) {
          ret_str <<- c(
            ret_str,
            sprintf("```{r%s}", chunk_opts_string),
                    ifelse(as.character(ld$default_decorator) == "identity",
                           list_loc_ind_to_name(pl),
                           paste0(as.character(ld$default_decorator),
                                  "(", list_loc_ind_to_name(pl), ")")),
                    "```",
                    "")
        }
      }
    }
  }
  depth_first_concat_impl(cc_list = cc_list, ld = ld, heading = heading,
                          list_location = list_location)
  while (length(ret_str) > 0 && ret_str[length(ret_str)] == "") {
    ret_str <- ret_str[-length(ret_str)]
  }
  ret_str
}
