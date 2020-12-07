#' @title Create a listdown Object
#'
#' @description A listdown object provides information for how a presentation
#' list should be used to create an R Markdown document. It requires an
#' unquoted expression indicating how the presentation list will be loaded.
#' In addition, libraries required by the outputted document and other
#' parameters can be specified.
#' @param package a quoted list of package required by the outputted document.
#' @param decorator a named list mapping the potential types of list elements
#' to a decorator function.
#' @param decorator_chunk_opts a named list mapping the potential types of list
#' elements to chunk options that should be included for those types.
#' @param default_decorator the decorator to use for list elements whose type
#' is not inherited from the decorator list. If NULL then the those
#' elements will not be included when the chunks are written. By default
#' this is identity, meaning that the elements will be passed directly
#' (through the identity() function).
#' @param setup_expr an expression that is added before package are 
#' loaded. The expression is put into a chunk named `setup` with option
#' `include = FALSE` and is intended for initializing the document. For
#' example the expression `knitr::opts_chunk$set(echo = FALSE)` could be
#' used to turn echo'ing off for the entire document.
#' @param init_expr an initial expression that will be added to the outputted
#' document after the libraries have been called. This expression appears
#' after packages are loaded and before data is read.
#' @param load_cc_expr either an unquoted expression or a character string
#' that will be turned into an unquoted expression via str2lang to load the 
#' presentation list.
#' @param ... default options sent to the chunks of the outputted document.
#' @param chunk_opts a named list of options sent to the chunks of outputted
#' documents. Note: takes priority over argument provided to ...
#' @importFrom crayon red
#' @export
listdown <- function(package = NULL,
                     decorator = list(),
                     decorator_chunk_opts = list(),
                     default_decorator = identity,
                     setup_expr = NULL,
                     init_expr = NULL,
                     load_cc_expr = NULL,
                     ...,
                     chunk_opts = NULL) {

  if ( !("default_decorator" %in% names(as.list(match.call))) ) {
    default_decorator <- as.symbol("identity")
  } else {
    default_decorator <- as.list(match.call()$default_decorator)
  }

  if (is.null(chunk_opts)) {
    chunk_opts <- list(...)
  }

  not_r_chunk_opts <- not_r_chunk_opts(names(chunk_opts))
  if (length(not_r_chunk_opts) > 0) {
    stop(red("Unrecognized options:\n\t",
             paste(not_r_chunk_opts, collapse = "\n\t"),
             "\n", sep = ""))
  }

  # Check the chunk options of decorator_chunk_opts.
  for (i in seq_along(decorator_chunk_opts)) {
    not_r_chunk_opts <- not_r_chunk_opts(names(decorator_chunk_opts[[i]]))
    if (length(not_r_chunk_opts) > 0) {
      stop(red("Unrecognized options for element type",
               names(decorator_chunk_opts)[i], ":\n\t",
               paste(not_r_chunk_opts, collapse = "\n\t"),
               "\n", sep = ""))
    }
  }
  if ( !("decorator" %in% names(match.call())) ) {
    decorator <- NULL
  } else {
    if (as.character(as.list(match.call()$decorator)[[1]]) == "list") {
      decorator <- as.list(match.call()$decorator)[-1]
    } else {
      decorator <- eval(match.call()$decorator)
    }
    if ("list" %in% names(decorator)) {
      stop("You may not decorate a list. Consider making a list element you ",
           "would\nlike to present a class instance and define a decorator.")
    }
  }
  if (!is.null(load_cc_expr)) {
    load_cc_expr <- create_load_cc_expr(match.call()$load_cc_expr)
  }
  ret <- list(load_cc_expr = load_cc_expr,
              decorator = decorator,
              package = package,
              init_expr = match.call()$init_expr,
              setup_expr = match.call()$setup_expr,
              decorator_chunk_opts = decorator_chunk_opts,
              default_decorator = default_decorator,
              chunk_opts = chunk_opts)

  class(ret) <- "listdown"
  ret
}

#' @importFrom crayon yellow bold
#' @export
print.listdown <- function(x, ...) {
  cat(bold("\nListdown object description\n"))
  cat("\n")
  if ("package" %in% names(x)) {
    cat(bold("    Package(s) imported:\n"))
    for (package in x$package) {
      cat("\t", package, "\n", sep = "")
    }
  } else {
    warning(yellow("No packages imported."))
  }
  if ("setup_expr" %in% names(x)) {
    cat("\n")
    cat(bold("    Setup expression(s) (run before packages are loaded):\n"))
    cat("\t")
    if (length(x$setup_expr) == 0) {
      cat("(none)\n")
    } else {
      cat(deparse(x$setup_expr), sep = "\n\t")
    }
  }
  if ("init_expr" %in% names(x)) {
    cat("\n")
    cat(bold("    Initial expression(s) (run after packages are loaded):\n"))
    cat("\t")
    if (length(x$init_expr) == 0) {
      cat("(none)\n")
    } else {
      cat(deparse(x$init_expr), sep = "\n\t")
    }
  }
  if ("load_cc_expr" %in% names(x)) {
    cat("\n")
    cat(bold("    Expression to read data:\n"))
    cat("\t", deparse(x$load_cc_expr), "\n", sep = "")
  } else {
    warning(yellow("No load_cc expression provided."))
  }
  if ("decorator" %in% names(x)) {
    cat("\n")
    cat(bold("    Decorator(s):\n"))
    if (length(x$decorator) == 0) {
      cat("\t(none)\n")
    } else {
      ns <- format(c("Type", names(x$decorator)))
      cv <- c("Method", as.vector(unlist(sapply(x$decorator, deparse))))
      for (i in seq_along(ns)) {
        if (i == 1) {
          cat("\t", bold(ns[i]), "\t", bold(cv[i]), "\n", sep = "")
        } else {
          cat("\t", ns[i], "\t", cv[i], "\n", sep = "")
        }
      }
    }
  }
  if ("default_decorator" %in% names(x)) {
    cat("\n")
    cat(bold("    Defaut decorator:\n"))
    cat("\t", deparse(x$default_decorator), "\n", sep = "")
  }
  if ("chunk_opts" %in% names(x)) { 
    cat("\n")
    cat(bold("    Chunk option(s):\n"))
    if (length(x$chunk_opts) == 0) {
      cat("\t(none)\n")
    } else {
      for (i in seq_along(x$chunk_opts)) {
        cat("\t", names(x$chunk_opts)[i], " = ", 
            deparse(x$chunk_opts[[i]]), "\n",
            sep = "")
      }
    }
  }
  if ("decorator_chunk_opts" %in% names(x)) {
    cat("\n")
    cat(bold("    Decorator chunk option(s):\n"))
    if (length(x$decorator_chunk_opts) == 0) {
      cat("\t(none)\n")
    } else {
      for (i in seq_along(x$decorator_chunk_opts)) {
        cat("\t", bold("Type: "), names(x$decorator_chunk_opts)[i], ":", 
            sep = "")
        ns <- names(x$decorator_chunk_opts[[i]])
        ns[ns == ''] <- "(chunk name)"
        ns <- c("Option", ns)
        ns <- format(ns)
        cv <- unlist(x$decorator_chunk_opts[[i]])
        cv <- c("Value", cv)
        for (j in seq_along(ns)) {
          if (j == 1) {
            cat("\n\t\t", bold(ns[j]), " ", bold(cv[j]))
          } else {
            cat("\n\t\t", ns[j], " ", cv[j])
          }
        }
        cat("\n")
      }
    }
  }
  cat("\n")
  invisible(x)
}

#' @title Write a listdown Object to a String
#'
#' @description After a presentation list and listdown object have been
#' constructed the chunks can be rendered to a string, which can be appended
#' to a file, with appropriate headers, resulting in a compilable R Markdown
#' document.
#' @param ld the listdown object that provides
#' information on how a presentation object should be displayed in the
#' output.
#' @seealso \code{\link{listdown}}
#' @export
ld_make_chunks <- function(ld) {
  UseMethod("ld_make_chunks", ld)
}

#' @importFrom crayon red
ld_make_chunks.default <- function(ld) {
  stop(red("Don't know how to render an object of class ",
           paste(class(ld), collapse = ":"), ".", sep = ""))
}

expr_to_string <- function(expr) {
  if (deparse(expr[[1]]) == "{") {
    unlist(lapply(expr[-1], function(x) c(deparse(x))))
  } else {
    deparse(expr)
  }
}

#' @export
ld_make_chunks.listdown <- function(ld) {
  if (is.null(ld$load_cc_expr)) {
    stop("The load_cc_expr needs to be specified. ",
         "Use `create_load_cc_expr()` to set it.")
  }
  cc_list <- eval(ld$load_cc_expr)
  if (is.character(cc_list)) {
    cc_list <- eval(parse(text = cc_list))
  }
  ret_string <- ""
  if (length(ld$setup_expr)) {
    ret_string <- c(ret_string, 
      "```{r setup, include = FALSE}",
      expr_to_string(ld$setup_expr),
      "```",
      "")
  }
  ret_string <-
    c(ret_string,
      sprintf("```{r%s}", make_chunk_option_string(ld$chunk_opts)))
  if (length(ld$package) > 0) {
    ret_string <-
      c(ret_string,
        as.character(vapply(eval(ld$package),
                     function(x) sprintf("library(%s)", as.character(x)),
                     NA_character_)),
        "")
  }
  if (length(ld$init_expr)) {
    ret_string <-
      c(ret_string,
        expr_to_string(ld$init_expr),
        "")
  }
  c(ret_string, 
    sprintf("cc_list <- %s", deparse(ld$load_cc_expr)),
    "```",
    depth_first_concat(cc_list, ld))
}
