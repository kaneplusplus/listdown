
#' Create a `listdown` Document Bundle
#'
#' @description A page bundle encapsulates the computational components, 
#' R Markdown header, and listdown object. Together, these three objects 
#' are sufficient to create a document, which can be written with the
#' `ld_create_document()` function.
#' @seealso ld_create_document
#' @param cc the computational component list that will be presented.
#' @param header a `list` with the header information for the document.
#' @param ld a `listdown` object describing how to present the computational
#' components.
#' @examples
#' library(ggplot2)
#' cc <- list(
#'     iris = iris,
#'      Sepal.Length = list(
#'        Sepal.Width = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#'             geom_point(),
#'        Petal.Length = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#'             geom_point(),
#'      Colored = list(
#'           Sepal.Width = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
#'             color = Species)) + geom_point(),
#'           Petal.Length = ggplot(iris, aes(x = Sepal.Length, y = Petal.Length,
#'             color = Species)) + geom_point())))
#'
#' header <- ld_rmarkdown_header("Test header", author = "Some Dude",
#'                               date = "2020")
#'
#' ld <- listdown(package = "ggplot2")
#'
#' ld_bundle_doc(cc, header, ld)
#' @export
ld_bundle_doc <- function(cc, header, ld) {
  mc <- match.call()

  cc_in_memory <- FALSE
  if (inherits(mc$cc, "name") || inherits(mc$cc, "call")) {
    if (is.list(cc)) {
      cc_in_memory <- TRUE
    } else if (is.character(cc)) {
      ld$load_cc_expr <- create_load_cc_expr(cc)
      cc <- NULL
    }
  } else if (inherits(mc$cc, "call") || inherits(mc$cc, "character")) {
    ld$load_cc_expr <- create_load_cc_expr(mc$cc)
    cc <- NULL
  } else if (inherits(mc$cc, "NULL")) {
    if (is.null(ld$load_cc_expr)) {
      stop("A computational arugment must be specified.")
    }
  } else {
    stop("Unsupported `cc` argument type.")
  }

  if (!inherits(ld, "listdown")) {
    stop("The ld parameter must inherit from type `listdown`.")
  }

  ret <- list(cc = cc, ld = ld, header = header, cc_in_memory = cc_in_memory)
  class(ret) <- "ld_page_bundle"
  ret
}

get_bundle_cc <- function(x) {
  if (x$cc_in_memory) {
    x$cc
  } else {
    eval(x$ld$load_cc_expr)
  }
}

#' @export
print.ld_page_bundle <- function(x, ...) {
  cat("\nHeader\n\n    ")
  cat(paste(as.character(x$header), collapse = "\n    "))
  cat("\n\n")
 
  cc <- get_bundle_cc(x) 
  cat("Computational components\n\n    ")
  cat(paste(ld_cc_dendro(cc)[-(1:2)], collapse = "\n    "))
  cat("\n")

  print(x$ld)

  invisible(x)
}

#' @importFrom utils tail
remove_file_extension <- function(x) {
  re <- gregexpr("\\.", basename(x))
  vapply(
    seq_along(re),
    function(i) {
      if (re[[i]][1] == -1) {
        x[i]
      } else {
        rind <- tail(re[[i]], 1)
        substr(x[i], 1, rind - 1)
      }
    },
    NA_character_
  )
}

#' Create a Minimal Site YAML List
#' 
#' @param site_name the name of the site.
#' @param tab_name the name of the tabs on the site.
#' @param rmd_name the name of the Rmarkdown files that will generate the
#' respective tabs.
#' @param navbar_title the title of the navigation bar (Default is the
#' `site_name` argument.
#' @importFrom checkmate assert check_character check_null
#' @importFrom yaml as.yaml
#' @export
ld_site_yaml <- 
  function(site_name, tab_name, rmd_name, navbar_title = site_name) {

  assert(
    check_character(site_name),
    check_character(navbar_title),
    combine = "and"
  )

  assert(
    check_null(rmd_name),
    check_character(rmd_name),
    combine = "or"
  )

  if (length(tab_name) != length(rmd_name)) {
    stop("There must be one `tab_name` per `rmd_name`.")
  }
  
  html_name <- paste0(remove_file_extension(basename(rmd_name)), ".html")

  tab_layout <- 
    lapply(
      seq_along(tab_name),
      function(i) {
        list(text = tab_name[i],
             href = html_name[i])
      })

  list(name = site_name,
       navbar = list(title = navbar_title, left = tab_layout))
}

tabs_and_href <- function(l) {
  ul <- unlist(l)
  nav_text_inds <- grep("navbar.+text", names(ul))
  with_href <- grep("navbar.+href", names(ul)[nav_text_inds + 1])
  nav_text_inds <- nav_text_inds[with_href]
  data.frame(name = ul[nav_text_inds], href = ul[nav_text_inds + 1])
}

make_dirs_as_needed <- function(dir_paths) {
  unmade_dirs <- dir_paths[which(!dir.exists(dir_paths))]
  for (i in seq_along(unmade_dirs)) {
    dir.create(unmade_dirs[i], recursive = TRUE)
  }
  unmade_dirs
}

#' @title Build an html Site from listdown Document Bundles
#'
#' @description This function creates an html website with each tab in the
#' page being desribed by a listdown document bundle.
#' @param doc_bundles a named list of document bundles. There can be up to one
#' unnamed bundle, which will be assumed to correspond to an index.rmd file.
#' @param site_yaml a list of site information, which will be written
#' to the _site.yml file.
#' @param site_dir the directory where the site (rmd, data, and html files)
#' will be written to.
#' @param rmd_dir the directory where the R Markdown files will reside. By 
#' default an "rmarkdown" file is written to `tempdir()`.
#' @param data_dir the location where data can be found for each bundle.
#' If the data is held in memory for a listdown document bundle, then it will
#' be written to the specified directory. If mulitple directories are specified,
#' then the directory is specified per bundle, with index recycling used if
#' the number of directories is not the same as the number of bundles.
#' @param html_dir the location of the rendered document, relative to the 
#' directory specified by `rmd_dir`. Note that this is an {{rmarkdown}}
#' convention. By default a directory names "html" is created in the 
#' directory specified by `rmd_dir` and rendered documents are place there.
#' @param render_site should the page be rendered? If not then the 
#' `html_dir` is not created.
#' @param view should the output document be opened after rendering? By 
#' default, if `render_doc` is `TRUE` and this argument is `TRUE` then
#' the browser will open for you to examine the output.
#' @param make_data_dir if the `data_dir` directory is not present, should it
#' be created? This can be set to `FALSE` when data already resides on disk
#' to verify that it is not being created and written.
#' @param make_rmd_dir if the `rmd_dir` directory is not present, should it
#' be created? This can be set to `FALSE` when data already resides on disk
#' to verify that it is not being created and written.
#' @param ... argument to be passed to the `rmarkdown::render_site()` function.
#' @seealso ld_bundle_doc ld_create_doc
#' @importFrom checkmate assert check_character check_list
#' @importFrom tibble tibble as_tibble
#' @importFrom rmarkdown render_site
#' @importFrom yaml write_yaml
#' @importFrom stats na.omit
#' @importFrom fs path_rel
#' @importFrom utils browseURL
#' @export
ld_build_html_site <- 
  function(
    doc_bundles, 
    site_yaml,
    site_dir = tempdir(),
    rmd_dir = file.path(site_dir, "rmarkdown"), 
    data_dir = file.path(site_dir, "data"),
    html_dir = file.path(site_dir, "html"),
    render_site = TRUE,
    view = interactive(),
    make_data_dir = TRUE,
    make_rmd_dir = TRUE,
    ...) {

  if (!all(vapply(doc_bundles, 
                  function(x) inherits(x, "ld_page_bundle"), NA))) {
    stop("All pages must inherit from `ld_page_bundle`")
  }

  assert(
    check_character(rmd_dir),
    check_character(data_dir),
    check_character(html_dir),
    check_list(site_yaml),
    combine = "and"
  )

  if (make_data_dir) {
    make_dirs_as_needed(data_dir)
  }
  if (make_rmd_dir) {
    make_dirs_as_needed(rmd_dir)
  }

  if (length(data_dir) != 1 && length(data_dir) != length(doc_bundles)) {
    stop("The data directory must be a single directory or one per bundle.")
  }
  if (is.null(site_yaml$output_dir) || site_yaml$output_dir == "") {
    site_yaml$output_dir <- path_rel(html_dir, rmd_dir)
  } else {
    warning(paste("Output dir specified in yaml list. The `html_dir`", 
                  "argument will be ignored."))
  }

  out_fns <- tabs_and_href(site_yaml)
  out_fns$rmd <- paste0(remove_file_extension(out_fns$href), ".Rmd")

  if ( !( ("index.html" %in% out_fns$href) || 
          ("" %in% names(doc_bundles)) ) ) {
    stop("An index.html file must be specified.")
  }

  # See if we have an unnamed bundle. It should be the index.rmd file. 
  if (any(names(doc_bundles) == "")) {
    if (sum(names(doc_bundles) == "") > 1) {
      stop("You may only have 1 unnamed page bundle to be the index.rmd file.")
    }
    if (any(names(doc_bundles) == "index")) {
      stop("You may not specify an index bundle and have an unnamed bundle.")
    }
    unnamed_id <- which(names(doc_bundles) == "")
    ld_create_doc(
      ldb = doc_bundles[[unnamed_id]], 
      rmd_file_name = "index.Rmd",
      cc_file_name = "index.rds",
      rmd_dir = rmd_dir,
      data_dir = data_dir[recycle(unnamed_id, length(rmd_dir))],
      output_dir = html_dir,
      render_doc = FALSE)

    doc_bundles <- doc_bundles[-unnamed_id]
  }
  bundles <- tibble(name = names(doc_bundles), bundle = doc_bundles)
  if ( !("name" %in% names(bundles)) ) { 
    bundles$name <- out_fns$name
  }
  bundles <- as_tibble(merge(bundles, out_fns, by = "name", all = TRUE))
  bundles$rds<- paste0(remove_file_extension(bundles$rmd), ".rds")
  if (nrow(bundles) != nrow(na.omit(bundles))) {
    stop("Unknown problem merging page bundles with file names.")
  }
  for (i in seq_len(nrow(bundles))) {
    ld_create_doc(
      ldb = bundles$bundle[[i]],
      rmd_file_name = bundles$rmd[i],
      cc_file_name = bundles$rds[i],
      rmd_dir = rmd_dir,
      data_dir = path_rel(data_dir[recycle(i, length(rmd_dir))], rmd_dir),
      output_dir = html_dir,
      render_doc = FALSE)
  }
  write_yaml(site_yaml, file.path(rmd_dir, "_site.yml"))
  
  if (render_site) {
    render_site(rmd_dir, ...)
    if (view) {
      browseURL(file.path(rmd_dir, site_yaml$output_dir, "index.html"))
    }
  }
  normalizePath(file.path(html_dir, "index.html"), mustWork = FALSE)
}

#' @importFrom checkmate check_numeric
recycle <- function(ind, len) {
  assert(
    check_numeric(ind),
    check_numeric(len),
    length(len) == 1,
    combine = "and"
  )
  (ind - 1) %% len + 1
}

#' @title Create a Document from a `listdown` Bundle
#'
#' @description This function creates a document, defined by a listdown bundle
#' in a specified location on disk and, optionally, opens the document in the
#' browser.
#' @seealso ld_bundle_doc
#' @param ldb a listdown doc bundle.
#' @param rmd_file_name the name of the R Markdown file to create. By default,
#' a temporary file is created.
#' @param rmd_dir the directory where the output R Markdown file should be
#' written to. By default, this is `tempdir()`.
#' @param output_dir the location of the rendered document, relative to the 
#' directory specified by `rmd_dir`. Note that this is an {{rmarkdown}}
#' convention. By default a directory names "pres" is created in the 
#' directory specified by `rmd_dir` and rendered documents are place there.
#' @param render_doc should the page be rendered? If not then the 
#' `output_dir` is not created.
#' @param cc_file_name the name of the list specifying the computational 
#' components. If this is `NULL` (the default) then the listdown bundle
#' is checked to make sure it's `load_cc_expr` attribute has been specified.
#' If it is specified, and the bundles `load_cc_expr` has not been specified,
#' then it will be written to disk (in the corresponding data directory, 
#' specified by `data_dir`) and read via the `saveRDS()` function.
#' @param data_dir the directory where data should be written. If the
#' `cc_file_name` argument is `NULL` then this argument is ignored. If the
#' `cc_file_name` argument is specfied but `data_dir` is not, then `tempdir()` 
#' is used.
#' @param view should the output document be opened after rendering? By 
#' default, if `render_doc` is `TRUE` and this argument is `TRUE` then
#' the browser will open for you to examine the output.
#' @param ... options to send to the rmarkdown::render() function.
#' @importFrom checkmate assert check_class
#' @importFrom rmarkdown render
#' @importFrom fs path_rel path_abs
#' @export
ld_create_doc <- 
  function(
    ldb, 
    rmd_file_name = basename(tempfile(pattern = "rmarkdown", fileext = ".Rmd")),
    rmd_dir = tempdir(),
    output_dir = rmd_dir,
    render_doc = TRUE,
    cc_file_name = NULL,
    data_dir = ".",
    view = interactive(),
    ...) {

  assert(
    check_class(ldb, "ld_page_bundle"),
    combine = "and"
  )

  if (ldb$cc_in_memory) {
    if (is.null(data_dir)) {
      data_dir <- "../data"
      warning("Argument `data_dir` is not specified, ../data will be used.")
    }
    if (is.null(cc_file_name)) {
      cc_file_name <- basename(tempfile(pattern = "data", fileext = ".rds"))
      warning("Argument `cc_file_name` not specified ", 
              cc_file_name, " will be used.")
    }
    data_path <- file.path(rmd_dir, data_dir, cc_file_name)
    make_dirs_as_needed(dirname(path_abs(data_path)))
    saveRDS(ldb$cc, path_abs(data_path))
    ldb$ld$load_cc_expr <- 
      create_load_cc_expr(
        paste0('readRDS("', file.path(data_dir, cc_file_name), '")'))
  }

  rmd_path <- file.path(rmd_dir, rmd_file_name)
  ld_write_file(
    rmd_header = ldb$header, 
    ld = ldb$ld, 
    file_name = rmd_path)

  if (render_doc) {
    output_path <- render(input = rmd_path, output_dir = output_dir, ...)
    if (view) {
      browseURL(output_path)
    }
  }

  invisible(ldb)
}


