#' @title Create a workflowr Header
#' @description Output a workflowr R Markdown header with specified title.
#' @param title the title of the page.
#' @param toc should the table of contents be generated? Default FALSE.
#' @importFrom ymlthis yml yml_title
#' @export
ld_workflowr_header <- function(title, toc = FALSE) {
  yml(
    list(title = title,
         site = "workflowr::wflow_site",
         output = 
           list(`workflowr::wflow_html` = 
                list(toc = toc)),
         editor_options = list(chunk_output_type = "console")),
    author = FALSE,
    date = FALSE)
}

