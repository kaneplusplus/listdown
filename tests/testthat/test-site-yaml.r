test_that("Basic site yaml list works.", {

  source("reference.r")

  site_name <- "A Test Site"
  tab_name <- c("Tab 1", "Tab 2")
  rmd_name <- c("file1.rmd", "file2.rmd")
  output_dir <- "../html"
  navbar_title <- "Study 1234"
  expect_error(
    ld_site_yaml(site_name, tab_name[-1], rmd_name, navbar_title))

  site_yaml <- 
    ld_site_yaml(site_name, tab_name, rmd_name, navbar_title)

  write_if_make_reference(site_yaml, "site_yaml.rds")

  expect_equal(site_yaml, read_reference("site_yaml.rds"))
})
