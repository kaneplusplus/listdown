<!-- README.md is generated from README.Rmd. Please edit that file -->

# listdown

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/listdown?color=blue)](https://cran.r-project.org/package=listdown)
[![R-CMD-check](https://github.com/kaneplusplus/listdown/workflows/R-CMD-check/badge.svg)](https://github.com/kaneplusplus/listdown/actions)
[![Codecov test
coverage](https://codecov.io/gh/kaneplusplus/listdown/branch/master/graph/badge.svg)](https://codecov.io/gh/kaneplusplus/listdown?branch=master)
<!-- badges: end -->

## Overview

The {listdown} package provides functions to programmatically create R
Markdown files from named lists. It is intended for data analysis
pipelines where the presentation of the results is separated from their
creation. For this use case, a data processing (or analysis) is
performed and the results are provided in a single named list, organized
hierarchically. With the list and a {listdown} object a workflowr, pdf,
word, or html page. List element names denote sections, subsections,
subsubsection, etc. and the list elements contain the data structure to
be presented including graphs and tables. The goal of the package is not
to provide a finished, readable document. It is to provide a document
with all tables and visualization that will appear (*computational*
components). This serves as a starting point from which a user can
organize outputs, describe a study, discuss results, and provide
conclusions (*narrative* components).

{listdown} provides a reproducible means for producing a document with
specified computational components. It is most compatible with data
analysis pipelines where the data format is fixed but the analyses are
either being updated, which may affect narrative components including
the result discussion and conclusion, or where the experiment is
different, which affects all narrative components If the narrative
components are not changing with the data being pushed through your
analysis pipeline, then you may be better off writing the R Markdown
code manually.

## Installation

You can install the released version of listdown from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("listdown")
```

The development version of {listdown} can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kaneplusplus/listdown")
```

## Example

As a toy example, suppose we would like to create an html document
plotting Anscombe’s quartet with each plot having it’s own section. To
construct the document, we will need to two objects. The first is a
presentation list, whose names indicate section (or subsection) titles
and whose elements are the objects to present. The second is a
`listdown` object, which describes how the object should be rendered in
the document.

``` r
library(listdown)
library(ggplot2)

data(anscombe)

# Create the ggplot objects to display.
pres_list <- list(
  Linear = ggplot(anscombe, aes(x = x1, y = y1)) + geom_point() + theme_bw(),
  `Non Linear` = ggplot(anscombe, aes(x = x2, y = y2)) + geom_point() + theme_bw(),
  `Outlier Vertical`= ggplot(anscombe, aes(x = x3, y = y3)) + geom_point() + theme_bw(),
  `Outlier Horizontal` = ggplot(anscombe, aes(x = x4, y = y4)) + geom_point() + theme_bw())

# Save the pres_list object so that it can be used in the R Markdown document.
saveRDS(pres_list, "pres-list.rds")

# Create a listdown object.
ld <- listdown(load_cc_expr = readRDS("pres-list.rds"), # The expression to load pres_list.
               package = "ggplot2")                     # The packages needed to render plots.

# Output an html document to a string.
doc <- c(
  as.character(
    ld_rmarkdown_header("Anscombe's Quartet",
                        author = "Francis Anscombe",
                        date = "1973")),
  ld_make_chunks(ld))

cat(paste(doc, collapse = "\n"))
```

    #> ---
    #> title: Anscombe's Quartet
    #> author: Francis Anscombe
    #> date: '1973'
    #> output: html_document
    #> ---
    #> 
    #> ```{r}
    #> library(ggplot2)
    #> 
    #> cc_list <- readRDS("pres-list.rds")
    #> ```
    #> 
    #> # Linear
    #> 
    #> ```{r}
    #> cc_list$Linear
    #> ```
    #> 
    #> # Non Linear
    #> 
    #> ```{r}
    #> cc_list$`Non Linear`
    #> ```
    #> 
    #> # Outlier Vertical
    #> 
    #> ```{r}
    #> cc_list$`Outlier Vertical`
    #> ```
    #> 
    #> # Outlier Horizontal
    #> 
    #> ```{r}
    #> cc_list$`Outlier Horizontal`
    #> ```

The document can then be written to a file, rendered, and viewed with
the following code.

``` r
library(rmarkdown)

writeLines(doc, file("anscombe.Rmd"))
render("anscombe.Rmd")
browseURL("anscombe.html")
```

<!-- 
## Example

This is a basic example which shows you how to solve a common problem:


```r
library(listdown)
## basic example code
```
-->

## Code of Conduct

Please note that the {listdown} project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
