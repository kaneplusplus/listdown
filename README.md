
<!-- README.md is generated from README.Rmd. Please edit that file -->

# listdown

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/kaneplusplus/listdown.svg?branch=master)](https://travis-ci.org/kaneplusplus/listdown)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/kaneplusplus/listdown?branch=master&svg=true)](https://ci.appveyor.com/project/kaneplusplus/listdown)
[![Codecov test
coverage](https://codecov.io/gh/kaneplusplus/listdown/branch/master/graph/badge.svg)](https://codecov.io/gh/kaneplusplus/listdown?branch=master)
<!-- badges: end -->

## Overview

The listdown package provides functions to programmatically create R
Markdown files from named lists. It is intended for data analysis
pipelines where the presentation of the results is separated from their
creation. For this use case, a data processing (or analysis) is
performed and the results are provided in a single named list, organized
heirarchically. From the list, listdown creates a workflowr, pdf, word,
or html page. List element names denote sections, subsections,
subsubsection, etc. and the list elements contain the data structure to
be presented including graphs and tables. The goal of the package is not
to provide a finished, readable document. It is to provide a document
with all tables and visualization that will appear. This serves as a
starting point from which a user can organize outputs, describe a study,
discuss results, and provide conclusions.

listdown provides a reproducible means for *analytical* document
elements. It is most compatible with data analysis pipelines where the
data format is fixed but the analyses are either being updated, which
may affect *narrative* elements including the result discussion and
conclusion, or where the experiment is different, which affects all
narrative elements.

## Installation

<!--
You can install the released version of listdown from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("listdown")
```

And the development version from [GitHub](https://github.com/) with:
-->

You can install the development version of listdown from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kaneplusplus/listdown")
```

<!-- 
## Example

This is a basic example which shows you how to solve a common problem:


```r
library(listdown)
## basic example code
```
-->
