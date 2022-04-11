#!/usr/bin/sh

Rscript -e "knitr::knit('listdown-jss.rmd', tangle = TRUE)"
Rscript -e "rmarkdown::render('listdown-jss.rmd')"
