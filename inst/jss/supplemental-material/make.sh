#!/usr/bin/sh

Rscript -e "knitr::knit('listdown-jss.rmd', tangle = TRUE)"
Rscript -e "knitr::knit('listdown-jss.rmd')"
