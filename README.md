<!-- README.md is generated from README.Rmd. Please edit that file -->
ggprovenance
============

**This project has been superceeded by the [provenance](https://cran.r-project.org/web/packages/provenance/index.html) package by Pieter Vermeesch. Although provenance is not using the grammar of graphics paradigm, it has much more functionality and mathematical rigor. Development on ggprovenance has currently stopped until I feel that it is necessary to revive it in the form of a "ggplot-wrapper for provenance".**

A package using H. Wickham's `ggplot2` to produce data plots commonly used in sediment provenance analysis. This package uses P. Vermeesch's `provenance` package for calculations, but adds 'aesthetics' (cf. `ggplot2`), like colour, symbols, linetypes. Output can easily be exported into a range of file formats thanks to `ggsave()` or further edited with e.g. `theme()`. Several helper functions of general use are provided.

### Installation

To install, if you do not have `devtools` installed, run:

    install.packages("devtools")

Then pull the latest version from GitHub:

    devtools::install_github("thegeologician/ggprovenance",build_vignettes = TRUE)

### Overview

The main workhorse functions are:

-   `plotKDE()`
-   `plotMDS()`

Besides, functions to easily import data from common file formats like csv and xls (thanks to the `gdata` package), and to convert these to/from data objects used by `provenance`, are provided, as well as 'lesser' plotting functions like `plotDendrogram()` and `plotShepard()`. See helpfiles and vignettes.

This is less of an add-on to ggplot2, than a collection of wrappers to produce 'pretty' plots easily, while trying to maintain the princiles of the 'Grammar of Graphics'.
