<!-- README.md is generated from README.Rmd. Please edit that file -->
ggprovenance
============

**AS OF YET IN EARLY DEVELOPMENT PHASE, COMPLETE CODE REWRITE! MANY THINGS ARE MISSING / WILL NOT WORK...**

A package using H. Wickham's `ggplot2` to produce data plots commonly used in sediment provenance analysis. This package uses P. Vermeesch's `provenance` package for calculations, but adds 'aesthetics' (cf. `ggplot2`), like colour, symbols, linetypes. Output can easily be exported into a range of file formats thanks to `ggsave()` or further edited with e.g. `theme()`. Several helper functions of general use are provided.

### Installation

To install, if you do not have `devtools` installed, run:

    install.packages("devtools")

Then pull the latest version from GitHub:

    devtools::install_github("thegeologician/ggprovenance")

### Overview

The main workhorse functions are:

-   `plotKDE()`
-   `plotMDS()`

Besides, functions to easily import data from common file formats like csv and xls (thanks to the `gdata` package), and to convert these to/from data objects used by `provenance`, are provided, as well as 'lesser' plotting functions like `plotDendrogram()` and `plotShepard()`. See helpfiles and vignettes.

This is less of an add-on to ggplot2, than a collection of wrappers to produce 'pretty' plots easily, while trying to maintain the princiles of the 'Grammar of Graphics'.
