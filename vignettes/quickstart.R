## ----echo=FALSE----------------------------------------------------------
suppressPackageStartupMessages(library('ggprovenance'))

## ----message=FALSE-------------------------------------------------------
library(ggprovenance)
path_to_data<-system.file("extdata", "Tarim.xls", package="ggprovenance")
agedata<-read.xls.flat(path_to_data)

## ------------------------------------------------------------------------
plotKDE(agedata,hist=TRUE,binwidth=50,markers="dash")

