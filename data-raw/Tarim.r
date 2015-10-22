require(ggprovenance)

Tarim<-read.xls.flat(system.file("extdata", "Tarim.xls", package="ggprovenance"))

save(Tarim,file='data/Tarim.rda',compress='xz')
