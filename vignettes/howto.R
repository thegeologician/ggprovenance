## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=TRUE,fig.width=6,fig.height=4)

## ----message=FALSE-------------------------------------------------------
library(ggprovenance)

## ---- results='asis', echo=FALSE-----------------------------------------
table<-read.xls(xls=system.file("extdata", "Tarim.xls", package="ggprovenance"),stringsAsFactors=FALSE)
knitr::kable(table[1:6,],format="markdown")

## ------------------------------------------------------------------------
agedata<-read.xls.flat(system.file("extdata", "Tarim.xls", package="ggprovenance"))

## ------------------------------------------------------------------------
names(agedata)

## ------------------------------------------------------------------------
cats<-data.frame(area=rep("n/a",length(agedata)),stringsAsFactors=FALSE)
row.names(cats)<-names(agedata)
cats$area[grep("Tb04",row.names(cats))]<-"Tarim"
cats$area[grep("Tb21",row.names(cats))]<-"Taklamakan"
cats$area[grep("Tb22",row.names(cats))]<-"Taklamakan"
cats$area[grep("Tb35",row.names(cats))]<-"Kunlun"
cats$area[grep("Tb38",row.names(cats))]<-"Kunlun"
cats$area[grep("Tb50",row.names(cats))]<-"Tian Shan"

## ---- results='asis', echo=FALSE-----------------------------------------
knitr::kable(cats,format="markdown")

## ------------------------------------------------------------------------
plotKDE(agedata)

## ------------------------------------------------------------------------
names(cats)
plotKDE(agedata,categories=cats,markers="dash",stack="close",limits=c(0,3500),mapping=aes(fill=area))

## ----eval=FALSE----------------------------------------------------------
#  ggsave("~/test.pdf",width=10,height=8)

## ----eval=FALSE----------------------------------------------------------
#  p1<-plotKDE(agedata)
#  
#  #
#  # ... a lot of clever code here, generating other plots (p2, p3,...)
#  #
#  
#  ggsave(plot=p1,filename="~/test.png",dpi=300,width=10,height=8)
#  # save p2, p3,...

## ----eval=FALSE----------------------------------------------------------
#  plotKDE<-function(ages,title,limits=c(0,max(unlist(agedata),na.rm=TRUE)),
#                    plotonly=names(ages),categories,mapping,breaks=NA,
#                    bandwidth=NA,splitat=NA,markers=c("none","dash","circle"),
#                    logx=FALSE,histogram=FALSE,binwidth=bandwidth,adaptive=TRUE,
#                    stack=c("equal","close","dense"),
#                    normalise=c("area","height","none"),lowcount=80,...)

## ------------------------------------------------------------------------
plotKDE(agedata)

## ------------------------------------------------------------------------
  plotKDE(agedata,plotonly=c("Tb22"))

