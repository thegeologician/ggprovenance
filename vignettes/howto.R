## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=TRUE,fig.width=6,fig.height=4)

## ----message=FALSE-------------------------------------------------------
library(ggprovenance)

## ---- results='asis', echo=FALSE-----------------------------------------
table<-read.xls(xls=system.file("extdata", "Tarim.xls", package="ggprovenance"),
                stringsAsFactors=FALSE)
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

## ------------------------------------------------------------------------
plotKDE(agedata,plotonly=c("Tb22"),hist=TRUE,binwidth=50)

## ------------------------------------------------------------------------
plotKDE(agedata,plotonly=c("Tb22"),markers="dash")

## ------------------------------------------------------------------------
plotKDE(agedata,plot=c("Tb22"),breaks=seq(200,2800,200))

## ------------------------------------------------------------------------
plotKDE(agedata, limits=c(50,600))

## ------------------------------------------------------------------------
plotKDE(agedata,logx=TRUE)

## ------------------------------------------------------------------------
#plotKDE(agedata,splitat=600)

## ------------------------------------------------------------------------
#plotKDE(agedata,limits=c(50,600,1600,2800))

## ------------------------------------------------------------------------
plotKDE(agedata, bandwidth=15)

## ------------------------------------------------------------------------
plotKDE(agedata,bandwidth=-1)

## ------------------------------------------------------------------------
plotKDE(agedata,categories=cats, mapping=aes(fill=area))

## ------------------------------------------------------------------------
# load categories:
cats<-read.table(file=system.file("extdata", "categories.csv", package="ggprovenance"),
                 header=TRUE,row.names=1,sep=",",stringsAsFactors=FALSE)
plotKDE(agedata,categories=cats,mapping=aes(fill=area,linetype=type),
        limits=c(0,1200),stack="close",bandwidth=-1)
# adapt categories.csv to needs, re-run, repeat...


## ----echo=FALSE, eval=FALSE----------------------------------------------
#  plotKDE(agedata,aes(fill="black"))+theme(panel.background=element_blank(),
#  	panel.grid=element_blank(),axis.ticks=element_line(colour="black"),
#  	axis.text=element_text(size=rel(0.8), colour="black"))

## ------------------------------------------------------------------------
g<-plotKDE(agedata,bandwidth=-1,logx=TRUE,limits=c(50,3200),
           plotonly=c("Tb38","Tb35","Tb50","Tb22"),normalise="height")
g+annotate("rect",xmin=c(90,260,385),xmax=c(125,320,490),ymin=0,ymax=1.1,
           fill=c("#FF000022","#00FF0022","#0000FF22"),colour="black")

## ----eval=FALSE----------------------------------------------------------
#  plotMDS(mds, diss, col="", sym="", nearest=TRUE, labels=TRUE, symbols=TRUE,
#  	fcolour=NA, stretch=FALSE)

## ----eval=FALSE----------------------------------------------------------
#  plotShepard(mds, diss, xlab="dissimilarity", ylab="distance", title="")

