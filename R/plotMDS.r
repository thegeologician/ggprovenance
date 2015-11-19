#########################################
## (c) Martin Rittner, 2015            ##
## sw@thegeologician.net               ##
#########################################

#########################################
##
## functions for plotting geological
## sediment provenance analysis data
##
## using Hadley Wickham's ggplot2 for
## graphical output
##
## maths and data transformation based on
## Pieter Vermeesch's provenance package
##
## including convenience data import and
## wrapper functions
##
#########################################

#' Plotting multi-dimensional scaling maps.
#'
#' @param x Input data. Optional (see \code{diss}.
#' @param diss Optional distances/dissimilarity matrix. Mandatory if \code{x}
#' is missing.
#' @param categories A data frame giving categorising information on samples.
#' @param mapping Aesthetic mapping, see \code{\link[ggplot2]{aes}}.
#' @param nearest Plot lines connecting nearest distances.
#' @param fixed Plot in fixed (1:1) aspect ratio.
#' @param axes Plot axes tick marks.
#' @param square Expand plot area to yield square plot.
#' @param metric Metric to use in dissimilarity calculation.
#'
#' @return A ggplot object containing the specified plot.
#'
#' @export
plotMDS<-function(x,diss,categories,mapping,nearest=TRUE,fixed=FALSE,
                  axes=FALSE,square=FALSE,metric=c("K-S","C-v-M")){
  # x ... optional! list of one- or two-dimensional vectors/data.frames/matrices, which causes distributional
  #       compositional data to be assumed.
  # diss ... optional distance matrix. If x is missing, diss must be given, and vice versa, or both, in which
  #           case diss is taken as the calculated distances, to which the plotted are compared (nearest=TRUE).

  if(missing(x) & missing(diss))stop("x and diss can not both be omitted")
  if(missing(x)){
    if(!(is.matrix(diss) || is.data.frame(diss)))stop("diss of wrong format with x missing")
  }
  if(is.list(x)){
    if(!((all(sapply(x,is.numeric))&all(sapply(x,is.vector))) ||
         (all(sapply(x,is.data.frame))&all(sapply(unlist(x),is.numeric))))){
      stop("can only handle numeric data in x")
    }
  }
  # TODO: many more tests
  # TODO: handle mixed data frames, by supplying the names of the actual data to compare...

#   zrndiss=dissimilarity(zrndata)
#   zrnmds<-isoMDS(as.matrix(zrndiss))$points
#   zrnmds<-as.data.frame(zrnmds)
#   names(zrnmds)<-c("x","y")
#   zrnmds$smpl<-row.names(zrnmds)
#   zrnmds$site<-site


}

# define a function to plot the MDS coordinates and connect the nearest neighbours, ggplot2-style
plotMDSo <- function(mds,diss,col="",sym="",nearest=TRUE,labels=TRUE,symbols=TRUE,fcolour=NA,
                    stretch=FALSE,axes=FALSE,expand=FALSE) {
  #quick and dirty, makes big assumptions about what mds looks like if it's not a two-column matrix.
  #mds can be: data.frame(x,y,any,further,columns,...)
  #
  # mds     ... data.frame() or matrix with at least 2 columns representing x and y
  #              coordinates calculated by cmdscale() or isoMDS()
  # diss    ... (dis-)similarities of samples, only required if nearest==TRUE
  # col     ... string giving the name of column in mds to be used for colour scale
  # sym     ... string giving the name of column in mds to be used for symbol scale
  # nearest ... boolean - plot lines connecting nearest neighbours?
  # labels  ... boolean - plot data labels (taken from row.names of mds)?
  # symbols ... boolean - plot data points (useful if plotting only labels)?
  # fcolour ... symbol fill colours - not very functional yet...
  # stretch ... boolean - strech plot to full extend, or use fixed x/y scale ratio
  # axes    ... boolean, plot axis labels?
  # expand  ... boolean, expand plot area to be square (only valid if stretch==FALSE)

  # TODO: let labels optionally be taken from a column in mds
  # TODO: if length(col) too great for brewer, switch to alternative colour scale
  # TODO: more symbols for symbol scale
  # TODO: check diss if nearest==TRUE

  require(ggplot2)
  require(scales)
  defcol<-"lightblue"
  ssize<-9
  tsize<-3
  ddf<-as.data.frame(mds)
  if(!all(c("x","y") %in% names(ddf)))names(ddf)[1:2]<-c("x","y")
  # create a new (empty) plot
  #	plot(mds[,1],mds[,2],type='n')
  p<-ggplot()
  # draw lines between closest neighbours
  #	for (j in 1:nrow(mds)) {
  #		lines(c(mds[j,1],x1[j]),c(mds[j,2],y1[j]),lty=1)
  #		lines(c(mds[j,1],x2[j]),c(mds[j,2],y2[j]),lty=2)
  #	}
  if(nearest){
    # indices of nearest and second nearest neighbours:
    i = t(apply(as.matrix(diss),1,function(x) order(x))[2:3,])
    # plot coordinates for the lines
    x1 = as.vector(ddf[i[,1],"x"])
    y1 = as.vector(ddf[i[,1],"y"])
    x2 = as.vector(ddf[i[,2],"x"])
    y2 = as.vector(ddf[i[,2],"y"])
    p<-p+geom_segment(data=ddf,x=ddf[["x"]],y=ddf[["y"]],xend=x1,yend=y1,linetype="solid")
    p<-p+geom_segment(data=ddf,x=ddf[["x"]],y=ddf[["y"]],xend=x2,yend=y2,linetype="dashed")
  }
  # plot the configuration as labeled circles
  #	points(mds[,1],mds[,2],pch=21,cex=2.5,col='red',bg='white')
  # TODO: a lot of checks on col and sym, sensible automatic assumptions
  if(symbols){
    if(length(ddf)>2){
      if(col==""){
        if(sym==""){
          p<-p+geom_point(data=ddf,aes_string(x="x",y="y"),fill=defcol,colour="black",size=ssize,shape=21)
        }else{
          p<-p+geom_point(data=ddf,aes_string(x="x",y="y",shape=sym),fill=defcol,colour="black",size=ssize)
        }
      }else{
        if(sym==""){
          p<-p+geom_point(data=ddf,aes_string(x="x",y="y",fill=col),colour="black",size=ssize,shape=21)
        }else{
          p<-p+geom_point(data=ddf,aes_string(x="x",y="y",fill=col,shape=sym),colour="black",size=ssize)
        }
      }
    }else{
      c2=defcol
      if(col!=""){
        if(length(col)>1){
          warning("more than one colour value specified - using default")
          c2<-defcol
        }else{
          # TODO: check if col IS a colour value
          c2<-col
        }
      }
      p<-p+geom_point(data=ddf,aes(x=x,y=y),fill=c2,colour="black",size=ssize,shape=21)
    }
  }
  #	text(mds[,1],mds[,2],row.names(mds))
  #add labels:
  if(labels)p<-p+geom_text(data=ddf,aes(x=x,y=y),label=row.names(ddf),size=tsize)

  #apply colour scale, symbol scale, format plot:

  cols<-c(rgb(t(col2rgb(defcol))/255))  #default colour
  #print(cols)
  #browser()
  if(col!=""){
    if(length(fcolour)==0||is.na(fcolour)){
      cols<-brewer_pal(type="div",palette=6)(length(unique(ddf[[col]])))
    }else if(length(fcolour)==1){
      #nothing to do
    }else if(length(fcolour)==length(unique(ddf[[col]]))){
      ucols<-fcolour
      names(ucols)<-unique(ddf[[col]])
      cols<-ucols[match(sort(unique(ddf[[col]])),names(ucols))]
      # FIXME: allow reordering here...
    }else{
      warning("invalid fill colour parameter - using default")
      #...to be precise: "leaving" default....
    }
  }else{
    if(length(fcolour)==1){
      cols<-rep(fcolour,times=length(ddf[[1]]))
    }
  }
  #print(cols)
  if(length(cols)>1)p<-p+scale_fill_manual(values=cols)
  if(sym!=""){
    bks<-unique(ddf[[sym]])	# TODO: warn if too many, define new ones?
    p<-p+scale_shape_manual(values=rep(c(21,22,24,23),length.out=10)[1:length(bks)])
  }
  p<-p+theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
    theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
    theme(panel.background=element_rect(fill=NA,colour="black"),plot.background=element_rect(fill=NA,colour=NA))
  if(!stretch){
    p<-p+coord_equal()
    if(expand){
      xrange<-range(ddf$x)
      yrange<-range(ddf$y)
      maxrange<-c(min(xrange[1],yrange[1]),max(xrange[2],yrange[2]))
      p<-p+xlim(maxrange)+ylim(maxrange)
    }
  }
  if(col!=""){
    p<-p+guides(fill=guide_legend(title=NULL,order=2,override.aes=list(fill=cols,shape=21)),shape=guide_legend(title=NULL,order=1))
  }
  if(!axes){
    p<-p+theme(axis.ticks=element_blank(), axis.text=element_blank())
  }
  return(p)
}

#########################################

plotShepard<-function(mds,diss,xlab="dissimilarity",ylab="distance",title=""){
  #create a Shepard plot to evaluate MDS quality
  # mds ... data.frame() or 2-column matrix containing x and y coordinates
  # dis ... distance- or dissimarity matrix
  # xlab, ylab ... x- and y-axis labels
  # title ... plot title

  require(MASS)
  shp<-Shepard(as.dist(diss),as.matrix(mds[,c("x","y")]))
  shp<-as.data.frame(shp)
  #calculate stress factor:
  stress<-100*sqrt(sum((shp$yf-shp$y)^2)/sum(shp$y^2))
  #create plot:
  p<-ggplot(data=shp)+
    geom_point(aes(x=x,y=y),size=2)+
    geom_step(aes(x=x,y=yf),size=1,colour="red")+
    geom_text(x=max(shp$x),y=min(shp$y),label=sprintf("stress: %.1f %%",stress),size=8,hjust=1,vjust=0)+
    xlab(xlab)+ylab(ylab)
  if(title!="")p<-p+geom_text(x=min(shp$x),y=max(shp$y),label=title,size=8,hjust=0,vjust=1)

  return(p)
}
