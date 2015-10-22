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

#' @title Plot one or several KDEs using ggplot2.
#'
#' @description \code{plotKDE} takes a simple named list containing the ages
#' measured on each sample, and produces rich graphics of kernel density
#' estimates (KDEs). A multitude of parameters makes the function complex, but
#' gives fine control over the final graphic, in many cases producing output
#' that needs no further editing in other software, see examples and vignette.
#' Although syntax differs from \code{ggplot} functions, the concept of mapping
#' data values (from parameter \code{categories}) onto aesthetics remains.
#'
#' @param ages A named list of ages for each sample/data set.
#' @param title Overal plot title.
#' @param limits Numerical vector giving the range of ages to plot.
#' @param breaks Numerical vector of break values along x-axis.
#' @param bandwidth Bandwidth for KDE and histogram calculation.
#' @param plotonly Vector of names of samples to plot.
#' @param splitat Numerical, split plots into left and right plot.
#' @param categories A data frame giving categorising information on samples.
#' @param markers Character, type of tick marks indicating input data.
#' @param logx Boolean, plot x-axis in log-scale.
#' @param hist Boolean, add histogram.
#' @param binwidth Bin width for histograms.
#' @param adaptive Boolean, use Abramson's adaptive bandwith modifier.
#' @param stack Character, stack plots more closely (see details).
#' @param normalise Character, normalise KDEs to height or area (default).
#' @param lowcount Minimum number for robust data set.
#'
#' @details If \code{limits} is of length 2 it specifies the range of ages to
#' plot. This will be split at age \code{splitat}, if given. If \code{limits}
#' is of length 4, it specifies the ranges for the left and right half plots,
#' respectively, and overrides \code{splitat}. KDEs and histograms are
#' calculated on the whole data, and normalised before truncation at
#' \code{limits}.
#'
#' If \code{bandwith = NA} (default), the median of calculated 'optimal
#' bandwidths' (function \code{botev} of package \code{provenance}) is used on
#' all KDEs. If a numerical value is given, this will be used for all KDEs. If
#' \code{bandwidth = -1}, the individual optimal bandwidths are used for each
#' KDE.
#'
#' \code{splitat} specifies an age to split the plots into half at. The two
#' halfes will both oppupy half the available space, at differing x scales to
#' accomodate the range especified by \code{limits} overall.
#'
#' \code{categories} contains any meta-information on samples, one line per
#' sample. Any categories given can be mapped onto aesthetics with the
#' \code{mapping} parameter.
#'
#' \code{markers} indicates the type of data markers along the x-axis to
#' indicate sampled ages. Possible values are \code{"dash"}, \code{"circle"},
#' or \code{"none"} (default).
#'
#' If histograms are plotted (\code{hist = TRUE}), the bin width can optionally
#' be set by specifying \code{binwidth}. Values of \code{NA} or \code{-1} both
#' cause the binwidth to be set automatically to the median of 'optimal
#' bandwidths'.
#'
#' \code{stack} specifies vertical spacing of stacked plots. One of
#' \code{"equal"} (default), giving each plot the same height, \code{"close"},
#' letting each plot only take up as much space as its maximum y value
#' requires, and \code{"dense"}, stacking non-filled KDEs as closely as
#' possible. \code{"close"} has no effect if \code{normalise="height"}, and
#' \code{"dense"} only works when \code{mapping=aes(fill=NULL)} is specified.
#'
#' \code{normalise} may be one of \code{"area"} (default), making KDEs the same
#' area, \code{"height"}, making KDEs the same height, or \code{"none"}.
#'
#' \code{lowcount} gives the minimum number to consider a data set robust.
#' Smaller sets will be drawn in a dashed line, unless the \code{linetype}
#' aestetic is specified in \code{mapping}. Set to \code{-1} to disable.
#'
#' Tip: The returned ggplot object can be further modified with
#' \code{ggplot2}'s functions (e.g. \code{\link{theme}}) and saved in many file
#' formats using \code{\link{ggsave}}.
#'
#' @return A ggplot object containing the specified plot.
#'
#' @export
#'
plotKDE<-function(ages,title,limits=c(0,3000),breaks=NA,bandwidth=NA,
                  plotonly=names(ages),splitat=NA,categories=NA,
                  markers=c("none","dash","circle"),logx=FALSE,hist=FALSE,
                  binwidth=bandwidth,adaptive=TRUE,
                  stack=c("equal","close","dense"),
                  normalise=c("area","height","none"),lowcount=80,mapping=NA){
                  # cutoffy=0,

  stack<-match.arg(stack)
  normalise<-match.arg(normalise)
  markers<-match.arg(markers)

  #check and preformat data
  if(length(ages)==0)stop("no data")
  if(!is.list(ages)){
    ages<-list(age=ages)
    plotonly<-c("age")
  }
  plotonly<-names(ages) %in% plotonly
  if(length(plotonly)==0)stop("data does not contain selected columns")
  ages<-ages[plotonly]

  #convert to list and cut out NA, NaN, Inf and -Inf values
  ages<-as.list(ages)
  for(i in seq_along(ages)){
    ages[[i]]<-as.numeric(ages[[i]])
    rejects<-is.na(ages[[i]])|!is.finite(ages[[i]])
    ages[[i]]<-ages[[i]][!rejects]
    if(length(ages[[i]])==0){
      warning(sprintf("%s contained no numeric data - removed",names(ages)[i]))
      ages[[i]]<-NULL
    }
  }
  if(length(ages)==0)stop("no data contained in selected columns")

  # set/check limits, adapt if splitat is given:
  if(length(limits)<1 && is.na(limits)){
    limits<-c(min(unlist(data)),max(unlist(data)))
  }else if(length(limits)==2){
    if(!is.na(splitat)){
      if((splitat>limits[1])&&(splitat<limits[2])){
        limits<-c(limits[1],splitat,splitat,limits[2])
      }else{
        warning("splitat outside age limits - ignored")
      }
    }
  }else if(length(limits)!=4){
    warning("invalid limits parameter - using default")
    limits<-c(0,3000)
  }
  allages<-unlist(ages)
  minage<-min(allages[allages>0],na.rm=TRUE)
  if(logx)limits[limits<=0]<-minage

  # Check on breaks...
  if(is.na(breaks)){
    if(logx){
      #breaks<-log_breaks()(limits)
      breaks<-prettyBreaks(limits,logsc=logx,frac.log=TRUE)
      #breaks=floor(trans_breaks("log10", function(x) 10^x,n=8)(limits))
    }else{
      breaks<-prettyBreaks(limits)
      #breaks<-pretty_breaks()(limits)
    }
  }
  if(any(!is.numeric(breaks)))warning("non-numeric break values - using default")
  breaks<-as.numeric(breaks)
  # TODO: check on breaks within limits (for manually given breaks)

  # Get KDEs from provenance package
  todist<-as.distributional(ages,range=limits)
  bw<-bandwidth
  same<-TRUE
  if(!is.na(bandwidth) && bandwidth== -1){
    bw<-NA
    same<-FALSE
  }
  kdes<-KDEs(todist,from=limits[1],to=limits[2],bw=bw,samebandwidth=same,log=logx,adaptive=adaptive,normalise=ifelse(normalise=="none",FALSE,TRUE))
  plotdf<-data.frame(x=kdes$kdes[[1]]$x,stringsAsFactors=FALSE)
  for(i in seq_along(kdes$kdes)){
    curkde<-kdes$kdes[[i]]$y
    #curkde[curkde<cutoffy]<-NA
    if(normalise=="height")curkde<-curkde/max(curkde,na.rm=TRUE)
    plotdf[[names(kdes$kdes)[i]]]<-curkde
  }
  plotdf<-melt(plotdf,id.vars="x",variable.name="smpl",value.name="density",na.rm=TRUE)
  plotdf$section<-1

  #molten input data for markers, also used for histograms:
  dm<-melt(ages,value.name="age",na.rm=TRUE)
  names(dm)[names(dm)=="L1"]<-"smpl"
  dm$section<-1
  dm<-dm[dm$age>=limits[1] & dm$age<=limits[2],]
#   dm$bw<-bw1
#   if(length(limits)==4){
#     dm$section[((dm$age>=limits[3])&(dm$age<=limits[4]))]<-2
#     bw2<-bw1/2
#     if(length(bandwidth)==2)bw2<-bandwidth[1]
#     dm$bw[dm$section==1]<-bw2
#     dm<-dm[(dm$age>=limits[1]&dm$age<=limits[2])|(dm$age>=limits[3]&dm$age<=limits[4]),]
#   }else{
#     dm<-dm[dm$age>=limits[1]&dm$age<=limits[2],]
#   }

  #create plot:
  lw<-rel(0.6)
  hw<-rel(0.2)
  g<-ggplot()

  #density:
  g<-g+geom_density(data=plotdf,aes(x=x,y=density,fill=smpl),stat="identity",size=hw)
  g<-g+geom_line(data=plotdf,aes(x=x,y=density),size=lw)

  #histogram:
  if(hist){
    if(is.na(binwidth) || binwidth==-1){
      bw<-provenance:::commonbandwidth(todist)
    }else{
      bw<-binwidth
    }
    hdata<-stat_bin(data=dm,aes(x=age,y=..count..,group=smpl),binwidth=bw,fill=NA,colour="grey40",size=hw,drop=TRUE)
    g<-g+hdata
  }

  #data markers:
  if(markers=="dash"){
    g<-g+geom_segment(data=dm,aes(x=age,xend=age,y=-0.02,yend=-0.06))
  }else if(markers=="circle"){
    g<-g+geom_point(data=dm,aes(x=age,y=-0.05),colour="#00000022",size=rel(2.5))
  }

  #breaks
  labels<-format(breaks)
  if(logx){
    g<-g+scale_x_log10(name="Ma",breaks=breaks,labels=labels)
  }else{
    g<-g+scale_x_continuous(name="Ma",breaks=breaks,labels=labels)
  }

  #layout
  g<-g+
    #we need no legend title:
    guides(fill=guide_legend(title=NULL))+
    #stack plots by sample name:
    facet_grid(smpl ~ section,scales=(if(stack=="close")"free_y" else "fixed"),
               space=(if(stack=="close")"free_y" else "fixed"))+
    #remove facet strips, horizontal grid lines, make background very light grey
    theme(strip.text=element_blank(),strip.background=element_blank(),
          panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank(),
          panel.grid.major.x=element_line(colour="#bbbbbbff",size=rel(0.3)),
          panel.grid.minor.x=element_blank(),panel.background=element_blank())

  #if no histogram, blank out y-axis:
  if(!hist)g<-g+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

  return(g)
}
