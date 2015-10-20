#########################################
## (c) Martin Rittner, 2015            ##
## sw@thegeologician.net               ##
#########################################

## helper functions #####################

#' @title Return "pretty" breaks for axis labelling.
#'
#' @param range Numeric vector of length 2 giving the min and max input values.
#' @param inner \code{TRUE}: return only "pretty" ticks within \code{range};
#' \code{FALSE}: return ticks spanning closest "pretty" ticks outside
#' \code{range}.
#' @param logsc Boolean, return values suitable for labelling log-scale axes.
#' @param majors Boolean, return major ticks.
#' @param minors Boolean, return minor ticks.
#' @param n.major (Approximate) number of major ticks to return.
#' @param n.minor Number of minor ticks to return between each pair of major
#' ticks.
#' @param force.limits If \code{inner = TRUE}, also include exact \code{range}
#' values.
#' @param frac.log Use fractions of log10 (10^0.5, 10^0.33, 10^0.25), to closer
#' match desired number of ticks.
#'
#' @description This function is similar to \code{pretty_breaks} in package
#' \code{scales}, but has advanced options for log scales, forced range limits,
#' and separate generation of major and minor ticks (useful for labelling).
#'
#' @return A numerical vector of generated breaks.
#'
#' @details Setting \code{major = FALSE, minor=TRUE} can be useful to return
#' minor ticks only, e.g. to plot them in a different style than major ticks.
#'
#' @keywords internal
#' @export
#'
prettyBreaks<-function(range, inner=TRUE, logsc=FALSE, majors=TRUE, minors=FALSE,
                       n.major=8,n.minor=0, force.limits=FALSE, frac.log=FALSE){

  # FIXME: some of this might be obsolete now by package scales - adapt/simplify this function?
  # FIXME: handle negative numbers (in linear scale)

  if((!is.numeric(range))||(length(range)!=2))stop("numeric vector of length 2 required")
  if(any(range<0)){
    if(logsc){
      stop("range must contain only positive values for logscale")
    }
    warning("negative values not yet implemented - returning non-optimal values")
    if(n.major==0)n.major<-8
    ret<-seq(range[1],range[2],length.out=n.major)
    if(all(is.wholenumber(range)))ret<-floor(ret)
    return(ret)
  }

  #major breaks
  breaks<-NULL
  if(logsc){
    #log scale major ticks
    scl<-floor(log10(range))
    scales<-c(5,10/3,2.5,1,1/2,1/3,1/4,1/10)
    scale<-1
    n.out<-n.major
    if(inner){
      if(frac.log){
        scale<-scales[rank(abs(n.out-(scl[2]-scl[1])/scales),ties.method="first")==1]
      }
      breaks<-seq(scl[1]+1,scl[2],by=scale)
    }else{
      if(force.limits)n.out<-n.out-2
      if(frac.log){
        scale<-scales[rank(abs(n.out-(scl[2]-scl[1]+1)/scales),ties.method="first")==1]
      }
      if(n.out>0)breaks<-seq(scl[1],ceiling(log10(range[2])/scale)*scale,by=scale)
    }
    scl<-10^scl
    breaks<-10^breaks
  }else{
    #linear scale major ticks
    #choose best scale (no. of output ticks is closest to n.major)
    scl<-10^floor(log10(range[2]-range[1]))
    scales<-c(5*scl,2*scl,scl,scl/2,scl/5,scl/10)
    n.out<-n.major
    if(!inner || force.limits)n.out<-n.out-2
    scl<-scales[rank(abs(n.out-(range[2]-range[1])/scales),ties.method="first")==1]

    if(inner){
      breaks<-seq(from=ceiling(range[1]/scl)*scl,to=floor(range[2]/scl)*scl,by=scl)
    }else{
      breaks<-seq(from=floor(range[1]/scl)*scl,to=ceiling(range[2]/scl)*scl,by=scl)
    }
  }
  #force add range (if calculated breaks not already wider)
  if(force.limits){
    if(range[1]<breaks[1])breaks<-c(range[1],breaks)
    if(range[2]>breaks[length(breaks)])breaks<-c(breaks,range[2])
  }
  #round values > 10 to whole numbers
  # CHECK: do we want to round log values?
  if(any(scl>=1))breaks[breaks>=10]<-floor(breaks[breaks>=10])

  if(majors & !minors){
    return(breaks)
  }else if(minors){
    #minor breaks
    minbreaks<-NULL
    # TODO: n.minor=0 - choose n.minor automatically
    if(n.minor>0){
      if(logsc){
        #log scale minor ticks
        if(frac.log){
          #equidistant in log scale - 10^1/2, 10^1/3, 10^1/4...
          for(j in 1:(length(breaks)-1)){
            dn<-log10(breaks[j])
            up<-log10(breaks[j+1])
            i<-(up-dn)/(n.minor+1)
            minbreaks<-c(minbreaks,10^(dn+seq(1,n.minor)*i))
          }
        }else{
          #equal interval - 1,5.5,10,55,100,550,...
          #                 1,2,3,...,10,20,30...,100,200...
          # TODO: more intuitive choice of intervals, e.g. n.minor=10 would split 10..50 into 10,20,30,... - ?
          for(j in 1:(length(breaks)-1)){
            i<-(breaks[j+1]-breaks[j])/(n.minor+1)
            minbreaks<-c(minbreaks,(breaks[j]+seq(1,n.minor)*i))
          }
        }
      }else{
        #linear scale minor ticks
        for(j in 1:(length(breaks)-1)){
          i<-(breaks[j+1]-breaks[j])/(n.minor+1)
          minbreaks<-c(minbreaks,(breaks[j]+seq(1,n.minor)*i))
        }
      }
    }else{
      warning(sprintf("minors==TRUE but n.minor = %d",major,n.minor))
    }
  }

  if(majors & minors){
    breaks<-sort(c(breaks,minbreaks))
  }else{
    breaks<-minbreaks
  }

  return(breaks)
}
