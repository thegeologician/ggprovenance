#########################################
## (c) Martin Rittner, 2015            ##
## sw@thegeologician.net               ##
#########################################

#########################################
## functions for data import

read.xls.tabbed<-function(filename,sheets=NULL,age.column="preferred.age",verbose=TRUE,...){
  # reads an XLS/XLSX file that containsage data in one sheet per data set/sample.
  # returns a list of the values of age.column in each sheet (or the sheets
  # specified in sheets), with the names of the respective worksheets.
  # filename ... file name
  # sheets ... optional vector specifying sheets to import
  # age.column ... colum name containing ages in each worksheet
  # ... further parameters passed to read.xls

  # TODO: check filename
  if(verbose)message("reading sheet names")
  allsheets<-sheetNames(filename)
  if(is.null(sheets)){
    sheets<-seq_along(allsheets)
  }else if(is.character(sheets)){
    sheets<-match(sheets,allsheets)
  }

  ret<-list()
  for(i in sheets){
    if(verbose)message(sprintf("reading sheet %d: %s",i,allsheets[i]))
    curdat<-read.xls(filename,sheet=i,...)
    if(!any(names(curdat) %in% age.column))warning(sprintf("'%s' not found in '%s'",age.column,allsheets[i]))
    curdat<-as.numeric(curdat[[age.column]])
    curdat<-curdat[!is.na(curdat)]
    ret[[allsheets[i]]]<-curdat
  }

  return(ret)
}

read.xls.flat<-function(filename,...){
  # very minimal wrapper around read.xls, reading an XLS/XLSX file containing
  # age data in columns (per sample/data set), returns a list
  # filename ... file name

  ret<-read.xls(filename,stringsAsFactors=FALSE,...)
  ret<-as.list(ret)
  for(i in seq_along(ret)){ret[[i]]<-ret[[i]][!is.na(ret[[i]])]}
  return(ret)
}

as.distributional<-function(x,err=NULL,method=c("KS","SH"),xlabel="Ma",range=NULL){
  # convert a raw list of distributional data (ages) to a 'distributional' object
  # cf. provenance package
  ret<-list()
  names(ret)<-NULL
  ret[['x']]<-x
  if(is.null(err)){
    ret[['err']]<-list()
  }else{
    ret[['err']]<-err
  }
  ret[['method']]<-match.arg(method)
  d<-unlist(x)
  if(is.null(range))range<-c(min(d),max(d))
  d<-d[d>=range[1] && d<=range[2]]
  nb<-log(length(d)/length(x),base=2)+2
  ret[['breaks']]<-seq(range[1],range[2],length.out=nb)
  ret[['xlabel']]<-xlabel
  class(ret)<-"distributional"
  return(ret)
}
