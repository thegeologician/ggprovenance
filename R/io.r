#########################################
## (c) Martin Rittner, 2015            ##
## sw@thegeologician.net               ##
#########################################

#########################################
## functions for data import

#' Read a 'tabbed' Excel age data file
#'
#' @param filename The filename to load.
#' @param sheets Optional vector specifying the worksheets to import.
#' @param age.column Column name containing the age data in each sheet.
#' @param verbose Print status messages during import.
#' @param ... Additional parameters passed on to \code{read.xls}.
#'
#' @description Reads distributional data (like measured ages) from an Excel
#' file which contains one worksheet per sample/data set. The sample names are
#' the worksheet names.
#'
#' @details Further arguments to \code{read.xls()} can be passed in the
#' \code{...} parameter, e.g. \code{skip=n} to skip n lines at the top of each
#' table.
#' \code{sheets} allow to import only a subset of worksheets, either a
#' character vector specifying the sheet names, or a numerical vector can be
#' given.
#' \code{age.column} is the the column name within each worksheet containing
#' the data to be loaded.
#' If \code{verbose=TRUE}, status messages are printed on the console during
#' data import, as \code{read.xls()} has to be called repeatedly for each
#' worksheet, which can get slow.
#'
#' @return A list of distributional data vectors (like e.g. ages) of varying
#' length. The names are taken from the (chosen) worksheet names.
#'
#' @seealso \code{\link{read.xls.tabbed}}, \code{\link[gdata]{read.xls}}
#'
#' @keywords internal
#' @export
#'
#' @examples
#' datafile<-system.file("extdata", "Tarim.xls", package="ggprovenance")
#' agedata<-read.xls.flat(datafile)
read.xls.tabbed<-function(filename,sheets=NULL,age.column="preferred.age",verbose=TRUE,...){
  # reads an XLS/XLSX file that contains age data in one sheet per data set/sample.
  # returns a list of the values of age.column in each sheet (or the sheets
  # specified in sheets), with the names of the respective worksheets.

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

#' Read a 'flat' Excel age data file
#'
#' @param filename The filename to load.
#' @param ... Additional parameters passed on to \code{read.xls}.
#'
#' @description Reads distributional data (like measured ages) from an Excel
#' file in which all data is contained in one worksheet, one column per sample.
#' The sample names are taken from the first line (headers).
#'
#' @details Further arguments to \code{read.xls()} can be passed in the
#' \code{...} parameter, e.g. \code{skip=n} to skip n lines at the top of the
#' table, or \code{sheet="abc"} to specify the name of the worksheet containing
#' the data.
#'
#' @return A list of distributional data vectors (like e.g. ages) of varying
#' length. The names are taken from the column names in the first line in the
#' data file, usually sample names.
#'
#' @seealso \code{\link{read.xls.tabbed}}, \code{\link[gdata]{read.xls}}
#'
#' @keywords internal
#' @export
#'
#' @examples
#' datafile<-system.file("extdata", "Tarim.xls", package="ggprovenance")
#' agedata<-read.xls.flat(datafile)
read.xls.flat<-function(filename,...){
  # very minimal wrapper around read.xls, reading an XLS/XLSX file containing
  # age data in columns (per sample/data set), returns a list

  ret<-read.xls(filename,stringsAsFactors=FALSE,...)
  ret<-as.list(ret)
  for(i in seq_along(ret)){ret[[i]]<-ret[[i]][!is.na(ret[[i]])]}
  return(ret)
}

#' Convert a ragged list into a \code{distributional} object.
#'
#' @param x List of values.
#' @param err Optional list or errors.
#' @param method Dissimilarity metric.
#' @param xlabel Data label.
#' @param range Data range.
#'
#' @return A \code{distributional} object for use with package \code{provenance}.
#' @keywords internal
#' @export
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
