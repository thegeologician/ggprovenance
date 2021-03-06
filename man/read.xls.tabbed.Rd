% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.r
\name{read.xls.tabbed}
\alias{read.xls.tabbed}
\title{Read a 'tabbed' Excel age data file}
\usage{
read.xls.tabbed(filename, sheets = NULL, age.column = "preferred.age",
  verbose = TRUE, ...)
}
\arguments{
\item{filename}{The filename to load.}

\item{sheets}{Optional vector specifying the worksheets to import.}

\item{age.column}{Column name containing the age data in each sheet.}

\item{verbose}{Print status messages during import.}

\item{...}{Additional parameters passed on to \code{read.xls}.}
}
\value{
A list of distributional data vectors (like e.g. ages) of varying
length. The names are taken from the (chosen) worksheet names.
}
\description{
Reads distributional data (like measured ages) from an Excel
file which contains one worksheet per sample/data set. The sample names are
the worksheet names.
}
\details{
Further arguments to \code{read.xls()} can be passed in the
\code{...} parameter, e.g. \code{skip=n} to skip n lines at the top of each
table.
\code{sheets} allow to import only a subset of worksheets, either a
character vector specifying the sheet names, or a numerical vector can be
given.
\code{age.column} is the the column name within each worksheet containing
the data to be loaded.
If \code{verbose=TRUE}, status messages are printed on the console during
data import, as \code{read.xls()} has to be called repeatedly for each
worksheet, which can get slow.
}
\examples{
datafile<-system.file("extdata", "Tarim.xls", package="ggprovenance")
agedata<-read.xls.flat(datafile)

}
\seealso{
\code{\link{read.xls.tabbed}}, \code{\link[gdata]{read.xls}}
}
\keyword{internal}

