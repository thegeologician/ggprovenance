% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotKDE.r
\name{plotKDE}
\alias{plotKDE}
\title{Plot one or several KDEs using ggplot2.}
\usage{
plotKDE(ages, title, limits = c(0, max(unlist(ages), na.rm = TRUE)),
  plotonly = names(ages), categories, mapping, breaks = NULL,
  bandwidth = NA, splitat = NA, markers = c("none", "dash", "circle"),
  logx = FALSE, histogram = FALSE, binwidth = bandwidth,
  adaptive = TRUE, stack = c("equal", "close", "dense"),
  normalise = c("area", "height", "none"), lowcount = 80, ...)
}
\arguments{
\item{ages}{A named list of ages for each sample/data set.}

\item{title}{Overal plot title.}

\item{limits}{Numerical vector giving the range of ages to plot.}

\item{plotonly}{Vector of names of samples to plot.}

\item{categories}{A data frame giving categorising information on samples.}

\item{mapping}{Aesthetic mapping, see \code{\link[ggplot2]{aes}}.}

\item{breaks}{Numerical vector of break values along x-axis.}

\item{bandwidth}{Bandwidth for KDE and histogram calculation.}

\item{splitat}{Numerical, split plots into left and right plot.}

\item{markers}{Character, type of tick marks indicating input data.}

\item{logx}{Boolean, plot x-axis in log-scale.}

\item{histogram}{Boolean, add histogram.}

\item{binwidth}{Bin width for histograms.}

\item{adaptive}{Boolean, use Abramson's adaptive bandwith modifier.}

\item{stack}{Character, stack plots more closely (see details).}

\item{normalise}{Character, normalise KDEs to height or area (default).}

\item{lowcount}{Minimum number for robust data set.}

\item{...}{Additional parameters passed on to \code{KDEs}.}
}
\value{
A ggplot object containing the specified plot.
}
\description{
\code{plotKDE} takes a simple named list containing the ages
measured on each sample, and produces rich graphics of kernel density
estimates (KDEs). A multitude of parameters makes the function complex, but
gives fine control over the final graphic, in many cases producing output
that needs no further editing in other software, see examples and vignette.
Although syntax differs from \code{ggplot} functions, the concept of mapping
data values (from parameter \code{categories}) onto aesthetics remains.
}
\details{
If \code{limits} is of length 2 it specifies the range of ages to
plot. This will be split at age \code{splitat}, if given. If \code{limits}
is of length 4, it specifies the ranges for the left and right half plots,
respectively, and overrides \code{splitat}. KDEs and histograms are
calculated on the whole data, and normalised before truncation at
\code{limits}.

If \code{bandwith = NA} (default), the median of calculated 'optimal
bandwidths' (function \code{botev} of package \code{provenance}) is used on
all KDEs. If a numerical value is given, this will be used for all KDEs. If
\code{bandwidth = -1}, the individual optimal bandwidths are used for each
KDE.

\code{splitat} specifies an age to split the plots into half at. The two
halfes will both oppupy half the available space, at differing x scales to
accomodate the range especified by \code{limits} overall.

\code{categories} contains any meta-information on samples, one line per
sample. Any categories given can be mapped onto aesthetics with the
\code{mapping} parameter.

\code{markers} indicates the type of data markers along the x-axis to
indicate sampled ages. Possible values are \code{"dash"}, \code{"circle"},
or \code{"none"} (default).

If histograms are plotted (\code{histogram = TRUE}), the bin width can
optionally be set by specifying \code{binwidth}. Values of \code{NA} or
\code{-1} both cause the binwidth to be set automatically to the median of
'optimal bandwidths'.

\code{stack} specifies vertical spacing of stacked plots. One of
\code{"equal"} (default), giving each plot the same height, \code{"close"},
letting each plot only take up as much space as its maximum y value
requires, and \code{"dense"}, stacking non-filled KDEs as closely as
possible. \code{"close"} has no effect if \code{normalise="height"}, and
\code{"dense"} only works when \code{mapping=aes(fill=NULL)} is specified.

\code{normalise} may be one of \code{"area"} (default), making KDEs the same
area, \code{"height"}, making KDEs the same height, or \code{"none"}. If
\code{histogram = TRUE}, KDEs are always normalised to area.

\code{lowcount} gives the minimum number to consider a data set robust.
Smaller sets will be drawn in a dashed line, unless the \code{linetype}
aestetic is specified in \code{mapping}. Set to \code{-1} to disable.
}
\note{
\code{normalise = "height"} can be misleading, visually
overemphasising distributions with broad peaks.

Tip: The returned ggplot object can be further modified with
\code{ggplot2}'s functions (e.g. \code{\link{theme}}) and saved in many file
formats using \code{\link{ggsave}}.
}
\seealso{
\code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{ggsave}},
\code{\link[provenance]{plot.KDE}}
}

