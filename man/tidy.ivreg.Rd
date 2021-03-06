% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ivreg.R
\name{tidy.ivreg}
\alias{tidy.ivreg}
\alias{augment.ivreg}
\alias{glance.ivreg}
\title{Tidiers for \code{ivreg} objects}
\usage{
tidy.ivreg(x, conf.int = FALSE, conf.level = 0.95, quick = FALSE, ...)

augment.ivreg(x, data = stats::model.frame(x), newdata = NULL, ...)

glance.ivreg(x, df = NULL, diagnostics = FALSE, ...)
}
\arguments{
\item{x}{\code{ivreg} object}

\item{conf.int}{whether to include a confidence interval}

\item{conf.level}{confidence level of the interval, used only if \code{conf.int=TRUE}}

\item{quick}{whether to compute a smaller and faster version, containing only the term and estimate columns.}

\item{...}{Extra arguments}

\item{data}{Original data, defaults to the extracting it from the model}

\item{newdata}{If provided, performs predictions on the new data}

\item{df}{the degrees of freedom to be used. See \code{\link[AER]{summary.ivreg}}.}

\item{diagnostics}{If \code{TRUE}, carry out diagnostic tests for the instrumental-variable regression. See \code{\link[AER]{summary.ivreg}}.}
}
\value{
All tidying methods return a data.frame without rownames. The structure depends on the method chosen.
}
\description{
Tidiers for \code{ivreg} objects returned by the
}
\details{
These methods tidy the coefficients of an instrumental
variable model produced by the \pkg{AER} function
\code{\link[AER]{ivreg}} into a summary, augment the
original data with information on the fitted values
and residuals, and construct a one-row glance of the
model's statistics.
}
