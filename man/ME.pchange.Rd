\name{ME.pchange}
\alias{ME.pchange}

\title{Compute the percentile change, as measure of influential data}
\description{
 Compute the percentile change, as measure of influential data. This unstandardized measure can serve to help interpret the magnitude of the influence single or combined grouping levels exert on mixed models. The percentile change in parameter estimates between a (mixed effects) regression model based on a full set of data, and a model from which a (potentially influential) subset of data is removed. A value of percentile change is calculated for each parameter in the model separately, based on the information returned by the estex() function.
}
\usage{
ME.pchange(estex, parameters = 0, plot=FALSE, sort=FALSE, to.sort=NA, abs=FALSE, ...)
}

\arguments{
  \item{estex}{An object as returned by the estex() function, containing the altered estimates of a mixed effects regression model}
  \item{parameters}{Used to define a selection of parameters. If parameters=0 (default), all DFBETAS is calculated for parameters in the model}
  \item{plot}{If plot=TRUE, the results from the ME.cook() function are forwarded to the dp.ME.dfbetas() function, which creates a visual representation of the Cook's Distances.}
  \item{sort}{If \code{sort=TRUE} the values of percentile change are ordered based on magnitude. If \code{sort=FALSE} (default) no sorting takes place.}
  \item{to.sort}{Specify on which variable the percentile changes must be sorted. If only one variable present (either in the model, or due to the selection specified in \code{parameters}), this parameter can be omitted. If percentile changes are calculated for multiple variables, and sort=TRUE, specification of \code{to.sort} is required, or an error is returned.}
  \item{abs}{If \code{abs=TRUE}, the absolute values of percentile change are returned, while if \code{abs=FALSE} (default), both positive and negative values are possible. If both \code{abs=TRUE} and \code{sort=TRUE}, the \code{abs} parameters precedes the \code{sort} parameter, and thus the absolute values of percentile change are sorted.}
  \item{\dots}{Further arguments passed on to the dp.ME.pchange() function.}
}

\value{
 A matrix is returned, containing values of percentile change  for each (selected) fixed parameter of the model, and separately for each evaluated set of influential data.
}

\references{
Belsley, D.A., Kuh, E. & Welsch, R.E. (1980). \emph{Regression Diagnostics. Identifying Influential Data and Source of Collinearity}. Wiley. 

Snijders, T.A. & Bosker, R.J. (1999). \emph{Multilevel Analysis, an introduction to basic and advanced multilevel modeling}. Sage. 

Van Der Meer, T., Te Grotenhuis, M. & Pelzer, B. \emph{Influential cases in multi-level modeling. A methodological comment on 'National context, religiosity, and volunteering' by Ruiter and De Graaf}. Current status: conditionally accepted by the American Sociological Review.
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}

\seealso{\code{\link{estex}}, \code{\link{ME.cook}}, \code{\link{ME.dfbetas}}}

\examples{
 data(school23)
 model <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)

 alt.est <- estex(model, "school.ID")
 ME.dfbetas(alt.est)
 ME.dfbetas(alt.est, plot=TRUE, layout=c(1,3))
}

\keyword{robust}
\keyword{models}
\keyword{regression}
