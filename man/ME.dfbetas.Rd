\name{ME.dfbetas}
\alias{ME.dfbetas}

\title{Compute the DFBETAS measure of influential data}
\description{
  DFBETAS (standardized difference of the beta) is a measure that standardizes the absolute difference in parameter estimates between a (mixed effects) regression model based on a full set of data, and a model from which a (potentially influential) subset of data is removed. A value for DFBETAS is calculated for each parameter in the model separately. This function computes the DFBETAS based on the information returned by the estex() function.
}
\usage{
ME.dfbetas(estex, parameters = 0, plot=FALSE, sort=FALSE, to.sort=NA, abs=FALSE, ...)
}

\arguments{
  \item{estex}{An object as returned by the estex() function, containing the altered estimates of a mixed effects regression model}
  \item{parameters}{Used to define a selection of parameters. If parameters=0 (default), DFBETAS is calculated for all parameters in the model}
  \item{plot}{If plot=TRUE, the results from the ME.dfbetas() function are forwarded to the dp.ME.dfbetas() function, which creates a visual representation of the values for DFBETAS}
  \item{sort}{If \code{sort=TRUE} the values of DFBETAS are ordered based on magnitude. If \code{sort=FALSE} (default) no sorting takes place.}
  \item{to.sort}{Specify on which variable the DFBETAS must be sorted. If only one variable present (either in the model, or due to the selection specified in \code{parameters}), this parameter can be omitted. If DFBETAS is calculated for multiple variables, and sort=TRUE, specification of \code{to.sort} is required, or an error is returned.}
  \item{abs}{If \code{abs=TRUE}, the absolute values of DFBETAS are returned, while if \code{abs=FALSE} (default), both positive and negative values are possible. If both \code{abs=TRUE} and \code{sort=TRUE}, the \code{abs} parameters precedes the \code{sort} parameter, and thus the absolute values of DFBETAS are sorted.}
  \item{\dots}{Further arguments passed on to the dp.ME.dfbetas() function.}
}

\value{
 A matrix is returned, containing DFBETAS-values for each (selected) fixed parameter of the model, and separately for each evaluated set of influential data.
}

\references{
Belsley, D.A., Kuh, E. & Welsch, R.E. (1980). \emph{Regression Diagnostics. Identifying Influential Data and Source of Collinearity}. Wiley. 

Snijders, T.A. & Bosker, R.J. (1999). \emph{Multilevel Analysis, an introduction to basic and advanced multilevel modeling}. Sage. 

Van Der Meer, T., Te Grotenhuis, M. & Pelzer, B. \emph{Influential cases in multi-level modeling. A methodological comment on 'National context, religiosity, and volunteering' by Ruiter and De Graaf}. Current status: Accepted for publication in the American Sociological Review.
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}

\seealso{\code{\link{estex}}, \code{\link{ME.cook}}}

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
