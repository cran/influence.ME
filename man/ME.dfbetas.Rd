\name{ME.dfbetas}
\alias{ME.dfbetas}

\title{Compute the DFBETAS measure of influential data}
\description{
  DFBETAS (standardized difference of the beta) is a measure that standardizes the absolute difference in parameter estimates between a (mixed effects) regression model based on a full set of data, and a model from which a (potentially influential) subset of data is removed. A value for DFBETAS is calculated for each parameter in the model separately. This function computes the DFBETAS based on the information returned by the estex() function.
}
\usage{
ME.dfbetas(estex, parameters = 0, plot=FALSE, ...)
}

\arguments{
  \item{estex}{An object as returned by the estex() function, containing the altered estimates of a mixed effects regression model}
  \item{parameters}{Used to define a selection of parameters. If parameters=0 (default), all DFBETAS is calculated for parameters in the model}
   \item{plot}{If plot=TRUE, the results from the ME.cook() function are forwarded to the dp.ME.dfbetas() function, which creates a visual representation of the Cook's Distances.}
  \item{\dots}{Further arguments passed on to the dp.ME.dfbetas() function.}
}

\value{
 A matrix is returned, containing DFBETAS-values for each (selected) fixed parameter of the model, and separately for each evaluated set of influential data.
}

\references{
Belsley, D.A., Kuh, E. & Welsch, R.E. (1980). \emph{Regression Diagnostics. Identifying Influential Data and Source of Collinearity}. Wiley. 

Snijders, T.A. & Bosker, R.J. (1999). \emph{Multilevel Analysis, an introduction to basic and advanced multilevel modeling}. Sage. 

Van Der Meer, T., Te Grotenhuis, M. & Pelzer, B. \emph{Influential cases in multi-level modeling. A methodological comment on 'National context, religiosity, and volunteering' by Ruiter and De Graaf}. Current status: conditionally accepted by the American Sociological Review.
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
