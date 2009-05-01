\name{ME.cook}
\alias{ME.cook}

\title{Compute the Cook's distance measure of influential data on mixed effects models}
\description{
    Cook's Distance is a measure indicating to what extent model parameters are influenced by (a set of) influential data on which the model is based. This function computes the Cook's distance based on the information returned by the estex() function.
}
\usage{
ME.cook(estex, parameters = 0, plot=FALSE, sort=FALSE, ...)
}

\arguments{
  \item{estex}{An object as returned by the estex() function, containing the altered estimates of a mixed effects regression model}
  \item{parameters}{Used to define a selection of parameters. If parameters=0 (default), Cook's Distance is calculated based on all parameters in the model}
  \item{plot}{If plot=TRUE, the results from the ME.cook() function are forwarded to the dp.ME.cook() function, which creates a visual representation of the Cook's Distances.}
  \item{sort}{If \code{sort=TRUE} the values of Cook's Distance are ordered based on magnitude. If \code{sort=FALSE} (default) no sorting takes place.}
  \item{\dots}{Further arguments passed on to the dp.ME.cook() function.}
}

\value{
 A one-column matrix is returned containing values for the Cook's Distance based on the selected (fixed) parameters of the model. Each row shows the Cook's Distance associated with each evaluated set of influential data (data nested within each evaluated level of the grouping factor).
}

\references{
Belsley, D.A., Kuh, E. & Welsch, R.E. (1980). \emph{Regression Diagnostics. Identifying Influential Data and Source of Collinearity}. Wiley. 

Snijders, T.A. & Bosker, R.J. (1999). \emph{Multilevel Analysis, an introduction to basic and advanced multilevel modeling}. Sage. 

Van Der Meer, T., Te Grotenhuis, M. & Pelzer, B. \emph{Influential cases in multi-level modeling. A methodological comment on 'National context, religiosity, and volunteering' by Ruiter and De Graaf}. Current status: conditionally accepted by the American Sociological Review.
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}

\seealso{\code{\link{estex}}, \code{\link{ME.dfbetas}}, \code{\link{dp.ME.cook}}}

\examples{
 data(school23)
 model <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)

 alt.est <- estex(model, "school.ID")
 ME.cook(alt.est)
 ME.cook(alt.est, plot=TRUE, tol=.35)
}

\keyword{robust}
\keyword{models}
\keyword{regression}
