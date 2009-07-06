\name{influence.ME-package}
\alias{influence.ME-package}
\alias{influence.ME}
\docType{package}

\title{
Influence.ME: Tools for detecting influential data in mixed effects models
}

\description{
influence.ME calculates measures of influence for mixed effects models estimated with lme4. The basic rationale behind measuring influential cases is that when iteratively single units are omitted from the data, models based on these data should not produce substantially different estimates. To standardize the assessment of how influential a (single group of) observation(s) is, several measures of influence are common practice. First, DFBETAS is a standardized measure of the absolute difference between the estimate with a particular case included and the estimate without that particular case. Second, Cook's distance provides an overall measurement of the change in all parameter estimates, or a selection thereof. 
}

\details{
\tabular{ll}{
Package: \tab influence.ME\cr
Type: \tab Package\cr
Version: \tab 0.7\cr
Date: \tab 2009-07-06\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
Calculating measures of influential data on a mixed effects regression model entails the re-estimation of this model for each set of potentially influential data separately. The estex() function does this, and returns the altered estimates resulting from each re-estimation. These altered estimates can subsequently be entered to the ME.cook() and ME.dfbetas() functions, to calculate Cook's Distance and the DFBETAS (standardized difference of the beta) measures.
}

\author{
Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis

Maintainer: Rense Nieuwenhuis <contact@rensenieuwenhuis.nl>
}

\references{
Belsley, D.A., Kuh, E. & Welsch, R.E. (1980). \emph{Regression Diagnostics. Identifying Influential Data and Source of Collinearity}. Wiley. 

Snijders, T.A. & Bosker, R.J. (1999). \emph{Multilevel Analysis, an introduction to basic and advanced multilevel modeling}. Sage. 

Van Der Meer, T., Te Grotenhuis, M. & Pelzer, B. \emph{Influential cases in multi-level modeling. A methodological comment on 'National context, religiosity, and volunteering' by Ruiter and De Graaf}. Current status: Accepted for publication in the American Sociological Review.
}

\keyword{package}
\keyword{robust}
\keyword{models}
\keyword{regression}

\seealso{
\code{\link{estex}}, \code{\link{exclude.influence}}, \code{\link{ME.cook}}, \code{\link{ME.dfbetas}}, \code{\link{ME.pchange}}
}

\examples{
 data(school23)
 model.a <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)
 alt.est.a <- estex(model.a, "school.ID")
 ME.cook(alt.est.a, plot=TRUE, tol=.17)
 
 model.b <- exclude.influence(model.a, "school.ID", "7472")
 alt.est.b <- estex(model.b, "school.ID")
 ME.cook(alt.est.b, plot=TRUE, tol=.18)

 model.c <- exclude.influence(model.b, "school.ID", "54344")
 alt.est.c <- estex(model.c, "school.ID")
 ME.cook(alt.est.c, plot=TRUE, tol=.19)
}
