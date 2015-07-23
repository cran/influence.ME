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
Version: \tab 0.9.2\cr
Date: \tab 2013-01-15\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
Calculating measures of influential data on a mixed effects regression model entails the re-estimation of this model for each set of potentially influential data separately. The influence() function does this, and returns the altered estimates resulting from each re-estimation. These altered estimates can subsequently be entered to the \code{\link{cooks.distance}} and \code{\link{dfbetas}} methods, to calculate Cook's Distance and the DFBETAS (standardized difference of the beta) measures.
}

\author{
Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis

Maintainer: Rense Nieuwenhuis <r.nieuwenhuis@utwente.nl>
}

\references{
Belsley, D.A., Kuh, E. & Welsch, R.E. (1980). \emph{Regression Diagnostics. Identifying Influential Data and Source of Collinearity}. Wiley. 

Snijders, T.A. & Bosker, R.J. (1999). \emph{Multilevel Analysis, an introduction to basic and advanced multilevel modeling}. Sage. 

Van der Meer, T., Te Grotenhuis, M., & Pelzer, B. (2010). \emph{Influential Cases in Multilevel Modeling: A Methodological Comment}. American Sociological Review, 75(1), 173-178.

}

\keyword{package}
\keyword{robust}
\keyword{models}
\keyword{regression}

\seealso{
\code{\link{influence}},
\code{\link{cooks.distance.estex}}, \code{\link{dfbetas.estex}}, 
\code{\link{pchange}}, \code{\link{sigtest}}
}

\examples{
data(school23)

model.a <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)
alt.est.a <- influence(model.a, "school.ID")
 
model.b <- exclude.influence(model.a, "school.ID", "7472")
alt.est.b <- influence(model.b, "school.ID")

cooks.distance(alt.est.b)

model.c <- exclude.influence(model.b, "school.ID", "54344")
alt.est.c <- influence(model.c, "school.ID")

cooks.distance(alt.est.c)
}
