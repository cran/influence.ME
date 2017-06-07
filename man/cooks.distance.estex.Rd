\name{cooks.distance.estex}
\alias{cooks.distance.estex}
\alias{cooks.distance}

\title{Compute the Cook's distance measure of influential data on mixed effects models}
\description{
    Cook's Distance is a measure indicating to what extent model parameters are influenced by (a set of) influential data on which the model is based. This function computes the Cook's distance based on the information returned by the influence() function.
}

\usage{
\method{cooks.distance}{estex}(model, parameters=0, sort=FALSE, \dots)
}

\arguments{
  \item{model}{An object as returned by the influence() function, containing the altered estimates of a mixed effects regression model}
  \item{parameters}{Used to define a selection of parameters. If parameters=0 (default), Cook's Distance is calculated based on all parameters in the model}
  \item{sort}{If \code{sort=TRUE} the values of Cook's Distance are ordered based on magnitude. If \code{sort=FALSE} (default) no sorting takes place.}
  \item{\dots}{Currently not used}
}

\value{
 A one-column matrix is returned containing values for the Cook's Distance based on the selected (fixed) parameters of the model. Each row shows the Cook's Distance associated with each evaluated set of influential data (data nested within each evaluated level of the grouping factor).
}

\references{
Nieuwenhuis, R., Te Grotenhuis, M., & Pelzer, B. (2012). Influence.ME: tools for detecting influential data in mixed effects models. \emph{R Journal}, 4(2), 38???47.

Belsley, D.A., Kuh, E. & Welsch, R.E. (1980). \emph{Regression Diagnostics. Identifying Influential Data and Source of Collinearity}. Wiley. 

Snijders, T.A. & Bosker, R.J. (1999). \emph{Multilevel Analysis, an introduction to basic and advanced multilevel modeling}. Sage. 

Van der Meer, T., Te Grotenhuis, M., & Pelzer, B. (2010). \emph{Influential Cases in Multilevel Modeling: A Methodological Comment}. American Sociological Review, 75(1), 173-178.
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}

\seealso{\code{\link{influence}}, \code{\link{dfbetas}}}

\examples{
\dontrun{
data(school23)
model <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)

alt.est <- influence(model, group="school.ID")
cooks.distance(alt.est)
}
}

\keyword{robust}
\keyword{models}
\keyword{regression}
