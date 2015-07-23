\name{dfbetas.estex}
\alias{dfbetas.estex}
\alias{dfbetas}

\title{Compute the DFBETAS measure of influential data}
\description{
  DFBETAS (standardized difference of the beta) is a measure that standardizes the absolute difference in parameter estimates between a (mixed effects) regression model based on a full set of data, and a model from which a (potentially influential) subset of data is removed. A value for DFBETAS is calculated for each parameter in the model separately. This function computes the DFBETAS based on the information returned by the influence() function.
}
\usage{
\method{dfbetas}{estex}(model, parameters = 0, sort=FALSE, to.sort=NA, abs=FALSE, ...)
}

\arguments{
  \item{model}{An object as returned by the influence() function, containing the altered estimates of a mixed effects regression model}
  \item{parameters}{Used to define a selection of parameters. If parameters=0 (default), DFBETAS is calculated for all parameters in the model}
  \item{sort}{If \code{sort=TRUE} the values of DFBETAS are ordered based on magnitude. If \code{sort=FALSE} (default) no sorting takes place.}
  \item{to.sort}{Specify on which variable the DFBETAS must be sorted. If only one variable present (either in the model, or due to the selection specified in \code{parameters}), this parameter can be omitted. If DFBETAS is calculated for multiple variables, and sort=TRUE, specification of \code{to.sort} is required, or an error is returned.}
  \item{abs}{If \code{abs=TRUE}, the absolute values of DFBETAS are returned, while if \code{abs=FALSE} (default), both positive and negative values are possible. If both \code{abs=TRUE} and \code{sort=TRUE}, the \code{abs} parameters precedes the \code{sort} parameter, and thus the absolute values of DFBETAS are sorted.}
  \item{\dots}{Currently not used}
}

\value{
 A matrix is returned, containing DFBETAS-values for each (selected) fixed parameter of the model, and separately for each evaluated set of influential data.
}

\references{
Nieuwenhuis, R., Te Grotenhuis, M., & Pelzer, B. (2012). Influence.ME: tools for detecting influential data in mixed effects models. \emph{R Journal}, 4(2), 38???47.

Belsley, D.A., Kuh, E. & Welsch, R.E. (1980). \emph{Regression Diagnostics. Identifying Influential Data and Source of Collinearity}. Wiley. 

Snijders, T.A. & Bosker, R.J. (1999). \emph{Multilevel Analysis, an introduction to basic and advanced multilevel modeling}. Sage. 

Van der Meer, T., Te Grotenhuis, M., & Pelzer, B. (2010). \emph{Influential Cases in Multilevel Modeling: A Methodological Comment}. American Sociological Review, 75(1), 173-178.
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}

\seealso{\code{\link{influence.mer}}, \code{\link{cooks.distance.estex}}}

\examples{
 data(school23)
 model <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)

 alt.est <- influence(model, group="school.ID")
 dfbetas(alt.est)
}

\keyword{robust}
\keyword{models}
\keyword{regression}
