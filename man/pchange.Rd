\name{pchange}
\alias{pchange}

\title{Compute the percentage change, as measure of influential data}
\description{
 Computes the percentile change, as a measure of influential data. This unstandardized measure can serve to help interpret the magnitude of the influence single or combined grouping levels exert on mixed effects models. The percentage change in parameter estimates between a (mixed effects) regression model based on a full set of data, and a model from which a (potentially influential) subset of data is removed. A value of percentage change is calculated for each parameter in the model separately, based on the information returned by the influence() function.
}
\usage{
pchange(estex, parameters = 0, sort=FALSE, to.sort=NA, abs=FALSE)
}

\arguments{
  \item{estex}{An object as returned by the influence() function, containing the altered estimates of a mixed effects regression model}
  \item{parameters}{Used to define a selection of parameters. If parameters=0 (default), percentage change are calculated for all parameters in the model}
  \item{sort}{If \code{sort=TRUE} the values of percentage change are ordered based on magnitude. If \code{sort=FALSE} (default) no sorting takes place.}
  \item{to.sort}{Specify on which variable the percentage changes must be sorted. If only one variable present (either in the model, or due to the selection specified in \code{parameters}), this parameter can be omitted. If percentage changes are calculated for multiple variables, and sort=TRUE, specification of \code{to.sort} is required, or an error is returned.}
  \item{abs}{If \code{abs=TRUE}, the absolute values of percentage change are returned, while if \code{abs=FALSE} (default), both positive and negative values are possible. If both \code{abs=TRUE} and \code{sort=TRUE}, the \code{abs} parameters precedes the \code{sort} parameter, and thus the absolute values of percentage change are sorted.}
}

\value{
 A matrix is returned, containing values of percentage change for each (selected) fixed parameter estimate of the model, and separately for each evaluated set of influential data.
}

\references{
Belsley, D.A., Kuh, E. & Welsch, R.E. (1980). \emph{Regression Diagnostics. Identifying Influential Data and Source of Collinearity}. Wiley. 

Snijders, T.A. & Bosker, R.J. (1999). \emph{Multilevel Analysis, an introduction to basic and advanced multilevel modeling}. Sage. 

Van der Meer, T., Te Grotenhuis, M., & Pelzer, B. (2010). \emph{Influential Cases in Multilevel Modeling: A Methodological Comment}. American Sociological Review, 75(1), 173-178.
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}

\seealso{\code{\link{influence}}, \code{\link{cooks.distance.estex}},
\code{\link{dfbetas.estex}}}

\examples{
\dontrun{
 data(school23)
 model <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)

 alt.est <- influence(model, group="school.ID")
 pchange(alt.est)
}
}
\keyword{robust}
\keyword{models}
\keyword{regression}
