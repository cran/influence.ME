\name{dp.ME.cook}
\alias{dp.ME.cook}

\title{Dotplot visualization of Cook's Distance}
\description{
This is a wrapper function to the dotplot() function in the lattice-package. It transforms the output from the ME.cook() function and calls dotplot() to provide the user with a visualization of the Cook's Distances. 
}

\usage{
dp.ME.cook(estex, parameters = 0, groups = 0, tol = 0, sort=FALSE, ...)
}

\arguments{
  \item{estex}{An object as returned by the estex() function, containing the altered estimates of a mixed effects regression model.}
  \item{parameters}{Used to define a selection of parameters. If parameters = 0 (default), Cook's Distance is calculated based on all parameters in the model.}
  \item{groups}{Used to define a selection of nesting groups that should be visualized. If \code{groups = 0} (default), the values of Cook's Distance for all nesting groups are shown.}
  \item{tol}{Values of Cook's Distance exceeding the specified level of tolerance (\code{tol}) are plotted visually different from values not exceeding tolerance. If \code{tol=0} (default), no such differentiation is made in the way values are plotted.}
  \item{sort}{If \code{sort=TRUE} the values of Cook's Distance are ordered based on magnitude before visualization. If \code{sort=FALSE} (default) no sorting takes place.}
  \item{\dots}{Further arguments passed on to the dotplot() function.}
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}

\seealso{
\code{\link{estex}}, \code{\link{ME.cook}}, \code{\link{dp.ME.dfbetas}}, \code{\link{dp.ME.pchange}}
}

\examples{
data(school23)
model <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)

alt.est <- estex(model, "school.ID")
dp.ME.cook(alt.est)
dp.ME.cook(alt.est, tol=.35)

}

\keyword{hplot}

