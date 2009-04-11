\name{dp.ME.dfbetas}
\alias{dp.ME.dfbetas}

\title{Dotplot visualization of DFBETAS}
\description{
 This is a wrapper function to the dotplot() function in the lattice-package. It transforms the output from the ME.dfbetas() function and calls dotplot() to provide the user with a visualization of the DFBETAS. 
}

\usage{
dp.ME.dfbetas(estex, parameters = 0, groups = 0, ...)
}
\arguments{
  \item{estex}{An object as returned by the estex() function, containing the altered estimates of a mixed effects regression model.}
  \item{parameters}{Used to define a selection of parameters. If parameters = 0 (default), values for DFBETAS are visualized for parameters in the model.}
  \item{groups}{Used to define a selection of nesting groups that should be visualized. If groups = 0 (default), the values of DFBETAS for all nesting groups are shown.}
  \item{\dots}{Further arguments passed on to the dotplot() function.}
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}

\seealso{
\code{\link{estex}}, \code{\link{ME.dfbetas}}, \code{\link{dp.ME.cook}}
}

\examples{
data(school23)
model <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)

alt.est <- estex(model, "school.ID")
dp.ME.dfbetas(alt.est)
dp.ME.dfbetas(alt.est, layout=c(1,3))
}

\keyword{hplot}

