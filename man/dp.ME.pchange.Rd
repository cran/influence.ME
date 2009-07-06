\name{dp.ME.pchange}
\alias{dp.ME.pchange}

\title{Dotplot visualization of percentile change as measure of influential data}
\description{
 This is a wrapper function to the dotplot() function in the lattice-package. It transforms the output from the ME.pchange() function and calls dotplot() to provide the user with a visualization of the percentile changes in mixed models due to the neutralization of the influence of grouping factors. 
}

\usage{
dp.ME.pchange(estex, parameters = 0, groups = 0, sort=FALSE, to.sort=NA, abs=FALSE, ...)
}
\arguments{
  \item{estex}{An object as returned by the \code{estex()} function, containing the altered estimates of a mixed effects regression model.}
  \item{parameters}{Used to define a selection of parameters. If parameters = 0 (default), values for percentile change are visualized for parameters in the model.}
  \item{groups}{Used to define a selection of nesting groups that should be visualized. If groups = 0 (default), the percentile changes for all nesting groups are shown.}
  \item{sort}{If \code{sort=TRUE} the values of percentile change are ordered based on magnitude before visualization. If \code{sort=FALSE} (default) no sorting takes place.}
  \item{to.sort}{Specify on which variable the percentile changes must be sorted. If only one variable present (either in the model, or due to the selection specified in \code{parameters}), this parameter can be omitted. If multiple variables are visualized, and sort=TRUE, specification of \code{to.sort} is required, or an error is returned.}
  \item{abs}{If \code{abs=TRUE}, the absolute values of percentile change are visualized, while if \code{abs=FALSE} (default), both positive and negative percentages are possible. If both \code{abs=TRUE} and \code{sort=TRUE}, the \code{abs} parameters precedes the \code{sort} parameter, and thus the absolute values of percentile change are sorted.}
  \item{\dots}{Further arguments passed on to the dotplot() function.}
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}

\seealso{
\code{\link{estex}}, \code{\link{ME.pchange}}
}

\examples{
data(school23)
model <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)

alt.est <- estex(model, "school.ID")
dp.ME.pchange(alt.est)
dp.ME.pchange(alt.est, layout=c(1,3))
}

\keyword{hplot}

