\name{plot.estex}
\alias{plot.estex}
\alias{plot}

\title{Dotplot visualization of measures of influence}
\description{
 This is a wrapper function to the dotplot() function in the lattice-package. 
}

\usage{

\method{plot}{estex}(x, which="dfbetas", sort=FALSE, to.sort=NA, abs=FALSE, cutoff=0,
						parameters=seq_len(ncol(estex$alt.fixed)),
                        groups=seq_len(nrow(estex$alt.fixed)), \dots)
           
}
\arguments{
  \item{x}{An object as returned by the influence() function, containing the altered estimates of a mixed effects regression model.}
  \item{which}{Select which measure of influence is to be plotted. Available options are: \code{"dfbetas"} to visualize dfbetas, \code{"cook"} to plot the cook's distances, \code{"pchange"} to plot the percentage change, and \code{"sigtest"} to plot the test statistic of a parameter estimate after deletion of specific cases.}
  \item{sort}{If \code{sort=TRUE} The values of the selected measure of influence are ordered based on magnitude before visualization. If \code{sort=FALSE} (default) no sorting takes place.}
  \item{to.sort}{Specify on which variable the values of the selected measure of influence must be sorted. If only one variable present (either in the model, or due to the selection specified in \code{parameters}), this parameter can be omitted. If multiple variables are visualized, and sort=TRUE, specification of \code{to.sort} is required, or an error is returned.}
  \item{abs}{If \code{abs=TRUE}, the absolute values of the values of the selected measure of influence are visualized, while if \code{abs=FALSE} (default), both positive and negative values are possible. If both \code{abs=TRUE} and \code{sort=TRUE}, the \code{abs} parameters precedes the \code{sort} parameter, and thus the absolute values of the selected measure of influence are sorted.}
  \item{cutoff}{Values of the selected measure of influence exceeding the specified (\code{cutoff}) value are plotted visually different from values not exceeding the cutoff. If \code{cutoff=0} (default), no such differentiation is made in the way values are plotted.}
  \item{parameters}{Used to define a selection of parameters. If left unspecified (default), values for the selected measure of influence are visualized for parameters in the model.}
  \item{groups}{Used to define a selection of nesting groups that should be visualized. If left unspecified (default), the values of the selected measure of influence for all nesting groups are shown.}  
  \item{\dots}{Further arguments passed on to the dotplot() function.}
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}

\seealso{
  \code{\link{influence}}, \code{\link{dfbetas.estex}},
  \code{\link{cooks.distance.estex}}, \code{\link{pchange}}, \code{\link{sigtest}}
}

\examples{
\dontrun{
data(school23)
model <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)

alt.est <- influence(model, "school.ID")
plot(alt.est, which="dfbetas")
plot(alt.est, which="cook", sort=TRUE)
}
}

\keyword{hplot}

