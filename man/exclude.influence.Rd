\name{exclude.influence}
\alias{exclude.influence}

\title{Exclude the influence of a grouped set of observations in mixed effects models.}
\description{
  Using mixed effects regression models, \code{exclude.influence} excludes the influence of a group of cases grouped within a single grouping factor, or a set of grouping factors. The function returns a model in which the influence a grouped set of observations has on both the variance and point-estimate of the (random) intercept.
}
\usage{
exclude.influence(model, grouping, level, gf="single") 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{A mixed effects regression model}
  \item{grouping}{The grouping factor of which one or more groupings levels are to be 'neutralized'}
  \item{level}{Vector of character strings, indicating either a single level or a set of grouping levels the influence of which is to be neutralized}
  \item{gf}{Indicates from which of the model's grouping factors the influence of the specified grouping factor is to be neutralized. If \code{gf="single"} (default), the levels of the specified grouping factor are only neutralized from the grouping factor specified in \code{group}. In its present form, \code{gf="single"} only works on mixed models with a maximum of 2 grouping factors. If \code{gf="all"}, the influence from the levels of \code{group} is neutralized regarding all grouping factors in the model. This option only applies to models with more than a single grouping factor.}
}
\details{
To apply the basic logic of influential cases to mixed effects models one has to measure the influence of a particular higher level unit on the estimates of a higher level predictor. This means that the mixed effects model has to be adjusted to neutralize the unit's influence on that estimate, while at the same time allowing the unit's lower-level cases to help estimate the effects of the lower-level predictors in the model. This procedure is based on a modification of the intercept and the addition of a dummy variable for the cases that might be influential.

The model that is returned by \code{exclude.influence} thus contains a modified intercept, and one or more additional dummy variables. To help identify this model as modified (which is required when in a later stage the influence of additional grouping levels is excluded), the intercept is renamed to 'intercept.alt'. The additional dummy variables, indicating the observations associated with the grouping factor levels of which the influence was neutralized, are labeled starting with 'estex.', combined with the label of the neutralized grouping level. 
}

\value{
Mixed effects regression model of class \code{'mer'}, with a modified random intercept and dummy variables indicating the estimates of the neutralized influence of selected grouping levels.
}

\references{
Belsley, D.A., Kuh, E. & Welsch, R.E. (1980). \emph{Regression Diagnostics. Identifying Influential Data and Source of Collinearity}. Wiley. 

Snijders, T.A. & Bosker, R.J. (1999). \emph{Multilevel Analysis, an introduction to basic and advanced multilevel modeling}. Sage. 

Van Der Meer, T., Te Grotenhuis, M. & Pelzer, B. \emph{Influential cases in multi-level modeling. A methodological comment on 'National context, religiosity, and volunteering' by Ruiter and De Graaf}. Current status: conditionally accepted by the American Sociological Review.
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}

\note{
Please note that in its present form, the \code{exclude.influence} function only works on mixed effects regression models of class \code{mer} that have been estimated using the functions in the \code{lme4} package. 

Also, it is required that the \code{mer} model was estimated using a factor variable to indicate group levels. When using something similar to \code{+ (1 | as.factor(variable))}, the function is not able of identifying the correct grouping factors, and returns an error.
}

\seealso{\code{\link{estex}}}

\examples{
 data(school23)
 model.a <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)
 summary(model.a)
 model.b <- exclude.influence(model.a, "school.ID", "7472")
 summary(model.b)
 model.c <- exclude.influence(model.a, "school.ID", c("7472", "62821"))
 summary(model.c)
 
 data(Penicillin)
 model.d <- lmer(diameter ~ (1|plate) + (1|sample), Penicillin)
 summary(model.d)
 model.e <- exclude.influence(model.d, "sample", "A", gf="all")
 summary(model.e)
}

\keyword{robust}
\keyword{models}
\keyword{regression}
