\name{estex}
\alias{estex}

\title{Estex returns mixed model estimates, iteratively excluding the influence of data nested within single grouping factors.}
\description{
   estex() is the workhorse function of the influence.ME package. Based on a priorly estimated mixed effects regression model (estimated using lme4), the estex() function iteratively modifies the mixed effects model to neutralize the effect a grouped set of data has on the parameters, and which returns returns the fixed parameters of these iteratively modified models. These are used to compute measures of influential data.
}

\usage{
estex(model, group, select = 0, gf="single", count = FALSE, delete=TRUE, ...)
}

\arguments{
  \item{model}{Mixed effects model of class 'mer'}
  \item{group}{Grouping factor in model of which iteratively levels are neutralized}
  \item{select}{Defines the selection of grouping factors that should be omitted. Defaults to 0, resulting in each level of the grouping factor being omitted iteratively. When a selection is defined, model parameters for the full model, and the altered model are returned. The selection can be a vector of multiple levels of the grouping factor.}
  \item{gf}{Indicates from which of the model's grouping factors the influence of the specified grouping factor is to be neutralized. If \code{gf="single"} (default), the levels of the specified grouping factor are only neutralized regarding the grouping factor specified in \code{group}. In its present form, \code{gf="single"} only works on mixed models with a maximum of 2 grouping factors. If \code{gf="all"}, the influence from the levels of \code{group} is neutralized regarding all grouping factors in the model. This option only applies to models with more than a single grouping factor.}
  \item{count}{If count=TRUE, the remaining number of grouping factors that still need to be omitted are printed.}
  \item{delete}{If delete=TRUE (default), the influence is excluded by simply deleting the observations nested within the higher level group. If delete=FALSE, the influence of higher level groups is excluded from the model by setting the intercept-vector for the observations nested within these groups to 0, and by adding a dummy-variable indicating these observations (Langford and Lewis, 1998). This latter option currently does not work with models that include factor variables.}
  \item{\dots}{Optional arguments that are passed on to the lmer/glmer function}
}

\details{
  The basic rationale behind measuring influential cases is that when iteratively single units are omitted from the data, models based on these data should not produce substantially different estimates. To apply this logic to mixed effects models one has to measure the influence of a particular higher level unit on the estimates of a higher level predictor. This means that the mixed effects model has to be adjusted to neutralize the unit's influence on that estimate, while at the same time allowing the unit's lower-level cases to help estimate the effects of the lower-level predictors in the model. This procedure is based on a modification of the intercept and the addition of a dummy variable for the cases that might be influential.
  
  estex() is the workhorse function of this likewise called package. Based on a priorly estimated mixed effects regression model (of the 'mer' class), the estex() function iteratively modifies the mixed effects model by neutralizing the effect a grouped set of data has on the parameters, and which returns returns the fixed parameters of these iteratively modified models. 
  
  The returned object (see 'value') contains information which is required for functions computing various measures of influential data.
}

\value{
  The object returned by estex() of class "alt.est" contains the 'altered estimates' required by several other functions to calculate measures of influential data. A list containing six elements is returned:
  \item{or.fixed}{Fixed estimates of the original model (based on the full data)}
  \item{or.se}{Standard Error of the estimates of the original model}
  \item{or.vcov}{Variance / Covariance matrix of the original model}
  \item{alt.fixed}{Matrix of the fixed parameters estimate, after iteratively subsets of data are removed. Altered estimates associated with the deletion of data nested within each grouping factor are provided.}
  \item{alt.se}{Matrix of the standard errors of the fixed parameter estimates, after iteratively subsets of data are removed. Altered estimates associated with the deletion of data nested within each grouping factor are provided.}
  \item{alt.vcov}{Variance / Covariance matrix of the altered models, after iteratively subsets of data are removed. Altered estimates associated with the deletion of data nested within each grouping factor are provided.}
}

\references{
 Belsley, D.A., Kuh, E. & Welsch, R.E. (1980). \emph{Regression Diagnostics. Identifying Influential Data and Source of Collinearity}. Wiley. 
 
 Langford, I. H. and Lewis, T. (1998). Outliers in multilevel data. Journal of the 
Royal Statistical Society: Series A (Statistics in Society), 161:121-160.

 Snijders, T.A. & Bosker, R.J. (1999). \emph{Multilevel Analysis, an introduction to basic and advanced multilevel modeling}. Sage. 

Van der Meer, T., Te Grotenhuis, M., & Pelzer, B. (2010). \emph{Influential Cases in Multilevel Modeling: A Methodological Comment}. American Sociological Review, 75(1), 173-178.
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}

\note{

Please note that in its present form, the \code{estex} function only works on mixed effects regression models of class \code{mer} that have been estimated using the functions in the \code{lme4} package. 

Also, it is required that the \code{mer} model was estimated using a factor variable to indicate group levels. When using something similar to \code{+ (1 | as.factor(variable))}, the function is not able of identifying the correct grouping factors, and returns an error.

Since estex() entails the re-estimation of the provided mixed effects model for each level of the specified grouping factor (after alteration of the data), executing this procedure can be computationally highly demanding. 

To facilitate the use of estex() with more complex models (i.e. models based on large amounts of data and / or with a large number of groups.
}

\seealso{
\code{\link{ME.dfbetas}}, \code{\link{ME.cook}}
}

\examples{
 data(school23)
 model.a <- lmer(math ~ structure + SES  + (1 | school.ID), data=school23)
 alt.est.a <- estex(model.a, "school.ID")
 alt.est.b <- estex(model.a, "school.ID", "7472")
 alt.est.c <- estex(model.a, "school.ID", c("7472", "62821"))
 
 data(Penicillin)
 model.b <- lmer(diameter ~ (1|plate) + (1|sample), Penicillin)
 alt.est.d <- estex(model.b, "plate")
 alt.est.e <- estex(model.b, "sample")
 alt.est.f <- estex(model.b, "sample", gf="all")
}

\keyword{robust}
\keyword{models}
\keyword{regression}
