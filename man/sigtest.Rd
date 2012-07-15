\name{sigtest}
\alias{sigtest}

\title{Test for changes in the level of statistical significance resulting from the deletion of potentially influential observations}
\description{Test for changes in the level of statistical significance resulting from the deletion of potentially influential observations}

\usage{
sigtest(estex, test = 1.96, parameters = 0, sort = FALSE, to.sort = NA)
}

\arguments{
  \item{estex}{Object of class 'estex', as returned from the \code{influence} function.}
  \item{test}{Value of the test statistic against which statistical significance is to be evaluated}
  \item{parameters}{Vector specifying the parameter(s) of which the significance is to be evaluated. If left unspecified, all parameters of the model are evaluated}
  \item{sort}{Specify whether the output should be sorted on the (absolute) magnitude of the test statistic after deletion of potentially influential cases}
  \item{to.sort}{If \code{sort==true}, the variable on which to sort the output needs to be be specified}
}
\details{
The \code{"sigtest"} function tests whether excluding the influence of a single case changes the statistical significance of any or more variables in the model. This test of significance is based on the test statistic provided by the \pkg{lme4} package. The nature of this statistic varies between different distributional families in the generalized mixed effects models. For instance, the t-statistic is related to a normal distribution while the z-statistic is related  to binomial distributions. 

For each of the cases that are evaluated, the test statistic of each variable is compared to a test-value specified by the user. For the purpose of this test, the parameter is regarded to statistically significant if the test statistic of the model exceeds the specified value. The \code{"sigtest"} function reports for each variable the test statistic after deletion of each evaluated case, whether or not this updated test statistic results in statistical significance based on the user-specified value, and whether or not this new statistical significance differs from the significance in the original model. So, in other words, if a parameter was statistically significant in the original model, but is not longer significant after the deletion of a specific case from the model, this is indicated by the output of the \code{"sigtest"} function. It is also indicated when an estimate was not significant originally, but reached statistical significance after deletion of a specific case. 

}
\value{
Returns a list. For each variable in the original model that was evaluated, this list contains a matrix showing the test statistic from the original model (column 1), the test statistic after a potentially influential case was excluded from the model (column 2) and the result (TRUE / FALSE) of the test whether statistical significance changed as a result from deletion of (potentially) influential cases.}


\author{Rense Nieuwenhuis, Manfred te Grotenhuis, Ben Pelzer}

\examples{

data(school23)
m23 <- lmer(math ~ homework + structure 
   + (1 | school.ID), 
   data=school23)

estex.m23  <- influence(m23, group="school.ID")
   
sigtest(estex.m23, test=-1.96)$structure
}

\keyword{influence}

