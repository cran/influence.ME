\name{school23}
\alias{school23}
\docType{data}
\title{Math test performance in 23 schools}
\description{
  The \code{school23} data contains information on students' performance on a math test, as well as several explanatory variables. These data are subset of the NELS-88 data (National Education Longitudinal Study of 1988). Both a selected number of variables and a selected number of observations are given here.
}
%\usage{data(school23)}
\format{
  A data frame with 519 observations on the following 15 variables.
  \describe{
    \item{\code{school.ID}}{a factor with 23 levels, representing the 23 schools within which students are nested.}
    \item{\code{SES}}{a numeric vector, representing the socio-economic status}
    \item{\code{mean.SES}}{a numeric vector, representing the mean socio-economic status per school}
    \item{\code{homework}}{a factor representing the time spent on math homework each week, with levels \code{None}, \code{Less than 1 hour}, \code{1 hour}, \code{2 hours}, \code{3 hours}, \code{4-6 hours}, \code{7-9 hours}, and \code{10 or more}}
    \item{\code{parented}}{a factor representing the parents' highest education level, with levels \code{Dod not finish H.S.}, \code{H.S. grad or GED}, \code{GT H.S. and LT 4yr degree}, \code{College graduate}, \code{M.A. or equivalent}, and \code{Ph.D., M.D., other}}
    \item{\code{ratio}}{a numeric vector, representing the student-teacher ratio}
    \item{\code{perc.minor}}{a factor representing the percent minority in school, with levels \code{None}, \code{1-5}, \code{6-10}, \code{11-20}, \code{21-40}, \code{41-60}, \code{61-90}, and \code{91-100}}
    \item{\code{math}}{a numeric vector, representing the number of correct answers on a mathematics test}
    \item{\code{sex}}{a factor with levels \code{Male} and \code{Female}}
    \item{\code{race}}{a factor with levels \code{Asian}, \code{Hispanic}, \code{Black}, \code{White}, and \code{American Indian}}
    \item{\code{school.type}}{a factor representing the school type, with levels \code{Public school}, \code{Catholic school}, \code{Private, other religious affiliation}, and \code{Private, no religious affiliation}}
    \item{\code{structure}}{a numeric vector representing the degree to which the classroom environment is structured. High values represent higher levels of (accurate) classroom environment structure}
    \item{\code{school.size}}{a factor representing the total school enrollment, with levels \code{1-199 Students}, \code{200-399}, \code{400-599}, \code{600-799}, \code{800-999}, \code{1000-1199}, and \code{1200+}}
    \item{\code{urban}}{a factor with levels \code{Urban}, \code{Suburban}, and \code{Rural}}
    \item{\code{region}}{a factor with levels \code{Northeast}, \code{North Central}, \code{South, and} \code{West}}
  }
}
\details{
Labels for the factors were found in an appendix in Kreft \& De Leeuw (1998). All labels were designated, although in some cases not all possible values are represented in the variable (i.e. \code{region}). This is probably due to the fact that this is only a subsample from the full NELS-88 data.

Also, some of the variable names were changed.
}
\source{
These data are used in the examples given in Kreft \& De Leeuw (1998). Both the examples and the data are publicly available from the internet: http://www.ats.ucla.edu/stat/examples/imm/. Data reproduced with permission from Jan de Leeuw.
}
\references{
Kreft, I. and De Leeuw, J. (1998). \emph{Introducing Multilevel Modeling}. Sage Publications.

}
\examples{

data(school23)
model <- lmer(math ~ structure + (1 | school.ID), data=school23)
summary(model)
}
\keyword{datasets}
