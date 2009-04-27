\name{grouping.levels}
\alias{grouping.levels}
\title{Returns the levels of a grouping factor in a mixed effects regression model}

\description{
Helper function returning all the levels of a grouping factor in a mixed effects regression model. This is used in combination with the influence.ME function, the divide long runs into multiple shorter ones.
}

\usage{
grouping.levels(model, group)
}

\arguments{
  \item{model}{Mixed effects model of class 'mer'}
  \item{group}{Grouping factor of 'model' of which the levels are returned}
}

\details{
 Please note that at times different results may be obtained by using nesting.levels(), compared with deriving the levels of the grouping factor directly from the (original) data. This is because nesting.levels() only extracts the nesting levels that were de facto used in the model. Due to missing values, this may diverge from those present in the actual data.
}

\value{
 Returns a character vector containing all the names / labels of levels of the grouping factor.
}

\references{http://www.ru.nl/mt/ic/downloads/}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}


\examples{
 # Penicillin data originates from the lme4 package.
 model <- lmer(diameter ~ (1|plate) + (1|sample), Penicillin)

 grouping.levels(model, "plate")
 grouping.levels(model, "sample")
}

\keyword{models}
\keyword{regression}
