\name{se.fixef}
\alias{se.fixef} 

\title{Standard errors of fixed estimates}
\description{
Returns the standard errors of the fixed estimates in a mixed effects model.
}
\usage{
se.fixef(model)
}

\arguments{
  \item{model}{Mixed effects regression model of class 'mer'}
}


\value{
A vector with the standard errors of the fixed parameters of the model.
}

\author{Rense Nieuwenhuis, Ben Pelzer, Manfred te Grotenhuis}
\note{
This is a small helper-function to the influence.ME package. For more elaborate functionality, refer to the se.fixef function in the 'car' package. 
}

\examples{
data(school23)
model <- lmer(math ~  homework + structure + (1 | school.ID), data=school23)
summary(model)
se.fixef(model)
}

\keyword{regression}

