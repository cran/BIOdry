\name{anova.modelFrame}
\alias{anova.modelFrame}
\title{Compare modelFrame objects}
\description{Models in \code{\link{modelFrame}} objects are compared with an
ANOVA method when the terminal fit is based on
\code{\link[nlme]{lme}}.}
\usage{\method{anova}{modelFrame}(object, 
    ..., test, type, 
    adjustSigma, Terms, 
    L, verbose)}
\arguments{
  \item{object}{An \code{object} inheriting from class \code{\link{modelFrame}}.}
  \item{\dots}{Other optional fitted model objects inheriting from classes
\code{"modelFrame"}, \code{"lme"}, \code{"lm"}, among
others accepted by \code{\link[nlme]{anova.lme}}.}
  \item{test}{test }
  \item{type}{type }
  \item{adjustSigma}{adjustSigma }
  \item{Terms}{Terms }
  \item{L}{L }
  \item{verbose}{verbose }
}

\value{A data frame inheriting from class \code{"anova.lme"} when the
underlying terminal fit is mixed-effects.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## ring-width chronologies of eight cores and the allometric
## parameters of the modelFrame example
data(Pchron, envir = environment())
biom_param <- c(2.87, 0.85, 0.05, 2.5)
## the biomass model with the two Zeide time forms, f(t) = t and
## f(t) = ln t, fitted by maximum likelihood
m_t <- Pchron |>
    modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
               log.t = FALSE, method = 'ML')
m_lnt <- Pchron |>
    modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
               log.t = TRUE, method = 'ML')
## sequential F tests of the fixed effects of one model
anova(m_t)
## both forms have the same number of parameters: compare AIC and BIC
anova(m_t, m_lnt)
}
