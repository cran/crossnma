% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crossnma-package.R
\docType{package}
\name{crossnma-package}
\alias{crossnma-package}
\title{crossnma: An R package for synthesizing cross-design evidence and
cross-format data using Bayesian methods in network meta-analysis
and network meta-regression}
\description{
An R package \bold{crossnma} for performing (network) meta-analysis
and (network) meta-regression (allows including up to 3 covariates)
of individual participant data and aggregate data or combination of
both (Hamza et al., 2024). Each format can come from randomized controlled
trials or non-randomized studies. Estimates are generated in a Bayesian
framework using JAGS. The implemented models are described by Hamza
et al. (2023).
}
\details{
The evidence in network meta-analysis (NMA) typically comes from
randomized controlled trials (RCT) where aggregate data (AD) are
extracted from published reports. Retrieving individual participant
data (IPD) allows considering participant covariates to explain
some of the heterogeneity/inconsistency in the network and identify
effect modifiers. Additionally, evidence from non-randomized
studies (NRS) reflects the reality in clinical practice and bridges
the efficacy-effectiveness gap. The cross-NMA/NMR model is a
Bayesian suite for evidence synthesis which extends and integrates
four different approaches that combine RCT and NRS evidence into a
three-level hierarchical model for the synthesis of IPD and AD. The
four approaches account for differences in the design and risk of
bias in the RCT and NRS evidence. These four approaches variously
ignoring differences in risk of bias, using NRS to construct
penalized treatment effect priors and bias-adjustment models that
control the contribution of information from high risk of bias
studies in two different ways.

Further details:
\itemize{
\item To have a list of all R functions available in
  \bold{crossnma} type \code{help(package = "crossnma")}
\item Hamza et al. (2024) is the preferred citation in publications
  for \bold{crossnma}. Type \code{citation("crossnma")} for a BibTeX entry of
  this publication.
\item To report problems and bugs send an email to
  \email{hamza.a.tasnim@gmail.com}
\item The development version of \bold{crossnma} is available on
  GitHub \url{https://github.com/htx-r/crossnma}.
}
}
\references{
Dias S, Welton NJ, Marinho VCC et al. (2010):
Estimation and adjustment of bias in randomized evidence by using
mixed treatment comparison meta-analysis.
\emph{Journal of the Royal Statistical Society: Series A},
\bold{173}, 613-29

Hamza T, Chalkou K, Pellegrini F et al. (2023):
Synthesizing cross-design evidence and cross-format data using
network meta-regression.
\emph{Research Synthesis Methods},
\bold{14}, 283-300

Hamza T, Schwarzer G, Salanti G (2024):
crossnma: An R Package to Synthesize Cross-Design Evidence and Cross-Format
Data Using Network Meta-Analysis and Network Meta-Regression.
\emph{BMC Medical Research Methodology},
\bold{24}, 169.

Plummer M (2003):
JAGS: A program for analysis of Bayesian graphical models using
Gibbs sampling

Saramago P, Sutton AJ, Cooper NJ, Manca A (2012):
Mixed treatment comparisons using aggregate and individual
participant level data.
\emph{Statistics in Medicine},
\bold{31}, 3516-36

Tramacere I, Del Giovane C, Salanti G et al. (2015):
Immunomodulators and immunosuppressants for relapsing-remitting
multiple sclerosis: a network meta-analysis.
\emph{Cochrane Database of Systematic Reviews}, \bold{9},
John Wiley & Sons, Ltd. \doi{10.1002/14651858.CD011381.pub2}

Verde PE (2021):
A bias-corrected meta-analysis model for combining studies of
different types and quality.
\emph{Biometrical Journal},
\bold{63}, 406-22
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/htx-r/crossnma}
}

}
\author{
Tasnim Hamza \email{hamza.a.tasnim@gmail.com},
Guido Schwarzer \email{guido.schwarzer@uniklinik-freiburg.de},
Georgia Salanti \email{georgia.salanti@ispm.unibe.ch}
}
\keyword{package}
