% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netconnection.crossnma.R
\name{netconnection.crossnma}
\alias{netconnection.crossnma}
\title{Get information on network connectivity (number of subnetworks,
distance matrix)}
\usage{
\method{netconnection}{crossnma}(data, ...)
}
\arguments{
\item{data}{An object produced by \code{\link{crossnma}}.}

\item{...}{\dots Additional arguments (passed on to
\code{\link{netconnection}})}
}
\value{
An object of class \code{netconnection} with corresponding
\code{print} function. The object is a list containing the
following components:
\item{treat1, treat2, studlab, title, warn, nchar.trts}{As defined
  above.}
\item{k}{Total number of studies.}
\item{m}{Total number of pairwise comparisons.}
\item{n}{Total number of treatments.}
\item{n.subnets}{Number of subnetworks; equal to 1 for a fully
  connected network.}
\item{D.matrix}{Distance matrix.}
\item{A.matrix}{Adjacency matrix.}
\item{L.matrix}{Laplace matrix.}
\item{call}{Function call.}
\item{version}{Version of R package netmeta used to create object.}
}
\description{
To determine the network structure and to test whether a given
network is fully connected. The function calculates the number of
subnetworks (connectivity components; value of 1 corresponds to a
fully connected network) and the distance matrix (in block-diagonal
form in the case of subnetworks). If some treatments are
combinations of
}
\examples{
\dontrun{
# We conduct a network meta-analysis assuming a random-effects
# model.
# The data comes from randomized-controlled trials and
# non-randomized studies (combined naively)
head(ipddata) # participant-level data
stddata # study-level data

# Create a JAGS model
mod <- crossnma.model(treat, id, relapse, n, design,
  prt.data = ipddata, std.data = stddata,
  reference = "A", trt.effect = "random", method.bias = "naive")

# Fit JAGS model
set.seed(1909)
fit <- crossnma(mod)

# Check network connectivity
netconnection(fit)
}

}
\seealso{
\code{\link[netmeta]{netconnection}}
}
\author{
Guido Schwarzer
  \email{guido.schwarzer@uniklinik-freiburg.de}
}
