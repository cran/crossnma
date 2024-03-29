#' Get information on network connectivity (number of subnetworks,
#' distance matrix)
#'
#' @description
#' To determine the network structure and to test whether a given
#' network is fully connected. The function calculates the number of
#' subnetworks (connectivity components; value of 1 corresponds to a
#' fully connected network) and the distance matrix (in block-diagonal
#' form in the case of subnetworks). If some treatments are
#' combinations of
#'
#' @param data An object produced by \code{\link{crossnma.model}}.
#' @param ... \dots Additional arguments (passed on to
#'   \code{\link{netconnection}})
#' 
#' @return
#' An object of class \code{netconnection} with corresponding
#' \code{print} function. The object is a list containing the
#' following components:
#' \item{treat1, treat2, studlab, title, warn, nchar.trts}{As defined
#'   above.}
#' \item{k}{Total number of studies.}
#' \item{m}{Total number of pairwise comparisons.}
#' \item{n}{Total number of treatments.}
#' \item{n.subnets}{Number of subnetworks; equal to 1 for a fully
#'   connected network.}
#' \item{D.matrix}{Distance matrix.}
#' \item{A.matrix}{Adjacency matrix.}
#' \item{L.matrix}{Laplace matrix.}
#' \item{call}{Function call.}
#' \item{version}{Version of R package netmeta used to create object.}
#'
#' @author Guido Schwarzer
#'   \email{guido.schwarzer@@uniklinik-freiburg.de}
#'
#' @seealso \code{\link[netmeta]{netconnection}}
#'
#' @examples
#' # We conduct a network meta-analysis assuming a random-effects
#' # model.
#' # The data comes from randomized-controlled trials and
#' # non-randomized studies (combined naively)
#' head(ipddata) # participant-level data
#' stddata # study-level data
#'
#' # Create a JAGS model
#' mod <- crossnma.model(treat, id, relapse, n, design,
#'   prt.data = ipddata, std.data = stddata,
#'   reference = "A", trt.effect = "random", method.bias = "naive")
#'
#' # Check network connectivity
#' netconnection(mod)
#'
#'@method netconnection crossnma.model
#'@export


netconnection.crossnma.model <- function(data, ...) { 
  chkclass(data, "crossnma.model")
  ##
  pw <- crossnma.model2pairwise(data)
  ##
  netconnection(pw, ...)
}
