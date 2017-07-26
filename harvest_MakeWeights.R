#' @title Convenience function for generating spatially-based harvest weights.
#'
#' @description
#'
#' The \link{harvest_HarvestTrees} function will create a time series of harvest locations based on input PFT data,
#' but it requires a weighting vector that is the same size as the spatial extent. This will generate a weighting
#' vector based on that input PFT and spatial maps of harvest probabilities.
#'
#' @param pft            PFT array - used to extract spatial dimensions.
#'
#' @param ...            Additional matrices that include the probability weights. If matrix
#'                       values do not sum to unity, they are scaled.
#'
#' @details
#'
#' Dimensions for 'pft' should be [1]: longitude, [2]: latitude, with other dimensions ignored.
#'
#'
#' @return
#'
#' Returns either a vector the same length as (latitudes * longitudes) or a matrix the same extent as a
#' lat/long dimension within 'pft'
#'
#' @export
#' @examples
#' weights <- harvest_MakeWeights(pft=pft_data, protected, ecoregion_harvests)
harvest_MakeWeights <- function(pft, ...) {
  # Input validation/QC ####
  prob.mats <- list(...)
  stopifnot(
    is.array(pft),
    all(class(unlist(prob.mats) == "matrix"))
  )
  k.nlong <- dim(pft)[1] # Number of 'longitudes'
  k.nlat <- dim(pft)[2] # Number of 'latitudes'
  latlong.mat <- matrix(data=NA, nrow=k.nlong, ncol=k.nlat, byrow=FALSE)
  # Scale the input probability matrices ####
  for (i in prob.mats) {
    prob.mat <- prob.mats[[i]]
    stopifnot(
      nrow(prob.mat) == k.nlong,
      ncol(prob.mat) == k.nlat
    )
    prob.mat <- prob.mat / sum(prob.mat, na.rm=TRUE)
    prob.mats[[i]] <- prob.mat
  }
  # Concatenate the input probability matrices ####
  weights <- eval(parse(text=paste(unlist(prob.mats), collapse="*")))
  # Wrap-up and return ####
}
