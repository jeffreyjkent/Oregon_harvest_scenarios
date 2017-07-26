#' Helper function for pulling dimension data from ncdf4 files (extension .nc)
#'
#' @description
#' The ncdf4 package has a print() method for .nc objects but sometimes fails. This function
#' supports ncdf4 package functionality by pulling dimension/variable names from a .nc
#' file or object of class "ncdf4".
#'
#'
#' @param filename        Filename to be passed to nc_open().
#'
#' @param nc              Object of class "ncdf4" to be passed instead of 'filename'.
#'
#' @return A data.frame with column names defined by the ncdf4 variables and the row entries
#'         defined by their corresponding dimensions.
#'
#' @details
#'
#' An error is returned if both/neither 'filename' and 'nc' are provided. If there is a
#' mismatch in assignment of dimension names to their corresponding place in the return
#' dataframe, then an error is also thrown.
#'
#'
#' @export
#' @examples
#' my_vars <- nc_GetDims(filename="my_clm_data.nc")
nc_GetDims <- function(filename=NULL, nc=NULL) {
  # Check input validity
  stopifnot(
    length(filename) > 0 | length(nc) > 0,
    !(length(filename) > 0 & length(nc) > 0)
  )
  if (length(filename) > 0) {
    nc <- ncdf4::nc_open(filename=filename)
  }
  dim.levels <- matrix(ncol=length(names(nc$var)), nrow=0)
  # This 'for' loop iterates through the length of variable names and reassigns
  #   a blank column to be filled by the loop.
  for (i in 1:(length(names(nc$var)))) {
    dim.subs <- vector(mode="character", length=0)
    # This 'for' loop iterates through the dimensions of the variable and fills in
    #   the blank columns with the variable labels.
    for (j in 1:(length(nc$var[[i]]$dim))) {
      dim.subs <- append(dim.subs, unlist(nc$var[[i]]$dim[[j]][1]))
    }
    # This 'while' loop iteratively adds extra rows to the 'dim.levels' matrix
    #   if the column data, previously fetched, won't fit in the matrix.
    while (nrow(dim.levels) < length(dim.subs)) {
      dim.levels <- rbind(dim.levels, vector(mode="character", length=ncol(dim.levels)))
    }
    # This 'while' loop iteratively adds NA's to the column data, previously feteched,
    #   if it is too short to bind to the matrix without throwing an error.
    while (nrow(dim.levels) > length(dim.subs)) {
      dim.subs <- append(dim.subs, NA)
    }
    # Once the column has been appropriately filled and is the correct length,
    #   it is assigned to the appropriate place in the return matrix.
    if (length(dim.subs) == nrow(dim.levels)) {
      dim.levels[,i] <- dim.subs
    } else {
      # I'm sure there could be better QC here, but this seems to work for now.
      stop("Can't parse dim levels")
    }
  }
  dim.levels <- as.data.frame(dim.levels, stringsAsFactors=FALSE)
  colnames(dim.levels) <- names(nc$var)
  return(dim.levels)
}
