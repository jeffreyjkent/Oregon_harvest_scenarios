#' Small wrapper function to pull ncdf4 variables en-mass from a given .nc file.
#'
#' @description
#'
#' nc_open() and ncvar_get() are used to pull variable data from .nc files, but it can be
#' tedious to pull them all out one-by-one if you're interested in exploring a new file. This
#' function will take a .nc extension filename and pull all of the variables into your
#' workspace
#'
#' @param filename        Filename to be passed to nc_* function pipeline.
#'
#' @param cutoff          Single-digit numeric giving function cutoff time, in minutes.
#'
#' @param save            Logical - should the R objects be saved to getwd(), instead of
#'                        the workspace?
#'
#' @return None - variables are assigned by their name within the .nc file to the global
#'         environment. If save=TRUE, objects are saved to current working directory instead.
#'
#' @details
#'
#' The 'cutoff' parameter will stop function execution after the given number of minutes
#' have passed - this will prevent the function from attempting to spend forever trying
#' to import especially large datasets.
#'
#' The 'save' parameter will dump the variable to a .Rdata file in the current working
#' directory and remove the variable from the R workspace. This might be easier/less time
#' consuming if the variables are large, and variables can then be called individually
#' with a call to load().
#'
#'
#' @export
#' @examples
#' nc_GetVars(filename="my_nc_file.nc")
#' nc_GetVars(filename="my_nc_file.nc", cutoff=60) # This will stop after a full hour (!)
nc_GetVars <- function(filename, cutoff=10, save=FALSE){
  nc.data <- ncdf4::nc_open(filename=filename)
  names.var.data <- names(nc.data$var)
  start <- proc.time()
  for (i in names.var.data) {
    assign(x=i, value=ncdf4::ncvar_get(nc.data,i), envir=.GlobalEnv)
    if (save == TRUE) {
      message(paste("Saving variable to", getwd(), ": ", i, sep=""))
      save(i, file=file.path(i, ".RData"))
      remove(i)
    }
    end <- proc.time()
    if ((end[3] - start[3]) > cutoff * 60) {
      message("Breaking ncvar_get, cutoff time reached")
      break
    }
  }
}
