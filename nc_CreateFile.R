#' Convenience function for generating ncdf4 files from input dataframes.
#'
#' @description
#' The ncdf4 function pipeline allows for the creation of ncdf4 files from variables,
#' dimensions, and other inputs - see the ncdf4 library for details. This function
#' streamlines the creation of ncdf4 files from data processed within R.
#'
#' @param data            Data frame containing all data to be written to file.
#'
#' @param vals            Vector of column numbers that represent data values. See 'details'.
#'
#' @param filename        Output filename. Requires extension '.nc' to work properly with ncdf4!
#'
#' @param units           Units for the data variables. Defaults to blanks ("").
#'
#' @param longitude       Which column is longitude?
#'
#' @param latitude        Which column is latitude?
#'
#' @param time            Which column is the time vector? Must be POSIXct or coercible
#'                        to such, time zone must be UTC+0 (=GMT).
#'
#' @param miss.val        The "missing value" is the number that ncdf4 translates from NA,
#'                        see ncdf4 documentation for more information.
#'
#' @param match           Should the lat/longs be matched to another ncdf4 file to
#'                        ensure congruity? Useful for collaborative projects.
#'
#' @details
#'
#' 'data'
#'
#' A dataframe containing all data values that are to be written to the file. At least one
#' column must contain longitude, one column latitude, and optional one column time.
#'
#' 'vals'
#'
#' A list of character vectors describing which dimensions are to be included in the variables.
#' The data values for each variables are taken from the column number of 'data' that
#' corresponds to
#'
#' 'units'
#'
#' Units should be a list of the same length as dims, with each element representing
#' the units for the corresponding dimension. Each element in 'units' should be the same
#' length as the unique entries in the corresponding 'dims' element.
#'
#' 'match'
#'
#' Match will provide a verification step (through nc_GetLatLongs) that lets you check to
#' see if the latitude / longitudes in a given ncdf4 objet will match your written ncdf4
#' file. Requires that latitude and longitude names are the same as in the file you're
#' attempting to write. Will return a generic error through stop() if they do not match.
#' Currently only supported for the default hemisphere in nc_GetLatLongs (=northern).
#'
#'
#' @examples
#' nc_CreateFile(data=mydata, vals=c("what","are","the","variables", filename="new_nc.nc")
nc_CreateFile <- function(data, vals, filename, units, longitude=1, latitude=2,
                          time=ncol(data), match=NULL, miss.val=c(-1)) {
  # Input Validity Check Section #####
  # Are all function parameters valid?
  if (missing("units")) {
    units <- rep(x="", times=length(names(df)))
  }
  # Time class handling - needs to be POSIXct before the stopifnot() checks
  if (length(time) > 0) {
    if (class(dims[[time]]) == "character") {
      dims[[time]] <- format(x=dims[[time]], tz="GMT", format="%Y-%m-%d %H:%M:%S")
    }
    if (class(dims[[time]]) == "POSIXlt") {
      dims[[time]] <- as.POSIXct(dims[[time]])
    }
    stopifnot(class(time) == "POSIXct")
  }
  stopifnot(
    is.list(dims),
    length(dims) > 1,
    class(filename) == "character",
    # 'filename' cannot already be in the current working directory.
    !(filename %in% list.files()),
    length(units) == length(names(df)),
    class(longitude) == "numeric",
    class(latitude) == "numeric"
    )
  # Execute the 'match' option, part of input validity checks
  if (length(time) > 0) {
    dims <- dims[order(dims[[longitude]], dims[[latitude]]), dims[[time]],]
  } else {
    dims <- dims[order(dims[[longitude]], dims[[latitude]])]
  }
  if (length(match) > 0) {
    stopifnot(
      class(match) == "ncdf4",
      length(match) > 1
    )
    latlongs <- nc_GetLatLongs(nc=match,long=names(dims)[longitude],
                               lat=names(dims)[latitude], return="df")
    message("Checking to see if lat/long sets match...")
    stopifnot(
      isTRUE(all.equal(unique(latlongs$latitude), unique(dims[[latitude]]))),
      isTRUE(all.equal(unique(latlongs$longitude), unique(dims[[longitude]])))
    )
  }
  # Work Section #####
  # Define the primary 3 dimensions:
  long <- ncdim_def(name=names(dims)[longitude], units=units[longitude],
                    vals=unique(dims[[longitude]]))
  lati <- ncdim_def(name=names(dims)[latitude], units=units[latitude],
                    vals=unique(dims[[latitude]]))
  if (length(time) > 0) {
    time <- ncdim_def(names=names(dims)[time], units=units[time],
                      vals=unique(dims[[time]]))
  }

  # CURRENT WORKING STATUS
  lati <- ncdim_def(name="long", units=units[which(names(df) == "longitude")],
                    vals=unique(df$longitude))
   for (i in names(df)){
    if (!(i %in% c("latitude", "longitude"))) {
      if (is.numeric(df[[i]])){
        assign(x=i,
               value=ncvar_def(name=i, units=units[which(names(df) == i)], dim=list(long,lati),
                               missval=miss.val)
        )
      }
      if (is.character(df[[i]])){
        df[[i]]<-as.factor(df[[i]])
        units.i<-paste(names(df[[i]]), collapse=", ")
        df[[i]]<-as.numeric(df[[i]])
        assign(x=i,
               value=ncvar_def(name=i, units=units.i, dim=list(long,lati), missval=miss.val)
        )
      }
    }
   }
  # assign the created .nc file and put in all the values
  #####
  df.dims<-names(df)[which(names(df) %in% c("latitude","longitude") == FALSE)]
  assign(x=filename,
         value=nc_create(filename=filename, dims=mget(df.dims)))
  for (i in names(df)){
    if (!(i %in% c("latitude", "longitude"))) {
      ncvar_put(nc=get(filename), varid=get(i), vals=df[[i]])
    }
  }
  # close the .nc file
  nc_close(nc=get(filename))
}
