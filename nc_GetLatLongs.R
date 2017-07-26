#' Convenience function for pulling the latitudes/longitudes out of a ncdf4 file
#' or an R object of class("ncdf4")
#'
#' @description
#'
#' Latitudes/longitudes are often important variables in ncdf4 files, and will just as
#' often be required to interpret their associated data. This function will retrive
#' lat/long information from a netcdf4 file or R object of class "ncdf4", assuming
#' the file structure is supported - see 'details'.
#'
#'
#' @param nc              Object of class "ncdf4" containing the latitude/longitudes.
#'
#' @param lat             The latitude identifier.
#'
#' @param long            The longitude identifier.
#'
#' @param variable        Variable containing the encoded latitude/longitudes. Optional,
#'                        see 'details' for when it is appropriate to use.
#'
#' @param return          Format of lat/longs to return - see 'details'.
#'
#' @param hemisphere      Hemisphere that the latitude/longitudes are in. Defaults to N.
#'
#' @return
#'
#' Returns either a matrix grid of latlong points (return="map") with latitude as rownames
#' and longitude as column names OR a dataframe (return="df") with each latitude/longitude
#' combination reflected as one row and both latitude AND longitude as seperate columns.
#'
#' @details
#'
#' Assumptions / general procedure:
#'
#'     1: Latitude 0 is the equator - N/S hemisphere must be specified. The range of
#'        latitude within the ncdf4 file/object must range from -90 to +90.
#'
#'     2: Longitude 0 is the Prime Meridian, and increases moving east (range 0 to 360)
#'
#'     3: S hemisphere will be converted to positive numbers and ordered to be increasing
#'        from N to S. N hemisphere latitude will be ordered decreasing from N to S.
#'
#'     4: Lat/long inputs are assumed to monotonically increase along with increasing
#'        dimension number. They are also assumed to be cartesian - e.g. longitude dimension
#'        101 is farther east than dimension 100.
#'
#'     5: In order for lat/longs to be correctly identified as their own variables within
#'        the ncdf4 object, their must either be two variables identified by 'lat=' and
#'        'long=' OR 'lat=' and 'long=' dimensions within a given 'variable', with the
#'        appropriately defined units associated with that dimension. If the former approach
#'        is used, longitudes should be encoded in rows and latitude in columns.
#'
#'     6. If a dataframe is returned, full longitude iterated for a given latitutde and
#'        each longitude iteration is run for every latitude. I.e., rows will run through
#'        all of the longitudes for a given latitude before moving on to the next latitude
#'        and subsequent set of all longitudes.
#'
#' @export
#' @examples
#' latitude_longitude <- nc_GetLatLongs(nc=my_ncdf4_object, lat="latitudes",
#'                                      long="longitudes", variable="pft")
nc_GetLatLongs<-function(nc, lat="LATIXY", long="LONGXY", variable=NULL,
                         return="map", hemisphere="north"){
  # Check for input validity
  stopifnot(
    class(nc) == "ncdf4",
    is.character(lat),
    is.character(long),
    is.character(variable) | length(variable) < 1,
    return %in% c("map","df")
  )
  # Check to see if lat/longs are encoded as seperate variables:
  if (length(variable) < 1) {
    if (all(c(lat,long) %in% names(nc$var))) {
      # This is very specific to a particular NC construction.
      longitude <- ncvar_get(nc=nc, varid=long)
      latitude  <- ncvar_get(nc=nc, varid=lat)
      if (length(dim(longitude)) > 1) {
        if (length(table(longitude[1, ]))>1) {
          stop("'Longitudes should be encoded in rows - columns for a given
               row should be identical")
        }
        longitude <- longitude[, 1]
      }
      if (length(dim(latitude)) > 1) {
        if (length(table(latitude[,1]))>1) {
          stop("'Longitudes should be encoded in rows - columns for a given
               row should be identical")
        }
        latitude <- latitude[1, ]
      }
    } else {
      stop(paste(lat,"/",long,"not found in variable names"))
    }
  } else {
    # Alternatively, check to see if they are the given as dimensions for any/given 'var'
    stopifnot(variable %in% names(nc$var))
    dim.names <- vector(length=0,mode="character")
    for (i in 1:length(nc$var[[variable]]$dim)) {
      if (nc$var[[variable]]$dim[[i]]$name %in% c(lat,long) == FALSE) {
        stop(paste(lat,"/",long,"not found in","'",variable,"'","dimensions"))
      }
      if (nc$var[[variable]]$dim[[i]]$name == lat) {
        latitude <- nc$var[[variable]]$dim[[i]]$vals
      }
      if (nc$var[[variable]]$dim[[i]]$name == long) {
        longitude <- nc$var[[variable]]$dim[[i]]$vals
      }
    }
  }
  # Data output integrity checks
  stopifnot(
    exists("longitude"),
    exists("latitude"),
    # these 'cummax' checks make sure that lat/long are monotonically increasing
    all(latitude == cummax(latitude)),
    all(longitude == cummax(longitude)),
    # these 'length' checks make sure you got the right columns for the lat/long data pulls
    length(unique(longitude)) == length(longitude),
    length(unique(latitude)) == length(latitude)
  )
  # The default is to return a 'map', or empty matrix with longitude as the row names
  #   and latitude as the column names. This represents a flat 'map' of XY coordinates.
  if (hemisphere == "north") {
    latitude <- latitude[order(latitude, decreasing=TRUE)]
  }
  if (hemisphere == "south") {
    latitude <- abs(latitude)
  }
  latlongs <- matrix(data=NA, nrow=length(unique(latitude)), ncol=length(unique(longitude)),
                     dimnames=list(latitude,longitude))
  # Return may also be a data.frame of all latitude and longitude conditions.
  if (return=="df") {
    # You could also probably accomplish this with the melt() function in library(reshape2)
    latitude <- dimnames(latlongs)[[1]]
    latitude <- latitude[order(latitude, decreasing=FALSE)]
    longitude <- dimnames(latlongs)[[2]]
    longitude.full  <- rep(x=longitude, times=length(latitude))
    latitude.full <- rep(x=latitude, each=length(longitude))
    latlongs<-data.frame(longitude.full,latitude.full)
    colnames(latlongs) <- c("longitude", "latitude")
    latlongs$latitude<-as.character(latlongs$latitude)
    latlongs$latitude<-as.numeric(latlongs$latitude)
    latlongs$longitude<-as.character(latlongs$longitude)
    latlongs$longitude<-as.numeric(latlongs$longitude)
  }
  return(latlongs)
}
