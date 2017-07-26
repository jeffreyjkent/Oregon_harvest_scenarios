nc_SpatialHeatmap <- function(nc, variable, latitude="latitude", longitude="longitude",save=FALSE) {
  require(ncdf4)
  require(ggplot2)
  stopifnot(
    class(nc) == "ncdf4",
    mode(save) == "logical"
            )
  if (missing("variable")) {
    variable <- names(nc$var)
  }
  stopifnot(is.character(variable))
  for (i in 1:length(variable)) {
    var.get <- ncvar_get(nc=nc,varid=variable[i])
    var.get <- c(var.get)
    stopifnot(is.vector(var.get))
    if (
        (length(nc$var[[variable[i]]]$dim) != 2) | 
        (nc$var[[variable[i]]]$dim[[1]]$name %in% c(latitude, longitude) == FALSE) |
        (nc$var[[variable[i]]]$dim[[2]]$name %in% c(latitude, longitude) == FALSE)
        ) {
      warning(paste("'",variable[i]," skipped - not a 2D spatial variable"))
    }
    latlongs <- nc_GetLatLongs(nc=nc,lat=latitude, long=longitude, variable=variable[i],
                               return="df")
    variable.df <- data.frame(latlongs$longitude, latlongs$latitude, var.get)
    colnames(variable.df) <- c("Longitude", "Latitude", variable[i])
    par(ask=TRUE)
    if (save == TRUE) {
      pdf(paste(variable[i],".pdf",sep=""))
    }
    print(ggplot(data=variable.df, aes_(~Longitude, ~Latitude)) +
            geom_tile(aes_(fill=~get(variable[i]))) +
            guides(fill=guide_legend(title=variable[i]))
    )
    if (save == TRUE) {
      dev.off()
    }
    par(ask=FALSE)
  }
}