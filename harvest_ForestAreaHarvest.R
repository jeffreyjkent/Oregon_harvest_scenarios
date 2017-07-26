#' @title Small workhorse function for generating harvest statistics from forest biomass and removal
#' rate.
#'
#' @description
#' Harvesting forests in CLM should be within the maximum rate that forest can be physically
#' harvested, and stochastic harvests should be checked against approximate harvested
#' cell counts. This function provides this, usually within other harvestCLM functions.
#'
#' As of 1 July 2017, this function is probably deprecated. The 'rates' parameter on harvest_HarvestTrees
#' should take care of anything this function needs.
#'
#' @param abg_biomass     Mean aboveground biomass in the ecoregion (kg C per sq. meter)
#'
#' @param removal_rate    Removal rate of carbon in the ecoregion in teragrams of carbon (TgC)
#'
#' @param area            Total area of harvest, in hectares.
#'
#' @param return          Which variable should be returned? See 'details'.
#'
#' @details
#'
#' 'abg_biomass' and 'removal_rate' must be vectors, and it is assumed that each row
#' corresponds to an 'ecoregion', although any arbitrary spatial extent could qualify
#' as an ecoregion.
#'
#' The 'return' parameter pulls variable names direcctly from the function code - acceptable
#' returns are:
#'
#' "TgCkm2"           - this is the abg_biomass converted to TgC per square kilometer
#' "cell_harvests"    - default, this is the expected # of 4km2 cells harvested with
#'                      the given removal rate.
#' "sqkm"             - sum of harvested cell extent, in square kilometers
#' "hect"             - sum of harvested cell extent, in hectares
#' "forest_area_dec"  - sum of harvested forest area, as decimal percent of
#'
#' @export
#' @examples
#' harvested_percentages <-
#' harvest_ForestAreaHarvest(abg_biomass = metadata$biomass, removal_rate=timber.rates,
#'                           return="forest_area_perc")
#' harvested_cells <-
#' harvest_ForestAreaHarvest(abg_biomass = c(24, 28, 12), removal_rate = c(0.1, 0.05, 0.3),
#'                           return="cell_harvests")
harvest_ForestAreaHarvest <-
function(abg_biomass, removal_rate, area, return="forest_area_perc") {
  # Input quality checks
  stopifnot(
    is.vector(abg_biomass),
    is.vector(removal_rate),
    return %in% c("TgCkm2", "cell_harvests", "sqkm",
                  "hect", "forest_area_dec", "forest_area_perc")
  )
  # Generating the 'return' variables.
  TgCkm2 <- abg_biomass * 0.001
  cell_harvests <- removal_rate / (TgCkm2 * 8)
  sqkm <- cell_harvests * 16
  hect <- sqkm * 100
  forest_area_dec <- hect / area
  forest_area_perc <- forest_area_dec * 100
  return(get(return))
}
