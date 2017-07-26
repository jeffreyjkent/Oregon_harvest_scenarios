#' @title Workhorse function for iterating through a PFT-time dataset and harvesting trees.
#'
#' @description
#'
#' Harvest simulations can be run in CLM by creating variables that map biomass reduction through time. The
#' iterative process requires knowledge of biomass removal rate and standing biomass, and can be weighted.
#'
#' @param pft            PFT array.
#'
#' @param time           Number of timesteps to iterate the harvests
#'
#' @param rate           Rate of biomass removal per PFT, expressed as percent of
#'                       land surface.
#'
#' @param wts        Weighting vector.
#'
#' @param intervals      Return interval limit vector, PFT-dependent.
#'
#' @param intensity      Harvest intensity vector, PFT-dependent.
#'
#' @param sample.limit   Cell samples to take before failing, per iteration.
#'
#' @details
#'
#' Dimensions for 'pft' should be [1]: longitude, [2]: latitude, [3]: pft. 'time'
#'     can be present as a fourth dimension, if so then the most recent year is
#'     taken as the initial harvest conditions and everything else is discarded.
#'
#' 'rate' is currently a length-1 numeric that describes the percent of the land
#'     surface to harvest.
#'
#' 'wts' is a cell weighting vector that is the length of the spatial extent of
#'     the land surface.
#'
#' 'intervals' is a harvest interval vector describing the rotation limits on each PFT
#'
#' 'intensity' is similiar to intervals, but describes the decimal fraction of each
#'     cell/PFT combination to harvest in a given harvest event
#'
#' @return
#'
#' Returns an array of the same PFT/spatial size as the input, with PFT percentages
#' replaced with biomass harvest percentages.
#'
#' @export
#' @examples
#' harvest.array <- harvest_HarvestTrees(pft = pft_data,
#'  rates=harvest_rates, wts=harvest_wts,
#'  intervals = c(0, 30, 20, 25, 15, 38, 20, 15, 5, 10, 35, 100, 14, 25, 35, 0, 0)),
#'  intensity = rep(x=50, times=17), sample.limit=1500)
harvest_HarvestTrees <-
  function(pft, time, rate, wts, intervals, intensity, sample.limit=1000) {
    # Define function constants ####
    k.npft <- dim(pft)[3] # Assumes PFT layer is the third dimension in the array
    k.nlong <- dim(pft)[1] # Number of 'longitudes'
    k.nlat <- dim(pft)[2] # Number of 'latitudes'
    k.tdim <- 4 # Which dimension is the 'time' dimension in the PFT input array?
    #Input verification checks ####
    stopifnot(
      class(pft) == "array",
      class(time) == "numeric", length(time) == 1,
      class(rate) == "numeric", length(rate) == 1,
      class(wts) == "numeric", length(wts) == (k.nlong * k.nlat),
      class(intervals) == "numeric", length(intervals) == k.npft,
      class(intensity) == "numeric", length(intensity) == k.npft,
      class(sample.limit) == "numeric", length(sample.limit) == 1
    )
    # Working section: Initialize the new harvest ####
    harvest.ncells <- (k.nlat * k.nlong) * (rate/100) #number of cells to sample based on "rate", which is % of land area we want to harvest
    pft <- pft[,,,dim(pft)[k.tdim]] # Subset the most recent year from the 'pft' array
    harvest.timeseries <- array(data=0, dim=c(dim(pft), time)) # Add the new time dimension back on and fill 4-D array with 0's
    cat("nc_HarvestTrees run status:", "\n")

    # Working section: Iterate through 'time' ####
    for (t in 1:time) {
      # Update the console on algorithm run status ####
      if (t %% 5 == 0 | t == 1) {
        cat(round((t - 1) / time * 100), "percent complete.", "\n")
      }
      # Prepare a fresh spatial harvest grid for the current timestep
      sample_grid <- matrix(data=seq(1,k.nlong*k.nlat,by=1), nrow=k.nlong, ncol=k.nlat, byrow=FALSE)
      new.harvest <- array(data=0, dim=dim(pft))
      # 'sample.limit' iteration - provides QC on cell sampling ####
      for (i in 1:sample.limit) { #limits number of times to try random-draw of gridcells for sufficient biomass, constrained by rotation limits and reserved forest status
        if (i == sample.limit) {
          cat("FAILURE:", (t - 1), "years of harvesting completed", "\n")
          stop("Sample limit reached - try reducing cell harvests or upping available forest")
        }
        keep <- TRUE
        # Take a sample of potential cells to harvest
        # The interval vector, 'intervals' is an npft-length vector describing
        #     the harvest rotation for a particular PFT.
        cells <- sample(x=sample_grid,
                        size=harvest.ncells, na.rm=TRUE),
                        replace=FALSE, prob=wts)
#cat("cells from samples are",cells)
        # 'while' check on cell sample for harvest rotation limit ####
        while.break <- 0
        pass <- vector(mode="logical", length=k.npft) #flags for loop control per-PFT; defaults to be filled with "FALSE"
#!        while (all(pass) == FALSE) {
          for (j in cells) { #j is a list of integers grabbed by sample() from sample_grid, they're unique cell IDs
            cell.dims <- which(sample_grid == j, arr.ind=TRUE) #get the indices of the current cell
            lati.dim <- cell.dims[2] # Latitude of cell, expressed as dimension of latitude units
            long.dim <- cell.dims[1] # Longitude of cell, expressed as dimension of longitude units
            startsum <- pmax(1,t-intervals,na.rm=T) #check back to timestep 1 unless we're more than a rotation into the future, then only go back one rotation
                        #pmax is the vectorized form, so it returns a vector of values from pairwise comparisons against 1
            for (k in 1:length(intervals))) { # check this for each PFT
              # For the cell lat/long and PFT specified by the time interval, check
              #    the ENTIRE interval (startsum:t) for any harvest amounts.
              if ((sum(harvest.timeseries[long.dim, lati.dim, k, startsum[k]:t])) > 0) {
                # If there is any harvest for that PFT within its harvest rotation interval[], discard it
                cells[which(cells == j)] <- sample(x=sample_grid, size=1,prob=wts)
                pass[k] <- FALSE #just resampled, need to repeat the interval test
                break
              } else {
                pass[k] <- TRUE #passed interval test, don't need to re-iterate
              }
            }
            if (all(pass) == FALSE) {
              break
            }
          } # end 'j' loop
          while.break <- while.break + 1
          stopifnot(while.break < 1e3)
#!                                    } # end 'while' loop
        # 'keep' QC checks ####
        # pseudo-95% CI check on sampling probabilities
#       if (sum(wts(cells)) < (0.05 * length(cells) * mean(wts)) |
#            sum(wts(cells)) > (0.95 * length(cells) * mean(wts))) {
#          keep <- FALSE
#        }
        # 'sample.limit' breaks - skip to next iteration OR finish the timestep ####
        if (keep == TRUE) {
          # Finish the timestep - add the cells to the new harvest timeseries
          cells.index <- sapply(X=cells, FUN=function(x) {
            which(sample_grid==x, arr.ind=TRUE)
            }) # Transforms cells to their array index equivalents
          # Row 1 is row, Row 2 is column, columns are 'cells'
          for (j in 1:k.npft) {
            harvest.timeseries[cells.index[1,],cells.index[2,],j,t] <- intensity[j]
          }
          break
        } else {
          # Re-sample the cells and try again
          next
        }
      }
    }

    # Wrap-up and return ####
    cat("Done.")
    return(harvest.timeseries)
  }
