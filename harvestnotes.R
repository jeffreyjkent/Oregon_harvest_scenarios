#figuring out how to make a harvest scenario netcdf
#have 4D array "pft", with dimensions [216,120,17,165]
dim(pft)
#all actual values are 0-100, so here's what's going on:
#it's defining the fraction coverage in 3 dimensions (long,lat,time) for each of 17 PFTs
#it doesn't contain the actual values for lat/long/time/pft because they're defined by their position in the array (ordinally)
sum(pft[1,46,,34]) #this sums the percent composition across all PFTs for a specific time/space coord ~ all vectors across the 3rd-dimension should sum to 100
pft[1,46,,34] #shows the PFT breakdown for this timepoint
pft[1,1,1,] #shows the change in this PFT across all timepoints
pft[1,,1,34] #fraction of this PFT across all latitudes
pft[,46,,34] #same, across longitudes

pft_lastyear <- pft[,,,165] #take the last year of data from the simulation (year 165 = 2015)
