#install.packages("tidyr")
library(tidyr)
library(ncdf4)
okthin <- read.table("data/Okay_for_regular_harvest_OR_new.txt",sep=",",header=TRUE)
avail<- read.table("data/Available_for_thinning_OR_new.txt",sep=",",header=TRUE)
hist <- nc_open("data/landuse.timeseries_simyr1850_2014_OR.nc",verbose=T)

harvest_MakeWeights(
  pft=pft_lastyear
)

source("R/harvest_HarvestTrees.R") # read in the latest version of the function
harvest_HarvestTrees(
  pft=pft,
  time=27,
  rate=1.5,
  wts=rep(0.5,times=25920),
  intervals=rep(60,times=17),
  intensity=rep(50,times=17),
  sample.limit=1500
)





