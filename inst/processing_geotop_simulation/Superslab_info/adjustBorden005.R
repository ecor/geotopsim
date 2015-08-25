# TODO: Add comment
# 
# Author: ecor
###############################################################################


rm(list=ls())


library(soilwater)
library(geotopbricks)
library(horizons)
library(geotopsim)
library(stringr)
library(ggplot2)
library(reshape2)
#source(
######



#				'/home/ecor/Dropbox/R-packages/geotopsim/R/SoilWaterStorage.R' )

loadDataFromPackage <- FALSE

wpath_pkg <- "/home/ecor/Dropbox/R-packages/geotopsim/inst"
wpath_data <- "/home/ecor/Dropbox/R-packages/geotopsim/data"
wpath_inputdata <- paste(wpath_pkg,"processing_geotop_simulation/inputdata",sep="/")
wpath <- '/home/ecor/Dropbox/R-packages/geotopsim_simulations/Borden/Borden05m_001'


#"/home/ecor/Dropbox/R-packages/geotopsim_simulations/geotop_simulation" ######		paste(wpath_pkg,"geotop_simulation",sep="/")
file.output.csv <- paste(wpath_pkg,"processing_geotop_simulation/output/geotop.borden005.output_wb.csv",sep="/")
##file.proofilethetaplot <- paste(wpath_pkg,"processing_geotop_simulation/output/theta.png",sep="/")


paramPrefix <- "Header"
inpts.file <- "geotop.inpts"
tz <- "GMT"
unit_geotop <- 0.001 # millimeters!
DemMap <- get.geotop.inpts.keyword.value("DemFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
SlopeMap <- get.geotop.inpts.keyword.value("SlopeMapFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
SoilMap <- get.geotop.inpts.keyword.value("SoilMapFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
NetMap <- get.geotop.inpts.keyword.value("RiverNetwork",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
BedrockDepthMap <- get.geotop.inpts.keyword.value("BedrockDepthMapFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
WaterTableDepthMap <- get.geotop.inpts.keyword.value("WaterTableDepthMap",wpath=wpath,inpts.file=inpts.file,raster=TRUE)

