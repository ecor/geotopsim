
rm(list=ls())

wpath_script <- '/home/ecor/Dropbox/R-packages/geotopsim/inst/processing_geotop_simulation'

files <- list.files(wpath_script,full.names=TRUE,pattern="analyzeV")

for (it in files) source(it)