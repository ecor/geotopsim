#! .... 

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
wpath <- '/home/ecor/Dropbox/R-packages/geotopsim_simulations/Vc_SC/Vc_SC4_001' 
wpath_outputdata <- paste(wpath_pkg,"processing_geotop_simulation/output",sep="/")
wpath_outputdata <- "/home/ecor/Dropbox/Public/geotop_intercomparison/Vc_SC4"



if (!file.exists(wpath_outputdata)) dir.create(wpath_outputdata,recursive=TRUE)

#"/home/ecor/Dropbox/R-packages/geotopsim_simulations/geotop_simulation" ######		paste(wpath_pkg,"geotop_simulation",sep="/")
file.output.csv <- paste(wpath_outputdata,"geotop.Vc_SC4.output.csv",sep="/")
##file.proofilethetaplot <- paste(wpath_pkg,"processing_geotop_simulation/output/theta.png",sep="/")
##
file.nctheta <-  paste(wpath_outputdata,"geotop.theta.Vc_SC4.nc",sep="/")
file.ncpsi <- paste(wpath_outputdata,"geotop.soilwaterpressure.Vc_SC4.nc",sep="/")
file.nchsup <- paste(wpath_outputdata,"geotop.surfacewater.Vc_SC4.nc",sep="/")
##




paramPrefix <- "Header"
inpts.file <- "geotop.inpts"
tz <- "GMT"
unit_geotop <- 0.001 ## vertical legth are returned in millimeters!!

DemMap <- get.geotop.inpts.keyword.value("DemFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
SlopeMap <- get.geotop.inpts.keyword.value("SlopeMapFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
SoilMap <- get.geotop.inpts.keyword.value("SoilMapFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
NetMap <- get.geotop.inpts.keyword.value("RiverNetwork",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
if (is.null(SoilMap)) {
	
	soillevel=1
	SoilMap <- DemMap*0+1
	
} else {
	
	soillevel=unique(SoilMap)
}
start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,inpts.file=inpts.file,tz="GMT")
end <-  get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,inpts.file=inpts.file,tz="GMT")
#### Soil Properties


DzName <- get.geotop.inpts.keyword.value(paste(paramPrefix,"SoilDz",sep=""),wpath=wpath,inpts.file=inpts.file)
SoilPar <- tryCatch(get.geotop.inpts.keyword.value("SoilParFile",wpath=wpath,inpts.file=inpts.file,data.frame=TRUE,level=soillevel),error=function(e){NULL})			
if (is.null(SoilPar)) {
	
	layer  <- get.geotop.inpts.keyword.value("SoilLayerThicknesses",numeric=TRUE,wpath=wpath,inpts.file=inpts.file)
	
}  else {	
	
    SoilPar <- list(SoilPar)
	layer <- SoilPar[[1]][,DzName]
}


time_duration <- 2  # Expressed in hours
dtmap <- get.geotop.inpts.keyword.value("OutputSoilMaps",numeric=TRUE,wpath=wpath,inpts.file=inpts.file)*3600

#seq(from=start,to=end,by=3600)-start

time_duration_when <- seq(from=start,to=end,by=dtmap)#start+seq(from=0,to=time_duration*3600,by=dtmap*3600)   
time_duration_when <-  time_duration_when[-1] 
#(1:time_duration)*3600




if (loadDataFromPackage==TRUE) { 

	psi <- NULL
	theta <- NULL
	data(PsiTheta_Vc1)
	
} else {
	psi <- brickFromOutputSoil3DTensor("SoilLiqWaterPressTensorFile",
		when=time_duration_when, layers = "SoilLayerThicknesses",
		tz=tz,timestep = "OutputSoilMaps",wpath=wpath,inpts.file=inpts.file)
	theta <- brickFromOutputSoil3DTensor("SoilLiqContentTensorFile",

		when=time_duration_when, layers = "SoilLayerThicknesses",
		tz=tz,timestep = "OutputSoilMaps",wpath=wpath,inpts.file=inpts.file)

	hsup <- rasterFromOutput2DMap("LandSurfaceWaterDepthMapFile",
		
		when=time_duration_when,
		tz=tz,timestep = "OutputSoilMaps",wpath=wpath,inpts.file=inpts.file)

	hsup <- lapply(X=hsup,FUN=function(x) {x+0})
	save(list=c("psi","theta","hsup"),file=paste(wpath_data,"PsiTheta_Vc1.rda",sep="/"))


}
#################### PREPARE CSV 

t <- 5

dz <- layer*unit_geotop  ## 50 millimiters layer depth ##transformed from millimiters to meters
dx <- xres(theta[[1]])

qpoints <- 0 ##Discharge	at	outlet	and	downstream	end	of	slab1	and	2	
names(qpoints) <- c("outlet")


outputCsv <- data.frame(time=as.numeric(time_duration_when-time_duration_when[1]+dtmap),geotop_surface_water=NA,
		geotop_soilwater=NA,
		geotop_unsat_soilwater=NA, 
		geotop_groundwt_soilwater=NA,
		geotop_topsoilmoisture_m2=NA
		)

##outputCsv[,c("geotop_qsub_outlet","geotop_qsup_outlet")] <- NA

###outputCsv <- outputCsv[,names_v]


for (t in 1:length(time_duration_when)) {
	
##	 dz is expressed in meters!!
	unsatWaterVolume <- SoilWaterStorage(theta[[t]],psi[[t]],layer=dz,comparison="<",psi_thres=0,fun=sum)
	satWaterVolume <- SoilWaterStorage(theta[[t]],psi[[t]],layer=dz,comparison=">=",psi_thres=0,fun=sum)
	WaterVolume <- SoilWaterStorage(theta[[t]],NULL,layer=dz,fun=sum) 
	TopSoilMoisture <- theta[[t]][[1]]
	## qdischarge_sub <- LateralSubsurfaceDischarge(psi[[t]],wpath=wpath,output.discharge=TRUE)$discharge
	
##	qdischarge_sup <- LateralSurfaceDischarge(hsup[[t]],wpath=wpath,output.discharge=TRUE)
	

	volumetot <- function(x) {sum(as.matrix(x),na.rm=TRUE)*xres(x)*yres(x)}

	outputCsv[t,"geotop_soilwater"]<-  volumetot(WaterVolume)  ##sum(xyFrom2PointLine(r=WaterVolume,points=transect)$value001,na.rm=TRUE)*dx
	outputCsv[t,"geotop_groundwt_soilwater"]<-  volumetot(satWaterVolume) ##sum(xyFrom2PointLine(r=satWaterVolume,points=transect)$value001,na.rm=TRUE)*dx
	outputCsv[t,"geotop_unsat_soilwater"]<- volumetot(unsatWaterVolume) ##sum(xyFrom2PointLine(r=unsatWaterVolume,points=transect)$value001,na.rm=TRUE)*dx
	outputCsv[t,"geotop_surface_water"]<- volumetot(hsup[[t]]*unit_geotop) ##sum(xyFrom2PointLine(r=hsup[[t]]/1000,points=transect)$value001,na.rm=TRUE)*dx
	outputCsv[t,"geotop_topsoilmoisture_m2"]<-  volumetot(TopSoilMoisture) ##sum(xyFrom2PointLine(r=TopSoilMoisture,points=transect)$value001,na.rm=TRUE)*dx
##    outputCsv[t,paste("geotop_qsup_outlet",sep="_")] <- qdischarge_sup[NetMap==1]
##	outputCsv[t,paste("geotop_qsub_outlet",sep="_")] <- qdischarge_sub[NetMap==1]
	 
	
	
	

}


## Find Disc

discharge <- get.geotop.inpts.keyword.value("DischargeFile",wpath=wpath,data.frame=TRUE,formatter="",inpts.file=inpts.file,date_field="DATE.day.month.year.hour.min.",format="%d/%m/%Y %H:%M",tz=tz)
time_d <- as.POSIXct(index(discharge))
time_d_sec <- as.numeric((time_d-start),units="secs")

discharge_add <- discharge[(time_d_sec %in% outputCsv$time),]
discharge_add$time <- outputCsv$time

discharge_add <- as.data.frame(discharge_add)
outputCsv$geotop_qoutlandsup_m3persec <- as.vector(discharge_add[,"Qoutlandsup.m3.s."])

write.table(outputCsv,file=file.output.csv,quote=FALSE,sep=",",row.names=FALSE)




### NetCDF for soil water potential pressure and soil water content 
##z*(1+unique(sin(SlopeMap*))^2)^0.5


times <- outputCsv$time

hsup <- lapply(X=hsup,FUN=brick)
hsup <- lapply(X=hsup,FUN=function(x,u){x*u},u=unit_geotop)
psi <- lapply(X=psi,FUN=function(x,u){x*u},u=unit_geotop)
dzl <- array(dz,nlayers(psi[[1]]))
depth <- dzl/2

for (i in 2:length(depth)) {
	
	depth[i] <- depth[i-1]+(dzl[i-1]+dzl[i])/2 
	
}



ncpsi <- buildnetCDF(psi,filename=file.ncpsi,time=times,depth=depth,name="SoilWaterPressureHead",longname="Soil Water Pressure Head",units_Time="seconds")
nctheta <- buildnetCDF(theta,filename=file.nctheta,time=times,depth=depth,name="SWC",longname="Soil Water Content",units="dimensionless",units_Time="second")
nchsup <- buildnetCDF(hsup,filename=file.nchsup,time=times,name="WaterDepth",longname="Surface Water Depth",units="meter",units_Time="second")


nc_close(ncpsi)
nc_close(nctheta)
nc_close(nchsup)



