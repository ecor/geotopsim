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
wpath <- "/home/ecor/Dropbox/R-packages/geotopsim_simulations/geotop_simulation" ######		paste(wpath_pkg,"geotop_simulation",sep="/")
file.output.csv <- paste(wpath_pkg,"processing_geotop_simulation/output/geotop.superslab.output.csv",sep="/")
file.proofilepsiplot <- paste(wpath_pkg,"processing_geotop_simulation/output/psi.png",sep="/")
file.proofilethetaplot <- paste(wpath_pkg,"processing_geotop_simulation/output/theta.png",sep="/")
##
file.nctheta <-  paste(wpath_pkg,"processing_geotop_simulation/output/geotop.theta.nc",sep="/")
file.ncpsi <- paste(wpath_pkg,"processing_geotop_simulation/output/geotop.soilwaterpressure.nc",sep="/")
file.nchsup <- paste(wpath_pkg,"processing_geotop_simulation/output/geotop.surfacewater.nc",sep="/")
##


paramPrefix <- "Header"
inpts.file <- "geotop.inpts"
tz <- "GMT"

DemMap <- get.geotop.inpts.keyword.value("DemFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
SlopeMap <- get.geotop.inpts.keyword.value("SlopeMapFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
SoilMap <- get.geotop.inpts.keyword.value("SoilMapFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)

start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,inpts.file=inpts.file,tz="GMT")
#### Soil Properties


DzName <- get.geotop.inpts.keyword.value(paste(paramPrefix,"SoilDz",sep=""),wpath=wpath,inpts.file=inpts.file)
SoilPar <- get.geotop.inpts.keyword.value("SoilParFile",wpath=wpath,inpts.file=inpts.file,data.frame=TRUE,level=unique(SoilMap))			
layer <- SoilPar[[1]][,DzName]


####
y <- (ymax(DemMap)+ymin(DemMap))/2

## 
x <- c(0,8,40,32,41)
names(x) <- c("Outlet","Slab1","Slab2","A","B")

xy <- data.frame(Name=names(x),x=x,y=y)

xy$cell <- cellFromXY(DemMap,xy[c("x","y")])

time_duration <- 13  # Expressed in hours


time_duration_when <- start+(1:time_duration)*3600


if (loadDataFromPackage==TRUE) { 

	psi <- NULL
	theta <- NULL
	data(PsiTheta)
	
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
	save(list=c("psi","theta","hsup"),file=paste(wpath_data,"PsiTheta.rda",sep="/"))


}
#################### PREPARE CSV 

t <- 5
dz <- layer/1000  ## 50 millimiters layer depth ##transformed from millimiters to meters
dx <- xres(theta[[1]])

qpoints <- c(0.5,8,40) ##Discharge	at	outlet	and	downstream	end	of	slab1	and	2	
names(qpoints) <- c("outlet","slab1","slab2")


outputCsv <- data.frame(time=(1:time_duration)*3600,geotop_surface_water_m2=NA,
		geotop_soilwater_m2=NA,
		geotop_unsat_soilwater_m2=NA, 
		geotop_groundwt_soilwater_m2=NA,
		geotop_topsoilmoisture_m=NA
		)

outputCsv[,c(paste("geotop_qsub_m2persec",names(qpoints),sep="_"),paste("geotop_qsup_m2persec",names(qpoints),sep="_"))] <- NA

###outputCsv <- outputCsv[,names_v]
for (t in 1:time_duration) {
	
##	 dz is expressed in meters!!
	unsatWaterVolume <- SoilWaterStorage(theta[[t]],psi[[t]],layer=dz,comparison="<",psi_thres=0,fun=sum)
	satWaterVolume <- SoilWaterStorage(theta[[t]],psi[[t]],layer=dz,comparison=">=",psi_thres=0,fun=sum)
	WaterVolume <- SoilWaterStorage(theta[[t]],NULL,layer=dz,fun=sum) 
	TopSoilMoisture <- theta[[t]][[1]]
	
	qdischarge_sub <- LateralSubsurfaceDischarge(psi[[t]],wpath=wpath,output.discharge=TRUE)$discharge
	
	qdischarge_sup <- LateralSurfaceDischarge(hsup[[t]],wpath=wpath,output.discharge=TRUE)
	
	
	transect <- data.frame(x=c(1,100)*dx-dx/2,y=y)
	qpoints_xy <- data.frame(x=qpoints,y=y)
	qpoints_xy$names <- names(qpoints)
	qpoints_xy$icell <- cellFromXY(qdischarge_sup,qpoints_xy[,c("x","y")])
	
	

	outputCsv[t,"geotop_soilwater_m2"]<- sum(xyFrom2PointLine(r=WaterVolume,points=transect)$value001,na.rm=TRUE)*dx
	outputCsv[t,"geotop_groundwt_soilwater_m2"]<- sum(xyFrom2PointLine(r=satWaterVolume,points=transect)$value001,na.rm=TRUE)*dx
	outputCsv[t,"geotop_unsat_soilwater_m2"]<- sum(xyFrom2PointLine(r=unsatWaterVolume,points=transect)$value001,na.rm=TRUE)*dx
	outputCsv[t,"geotop_surface_water_m2"]<- sum(xyFrom2PointLine(r=hsup[[t]]/1000,points=transect)$value001,na.rm=TRUE)*dx
	
	outputCsv[t,"geotop_topsoilmoisture_m"]<- sum(xyFrom2PointLine(r=TopSoilMoisture,points=transect)$value001,na.rm=TRUE)*dx
	
	outputCsv[t,paste("geotop_qsub_m2persec",qpoints_xy$names,sep="_")] <- qdischarge_sub[qpoints_xy$icell]
	outputCsv[t,paste("geotop_qsup_m2persec",qpoints_xy$names,sep="_")] <- qdischarge_sup[qpoints_xy$icell]
	 
	
	
	

}






write.table(outputCsv,file=file.output.csv,quote=FALSE,sep=",",row.names=FALSE)

##### SOIL VERTICAL PROFILES 
unit_geotop <- 0.001 ### millimeters!
psi <- lapply(X=psi,FUN=function(x,u){x*u},u=unit_geotop)

yv <- (ymax(psi[[1]])+ymin(psi[[1]]))/2
xp <- c(32,41)
points <- data.frame(x=xp,y=yv,id=sprintf("x%02dm",xp))

times <- c(1,2,4,8,12)
names(psi) <- sprintf("time%02dhr",1:length(psi))
names(theta) <- names(psi)
names(times) <- sprintf("time%02dhr",times)

dz <- as.numeric(sapply(X=str_split(names(psi[["time01hr"]]),"_"),FUN=function(x){x[2]}))
dz <- dz*unit_geotop
z <- dz/2.0
for (i in 2:length(z)) {
	
	z[i] <- z[i-1]+(dz[i]+dz[i-1])/2
}
cos_angle <- unique(cos(SlopeMap/180*pi))[1]
z_v <- z/cos_angle
## PSI PROFILE 

profiles <- as.data.frame(lapply(X=psi[names(times)],FUN=SoilVariableProfile,points=points[,c("x","y")],names=as.character(points$id)))
profiles$depth <- z_v
profiles_m <- melt(profiles,id="depth")
profiles_m$time <- sapply(X=str_split(profiles_m$variable,"[.]"),FUN=function(x){x[1]})
profiles_m$point <- sapply(X=str_split(profiles_m$variable,"[.]"),FUN=function(x){x[2]})

gpsi <- qplot(value,depth,data=profiles_m,geom="path",group=time)+facet_grid(point ~ time,scale="fixed")+scale_y_reverse()+ylab("Depth [m]")+xlab("Soil Water Pressure Head [m]")

ggsave(file.proofilepsiplot,gpsi) 
## THETA PROFILE 

profiles_th <- as.data.frame(lapply(X=theta[names(times)],FUN=SoilVariableProfile,points=points[,c("x","y")],names=as.character(points$id)))
profiles_th$depth <- z
profiles_mth <- melt(profiles_th,id="depth")
profiles_mth$time <- sapply(X=str_split(profiles_mth$variable,"[.]"),FUN=function(x){x[1]})
profiles_mth$point <- sapply(X=str_split(profiles_mth$variable,"[.]"),FUN=function(x){x[2]})

gtheta <- qplot(value,depth,data=profiles_mth,geom="path",group=time)+facet_grid(point ~ time,scale="fixed")+scale_y_reverse()+ylab("Depth [m]")+xlab("Soil Water Content")

ggsave(file.proofilethetaplot,gtheta) 





### NetCDF for soil water potential pressure and soil water content 
##z*(1+unique(sin(SlopeMap*))^2)^0.5


times <- names(psi)
times <- str_replace(times,"time","")
times <- str_replace(times,"hr","")
times <- as.numeric(times)
times <- times*3600 ## FROM hours to seconds 


hsup <- lapply(X=hsup,FUN=brick)
hsup <- lapply(X=hsup,FUN=function(x,u){x*u},u=unit_geotop)


ncpsi <- buildnetCDF(psi,filename=file.ncpsi,time=times,depth=z_v,name="SoilWaterPressureHead",longname="Soil Water Pressure Head",units_Time="seconds")
nctheta <- buildnetCDF(theta,filename=file.nctheta,time=times,depth=z_v,name="SWC",longname="Soil Water Content",units="dimensionless",units_Time="second")
nchsup <- buildnetCDF(hsup,filename=file.nchsup,time=times,,name="WaterDepth",longname="Surface Water Depth",units="meter",units_Time="second")


nc_close(ncpsi)
nc_close(nctheta)
nc_close(nchsup)
