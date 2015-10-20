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
wpath <- '/home/ecor/Dropbox/R-packages/geotopsim_simulations/Vc_SC/Vc_SC1_002' 
#wpath_outputdata <- paste(wpath_pkg,"processing_geotop_simulation/output",sep="/")
wpath_outputdata <- "/home/ecor/Dropbox/Public/geotop_intercomparison/Vc_Sc1_002"



if (!file.exists(wpath_outputdata)) dir.create(wpath_outputdata,recursive=TRUE)

#"/home/ecor/Dropbox/R-packages/geotopsim_simulations/geotop_simulation" ######		paste(wpath_pkg,"geotop_simulation",sep="/")
file.output.csv <- paste(wpath_outputdata,"geotop.Vc_Sc1.output.csv",sep="/")
##file.proofilethetaplot <- paste(wpath_pkg,"processing_geotop_simulation/output/theta.png",sep="/")
file.proofilepsiplot <- paste(wpath_outputdata,"psi_profile.png",sep="/")
file.proofilethetaplot <- paste(wpath_outputdata,"theta_profile.png",sep="/")
file.psiprofile <- paste(wpath_outputdata,"psi_profile.csv",sep="/")
file.thetaprofile <- paste(wpath_outputdata,"ptheta_profile.csv",sep="/")
file.volumepng <- paste(wpath_outputdata,"volume.png",sep="/")
##

##
file.nctheta <-  paste(wpath_outputdata,"geotop.theta.Vc_Sc1.nc",sep="/")
file.ncpsi <- paste(wpath_outputdata,"geotop.soilwaterpressure.Vc_Sc1.nc",sep="/")
file.nchsup <- paste(wpath_outputdata,"geotop.surfacewater.Vc_Sc1.nc",sep="/")
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

#
#Dear All,
#please provide me with pressure and relative saturation profiles/cross-section for the v-catchment case scenarios 3 and 4 for the following locations and times:
#		
#		Locations: at the hillslope outlet at x=55m, y=0m; 
#in the centre of the channel at x=55m y=50m;  
#along a line at the surface of the hillslope in the centre of the channel at x=55m from y=0 to 100m
#Times: 10, 20, 40, 80, 120 hours
#
#I received the data (see email below) for the superslab case for the following models: Cast3M, Geoptop, PF. Would be great to have the results of the other models soon to update the manuscript.
#
#Thank you,
#Stefan
##### SOIL VERTICAL PROFILES 


unit_geotop <- 0.001 ### millimeters!
psi <- lapply(X=psi,FUN=function(x,u){x*u},u=unit_geotop)

xorigin <- 10
yorigin <- 10 

yp <- c(0.5,50,99.6)+yorigin
xp <- 55+xorigin

points <- data.frame(x=xp,y=yp,id=sprintf("x%02dm_y%03dm",round(xp-xorigin),round(yp-yorigin)))

times <- c(10,20,40,80,120)*3600

names(psi) <- sprintf("time%05ds",outputCsv$time)
names(theta) <- names(psi)
names(times) <- sprintf("time%05ds",times)

dz <- as.numeric(sapply(X=str_split(names(psi[[1]]),"_"),FUN=function(x){x[2]}))
dz <- dz*unit_geotop
z <- dz/2.0
for (i in 2:length(z)) {
	
	z[i] <- z[i-1]+(dz[i]+dz[i-1])/2
}
##
#z_v <- z ##/cos_angle
z_v <- z 

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





write.table(profiles,file.psiprofile,sep=",",row.names=FALSE,quote=FALSE)
write.table(profiles_th,file.thetaprofile,sep=",",row.names=FALSE,quote=FALSE)

## VOLUME PLOT 


outputMelt <- melt(outputCsv,id="time")
variables <- c("geotop_soilwater","geotop_groundwt_soilwater","geotop_unsat_soilwater","geotop_surface_water")
outputMeltv <- outputMelt[outputMelt$variable %in% variables,]
ggv <- ggplot()+geom_line(mapping=aes(x=time,y=value,colour=variable),data=outputMeltv)
ggv <- ggv+xlab("time [s]")+ylab("volume [m3]")+ggtitle("Volume")





###
ggsave(file.volumepng,ggv) 


