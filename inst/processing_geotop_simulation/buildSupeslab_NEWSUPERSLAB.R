#! .... 

rm(list=ls())


library(soilwater)
library(geotopbricks)
library(horizons)

wpath_pkg <- "/home/ecor/Dropbox/R-packages/geotopsim/inst"
wpath_data <- "/home/ecor/Dropbox/R-packages/geotopsim/data"
wpath_inputdata <- paste(wpath_pkg,"processing_geotop_simulation/inputdata",sep="/")
wpath <- "/home/ecor/Dropbox/R-packages/geotopsim_simulations/geotop_simulation_NEWSUPERSLAB" ######		paste(wpath_pkg,"geotop_simulation",sep="/")

soilprop_tmp <- read.table(paste(wpath_inputdata,"soilprop_tmp_NEWSUPERSLAB.txt",sep="/"),sep=",",header=TRUE) 
#######

inpts.file <- "geotop.inpts"
paramPrefix <- "Header"

WiltingPoint <- get.geotop.inpts.keyword.value(paste(paramPrefix,"WiltingPoint",sep=""),wpath=wpath,inpts.file=inpts.file)
FieldCapacity <- get.geotop.inpts.keyword.value(paste(paramPrefix,"FieldCapacity",sep=""),wpath=wpath,inpts.file=inpts.file)
ThetaSat <- get.geotop.inpts.keyword.value(paste(paramPrefix,"ThetaSat",sep=""),wpath=wpath,inpts.file=inpts.file)
ThetaRes <- get.geotop.inpts.keyword.value(paste(paramPrefix,"ThetaRes",sep=""),wpath=wpath,inpts.file=inpts.file)
VG_Alpha <- get.geotop.inpts.keyword.value(paste(paramPrefix,"Alpha",sep=""),wpath=wpath,inpts.file=inpts.file)
VG_N <- get.geotop.inpts.keyword.value(paste(paramPrefix,"N",sep=""),wpath=wpath,inpts.file=inpts.file)
Dz <- get.geotop.inpts.keyword.value(paste(paramPrefix,"SoilDz",sep=""),wpath=wpath,inpts.file=inpts.file)
SoilInitPres <- get.geotop.inpts.keyword.value(paste(paramPrefix,"SoilInitPres",sep=""),wpath=wpath,inpts.file=inpts.file)




### CORRECT SOIL WATER
alpha <-  soilprop_tmp[,VG_Alpha]
n <- soilprop_tmp[,VG_N]
theta_sat <- soilprop_tmp[,ThetaSat]
theta_res <- soilprop_tmp[,ThetaRes]
###	=alpha,n=n,theta_sat=theta_sat,theta_res=theta_res)
waterdensity <- 1000 ## kg/m^3
gravity <- 9.81 ## m/s^2


psi_WP <- -1500*1000 ## Pa
psi_FC <- -33*1000  ##  Pa

psi_WP <- psi_WP/(waterdensity*gravity)*1000 ## converted to water millimiters according to GEOtop
psi_FC <- psi_FC/(waterdensity*gravity)*1000 ## converted to water millimiters according to GEOtop

soilprop_tmp[,FieldCapacity] <- swc(psi=psi_FC,alpha=alpha,n=n,theta_sat=theta_sat,theta_res=theta_res,type_swc="VanGenuchten")
soilprop_tmp[,WiltingPoint] <- swc(psi=psi_WP,alpha=alpha,n=n,theta_sat=theta_sat,theta_res=theta_res,type_swc="VanGenuchten")
###

write.table(soilprop_tmp,paste(wpath_inputdata,"soilprop_hillslope.txt",sep="/"),sep=",",quote=FALSE,row.names=FALSE) 

####
#
# BUILD THE HILLSLOPE SOIL PROFILES
#
####

slope <- 0.1
cos_slope <- 1/(1+slope^2)^0.5

xlen <- 100
zdepth <- 5

xres <- 1
zres <- 0.05 

nrows <- round(xlen/xres)
ncols <- round(zdepth/zres)


hillslope_profile <- raster(xmx=xlen,xmn=0,ymx=zdepth,ymn=0,nrows=nrows,ncols=ncols)


hillslope_profile[,] <- 0



slabs_files <- sprintf(paste(wpath_inputdata, 'polygon_slab%d.csv',sep="/"),1:2) 

slabs  <- lapply(X=slabs_files,FUN=read.table,sep=",",header=FALSE)
names(slabs) <- sprintf("slabs%s",1:length(slabs))
slabs  <- lapply(X=slabs,FUN=function(x){
			
			zdepth <- 5.0
			names(x) <- c("V","X","Y")
			x$Y <- zdepth-x$Y
			
			l <- nrow(x)
			
			 x <-x[c(1:l,1),]
			 Polygon(as.matrix(x[,c("X","Y")]))
			 
			
		})

for (i in 1:length(slabs)) {
	
	slabs[[i]] <- Polygons(list(slabs[[i]]),i)
	
}







spslabs <- SpatialPolygons(slabs)

#slabs names(	
#	cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
#cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
#pols <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), Polygons(list(Polygon(cds2)), 2)))
#cellFromPolygon(r, pols)	
#	
	#ciaovvvffdddddddddd
ic<- cellFromPolygon(hillslope_profile,spslabs)
names(ic) <- names(slabs)

for (i in 1:length(ic)) {
	
	hillslope_profile[ic[[i]]] <- i
	
	
}

#stop("WWW")

###############
#
# Detecting Soil Classes per each soil column
#
#

soilcols <- array(0,ncol(hillslope_profile))
soilp <- nrow(soilprop_tmp)
for (i in 1:length(soilcols)) {
	
		soilp[i] <- paste(as.matrix(hillslope_profile)[,i],collapse=",")
	
	
	
}

usoilp <- unique(soilp)
iusoilp <- index(usoilp)
names(iusoilp) <- usoilp

soilmap_hillslope <- iusoilp[soilp]
###
### CReate Soil Classes 
###
soilprefix <- get.geotop.inpts.keyword.value("SoilParFile",wpath=wpath,inpts.file=inpts.file,add_wpath=TRUE)			
soilfile <- paste(soilprefix,"%04d.txt",sep="")
row.names(soilprop_tmp)  <- as.character(soilprop_tmp$ID)


dz_layer <- zres*cos_slope*1000
watertabledepth <- zdepth*cos_slope*1000

for (it in names(iusoilp)) {
	
	is <- iusoilp[it]
	soilprofile <- (str_split(it,",")[[1]])
	
	
	
	
	soilclass <- soilprop_tmp[soilprofile,]
	
	soilclass[,Dz] <- dz_layer
	soilclass[,SoilInitPres] <- ((1:nrow(soilclass))*dz_layer-dz_layer/2-watertabledepth)*cos_slope
	
	
	namesSoilClass <- c(Dz,names(soilclass)[!(names(soilclass) %in% Dz)])
	
	soilclass <- soilclass[,namesSoilClass]
	
	write.table(soilclass,sprintf(soilfile,is),sep=",",quote=FALSE,row.names=FALSE) 
	
	
	
}



#######
#
# CREATES HILLSLOPE MAPS
#
#####

soilmapasc <- paste(get.geotop.inpts.keyword.value("SoilMapFile",wpath=wpath,inpts.file=inpts.file,add_wpath=TRUE),".asc",sep="")
elevmapasc <- paste(get.geotop.inpts.keyword.value("DemFile",wpath=wpath,inpts.file=inpts.file,add_wpath=TRUE),".asc",sep="")
landmapasc <- paste(get.geotop.inpts.keyword.value("LandCoverMapFile",wpath=wpath,inpts.file=inpts.file,add_wpath=TRUE),".asc",sep="")
netmapasc <-  paste(get.geotop.inpts.keyword.value("RiverNetwork",wpath=wpath,inpts.file=inpts.file,add_wpath=TRUE),".asc",sep="")


elevmap_hillslope <- ((1:length(soilmap_hillslope))*xres-xres/2)*slope+zdepth
netmap_hillslope <- array(0,length(soilmap_hillslope))
netmap_hillslope[1] <- 1

nrow <- 10

soilmapvalue <- matrix(rep(c(NA,NA,as.numeric(soilmap_hillslope),NA,NA),each=nrow),nrow=nrow)
elevmapvalue <- matrix(rep(c(NA,NA,elevmap_hillslope,NA,NA),each=nrow),nrow=nrow)
netmapvalue <- matrix(rep(c(NA,NA,netmap_hillslope,NA,NA),each=nrow),nrow=nrow)

border <- c(1,2,nrow(soilmapvalue)-1,nrow(soilmapvalue))

soilmapvalue[border,] <- NA
elevmapvalue[border,] <- NA
netmapvalue[border,] <- NA

xstart <- 0
xmin <- xstart-2*xres
xmax <- xmin+ncol(elevmapvalue)*xres

ymin <- 0
ymax <- ymin+nrow(elevmapvalue)*xres

soilmap <- raster(soilmapvalue,xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax)
elevmap <- raster(elevmapvalue,xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax)
landmap <- soilmap*0+1
netmap <- raster(netmapvalue,xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax)

writeRasterxGEOtop(x=soilmap,file=soilmapasc)
writeRasterxGEOtop(x=elevmap,file=elevmapasc)
writeRasterxGEOtop(x=landmap,file=landmapasc)
writeRasterxGEOtop(x=netmap,file=netmapasc)

#### Precipitation data 

rainfall_intensity <- 50 ### mm/hr
rainfall_intensity_mmps <- rainfall_intensity/3600 ### mm/hr
rainfall_duration <- 3*3600 ## seconds



duration <- 12*3600  ## hr 
dt <- 15*60 # 15 minutes, values expressed in seconds
time_counter <- seq(from=0,to=duration,by=dt)

rainfall_depth <- array(0,length(time_counter))
rainfall_intensity_ts <- rainfall_depth*0
rainfall_depth[time_counter<=rainfall_duration] <- rainfall_intensity_mmps*dt
rainfall_intensity_ts[time_counter<=rainfall_duration] <- rainfall_intensity


start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,inpts.file=inpts.file,tz="GMT")

HeaderDateDDMMYYYYhhmmMeteo <- get.geotop.inpts.keyword.value("HeaderDateDDMMYYYYhhmmMeteo",wpath=wpath,inpts.file=inpts.file)
HeaderIPrec <- get.geotop.inpts.keyword.value("HeaderIPrec",wpath=wpath,inpts.file=inpts.file)
MeteoPrefix <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,inpts.file=inpts.file,add_wpath=TRUE)
meteo <- data.frame( 
        HeaderDateDDMMYYYYhhmmMeteo = start+time_counter,
		TimeCounter=time_counter,
		HeaderIPrec=rainfall_intensity_ts)

names(meteo)[names(meteo)=="HeaderDateDDMMYYYYhhmmMeteo"] <- HeaderDateDDMMYYYYhhmmMeteo
names(meteo)[names(meteo)=="HeaderIPrec"] <- HeaderIPrec


index_m <- meteo[,HeaderDateDDMMYYYYhhmmMeteo]

meteo <- meteo[,names(meteo)!=HeaderDateDDMMYYYYhhmmMeteo]

meteo <- as.zoo(meteo)
index(meteo) <- index_m
create.geotop.meteo.files(x=meteo,file_prefix=MeteoPrefix,date_field=HeaderDateDDMMYYYYhhmmMeteo) 
#HeaderIPrec = "Prec"
#


		













