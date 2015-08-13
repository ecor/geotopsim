 
 
 
 library(geotopsim)
 library(horizons)
 
 data(PsiTheta)
  
 wpath_output <- "/Users/ecor/Dropbox/R-packages/geotopsim/inst/script/output"
 
 times <- 1:13
 dz <- 50 ## 50 millimiters layer depth
 
 unsatWaterVolume <- list()
 satWaterVolume <- list()
 
 for (t in times) {
 	
	 unsatWaterVolume[[t]] <- SoilWaterStorage(theta[[t]],psi[[t]],layer=dz,comparison="<",psi_thres=0)
 	 satWaterVolume[[t]] <- SoilWaterStorage(theta[[t]],psi[[t]],layer=dz,comparison=">=",psi_thres=0)
 
 }

 
 names(unsatWaterVolume) <- sprintf("geotop_unsatSW_mm_time_%04d_hr",times)
 names(satWaterVolume) <- sprintf("geotop_satSW_mm_time_%04d_hr",times)
 unsatWaterVolume <- stack(unsatWaterVolume)
 satWaterVolume <- stack(satWaterVolume)
 

 
 yv <- (ymax(theta[[1]])+ymin(theta[[1]]))/2
 xyp <- data.frame(x=(xres(unsatWaterVolume)*(c(1,100)-0.5)),y=yv)
 
 
 
 
 ######xy_B <- xyFrom2PointLine(r=dem,points,points=data.frame(x=c(0,100),y=yv),s),step=xres(theta[[1]])
xy_transect_unsatWV <- xyFrom2PointLine(r=unsatWaterVolume,points=xyp,step=xres(unsatWaterVolume))
xy_transect_satWV <- xyFrom2PointLine(r=satWaterVolume,points=xyp,step=xres(satWaterVolume))  

##xy_transect_satWVfile <- paste(wpath_output,"satVW_transect.csv",sep="/")
 

names(xy_transect_unsatWV)[-c(1,2,3)] <- names(unsatWaterVolume)
names(xy_transect_satWV)[-c(1,2,3)] <- names(satWaterVolume)


write.table(x=xy_transect_unsatWV,file=xy_transect_unsatWVfile,sep=",",col.names=TRUE,row.names=TRUE,quote=FALSE)
write.table(x=xy_transect_satWV,file=xy_transect_satWVfile,sep=",",col.names=TRUE,row.names=TRUE,quote=FALSE)

  
## TO GO ON apply(X=xy_transect_unsatWV,FUN=sum,MARGIN=2)/1000
 

## PROFILES

xvprf <- data.frame(x=c(32,41),y=yv)







     
     
     
     
     
     
