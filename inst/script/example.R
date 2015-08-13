 
 
 
 library(geotopsim)
 library(horizons)
 
 data(PsiTheta)
     
 t <- 5
 dz <- 50 ## 50 millimiters layer depth
 
 unsatWaterVolume <- SoilWaterStorage(theta[[t]],psi[[t]],layer=dz,comparison="<",psi_thres=0)
 satWaterVolume <- SoilWaterStorage(theta[[t]],psi[[t]],layer=dz,comparison=">=",psi_thres=0)
 
  
 yv <- (ymax(theta[[1]])+ymin(theta[[1]]))/2
 
 
 
 
 ######xy_B <- xyFrom2PointLine(r=dem,points,points=data.frame(x=c(0,100),y=yv),s),step=xres(theta[[1]])
xy_transect <- xyFrom2PointLine(r=unsatWaterVolume,points=data.frame(x=c(0,100),y=yv),step=xres(unsatWaterVolume))
  
  
  
  
     
     
     
     
     
     
     
     
