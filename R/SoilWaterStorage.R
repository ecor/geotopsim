NULL
#' Soil Moisture Mapping
#' 
#'  Vertical or Slope-Normal Integration/Aggregation of soil water content 
#' 
#' 
#' @param theta soil water content \code{RasterBrick-class} object
#' @param psi soil pressure head content \code{RasterBrick-class} object
#' @param layers layer thickness. It is a scalar or a vector whose length is equal to the number of \code{theta} layers.
#' @param psi_thres threshold value.  It is 0 for separating satureted and unsatureted zones, or \code{NA} if the two zones are not distinguished. 
#' @param fun function aggregation 
#' @param na.rm a logical value for \code{fun} indicating whether NA values should be stripped before the computation proceeds.
#' @param comparison comparison operator for \code{psi} .See \code{\link{Comparison}}. Default is the first element of \code{c("<",">","<=",">=","==","!=")}
#' @param indices see \code{\link{stackApply}}
#' @param bedrock.depth map of deprock depth. If it is not \code{NULL}, \code{theta} cells under bedrock are not considered.  
#' @param ... further aguments for \code{\link{stackApply}}
#' @export
#' @import raster
#' 
#' @examples 
#' 
#' data(PsiTheta)
#' 
#'  t <- 5
#'  dz <- 50 ## 50 millimiters layer depth
#'  unsatWaterVolume <- SoilWaterStorage(theta[[t]],psi[[t]],layer=dz,comparison="<",psi_thres=0)
#'  satWaterVolume <- SoilWaterStorage(theta[[t]],psi[[t]],layer=dz,comparison=">=",psi_thres=0)
#' 
#' 
#' 
#' 
#' 
#' 
#' 
SoilWaterStorage <- function(theta,psi,layers=1,fun=sum,na.rm=TRUE,psi_thres=0,comparison=c("<",">","<=",">=","==","!="),indices=1,bedrock.depth=NULL,...) {
	
	if (!is.null(psi)) {
		
		if(nlayers(psi)!=nlayers(theta)) {
			
			stop("Mismatching between theta and psi layers!")
			
		}
		
		
	}
	
	mask <- theta[[1]]*0+1
	print("BM:")
	print(bedrock.depth)
	if (!is.null(bedrock.depth)) bedrock.depth <- mask*bedrock.depth
	print(bedrock.depth)
	
	print(length(layers))
	print(layers)
	print(nlayers(theta))
	if ((length(layers)>1) & (length(layers)!=nlayers(theta))) {
		
			stop("Mismatching lenghth between theta layers and layer argument!")
		
	} else if (length(layers)<1) {
		
		
		 warning("Layer not set currectly. Assingned equal to 1!!!")	
		 layer <- array(1,nlayers(theta))

	 } else if (length(layers)==1) {
	
		 layers <- rep(layers[1],nlayers(theta))
	}
	print(layers)
	vtheta <- theta*layers
	theta <- vtheta
#	s <- stack(theta,psi)
	
	if (!is.null(psi)) {
		
		print(comparison)
		psilog <- do.call(what=comparison[1],args=list(psi,psi_thres))
		#print(psilog)
		##ipsilogfalse <- which(raster::as.vector(psilog==0))
		psilog[psilog==0] <- NA
		theta <- theta*psilog
		na.rm  <- TRUE
		
		
	}
	
	
	if (!is.null(bedrock.depth)) {
		print("bedrock map:")
		print(bedrock.depth)
		issoil <- theta*0+1
		
		
		depth <- layers 
		for ( i in 2:length(depth)) {
			
			depth[i] <- depth[i-1]+layers[i]
			issoil[[i]] <- bedrock.depth>=depth[i]
			
			
			
		}
		
		print(issoil)
		issoil <- issoil*mask
		
		
		
		theta[issoil==0] <- NA
		theta <- theta*issoil 
		
		
		
		
		
		
		
		
	}
	
	
	out <- stackApply(x=theta,fun=fun,na.rm=na.rm,indices=indices,...)
	
	out <- out*mask
	
	return(out)
	
}
