NULL
#' Create a dat file 
#' 
#' @param psi ... TO DO 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' @export
#' 
#' 
#' 

builddat <- function(psi,
		##filename="/home/ecor/psi.dat", In particular
		###CRSdefault="+ellps=WGS84 +proj=tmerc +lat_0=46.0 +lon_0=11 +k=1 +x_0=0 +y_0=0 +units=m +no_defs",
		time=1:length(psi),depth=1:nlayers(psi[[1]]),
		overwrite=TRUE,
		time.inv=time[1:2],
		elevation,slope,aspect,unit_z=0.001,...) {
	
	##print(time)
	print(time.inv)
	nl <- nlayers(psi[[1]])
	
	ntime <- length(time)
	if (length(depth)!=nl) {
		
		stop("Inconsistent depth length")
		
	}
	
	if (length(time)!=length(psi)) {
		
		stop("Inconsistent time length")
		
	}
	names(depth) <- names(psi[[1]])
	
	names(time) <- names(psi)
	
	itime <- which(time %in% time.inv)
	print(itime)
	psi <- psi[itime]
	time <- time[itime]
	print(time)
	print(psi)
	out <- lapply(X=psi,FUN=as.data.frame,xy=TRUE)
	##	melt(out[[1]],id=c("x","y"))
	out <- lapply(X=out,FUN=melt,id=c("x","y"))
	##out <- melt(out)
	
	for (i in 1:length(time)) {
		
		out[[i]]$time <- time[i]
		
	}
	str(out)
	out <- do.call(what=rbind,args=out)
	
	
	##out <- melt(out,id=c("x","y"))
	out$depth <- depth[as.character(out$variable)]

	#out$time <- time[as.character(out$L1)]
	
	
	 out <- out[,names(out)!="variable"]
	 
	 elevation <- as.data.frame(elevation,xy=TRUE)
	 slope <- as.data.frame(slope,xy=TRUE)
	 
	 names(elevation)[names(elevation)=="layer"] <- "elevation"
	 names(slope)[names(slope)=="layer"] <- "slope"

	 out <- merge(out,elevation)
	 out <- merge(out,slope)
	
	 out$z <- out$elevation-out$depth*unit_z/(cos(out$slope/180*pi))
	 out <- out[!is.na(out$value),]
##	out <- lapply(X=psi,FUN=function(x,depth){ x$z_depth <- depth },depth=depth)	
	
		return(out)
	     
	
	
	
}
