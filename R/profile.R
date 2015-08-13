NULL
#' Soil Variable weather Profile
#' 
#'  Vertical or Slope-Normal Integration/Aggregation of soil water content 
#' 
#' 
#' @param x soil variable \code{RasterBrick-class} object
#' @param points data frame with x and y coordinates of the pointe where to extract the soil profile
#' @param names names of the points 
#' @param ...  further arguments
#' 
# @param psi soil pressure head content \code{RasterBrick-class} object
#@param layers layer thickness. It is a scalar or a vector whose length is equal to the number of \code{theta} layers.
# @param psi_thres threshold value.  It is 0 for separating satureted and unsatureted zones, or \code{NA} if the two zones are not distinguished. 
# @param fun function aggregation 
# @param na.rm a logical value for \code{fun} indicating whether NA values should be stripped before the computation proceeds.
# @param comparison comparison operator for \code{psi} .See \code{\link{Comparison}}. Default is the first element of \code{c("<",">","<=",">=","==","!=")}
# @param indices see \code{\link{stackApply}}
# @param ... further aguments for \code{\link{stackApply}}
#' @export
#' @import raster
#' 
#' @examples 
#' 
#' data(PsiTheta)
#' 
#'  t <- 5
#'  map <- psi[[t]]
#'  yv <- (ymax(map)+ymin(map))/2
#'  points <- data.frame(x=c(32,41),y=yv,id=c("A","B"))
#' 
#' mapProfile <- SoilVariableProfile(map,points[,c("x","y")],names=as.character(points$id))
#' 
#' plot(mapProfile[,2],c(100:1),type="l")
#' lines(mapProfile[,1],c(100:1))
#' 
#' ####
#' 
#' library(stingr)
#' library(ggplot2)
#' library(reshape2)
#' 
#' times <- c(1,2,4,8)
#' names(psi) <- sprintf("time%02dhr",1:length(psi))
#' names(theta) <- names(psi)
#' names(times) <- sprintf("%02dhr",times)
#' 
#' dz <- as.numeric(sapply(X=str_split(names(psi[["01hr"]]),"_"),FUN=function(x){x[2]}))
#' z <- dz/2.0
#' for (i in 2:length(z)) {
#' 
#' 			z[i] <- z[i-1]+(dz[i]+dz[i-1])/2
#' }
#' 
#' 
#' profiles <- as.data.frame(lapply(X=psi[names(times)],FUN=SoilVariableProfile,points=points[,c("x","y")],names=as.character(points$id)))
#' profiles$depth <- z
#' profiles_m <- melt(profiles,id="depth")
#' profiles_m$time <- sapply(X=str_split(profiles_m$variable,"[.]"),FUN=function(x){x[1]})
#' profiles_m$point <- sapply(X=str_split(profiles_m$variable,"[.]"),FUN=function(x){x[2]})
#' 
#' 
#' g <- qplot(value,depth,data=profiles_m,geom="path",group=time)+facet_grid(point ~ time,scale="fixed")+scale_y_reverse()
#' 
#' ## THeta
#' 
#' 
#' 
#' 

SoilVariableProfile <- function(x,points,names=NULL...) {
	
	if (is.data.frame(points)) {                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
		
		
		points <- cellFromXY(x,points)
		                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
		
	}                                                                                                                                                                                                      
	
	
	out <- x[points]
	out <- t(out)
	if (!is.null(names)) {
		
		colnames(out) <- names
		
	}
	
	return(out)
	
}
                                             

