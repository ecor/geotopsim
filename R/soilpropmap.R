NULL
#'  Creation of a Physical Variable Map from a Catagory Map
#' 
#' This functions creates physicalvariable maps from catagorical information, e.g. a map of hydraulic conductivity from the map of Soil Categories. 
#' 
#' @param x variable or variables to map
#' @param map catagory input map, i.e. soiltype maps or related GEOtop keyword. 
#' @param metacat list of dataframe containingcatagory definitions, e. g. the list of soil properties for each class or related GEOtop keyword. 
#' @param header header sting using in \code{geotop.inpts} file. Default is \code{"Header"}
#' @param names names for the layer of the \code{RasterBrick-class} object returned by the function.
#' @param use.soilwater.function  list. It it is not \code{NULL}, it contanis a function of \code{soilwater} package which calculated using the variables indicated form \code{x} as input. See examples and details. Default is \code{NULL} nd then ingored.                                                                                                                                  li
#' @param psi \code{RasterBrick-class} object containg \code{psi} (pressure head values) use for \code{\link{soilwater}} function in case \code{use.soilwater.function} is not \code{NULL}.
#' @param ... further arguments for \code{\link{get.geotop.inpts.keyword.value}} such as \code{wpath} or \code{inpts.file}                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       s
#' 
#' @export
#' @import geotopbricks
#' @examples
#' 
#' 
#' wpath <- system.file("geotop_simulation",package="geotopsim")
#' x <- "Alpha"
#' b <- SoilPropertyMap(x,wpath=wpath)
#' 
#' xp <- c("LateralHydrConductivity","N","Alpha")
#' bp <- SoilPropertyMap(xp,wpath=wpath)
#' 
#' ## Use soilwater function 
#' library(soilwater)
#' data(PsiTheta)
#' 
#' t <- 5
#' psit <- psi[[t]]
#' 
#' 
#' 
#' #khy(psi = 0.5, v = 0.5, ksat = 0.01, alpha = 1, n = 1.5, m = 1 -
#' #  1/n, theta_sat = 0.4, theta_res = 0.05, psi_s = -1/alpha, lambda = m *
#' #  n, b = NA, type_swc = "VanGenuchten", type_khy = c("Mualem",
#' #  "BrooksAndCorey"), ...)
#' 
#' 
#' use.soilwater.function <- list()
#' use.soilwater.function$fun <- "khy" ## called function of "soilwater" packege
#' use.soilwater.function$args <- list(ksat="LateralHydrConductivity",alpha="Alpha",n="N", 
#' 								  type_swc = "VanGenuchten", type_khy = "Mualem")
#' 
#' 
#' xs <- c("LateralHydrConductivity","Alpha","N")
#' 
#' khmap <- SoilPropertyMap(xs,wpath=wpath,use.soilwater.function=use.soilwater.function,psi=psit)
#' 
#' 
#' 
#' ### BELOW a list of some keywords used for soil property variables
#' #HeaderSoilDz	=	"Dz"
#' #HeaderLateralHydrConductivity	=	"Kh"
#' #HeaderNormalHydrConductivity	=	"Kv"
#' #HeaderThetaRes	=	"vwc_r"
#' #HeaderWiltingPoint	=	"vwc_w"
#' #HeaderFieldCapacity	=	"vwc_fc"
#' #HeaderThetaSat	=	"vwc_s"
#' #HeaderAlpha	=	"alpha"
#' #HeaderN	=	"n"
#' #HeaderSpecificStorativity	=	"stor"
#' #HeaderKthSoilSolids	=	"Kth"
#' #HeaderCthSoilSolids	=	"Cth"
#' #HeaderSoilInitPres = "InitPsi"

SoilPropertyMap <- function(x,map="SoilMapFile",metacat="SoilParFile",header="Header",names=NULL,use.soilwater.function=NULL,psi=NULL,...) {
	
	
	if (is.character(map)) {
		
		
		map <- geotopbricks::get.geotop.inpts.keyword.value(map[1],raster=TRUE,...)
		geotop <- TRUE
		
		
	} else {
		
		geotop <- FALSE
	}
	level <- 1:max(unique(map)) ## map must be contained values ranged from 1 to nsoilclass
	
	if (is.character(metacat)) {
		
		metacat <- geotopbricks::get.geotop.inpts.keyword.value(metacat,data.frame=TRUE,level=level,...)
		geotop <- TRUE
		
	} else {
		
		geotop <- FALSE
	}
	
	if (geotop==TRUE) {
		
		
		xheader <- geotopbricks::get.geotop.inpts.keyword.value(paste("Header",x,sep=""),add_wpath=FALSE,...)
		
		
		xindex <- which(sapply(X=xheader,FUN=is.null)==FALSE)
		xheader <- xheader[xindex]
		x <- x[xindex]
		
		xindex <- which(sapply(X=xheader,FUN=is.na)==FALSE)
		xheader <- xheader[xindex]
		x <- x[xindex]
		
	} else {
		
		xheader <- x
	}
	
	if (is.data.frame(metacat)) metacat <- list(metacat)
	
	nlayer <- nrow(metacat[[1]])
	
	## MUltiple parameters:
	
	
	
	
	onemap <- function(xheader,map=map,metacat=metacat,names=NULL,nlayer=nlayer) {
		##		print(nlayer)
		##		print(map)
				out <- brick(lapply(X=as.list(array(1:nlayer,nlayer)),FUN=function(x,map,metacat,xheader){
				
					values <- unlist(lapply(X=metacat,FUN=function(s,l,xheader) { s[l,xheader]},l=x,xheader=xheader))
				
				
					out <- map
					out[!is.na(map)] <- values[map[!is.na(map)]]
					return(out)
					
				
				
			},map=map,metacat=metacat,xheader=xheader))

			if (!is.null(names)) names(out) <- names
			return(out)
		}
	
  

		
   
	out <- lapply(X=xheader,FUN=onemap,map=map,metacat=metacat,names=names,nlayer=nlayer)
	
	names(out) <- x 
	
	if (!is.null(use.soilwater.function)) {
	##	args <- use.soilwater.function$args
		########## TO DO!!!!!! 
		np <- intersect(x,unlist(use.soilwater.function$props))
######################### TODO HERE 
		###use.soilwater.function$props <- use.soilwater.function$props[np]
	#	use.soilwater.function$props <- 
		# np<- use.soilwater.function$args[
	###	str(out)
		props <- unlist(use.soilwater.function$args[use.soilwater.function$args %in% x])
		args <- out[props]
		
		names(args) <- names(props)
		
		use.soilwater.function$args[names(args)] <- args
		
		if (!is.null(psi)) use.soilwater.function$args$psi <- psi 
		
		
		
		out <- do.call(what=use.soilwater.function$fun,args=use.soilwater.function$args)
		
		
	}
	
	
	return(out)
	
	
}

NULL
#'
#' @export 
#' @rdname SoilPropertyMap
#' 

PropertyMap <- function(x,map="LandUseMapFile",...) {
	
	out <- NULL
	if (is.character(map)) {
		
		
		map <- geotopbricks::get.geotop.inpts.keyword.value(map[1],raster=TRUE,...)
		geotop <- TRUE
		
		
	} else {
		
		geotop <- FALSE
	}
	
	out <- lapply(X=x,FUN=function(x,map,...) {
		if (is.character(x)) {
					
		 	x <- geotopbricks::get.geotop.inpts.keyword.value(x,numeric=TRUE,...)
		}
		if (length(x)==1) {
			
			out <- map*0+x
			
		} else {
			
			mapv <- sort(unique(as.vector(map)))
			
			if (!identical(x,mapv)) {
				
				message <- "PropertyMap: inconsistency between x and map!"
				str(x)
				str(mapv)
				stop(message)
				
			}
			
			out <- map*0
			out[!is.na(map)] <- values[map[!is.na(map)]]
			
		}
		
		
		
		return(out)
		
	},map=map,...)
	
	names(out) <- x
	
	
	return(out)

}