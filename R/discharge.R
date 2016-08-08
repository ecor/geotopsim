NULL
#'  Lateral Subsurface Discharge
#' 
#' @param psi  soil presssure head. It is  \code{RasterBrick-class} object.
#' @param hsub surfdace water depth. It is  \code{RasterBrick-class} object. 
#' @param elevation,slope,aspect elevation  maps of elevation,slope,aspect. Slope and aspect must be expressed in degrees.
#' @param neighbors Integer. Indicating how many neighboring cells to use to compute slope for any cell. Either 8 (queen case) or 4 (rook case). Only used for slope and aspect, see \code{\link{terrain}}.
#' @param unitelev ratio between unis of \code{elevation} and \code{psi}, e.g. if elevetion is exxxpressed in meters and psi in millimeters , it is 1000 (Default).                                                                                                               
#' @param soilmap raster map of soil class types or related keyword of GEOtop file \code{geotop.inpts} (\code{inpts.file} argument in \code{\link{get.geotop.inpts.keyword.value}}.
#' @param soiltypes list of soil class types or related keyword of GEOtop file \code{geotop.inpts} (\code{inpts.file} argument in \code{\link{get.geotop.inpts.keyword.value}}. See \code{metacat} arguments in \code{\link{SoilPropertyMap}}.
#' @param Dz layer thickness or or related GEOtop keyword used in the \code{soiltypes} file.
#' @param use_XY logical vaule. It it \code{TRUE}, the function returns the flux along the X (eastward) and Y (northward) directions. 
#' @param use.soilwater.function  list object passed to \code{\link{SoilPropertyMap}}. It is utilized to calculate unsaturated hydraulic conductivity used in Darcy-Buckingham Law (see also \code{\link{khy}}).
#' @param param.use.soilwater.function GEOtop keywords of the soil parameters utilized to calculate unsaturated hydraulic conductivity (see also \code{khy})
#' @param ... further arguments for \code{\link{SoilPropertyMap}}
##  ‘soilmap’ ‘soiltypes’ ‘Dz’ ‘param.use.soilwater.function’ ‘use_XY’
 

#' 
#' 
#' @seealso \code{\link{terrain}},\code{\link{}},\code{\link{SoilPropertyMap}}
#' @export 
#' 
#' 
#' 
#' 
#' @examples
#' wpath <- system.file("geotop_simulation",package="geotopsim")
#' 
#' #' ## Use soilwater function 
#' library(soilwater)
#' data(PsiTheta)
#' 
#' t <- 5
#' psit <- psi[[t]]
#' hsupt <- hsub[[t]]
#' 
#' q <- LateralSubsurfaceDischarge(psi=psit,wpath=wpath)
#' qxy <- LateralSubsurfaceDischarge(psi=psit,wpath=wpath,,output.discharge=TRUE)
#' 
#' 
#' 
#' 
#' 
#' qdischarge_sub <- LateralSubsurfaceDischarge(psi=psit,wpath=wpath,output.discharge=TRUE)
#' 
#' qdischarge_sup <- LateralSurfaceDischarge(hsup=hsupt,wpath=wpath,output.discharge=TRUE)
#' 
# # <<-
LateralSubsurfaceDischarge <- function(psi,
		elevation="DemFile",slope="SlopeMapFile",aspect=NULL,neighbors=8,soilmap="SoilMapFile",soiltypes="SoilParFile",Dz="SoilDz",unitelev=1000,
		use.soilwater.function=list(fun="khy", ## called function of "soilwater" package
				 args=list(ksat="LateralHydrConductivity",alpha="Alpha",n="N",type_swc = "VanGenuchten", type_khy = "Mualem")),
		param.use.soilwater.function=c("LateralHydrConductivity","Alpha","N"),use_XY=FALSE,
		unit_input=c("millimeter","centimeter","meter"),output.discharge=FALSE,...) {
	
	### 
	
	
	unit_input <- unit_input[1]
	
	if (unit_input=="millimeter") unit_input <- 0.001
	if (unit_input=="centimeter") unit_input <- 0.01
	if (unit_input=="meter") unit_input <- 1
	
	#### CRS: +proj=laea +lat_0=<LAT> +lon_0=<LON> +ellps=WGS84 +units=m +no_defs
	#### http://gis.stackexchange.com/questions/87152/how-to-reproject-wgs84-to-a-metric-coordinate-system-with-own-reference-point-in
	if (is.character(elevation)) elevation <- get.geotop.inpts.keyword.value(elevation[1],raster=TRUE,...)
#	if (is.charcacter(soilmap))  soilmap <- get.geotop.inpts.keyword.value(soilmap[1],raster=TRUE,...)
	
	if (is.character(slope)) slope <- get.geotop.inpts.keyword.value(slope[1],raster=TRUE,...)
#	if (is.charcacter(aspect)) aspect <- get.geotop.inpts.keyword.value(aspect[1],raster=TRUE,...)
	

	
	if (is.na(projection(elevation))) {
		
		CRSdefault <- "+ellps=WGS84 +proj=tmerc +lat_0=46.0 +lon_0=11 +k=1 +x_0=0 +y_0=0 +units=m +no_defs"
		message <- sprintf("Missing CRS/projection in elevation map: %s added as default!!",CRSdefault)
		warning(message)
		projection(elevation) <- CRS(CRSdefault)
		CRSdefault <- projection(elevation)
	} else {
		
		CRSdefault <- projection(elevation)
	}
	if (is.na(projection(psi))) {
		
		
	    message <- sprintf("Missing CRS/projection in psi map: %s added",CRSdefault)
		warning(message)
		projection(psi) <- CRS(CRSdefault)

	}
	
	if (identical(projection(psi),projection(elevation))) {
		
		CRSdefault <- projection(psi)
		
	} else {
		
		stop("Inconsteny on CRS/projection: psi and elevation")
		
	}
	
	
	
	
	if (is.null(slope)) {
		
		slope <- terrain(elevation,opt="slope",unit="degrees",neighbors=neighbors)
		
		
	} 
	if (is.na(projection(slope))) projection(slope) <- CRSdefault
	
	if (class(Dz)=="RasterBrick")         {
		
		if (nlayers(Dz)!=nlayers(psi)) {
			
			stop("Incontency betwwen Dz and psi sizes!")
		}
		
		DzMap <- psi*0+Dz
		
	} else if (is.character(Dz)) {
		
		DzMap <- SoilPropertyMap(x=Dz,map=soilmap,metacat=soiltypes,header="Header",names=names(psi),...) [[Dz[1]]]
		
	
		
		if (nlayers(DzMap)!=nlayers(psi)) {
			
			stop("Incontency betwwen DzMap and psi sizes!")
		}
	} else if (is.numeric(Dz)) {
		
		if (length(Dz)!=nlayers(psi)) {
				
			 stop("Incontency betwwen Dz and psi sizes!")
		}
		
		DzMap <- psi*0+Dz
		
	}	
	names(DzMap) <- names(psi)	
	projection(DzMap) <- CRSdefault
	
	zelev <- DzMap/2
	
	for (i in 2:nlayers(zelev)) {
		
		zelev[[i]] <- zelev[[i-1]]+(DzMap[[i-1]]+DzMap[[i]])/2
		
	}
	

	
	slope_cos <- cos(slope/pi*180)
	
	
	unitelev <- 1/unit_input
	
	zelev <- elevation*unitelev-zelev*slope_cos
	
	
	

	out <- list(flux=psi*NA,dir=psi*NA,x=psi*NA,y=psi*NA) ###,zelev=zelev,elevation=elevation,DzMap=DzMap)
	nl <- nlayers(psi)
	

	for (l in 1:nl) {
		
		
		
		message <- sprintf("Analyzing %s on %s",l,nl)
		message(message)
#		gradient <- terrain(psi[[l]],opt="slope",unit="tangent",neighbors=neighbors)+slope
		out$flux[[l]] <- 0+terrain(psi[[l]]+zelev[[l]],opt="slope",unit="tangent",neighbors=neighbors)
		out$dir[[l]] <- terrain(psi[[l]]+zelev[[l]],opt="aspect",unit="degrees",neighbors=neighbors)
		
	
		
		
	}
	
	out <- lapply(X=out,FUN=function(x,names_layer){
				
				
				out <- stack(x)
				out <- brick(out)
				names(out) <- names_layer
				return(out)
				
				
			},names_layer=names(psi))

	
## 	names(out) <- names(psi)


## ADD CONDUCTIVITY TO SLOPE 
# 
#      use.soilwater.function <- list()
# use.soilwater.function$fun <- "khy" ## called function of "soilwater" packege
# use.soilwater.function$args <- list(ksat="LateralHydrConductivity",alpha="Alpha",n="N", 
# 								  type_swc = "VanGenuchten", type_khy = "Mualem")
# 
# 
# xs <- c("LateralHydrConductivity","Alpha","N")
# 
# khmap <- SoilPropertyMap(xs,wpath=wpath,use.soilwater.function=use.soilwater.function,psi=psit)
	if (!is.null(use.soilwater.function)) {
		
		if (length(param.use.soilwater.function)==0) {
		
			param.use.soilwater.function <- unlist(use.soilwater.function$args)
		
		}
	
		khmap <- SoilPropertyMap(param.use.soilwater.function,use.soilwater.function=use.soilwater.function,psi=psi,...)
 
		out$flux <- khmap*out$flux*unit_input
		
		
		
	}
	

	if (output.discharge==TRUE) { 
		use_XY <- TRUE
		out$flux <- out$flux*DzMap*unit_input
		
	}	
	if (use_XY==TRUE)	{
		
		
		out$x <- out$flux*sin(out$dir/180*pi)
		out$y <- out$flux*cos(out$dir/180*pi)
		
		out <-out[c("x","y")]
		
		
	}

	if (output.discharge==TRUE)  {
		
		fun_aggr <- function (q,...) {
			
			if (length(q)>1) {
				
				sum(q,...)
				
			} else {
				
				q[1]
			}
		}
			
			
		
		out$x <- stackApply(x=out$x,indices=1,fun=fun_aggr,na.rm=TRUE)
		out$y <- stackApply(x=out$y,indices=1,fun=fun_aggr,na.rm=TRUE)			
		
		
		out$discharge <- (out$x^2+out$y^2)^0.5
		
		
	}
   
	message("Done")
	####message("THIS FUNCTION IS UNDER DEVELOPMENT")	
	
	
	
	
	
	
	return(out)
	
	
	
}











NULL
#'  
#' Lateral Surface Discharge - Runoff
#' 
#' @export
#' @rdname LateralSubsurfaceDischarge
#' 

LateralSurfaceDischarge <- function(hsup,elevation="DemFile",slope="SlopeMapFile",aspect=NULL,neighbors=8,landmap="LandCoverMapFile",flowres="SurFlowResLand",expflowres="SurFlowResExp",
		unit_input=c("millimeter","centimeter","meter"),output.discharge=FALSE,use_XY=FALSE,...) {
#		,soilmap="SoilMapFile",soiltypes="SoilParFile",Dz="SoilDz",unitelev=1000,
#		use.soilwater.function=list(fun="khy", ## called function of "soilwater" package
#				args=list(ksat="LateralHydrConductivity",alpha="Alpha",n="N",type_swc = "VanGenuchten", type_khy = "Mualem")),
#		param.use.soilwater.function=c("LateralHydrConductivity","Alpha","N"),use_XY=FALSE,...) {

	
	#### CRS: +proj=laea +lat_0=<LAT> +lon_0=<LON> +ellps=WGS84 +units=m +no_defs
	#### http://gis.stackexchange.com/questions/87152/how-to-reproject-wgs84-to-a-metric-coordinate-system-with-own-reference-point-in
	CRSdefault <- "+ellps=WGS84 +proj=tmerc +lat_0=46.0 +lon_0=11 +k=1 +x_0=0 +y_0=0 +units=m +no_defs"
	if (is.na(projection(hsup))) {
		
		
		message <- sprintf("Missing CRS/projection in hsup map: %s added",CRSdefault)
		warning(message)
		projection(hsup) <- CRS(CRSdefault)
		
	} else {
		
		CRSdefault <- projection(hsup)
		
	}
	
	CRSdefault <- projection(hsup)
	
	if (is.character(elevation)) elevation <- get.geotop.inpts.keyword.value(elevation[1],raster=TRUE,...)
#	if (is.charcacter(soilmap))  soilmap <- get.geotop.inpts.keyword.value(soilmap[1],raster=TRUE,...)
	
	if (is.character(slope)) slope <- get.geotop.inpts.keyword.value(slope[1],raster=TRUE,...)
#	if (is.charcacter(aspect)) aspect <- get.geotop.inpts.keyword.value(aspect[1],raster=TRUE,...)
	
	if (is.null(slope)) {
	
		if (is.na(projection(elevation))) {
		
			
			message <- sprintf("Missing CRS/projection in elevation map: %s added as default!!",CRSdefault)
			warning(message)
			projection(elevation) <- CRS(CRSdefault)
			
		} 
	
		if (identical(projection(hsup),projection(elevation))) {
		
			CRSdefault <- projection(hsup)
		
		} else {
		
			stop("Inconsteny on CRS/projection: hsup and elevation")
		
		}
	
		slope <- terrain(elevation,opt="slope",unit="degrees",neighbors=neighbors)
	
	}
	
	if (is.na(projection(slope))) projection(slope) <- CRSdefault
	
	if (is.character(flowres)) {
		
		flowres <- PropertyMap(x=flowres,map=landmap,...)[[flowres]]
		print(flowres)
		print(CRSdefault)
		if (is.na(projection(flowres))) projection(flowres) <- CRSdefault
		
		if (!identical(projection(hsup),CRSdefault)) {
			
			stop("Inconstency on CRS/projection: flow resistence coefficient and elevation")
			
		} 
	
	}
	
	if ((is.character(expflowres)) | (is.numeric(expflowres))) {
		
		expflowres <- PropertyMap(x=expflowres,map=landmap,...)[[expflowres]]
		
		if (is.null(expflowres)) expflowres <- landmap*0+0.67
		
		if (is.na(projection(flowres))) projection(flowres) <- CRSdefault
		
		if (!identical(projection(hsup),CRSdefault)) {
			
			stop("Inconstency on CRS/projection: flow resistence exponent and elevation")
			
		} 
		
	} else  {
		
		expflowres <- 0.67
		
		
	}	
		
	# runoff velocity 
	unit_input <- unit_input[1]
	
	if (unit_input=="millimeter") unit_input <- 0.001
	if (unit_input=="centimeter") unit_input <- 0.01
	if (unit_input=="meter") unit_input <- 1
	
	out <- flowres*(hsup*unit_input)^expflowres*tan(slope/180*pi)^0.5
	
	
	if (output.discharge==TRUE)     out <- out*(hsup*unit_input)
	
	
	if (use_XY==TRUE) {
		
		if (is.character(aspect)) {
			
			aspect <- get.geotop.inpts.keyword.value(aspect[1],raster=TRUE,...)
			
			
		} else { 
			
			aspect <- terrain(elevation,opt="aspect",unit="degrees",neighbors=neighbors)
			
		}
		
		if (is.na(projection(aspect))) projection(aspect) <- CRSdefault
		
		flux <- out
		out <- list()
			
			out$x <- flux*sin(aspect/180*pi)
			out$y <- flux*cos(aspect/180*pi)
			
			out <-out[c("x","y")]
			
			
		}
		
		return(out)
		
	}
	
	
	








