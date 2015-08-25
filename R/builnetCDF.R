# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL
#' Collect Psi or Theta in a netCDF file 
#' 
#' @param psi  variable, a list of \code{RasterBrick-class} object
#' @param filename string containing full name of the netCDF file
#' @param CRSDefaul   string with CRS default (used if it is missing in \code{psi)}
#' @param overwrite logical value. Default is \code{TRUE}. 
#' @param dimTime,dimDepth  string containing the name for time or (vartical) depth netCDF dimension respectively.
#' @param units_Time,units_Depth string containing the units for time or (vartical) depth netCDF dimension respectively.
#' @param name,longname string containing the name or the long name of \code{psi} variable.
#' @param ... further arguments.
#' 
#' 
#' @return Function returns a netCF object containg the \code{psi} variable information.
#' 
#' @export
#' @examples 
#' data(PsiTheta)
#' filename="/home/ecor/psi.nc"
#'
#' nc <- buildnetCDF(psi,filename=filename)
#' 
#' nc_close(nc)
#'
#'
#'
#' psiz <- lapply(X=psi,FUN=function(x) {is.na(x)*0+1:nlayers(x)})
#' psit <- psiz
#' for (i in 1:length(psit)) {
#'	 psit[[i]] <- psit[[i]]*0+i
#' }
#'
#' ft="/home/ecor/psi_ft.nc"
#' fz="/home/ecor/psi_fz.nc"
#'
#' ncz <- buildnetCDF(psiz,filename=fz)
#' nct <- buildnetCDF(psit,filename=ft)
#'
#' nc_close(ncz)
#' nc_close(nct)
#' 
#' 


buildnetCDF <- function(psi,filename="/home/ecor/psi.nc",CRSdefault="+ellps=WGS84 +proj=tmerc +lat_0=46.0 +lon_0=11 +k=1 +x_0=0 +y_0=0 +units=m +no_defs",time=1:length(psi),depth=1:nlayers(psi[[1]]),
		overwrite=TRUE,
		dimTime="Time",dimDepth="Depth",units_Time="hour",units_Depth="meter",units="meter",name="psi",longname=name,

		
		...) {
	print(depth)
	ntime <- length(time)
	nl <- nlayers(psi[[1]])
	filename_temp <- str_replace(filename,".nc","TEMP.nc")
	
	
	if (length(depth)!=nl) {
		
		stop("Inconsistent depth length")
		
	}
	
	if (length(time)!=length(psi)) {
		
		stop("Inconsistent time length")
		
	}
	
	
	
	psi <- stack(psi)
	
	if (is.na(projection(psi))) projection(psi) <- CRSdefault
	
	writeRaster(psi,filename_temp,overwrite=overwrite,...)
	nc <- nc_open(filename_temp)
	
	
	
	
	
	

	var <- ncvar_get(nc,varid=names(nc$var)[1])
	varatt <- ncatt_get(nc,varid="variable")
	varatt <- varatt[!(names(varatt) %in% c("min","max","long_name","units"))]
	
	psimat <- array(var,c(dim(var)[1:2],nl,ntime))
	ncdimTime <- ncdim_def(dimTime,units=units_Time,vals=time,unlim=TRUE,create_dimvar=TRUE)
	ncdimDepth <- ncdim_def(dimDepth,units=units_Depth,vals=depth,unlim=FALSE,create_dimvar=TRUE)
#	
	
	dim <- nc$var[[1]]$dim[1:2]
	dim[[3]] <- ncdimDepth
	dim[[4]] <- ncdimTime
	psivar <- ncvar_def(name,units=units,dim=dim,longname=longname,missval=varatt$missing_value)
###	nc <- ncvar_add( nc, psivar)
	
    out <- nc_create(filename,psivar, force_v4=FALSE)
#	var <- nc_get_
	ncvar_put(out,psivar,psimat)
	
	for (it in names(varatt)) {
		
		ncatt_put(out,psivar,attname=it,attval=varatt[[it]])
		
	}
	nc_close(nc)
	
	file.remove(filename_temp)https://mail.google.com/mail/
	
	
	return(out)
	
	
	
}

