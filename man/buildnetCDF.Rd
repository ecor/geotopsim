% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/builnetCDF.R
\name{buildnetCDF}
\alias{buildnetCDF}
\title{Collect Psi or Theta in a netCDF file}
\usage{
buildnetCDF(psi, filename = "/home/ecor/psi.nc",
  CRSdefault = "+ellps=WGS84 +proj=tmerc +lat_0=46.0 +lon_0=11 +k=1 +x_0=0 +y_0=0 +units=m +no_defs",
  time = 1:length(psi), depth = 1:nlayers(psi[[1]]), overwrite = TRUE,
  dimTime = "Time", dimDepth = "Depth", units_Time = "hour",
  units_Depth = "meter", units = "meter", name = "psi", longname = name,
  ...)
}
\arguments{
\item{psi}{variable, a list of \code{RasterBrick-class} object}

\item{filename}{string containing full name of the netCDF file}

\item{overwrite}{logical value. Default is \code{TRUE}.}

\item{dimTime, dimDepth}{string containing the name for time or (vartical) depth netCDF dimension respectively.}

\item{units_Time, units_Depth}{string containing the units for time or (vartical) depth netCDF dimension respectively.}

\item{name, longname}{string containing the name or the long name of \code{psi} variable.}

\item{...}{further arguments.}

\item{CRSDefaul}{string with CRS default (used if it is missing in \code{psi)}}
}
\value{
Function returns a netCF object containg the \code{psi} variable information.
}
\description{
Collect Psi or Theta in a netCDF file
}
\examples{
data(PsiTheta)
filename="/home/ecor/psi.nc"

nc <- buildnetCDF(psi,filename=filename)

nc_close(nc)



psiz <- lapply(X=psi,FUN=function(x) {is.na(x)*0+1:nlayers(x)})
psit <- psiz
for (i in 1:length(psit)) {
 psit[[i]] <- psit[[i]]*0+i
}

ft="/home/ecor/psi_ft.nc"
fz="/home/ecor/psi_fz.nc"

ncz <- buildnetCDF(psiz,filename=fz)
nct <- buildnetCDF(psit,filename=ft)

nc_close(ncz)
nc_close(nct)


}

