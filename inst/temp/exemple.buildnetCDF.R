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
