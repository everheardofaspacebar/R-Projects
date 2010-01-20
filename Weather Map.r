loc=file.path("ftp://ftp.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs.2009121700/gfs.t00z.sfluxgrbf03.grib2")
download.file(loc,"temp.grb",mode="wb")

library(ncdf)
landFrac <-open.ncdf("LAND.nc")
land <- get.var.ncdf(landFrac,"LAND_surface")
x <- get.var.ncdf(landFrac,"longitude")
y <- get.var.ncdf(landFrac,"latitude")

library(fields)
rgb.palette <- colorRampPalette(c("snow1","snow2","snow3","seagreen","orange","firebrick"), space = "rgb")#colors
image.plot(x,y,t2m.mean,col=rgb.palette(200),axes=F,main=as.expression(paste("GFS 24hr Average 2M Temperature",day,"00 UTC",sep="")),axes=F,legend.lab="o C")
contour(x,y,land,add=TRUE,lwd=1,levels=0.99,drawlabels=FALSE,col="grey30") #add land outline