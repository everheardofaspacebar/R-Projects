library(sp)
library(ggplot2)
library(maptools)
library(shapefiles)
library(plyr)

#### Read in shapefile

UKmap<-readShapePoly("//Shapefiles//MSOA_AUG_2004_EW_BGC")
MappingTable<-read.csv("//Shapefiles//OA_LSOA_MSOA_LA_Apr05.txt")

#### Summarise Mapping Table by MSOA name
MappingTable2<-subset(MappingTable, select=c("MSOA_name", "LA_name"))
MappingTable2<-strip.dups(MappingTable2)
MappingTable2<-subset(MappingTable2, MappingTable2[,1]!="")

#### Get list of LAs in map
MappingTable2a<-subset(MappingTable, select=c("LA_name"))
MappingTable2a<-strip.dups(MappingTable2a)
MappingTable2a<-subset(MappingTable2a, MappingTable2a[,1]!="")
summary(MappingTable2a)

#### Merge mapping table on to UK map2
UKmap2<-spCbind(UKmap, MappingTable2[,1])
UKmap2<-spCbind(UKmap2, MappingTable2[,2])

#####
ggUKmap<-fortify.SpatialPolygonsDataFrame(UKmap2, region="MappingTable2...2.")

spplot(UKmap2)
