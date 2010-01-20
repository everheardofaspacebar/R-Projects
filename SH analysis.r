library(ggplot2)
library(sp)
library(maptools)

#### Load in data

load("/Users/markbulling/Documents/workspace/Sport England/APS3byLA.rData")

load("/Users/markbulling/Documents/workspace/Sport England/APS2abyLA.rData")

#### Identify top 10s 

APS3byLAtop10<-APS3byLA[order(APS3byLA$APS3Residual,decreasing=TRUE), ][1:10,]

APS3byLAbottom10<-APS3byLA[order(APS3byLA$APS3Residual,decreasing=FALSE), ][1:10,]

APS2byLAtop10<-APS2byLA[order(APS2byLA$APS2Residual,decreasing=TRUE), ][1:10,]

APS2byLAbottom10<-APS2byLA[order(APS2byLA$APS2Residual,decreasing=FALSE), ][1:10,]

#### Plot scatterplot of predicted versus actual

qplot(data=APS2byLA, x=APS2Predicted, y=APS2Actual)
qplot(data=APS3byLA, x=APS3Predicted, y=APS3Actual)

#### Read in map data
#### Not working at the moment

UKmap<- readShapePoly("/Shapefiles/MSOA_AUG_2004_EW_BGC.shp")
slot(UKmap, "ID")

ggUKmap<-fortify.SpatialPolygonsDataFrame(UKmap, region="MSOA04NM")

MappingTable<-read.csv("/Shapefiles/OA_LSOA_MSOA_LA_Apr05.txt")



