##### Get Weather Underground Data

library(RCurl)
library(XML)
library(ggplot2)
library(plyr)

#### Get list of weather stations in the uk

uk.weatherstation.details<-postForm("http://api.wunderground.com/auto/wui/geo/GeoLookupXML/index.xml?query=United%20Kingdom")

tree<-xmlTreeParse(uk.weatherstation.details)

#### Process this into a dataframe

tmp <- xmlSApply(tree, function(x) xmlSApply(x, xmlValue))
tmp<-t(tmp)
tmp.df<-data.frame(tmp)
tmp.df$id<-gsub("/global/stations/", "", tmp.df$link)
tmp.df$id<-gsub(".html", "", tmp.df$id)

#### Get Long and Lat of location

GetLocation<-function(id)
{
	url2<-paste("http://www.wunderground.com/global/stations/", id,".html", sep="")
tree<-htmlTreeParse(url2)
location<-tree[[3]][[1]][[1]][[4]]
location
}


#### Construct URL
GetWeather<-function(df){
	id<-df$id
	full.data<-NULL
	for (year in 2007:2007){
		for (month in 1:12){

url<-paste("http://www.wunderground.com/history/station/", id, "/", year, "/", month, "/1/MonthlyHistory.html?req_city=NA&req_state=NA&req_statename=NA&format=1", sep="", collapse=NULL)

data.new<-read.csv(url)
data.new<-subset(data.new, MeanDew.PointC!="NA")
colnames(data.new) <- c("Date", "MaxTemp", "MeanTemp", "MinTemp", "DewPoint", "MeanDewPoint", "MinDewPoint", "MaxHumidity", "MeanHumidity", "MinHumidity", "MaxPressure", "MeanPressure", "MinPressure", "MaxVis", "MeanVis", "MinVis", "MaxWind", "MeanWind", "MaxGust", "Precipitation", "CloudCover", "Events")
data.new$Date<-as.Date(data.new$Date)
full.data<-rbind(full.data, data.new)
}}

full.data}

tmp.df.1<-tmp.df[1:100,]
tmp.df.2<-tmp.df[101:200,]
tmp.df.3<-tmp.df[201:292,]

output.1.2007<-ddply(tmp.df.1, .(id), "GetWeather", .progress = progress_text(char = "-"))

output.2.2007<-ddply(tmp.df.2, .(id), "GetWeather", .progress = progress_text(char = "-"))

output.3.2007<-ddply(tmp.df.3, .(id), "GetWeather", .progress = progress_text(char = "-"))

weather.data<-rbind(output.1.2007, output.2.2007, output.3.2007, output.1.2008, output.2.2008, output.3.2008, output.1.2009, output.2.2009, output.3.2009)

tmp.df$location<-sapply(tmp.df$id, GetLocation)

write.csv(weather.data, file="/Weather Data.csv")
write.csv(tmp.df, file="/Weather Location.csv")

latlongdata<-read.csv("/Weather Location.csv")

latlong.weather.data<-merge(weather.data, latlongdata, by.x="id", by.y="id2")

latlong.weather.sub<-subset(latlong.weather.data, Date=="2009-12-01")

library(animation)

#### there are 291 stations

latlong.weather.data$Date<-as.Date(latlong.weather.data$Date, "%Y-%m-%d")
latlong.weather.data2<-latlong.weather.data[order(latlong.weather.data$Date),]

library(chron)

saveMovie( 
{for (y in 2007:2009) {
		for (i in 1:12) {
			print(qplot(data=subset(latlong.weather.data, as.numeric(format(Date, "%m"))==i & years(Date)==y), x=long, y=lat, colour=MeanTemp)+scale_colour_gradient(limits=c(-10, 30)))
		}}}, 
		interval=0.2, moviename = "movie", movietype = "gif", outdir = getwd()
)





library(animation)

