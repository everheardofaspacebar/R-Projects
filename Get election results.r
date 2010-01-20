library(RCurl)
library(XML)
library(plyr)
library(ggplot2)

ConstituencyList<-read.csv("/Constituencies.csv")

GetConstituencyData<-function(df){
	Name<-df$Name
Link<-paste("http://www.electoralcommission.org.uk/elections/results/general_elections/uk-general-election-2006/", Name, sep="", collapse=NULL)
x<-data.frame(readHTMLTable(as.character(Link)))
x$constituency<-as.character(Name)
x
}

ConstituencyListSub<-ConstituencyList[1,]
ConstituencyListSub<-subset(ConstituencyList, select=c("Name"))

Data<-ddply(ConstituencyList, .(Name), "GetConstituencyData")

#### Get locations of each constituency from TheyWorkForYou

load("ElectionData.rData")

x<-getForm("http://www.theyworkforyou.com/api/getGeometry", "output"="xml", "key"="EAxqn2AgGSFqEWhfwEDrRpyb")

x2<-htmlTreeParse(x, asText=TRUE)

### Pull out each element from the XML

FullData<-NULL

for (i in 1:1500)

{
iData<-NULL

if(xmlSize(x2[[3]][[2]][[1]][[1]][[2]][[i]])!=0){

Constituency<-as.character(x2[[3]][[2]][[1]][[1]][[2]][[i]][[1]][[1]])
iData$Constituency<-Constituency
Lat<-as.character(x2[[3]][[2]][[1]][[1]][[2]][[i]][[2]][[1]])
iData$Lat<-Lat[[6]]
Long<-as.character(x2[[3]][[2]][[1]][[1]][[2]][[i]][[3]][[1]])
iData$Long<-Long[[6]]
iData<-as.data.frame(iData)
FullData<-rbind(FullData, iData)

} else iData<-NULL
}

FullData.Sub<-subset(FullData, Constituency!="NULL")
FullData.Sub<-subset(FullData.Sub, Constituency!="text")

write.csv(FullData.Sub, "ConstituencyLocations.csv")

FullData.sub<-read.csv("ConstituencyLocations.csv")

#### Fuzzy matching


FuzzyMatch<-function (InputName){
{  
   Match <- agrep(InputName, Data$Name, max.distance = 0.1, value=TRUE)[1]
   Match
}}


FullData.sub$MatchName<-sapply(FullData.sub$Constituency, FuzzyMatch)

#### Plot data

p<-ggplot(data=FullData.sub, aes(x=Long, y=Lat))

p+geom_point()


