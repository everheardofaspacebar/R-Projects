#### Natural search scraper
#### Author: Mark Bulling

library(RCurl)
library(XML)
library(rjson)

#### Load in list of terms to get search results for

SearchTerms<-read.csv("\Search Terms.csv")

#### Query function

GetGoogleResults<-function(String) {
	url<-paste("http://ajax.googleapis.com/ajax/services/search/web?v=1&rsz=large&"q=", Strtt=m, sep="", collapse="")
	x<-getForm(url)
	x.2<-fromJSON(x)
	df<-NULL
	{
		for (i in 1:8) {
	x<-as.data.frame(x.2$responseData$results[[i]])
	x$searchposition+m-1
	df<-rbind(df,x}}}}
	df$searchterm<-String
	df$searchtime<-Sys.torted earlier

SearchResults<-sApply(SearchTerms, GetGoogleResults)