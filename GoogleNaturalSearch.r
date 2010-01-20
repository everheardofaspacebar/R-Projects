#### Natural search scraper
#### Author: Mark Bulling

library(RCurl)
library(XML)
library(rjson)

#### Load in list of terms to get search results for

SearchTerms<-read.csv("/Search Terms.csv")

#### Query function 
#### At the moment, provides first 8 results
#### Use start= and a loop to get the next page of results

GetGoogleResults<-function(String) 
{
	String2<-gsub(" ", "+", String)
	df<-NULL
		
	for (m in seq(0, 31, 8))
		{
	url<-paste("http://ajax.googleapis.com/ajax/services/search/web?v=1.0&gl=gb&rsz=large&start=", m, "&q=", String2, sep="", collapse="")
	x<-getForm(url)
	x.2<-fromJSON(x)
	
	for (i in 1:8) {
	x<-as.data.frame(x.2$responseData$results[[i]])
	x$searchposition<-i+m
	df<-rbind(df, x)}
	}
	df$searchterm<-String
	df$searchtime<-Sys.time()
	df
}
	


### Identify the urls that are on client list

### Chart up where client is across all terms
### e.g. has number 1 result on 4 of search terms






