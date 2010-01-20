library(ggplot2)
library(zoo)

x<-read.csv("source data.csv")

x$date<-as.Date(x$date, "%d/%m/%Y")
x$month<-months(x$date)
x$yearqtr<-as.yearqtr(x$date)
x<-subset(x, age<999) #get rid of murder with age 999

pdf(file="Philly Murders.pdf", width = 8, height = 15)
ggplot(data=x, aes(x=long, y=lat, alpha=0.3, colour=race, size=age))+geom_point()+facet_wrap(~yearqtr, ncol=4)+theme_bw()
dev.off()

pdf(file="Philly Murders 2.pdf", width = 8, height = 15)
ggplot(data=x, aes(x=long, y=lat, colour=race))+geom_path()+facet_wrap(~yearqtr, ncol=4)+theme_bw()
dev.off()

pdf(file="Philly Murders 3.pdf", width = 12, height =40)
ggplot(data=x, aes(x=weapon, y=..count..))+geom_histogram()+coord_flip()+facet_wrap(~yearqtr, ncol=4)
dev.off()

ggplot(data=subset(x, weapon="FIREARM"), aes(x=date, y=..count..))+geom_bar()+facet_grid(sex~.)

ggplot(data=subset(x, weapon="FIREARM"), aes(x=yearqtr, y=..count..))+geom_bar()+facet_grid(motive~.)