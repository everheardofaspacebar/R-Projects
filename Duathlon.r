library(ggplot2)

x<-read.csv("/Challenge_Distance_Results.csv")

x<-x[, c(1:7, 9, 12, 15, 18, 21, 24)]

x$runpct<-(x$Run1.Secs+x$Run2.Secs)/x$Total.Secs
x$bikepct<-(1-x$runpct)
x$run1speedms<-9000/x$Run1.Secs
x$run2speedms<-5000/x$Run2.Secs
x$bikespeedms<-20000/x$Bike.Secs
x$Category<-gsub("M", "", x$Category)
x$Category<-gsub("F", "", x$Category)
x$run1vsrun2<-x$run1speedms-x$run2speedms

qplot(data=x, x=run1speedms, y=run2speedms, alpha=0.8, colour=run1vsrun2)+facet_grid(~Gender)+stat_smooth(method=lm)

x.m<-melt(x, id=c("Race.", "Forename", "Surname", "Gender", "GenderPos", "Category", "CategoryPos"))
x.m$min<-as.numeric(x.m$value)/60
x.m$Category<-gsub("F", "", x.m$Category)

x.m.notrans<-subset(x.m, variable!="T1.Secs")
x.m.notrans<-subset(x.m.notrans, variable!="T2.Secs")
x.m.nototal<-subset(x.m.notrans, variable!="Total.Secs")
pct<-x.m.notrans$variable %in% c("runpct", "bikepct")
notpct<-x.m.notrans$variable  %in% c("Run1.Secs", "Run2.Secs", "Total.Secs", "Bike.Secs")
x.m.mix<-subset(x.m.notrans, pct)
x.m.notrans<-subset(x.m.notrans, notpct)

r<-ggplot(x.m.mix, aes(x=value))
r+geom_density()+facet_grid(Gender~variable)

p3<-ggplot(data=x, aes(x=Run1.Secs, y=Run2.Secs, alpha=0.4, ymax=3000))
p3+geom_point()+facet_grid(Category~Gender, scales="free")

friends<-x.m.notrans$Surname %in% c("Bulling", "Chandler", "Gooch", "Whitehorn", "Doust", "Wakem")
x.m.friends<-subset(x.m.notrans, friends)



breakdown<-x.m.notrans$variable %in% c("Run1.Secs", "Bike.Secs", "Run2.Secs")
x.m.breakdown<-subset(x.m.notrans,breakdown)
me<-x.m.notrans$Surname %in% c("Bulling")
x.m.me<-subset(x.m.notrans,me)

x.m.total<-subset(x.m.notrans, variable "Total.Secs"
qplot(data=x.m.friends, x=variable, y=value, label=Forename)+geom_text()

qplot(data=x.m.nototal, x=value, geom="density", fill=variable, position="stack")

pdf()
p<-ggplot(x.m.notrans, aes(x=as.numeric(min), fill=as.factor(Gender), grouping=as.factor(Gender))) 
p+geom_density(alpha=0.5)+facet_wrap(~variable, ncol= 1, scales="free")+xlab("Minutes")+geom_vline(data=x.m.me, aes(xintercept=min, linetype=factor(Forename)))
dev.off()

q<-ggplot(x.m, aes(x=as.numeric(min), fill=as.factor(Category), grouping=as.factor(Category)))
q+geom_density(alpha=0.5)+facet_wrap(~variable, scales="free")

q2<-ggplot(x.m.notrans, aes(x=as.factor(Category), y=as.numeric(min), fill=as.factor(Category), grouping=as.factor(Category)))
q2+geom_boxplot(outlier.size=0)+facet_grid(Gender~variable, scales="free")