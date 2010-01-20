# 
# Program: Modelling using Zelig
# Author: markbulling
###############################################################################


#### Load up zelig 

### source("http://gking.harvard.edu/zelig/install.R")
library(Zelig)
library(ggplot2)
library(memisc)

#### Load in model data

load("/Users/markbulling/Documents/workspace/Sport England/APS3ModelData.rData")
load("/Users/markbulling/Documents/workspace/Sport England/APS2ModelData.rData")

names(ModelData)
#### Create ln(Deprivation Index)

ModelData$imdlnadj<-log(ModelData$DeprivationScore2007)
ModelData2$imdlnadj<-log(ModelData2$DeprivationScore2007)

ModelData<-subset(ModelData, AnnualWeight!=0)
ModelData2<-subset(ModelData2, AnnualWeight!=0)

##### Model using a sample of data
ModelDataSample<-ModelData[sample(nrow(ModelData), size=20000), ]

#### Run the model(s) to test

APS2modelSH<-zelig(data=ModelData2, q15adj~d13adj+q19adj+q21_adj+s3adj+Male+age16_24 + age25_34+age55_64+age75_84+age85plus
				+d5adj+ethwethnic+ethasian+ethblack+ethchinese+d6heduc1+d6heduc2+d6alevels+d6gcse+d7own+d7council
				+d10child1+d10child2+d10child3+d19wpart+d19stufull+d19retired+regneast+regwmids+regeast+
				regswest+regseast+reglondon+d19unempl1+d11badj+d11old1+d11old2+sec4prof+sec4manag+sec4skill1+
				imdlnadj+d23incm1+d23incm2+d23incm3+d23incm4+d23incm7+d23incm8+d23incm9+d23incm10+d23incm11,
				weights = ModelData2$AnnualWeight,na.action=na.omit,
				model="logit")
				
APS3modelSH<-zelig(data=ModelData, q15adj~d13adj+q19adj+q21_adj+s3adj+Male+age16_24 + age25_34+age55_64+age75_84+age85plus
				+d5adj+ethwethnic+ethasian+ethblack+ethchinese+d6heduc1+d6heduc2+d6alevels+d6gcse+d7own+d7council
				+d10child1+d10child2+d10child3+d19wpart+d19stufull+d19retired+regneast+regwmids+regeast+
				regswest+regseast+reglondon+d19unempl1+d11badj+d11old1+d11old2+sec4prof+sec4manag+sec4skill1+
				imdlnadj+d23incm1+d23incm2+d23incm3+d23incm4+d23incm7+d23incm8+d23incm9+d23incm10+d23incm11,
				weights = ModelData$AnnualWeight,na.action=na.omit,
				model="logit")				
library(utils)

#### Output coefficient results to Latex and as a dot plot
#### Add in APS1 on Monday
#### Update models with variables I've missed out
#### Sort out 

outreg(list(APS2modelSH, APS3modelSH), lyx=F)

library(arm)

pdf("\coef dotplot.pdf" width = 5, height = 8)
coefplot(APS3modelSH, cex.var=0.5)
coefplot(APS2modelSH, add=TRUE, col.pts="blue", offset=-0.4)
dev.off()



##### New modelling

#### Variable Groups		
BaseDemographics<-c("Male + d19_bands_6 + d23_bands_7 + d3_bands + sec4r + NumAdultsHousehold + I(CarVanAvailable==1) + SE_Region")
Education<-c("SportCollegeCount")		
Obesity<-c("OverweightReception+ ObeseReception+ OverweightReception+ ObeseYear6")
LA_CPA<-c("CPAstarcategory+ CPACorporateassessment")
LASpends<-c("Museumsgalleriestotalspend+ Sportsdevtotalspend+ Sportsfacilitiestotalspend")
Accreditation<-c("CharterMarkCount+ SportsMarkCount+ ActiveMarkCount+ QuestCount+ IIECount+ ISO9001Count")		
Deprivation<-c("imdlnadj")

#### Build formula
formula<-as.formula(paste("q15adj~", paste(BaseDemographics, Obesity, Deprivation, sep="+")))

names(ModelDataSample)
#### Run model
APS3modelMS<-zelig(data=ModelDataSample, formula, model="logit",na.action=na.omit)
summary(APS3modelMS)

#### Send results to Latex

outreg(list(APS3modelMS), tight=F, lyx=F)
outreg
#### Calculate expected values

APS3modelMS.out<-setx(APS3modelMS, fn=NULL)
s.out <- sim(APS3modelMS, x = APS3modelMS.out)

#### Compare model to SH with a ROC plot
#### Continuous line is SH, the nearer the curve is to top right the better

rocplot(APS3modelSH$y, APS3modelMS$y, fitted(APS3modelSH), fitted(APS3modelMS))

#### Run simulation to calculate predicted probability for a variable

x.out <- setx(APS3modelMS, Male = 1)
s.out <- sim(APS3modelMS, x=x.out)
summary(s.out)
plot(s.out)

#### Run simulation to calculate the change in probability by turning variable on and off

x.low <- setx(APS3modelMS, Male=0)
x.high <- setx(APS3modelMS, Male=1) 
s.out <- sim(APS3modelMS, x=x.low, x1=x.high)
summary(s.out)

#### Need to automate this across the model
#### Calculate average effect of each variable in model

