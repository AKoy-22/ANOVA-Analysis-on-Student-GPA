install.packages(c( "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
mydata=read.csv("OriginalData.csv")
summary(mydata)
one.way=aov(GPA~LanguageSkill, data=mydata)
one.way
summary(one.way)
tukey.one.way=TukeyHSD(one.way,conf.level = 0.90)
tukey.one.way

one.way1=aov(GPA~WorkHrs, data=mydata)
one.way1
summary(one.way1)
tukey.one.way1=TukeyHSD(one.way1)
tukey.one.way1


#start here 
library(agricolae)
library(readr)
df<- read.csv("OriginalDataCategorical.csv")
spec(df)
two.way<-aov(GPA~LanguageSkill+Living+block, data=df)
summary(two.way)
tukeytwo<-TukeyHSD(two.way,conf.level = 0.90)
tukeytwo

#ParentsChild ----six***
oneway<-aov(GPA~ParentsChild, data=df)
summary(oneway)
tukeyone<-TukeyHSD(oneway, conf.level = 0.85)
tukeyone
psig<-as.numeric(apply(tukeyone$ParentsChild[,2:3],1,prod)>=0)+1
plot(tukeyone,las=1,col=psig)

tukeyoneP2<-HSD.test(oneway, trt = "ParentsChild", alpha = 0.15)
tukeyoneP2


#Language Skill ----moderate***
onewayLang<-aov(GPA~LanguageSkill, data=df)
summary(onewayLang)
tukeyoneLang<-TukeyHSD(onewayLang, conf.level = 0.90)
psig<-as.numeric(apply(tukeyoneLang$LanguageSkill[,2:3],1,prod)>=0)+1
tukeyoneLang
plot(tukeyoneLang, las=1, col=psig, cex.axis=0.53) 

tukeyoneL2<-HSD.test(onewayLang, trt = "LanguageSkill", alpha = 0.10)
tukeyoneL2



#Job Mket ---Bad and very bad***
onewayJob<-aov(GPA~JobMkt, data=df)
summary(onewayJob)
tukeyoneJob<-TukeyHSD(onewayJob, conf.level = 0.75)
tukeyoneJob
psig<-as.numeric(apply(tukeyoneJob$JobMkt[,2:3],1,prod)>=0)+1
plot(tukeyoneJob, las=2, col=psig, cex.axis=0.30)


tukeyoneJ2<-HSD.test(onewayJob, trt = "JobMkt", alpha = 0.25)
tukeyoneJ2


#Textbook --- other retail ***
onewayText<-aov(GPA~Textbook, data=df)
summary(onewayText)
tukeyoneText<-TukeyHSD(onewayText, conf.level = 0.65)
tukeyoneText
psig<-as.numeric(apply(tukeyoneText$Textbook[,2:3],1,prod)>=0)+1
plot(tukeyoneText, las=1, col=psig, cex.axis=0.35)

tukeyoneT2<-HSD.test(onewayText, trt = "Textbook", alpha = 0.35)
tukeyoneT2

#Religious-------Religous***
onewayReligious<-aov(GPA~Religious, data=df)
summary(onewayReligious)
tukeyoneReligious<-TukeyHSD(onewayReligious, conf.level = 0.75)
tukeyoneReligious
psig<-as.numeric(apply(tukeyoneReligious$Religious[,2:3],1,prod)>=0)+1
plot(tukeyoneReligious, las=1, col=psig, cex.axis=0.35)

tukeyoneRel2<-HSD.test(onewayReligious, trt = "Religious", alpha = 0.25)
tukeyoneRel2

#Balance
onewayBal<-aov(GPA~Balance, data=df)
summary(onewayBal)
tukeyoneBal<-TukeyHSD(onewayBal, conf.level = 0.90)
tukeyoneBal
psig<-as.numeric(apply(tukeyoneBal$Balance[,2:3],1,prod)>=0)+1
plot(tukeyoneBal, las=1, col=psig, cex.axis=0.35)

tukeyoneBal2<-HSD.test(onewayBal, trt = "Balance", alpha = 0.10)
tukeyoneBal2






#Ideal CHild
onewayChild<-aov(GPA~IdealChild, data=df)
summary(onewayChild)
tukeyoneChild<-TukeyHSD(onewayChild, conf.level = 0.70)
tukeyoneChild
plot(tukeyoneChild, las=1)

#Living
onewayLiving<-aov(GPA~Living, data=df)
summary(onewayLiving)
tukeyoneLiving<-TukeyHSD(onewayLiving, conf.level = 0.50)
tukeyoneLiving
plot(tukeyoneLiving, las=1)

#Newspaper
onewayNews<-aov(GPA~Newspaper, data=df)
summary(onewayNews)
tukeyoneNews<-TukeyHSD(onewayNews, conf.level = 0.50)
tukeyoneNews
plot(tukeyoneNews, las=1)


#PC Access
onewayPC<-aov(GPA~PCAccess, data=df)
summary(onewayPC)
tukeyonePC<-TukeyHSD(onewayPC, conf.level = 0.50)
tukeyonePC
plot(tukeyonePC, las=1)


#OS
onewayOS<-aov(GPA~OpSystem, data=df)
summary(onewayOS)
tukeyoneOS<-TukeyHSD(onewayOS, conf.level = 0.50)
tukeyoneOS
plot(tukeyoneOS, las=1)

#ELectronic
onewayE<-aov(GPA~PreferElecronic, data=df)
summary(onewayE)
tukeyoneE<-TukeyHSD(onewayE, conf.level = 0.50)
tukeyoneE
plot(tukeyoneE, las=1)

#Linux
onewayLinux<-aov(GPA~Linux, data=df)
summary(onewayLinux)
tukeyoneLinux<-TukeyHSD(onewayLinux, conf.level = 0.50)
tukeyoneLinux
plot(tukeyoneLinux, las=1)


#iPod
onewayiPod<-aov(GPA~iPod, data=df)
summary(onewayiPod)
tukeyoneiPod<-TukeyHSD(onewayiPod, conf.level = 0.50)
tukeyoneiPod
plot(tukeyoneiPod, las=1)

#Politics
onewayPolitics<-aov(GPA~Politics, data=df)
summary(onewayPolitics)
tukeyonePolitics<-TukeyHSD(onewayPolitics, conf.level = 0.50)
tukeyonePolitics
plot(tukeyonePolitics, las=1)




#CellMinutes
onewayCell<-aov(GPA~CellMinutes, data=df)
summary(onewayCell)
tukeyoneCell<-TukeyHSD(onewayCell, conf.level = 0.50)
tukeyoneCell
plot(tukeyoneCell, las=1)

#CarAge
onewayCar<-aov(GPA~CarAge, data=df)
summary(onewayCar)
tukeyoneCar<-TukeyHSD(onewayCar, conf.level = 0.65)
tukeyoneCar
plot(tukeyoneCar, las=1)


#workingHrs
onewayWork<-aov(GPA~WorkHrs, data=df)
summary(onewayWork)
tukeyoneWork<-TukeyHSD(onewayWork, conf.level = 0.60)
tukeyoneWork
plot(tukeyoneWork, las=1)


