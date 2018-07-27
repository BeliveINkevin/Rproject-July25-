library(readr)
df<-read_csv("C:/Documents/My Excel/Poverty.csv")
poverty<-df[-c(4,8,50,56,61,70),]# Note the reason why you're deleting these rows
#poverty<-sapply(df1[1:91,1:7], as.numeric)
poverty$GNP<-as.numeric(poverty$GNP)
head(poverty)
tail(poverty)

install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

#Identifying a relationship b/t birth and death rates using regression
birthdeath=lm(poverty$BirthRt~poverty$DeathRt)
summary(birthdeath)
deathbirth=lm(poverty$DeathRt~poverty$BirthRt)
summary(deathbirth)
summary(poverty$BirthRt)
plot(birthdeath)
plot(deathbirth)
plot(poverty$BirthRt,poverty$DeathRt)

#Identifying a relationship between life expentancy for males and females
LExpMF=lm(poverty$LExpM~poverty$LExpF)
summary(LExpMF) # Looks like this is better R2

#calculate the summary of infant mortality. Which chountry has the lowest and highest
#I'm going to list the countries with InFMort > 90
summary(poverty$InfMort) #Min goes to Japan while max goes to Alganistan
infMortabove90=subset(poverty,InfMort > 90)
unique(infMortabove90$Country) #notice most of them fall into region 5 and 6

#Does death rate impact infMort as well as life expectancy
Deathmort=lm(poverty$DeathRt~poverty$InfMort)
summary(Deathmort) #Remember that this only has correlation in certain countries. Not all

#Is there a relationship b/t GNP and life expectancy for both males and females?
GNPMF=lm(poverty$GNP~poverty$LExpM+poverty$LExpF)
summary(GNPMF) #Since the R2 is .41, what can you say about this?

#I'm going to find out which regrion has the highest and lowest gdp
reg1<-dplyr::filter(poverty,Region=='1')
reg2<-dplyr::filter(poverty,Region=='2')
reg3<-dplyr::filter(poverty,Region=='3')
reg4<-dplyr::filter(poverty,Region=='4')
reg5<-dplyr::filter(poverty,Region=='5')
reg6<-dplyr::filter(poverty,Region=='6')
summary(reg1)
summary(reg2)
summary(reg3)
summary(reg4)
summary(reg5)
summary(reg6)

a<-ggplot(poverty,aes(GNP,DeathRt))
a+geom_jitter()+geom_smooth(method="lm") #What can we say about this plot?

#My next goal is to predict the GNP for the rows I previously cleaned
GNPpredictor<-lm(poverty$GNP~poverty$BirthRt+poverty$DeathRt+poverty$InfMort+poverty$LExpM+poverty$LExpF)
summary(GNPpredictor)
#explain why you weren't able to accurately predict the GNP

#Plotting Birth rate vs death rate by region
a<-ggplot(poverty,aes(x=BirthRt,y=DeathRt,color=factor(Region)))
a+geom_jitter()

#Plotting 
a<-ggplot(poverty,aes(x=Region,y=GNP,color=factor(Region)))
a+geom_jitter()
