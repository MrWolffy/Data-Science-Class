library(MASS)
library(plyr)
library(ggplot2)
library(knitr)
library(RCurl)
library(corrplot)
library(WDI)


#Question 1
crime = read.table("https://raw.githubusercontent.com/TonyHuaHowell/DataScienceClass2017/master/crime_simple.txt",sep = "\t", header = TRUE)
colnames(crime) = c("crime.per.million", "young.males", "is.south", "average.ed",
                     "exp.per.cap.1960", "exp.per.cap.1959", "labour.part",
                     "male.per.fem", "population", "nonwhite",
                     "unemp.youth", "unemp.adult", "median.assets", "num.low.salary")
crime = transform(crime, is.south = as.factor(is.south),
                   average.ed = average.ed / 10,
                   median.assets = median.assets / 100)
qplot(average.ed, crime.per.million, data = crime)
crime.lm = lm(crime.per.million ~ young.males + average.ed + exp.per.cap.1960 +
                   unemp.adult + num.low.salary, data = crime)
options(scipen = 4)
summary(crime.lm)
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
plot(crime.lm,which = c(1, 2))


#Question 2
#(a)
data.wdi = WDI(indicator = c('NY.GDP.MKTP.CD', 'NE.GDI.FTOT.KD', 'NE.CON.PRVT.KD', 'NE.CON.GOVT.KD'), 
               #use GDP as y variable
               #use fixed investment, private consumption and government consumption as x variables
           country = 'all', start = 2005, end = 2012)
head(data.wdi)
data.wdi = transform(data.wdi, NY.GDP.MKTP.CD = NY.GDP.MKTP.CD / 1000000, 
                     NE.GDI.FTOT.KD = NE.GDI.FTOT.KD / 1000000, 
                     NE.CON.PRVT.KD = NE.CON.PRVT.KD / 1000000, 
                     NE.CON.GOVT.KD = NE.CON.GOVT.KD / 1000000)
ggplot(data.wdi, aes(NY.GDP.MKTP.CD, NE.GDI.FTOT.KD)) + geom_point()
ggplot(data.wdi, aes(NY.GDP.MKTP.CD, NE.CON.PRVT.KD)) + geom_point()
ggplot(data.wdi, aes(NY.GDP.MKTP.CD, NE.CON.GOVT.KD)) + geom_point()
#(b)
gdp.lm.1 = lm(NY.GDP.MKTP.CD ~ NE.GDI.FTOT.KD, data = data.wdi)
options(scipen = 4)
summary(gdp.lm.1)
gdp.lm.2 = lm(NY.GDP.MKTP.CD ~ NE.GDI.FTOT.KD + NE.CON.PRVT.KD + NE.CON.GOVT.KD, data = data.wdi)
summary(gdp.lm.2)
#(c)
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
plot(gdp.lm.2, which = c(1, 2))
#(d)
economic.var.names = c('NE.GDI.FTOT.KD', 'NE.CON.PRVT.KD', 'NE.CON.GOVT.KD')
pairs(data.wdi[,economic.var.names], lower.panel = NULL)
data.wdi = na.omit(data.wdi)
corrplot(cor(data.wdi[,economic.var.names]), method = "number",type = "upper")




