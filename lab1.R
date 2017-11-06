#2(a)
3:12

#2(b)
seq(3, 30, 3)

#2(c)
x = 3:12
y = seq(3, 30, 3)
x * y

#3(a)
mean(cars$speed)
sd(cars$speed)
mean(cars$dist)
sd(cars$dist)

#3(b)
plot(cars$speed, cars$dist)

#4
Health <- c("dialysis" , "Dialysis", "dialysis" ,"none", "None", "nnone")
mapvalues(Health, c('Dialysis', 'None', 'nnone'), c('dialysis', rep('none', 2)))
summary(Health)

#5(a)
library(RCurl)
survey <- read.csv(text=getURL("https://raw.githubusercontent.com/TonyHuaHowell/DataScienceClass2017/master/Course_survey.csv"), header=T)
library(plyr)
survey <- transform(survey, programNew = mapvalues(survey$Program, 
                                                   c('OtherEcon', 'SOE'),
                                                   rep('Econ', 2)))

#5(b)
head(survey)

#5(c)
survey <- transform(survey, GamingHours = as.numeric(gsub('^[0-9.]', '', GamingHours)))
survey <- na.omit(survey)
tapply(survey$GamingHours, INDEX = survey['programNew'], FUN = mean)

#5(d)
survey <- transform(survey,
                    useR=mapvalues(Rexperience,c('Never used','Basic competence','Installed on machine'),
                                   c('No Experience', rep('Experience',2))))
table(survey$programNew, survey$useR)

#5(e)
tapply(survey$GamingHours, INDEX = survey[c('programNew', 'useR')], FUN = mean)