#This is the sourse code of homework 1 for the EEM course.
#Created by Yin Chenqiao(student number: 1500015533), Oct/17/2017.


#Problem 1
library(ggplot2)
library(maps)

#1(a)
data.arrests = USArrests
head(data.arrests)
base.plot.1 = ggplot(data.arrests, aes(UrbanPop, Murder))
base.plot.1 + geom_point() + ggtitle('Murder Rate by Urban Population') +
    xlab('Urban Population') + ylab('Murder Rate')

#1(b)
base.plot.1 + geom_point() + geom_smooth(method = 'lm') + 
    ggtitle('Murder Rate by Urban Population with Regression Line') +
    xlab('Urban Population') + ylab('Murder Rate')


#1(d)
data.states = map_data('state')
head(data.states)
names(data.arrests) = tolower(names(data.arrests))
data.arrests$region = tolower(rownames(data.arrests))
head(data.arrests)
choro.1 = merge(data.arrests, data.states, by = 'region')
head(choro.1)
qplot(long, lat, data = choro.1, group = group, fill = urbanpop, geom = 'polygon') +
    scale_fill_gradient(low = '#56B1F7', high = '#132B43') +
    ggtitle('Urban Population in Each State')

#1(e)
unique(
    choro.1$region[
        which(choro.1$urbanpop == 
                  max(choro.1$urbanpop, na.rm = T))
        ]
    )




#Problem 2
library(ggplot2)
library(maps)
library(MASS)

#2(a)
data(unemp)
head(unemp)
ggplot(unemp, aes(pop, unemp)) + geom_point() + 
    ggtitle('Unemployment Rate by Population Size') +
    xlab('Population Size') + ylab('Unemployment Rate')

#2(b)
crit = mean(unemp$pop) + 3*sd(unemp$pop)
unemp.sub = subset(unemp, subset = pop < crit)
ggplot(unemp.sub, aes(pop, unemp)) + geom_point() + geom_smooth(method = 'lm') +
    ggtitle('Unemployment Rate by Population Size with Regression Line') +
    xlab('Population Size') + ylab('Unemployment Rate')

#2(d)
data(county.fips)
color.function = colorRampPalette(c('steel blue','light gray','firebrick4'))
col.ramp = color.function(5)
color.vector = cut(rank(unemp$pop), breaks = 5, labels = col.ramp)
color.vector = as.character(color.vector)
this.order = match(county.fips$fips, unemp$fips)
color.vec.ordered = color.vector[this.order]
map(database = 'county', col = color.vec.ordered, fill = T, lty = 0)
title(main = 'Population Size by County in 2009')
map.scale(metric = F, ratio = F, relwidth = 0.15, cex = 0.5)
quantile(unemp$pop, (0:5)/5)
legend.text = c('52.0-4355.2', '4355.2-9007.6', '9007.6-17537.2', '17537.2-43245.2', '43245.2-4923821.0')
legend('bottomright', bg = 'white',
       pch = 19, pt.cex = 1.5, cex = 0.7,
       legend = legend.text, 
       col = col.ramp, 
       box.col = 'white',
       title = 'Population Size'
)

#2(e)
county.fips$polyname[
    which(county.fips$fips == 
              unemp$fips[
                  which(unemp$pop == max(unemp$pop, na.rm = T))
                  ]
          )
    ]




#Problem 3
library(WDI)
library(ggplot2)
library(plyr)

#3(b)
interest = WDIsearch('interest')

#3(c)
country = WDI_data$country  #Browse the country names
View(country)
data.interest = WDI(indicator = 'FR.INR.DPST',  #Deposit interest rate (%)
                    country = c('CN', 'US', 'IN', 'JP', 'AU', 'BR'),  
                        #Use the data of Australia and Brazil
                    start = 2001, end = 2010)
data.interest

#3(d)
ggplot(data.interest, aes(year, FR.INR.DPST)) + geom_line() + facet_grid(. ~ country)

#3(e)
data.interest$FR.INR.DPST[which(is.na(data.interest$FR.INR.DPST) == 1)] = 0
AggrData = ddply(data.interest, .(country), summarize, 
                 InterestAvg = mean(FR.INR.DPST, na.rm = T))
ggplot(AggrData, aes(x = country, y = InterestAvg)) + 
    geom_bar(stat = 'identity', position = 'dodge')




#Problem 4
library(WDI)
library(ggplot2)
library(rworldmap)

#4(a)
data.interest.all = WDI(indicator = c('FR.INR.DPST', 'NY.GDP.PCAP.KD.ZG'), 
                        country = 'all', 
                        start = 2010, end = 2010)

#4(b)
ggplot(data = data.interest.all, aes(FR.INR.DPST, NY.GDP.PCAP.KD.ZG)) +
    geom_point() + geom_smooth(method = 'lm')

#4(c)
mapped_data = joinCountryData2Map(data.interest.all, joinCode = 'ISO2', 
                                  nameJoinColumn = 'iso2c')
mapped_data = subset(mapped_data, REGION != 'Antarctica')
mapCountryData(mapped_data,
               mapTitle='Deposit interest rate (%), 2010',
               nameColumnToPlot='FR.INR.DPST', 
               catMethod = c(
                   min(data.interest.all$FR.INR.DPST, na.rm = T):
                   max(data.interest.all$FR.INR.DPST, na.rm = T))
)