#Prepare R workspace
library(ggplot2)
head(mpg)
str(mpg)
summary(mpg)
names(mpg)
#The variable displ is continuous, and the variables year, cyl, cty and hwy are categorical.

#Data Manipulations
colnames(mpg)[3] = 'EngSize'
colnames(mpg)[9] = 'FuelEff'
names(mpg)

#Visualize relationships
mpg.plot = ggplot(data = mpg, aes(x = EngSize, y = FuelEff))
mpg.plot + geom_point()
mpg.plot = ggplot(data = mpg, aes(x = EngSize, y = FuelEff, color = class))
mpg.plot + geom_point()
mpg.plot + geom_point() + geom_smooth(method = 'lm') + ggtitle('FuelEff by EngSize')

#Remove Outliers
mpg.sub = subset(mpg, subset = class != '2seater')
mpg.sub.plot = ggplot(data = mpg, aes(x = EngSize, y = FuelEff, color = class))
mpg.sub.plot + geom_point() + geom_smooth(method = 'lm') + ggtitle('FuelEff by EngSize without 2esater Cars')
#I do not notice any change inlib the regression line. It seems they are the same.