library(readr)
library(ggplot2)
library(maptools)
library(plyr)
FirmAsif <- read_csv("~/Documents/GitHub/Data-Science-Class/Final Project/FirmAsif.csv")
View(FirmAsif)


# 筛选出在市工商局注册的公司
FirmAsif$city = as.integer(FirmAsif$city) 
firm.sub = subset(FirmAsif, city > 100 & city %% 100 != 0)
unique(firm.sub$city)


# 画基础的中国地图
chinaMap <- readShapePoly('~/Documents/GitHub/Data-Science-Class/Final Project/bou2_4p.shp')
ggplot() + geom_polygon(data = chinaMap, aes(x = long, y = lat, group = group), 
                        fill = "grey95", colour = "grey60", size = 0.25)


