library(readr)
library(ggplot2)
library(maptools)
library(plyr)
library(readxl)
library(maps)
FirmAsif <- read_csv("~/Documents/GitHub/Data-Science-Class/Final Project/FirmAsif.csv")
View(FirmAsif) #ind2、ind3和hylb应该改成int类型的


# 筛选出在市工商局注册的公司
FirmAsif$city = as.integer(FirmAsif$city) 
firm.sub = subset(FirmAsif, city > 100 & city %% 100 != 0)
sort(unique(firm.sub$city))


# 画基础的中国地图
chinaMap <- readShapePoly('~/Documents/GitHub/Data-Science-Class/Final Project/bou2_4p.shp')
ggplot() + geom_polygon(data = chinaMap, aes(x = long, y = lat, group = group), 
                        fill = "grey95", colour = "grey60", size = 0.25)


# 添加城市名
city_code <- read_excel("~/Documents/GitHub/Data-Science-Class/Final Project/city_code.xls")
summary(city_code)
colnames(city_code) = c('cityName', 'city')
city_code$city = as.integer(city_code$city)
city_code = subset(city_code, city %% 100 == 0)
city_code$city = city_code$city / 100
firm.sub = merge(firm.sub, city_code, by = 'city', sort = F)


# 添加省份名
prov_code = subset(city_code, city %% 100 == 0)
colnames(prov_code) = c('provName', 'prov')
prov_code$prov = prov_code$prov / 100
firm.sub = merge(firm.sub, prov_code, by = 'prov', sort = F)


# 筛选出广东的公司
firm.guangdong = subset(firm.sub, prov == 44)
View(firm.guangdong)



