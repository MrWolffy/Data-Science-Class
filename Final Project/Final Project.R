library(readr)
library(ggplot2)
library(maptools)
library(plyr)
library(readxl)
library(maps)
FirmAsif <- read_csv("~/Documents/GitHub/Data-Science-Class/Final Project/FirmAsif.csv")
#View(FirmAsif)
#summary(FirmAsif)


# 修改数据类型
FirmAsif$ind2 = as.integer(FirmAsif$ind2)
FirmAsif$ind3 = as.integer(FirmAsif$ind3)
FirmAsif$hylb = as.integer(FirmAsif$hylb)
FirmAsif$ownership = as.factor(FirmAsif$ownership)
FirmAsif$export = as.factor(FirmAsif$export)
FirmAsif$loan = as.factor(FirmAsif$loan)
FirmAsif$SEZ = as.factor(FirmAsif$SEZ)
FirmAsif$SEZcity = as.factor(FirmAsif$SEZcity)
FirmAsif$SEZnat = as.factor(FirmAsif$SEZnat)
FirmAsif$SEZprov = as.factor(FirmAsif$SEZprov)
FirmAsif$length = as.integer(FirmAsif$length)


# 筛选出在市工商局注册的公司
FirmAsif$city = as.integer(FirmAsif$city) 
FirmAsif = subset(FirmAsif, city > 100 & city %% 100 != 0 
                  | city == 1100 | city == 1200 | city == 3100 | city == 5000)
#sort(unique(FirmAsif$city))
# 原来的代码会把直辖市的数据删掉


# 添加城市名
city_code <- read_excel("~/Documents/GitHub/Data-Science-Class/Final Project/city_code.xls")
#summary(city_code)
colnames(city_code) = c('cityName', 'city')
city_code$city = as.integer(city_code$city)
city_code = subset(city_code, city %% 100 == 0)
city_code$city = city_code$city / 100
FirmAsif = merge(FirmAsif, city_code, by = 'city')


# 添加省份名
prov_code = subset(city_code, city %% 100 == 0)
colnames(prov_code) = c('provName', 'prov')
prov_code$prov = prov_code$prov / 100
FirmAsif = merge(FirmAsif, prov_code, by = 'prov')


# 筛选出广东的公司
firm.sample = subset(FirmAsif, prov == c(44,11,12,31,32,33,37,50,51,42,35))
#firm.guangzhou = subset(firm.sample, city == 4401)
#View(firm.sample)
# 筛选之后先进行总的回归


# 按照城市汇总
firm.cityAggr = ddply(firm.sample, .(cityName), summarize, 
                      tfplp = mean(tfplp, na.rm = T))
firm.cityAggr = merge(firm.cityAggr, count(firm.sample, vars = "cityName"), 
                      by = "cityName")
ggplot(firm.cityAggr, aes(freq, tfplp)) + 
    geom_point() + geom_smooth(method = 'lm')







# 画基础的中国地图
chinaMap <- readShapePoly('~/Documents/GitHub/Data-Science-Class/Final Project/bou2_4p.shp')
ggplot() + geom_polygon(data = chinaMap, aes(x = long, y = lat, group = group), 
                        fill = "grey95", colour = "grey60", size = 0.25) +
    coord_map()
# 画填色的中国地图
chinaMap$NAME = iconv(chinaMap$NAME, from = "GBK") #这一步只能做一次！
tmp = chinaMap$NAME
chinaMap = fortify(chinaMap)
chinaMap = transform(chinaMap, id = iconv(id, from = 'GBK'), group = iconv(group, from = 'GBK'))
chinaMap$id = as.integer(chinaMap$id)
chinaMap$provName = tmp[chinaMap$id + 1]
# 能够画单个省地图的代码
#ggplot() + geom_polygon(data = subset(chinaMap, provName == '辽宁省'), 
#                        aes(x = long, y = lat, group = group), 
#                        fill = "grey95", colour = "grey60", size = 0.25) +
#    coord_map()
# 使用公司数量填色
firm.provAggr = ddply(FirmAsif, .(provName), summarize, 
                      tfplp = mean(tfplp, na.rm = T))
firm.provAggr = merge(firm.provAggr, count(FirmAsif, vars = "provName"), 
                      by = "provName")
chinaMap = join(chinaMap, firm.provAggr, by = "provName")
colnames(chinaMap)[10] = 'firmNum'
qplot(long, lat, data = chinaMap, group = group, fill = firmNum,
      geom = "polygon") + scale_fill_gradient(low = "#56B1F7", high = "#132B43")
#ggplot() + geom_polygon(data = chinaMap, aes(x = long, y = lat, group = group, fill = freq), 
#                        colour = "grey60", size = 0.25) +
#    coord_map() + scale_fill_gradient(low = "#56B1F7", high = "#132B43")


# 构造localization变量
#firm.sample$city = as.factor(firm.sample$city)
temp = ddply(firm.sample, .(cityName), summarize, 
             cyrsSum = sum(cyrs, na.rm = T))
#firm.cityAggr = merge(firm.cityAggr, temp, by = 'cityName')
firm.sample = merge(temp, firm.sample, by = 'cityName')
firm.sample$localization = log(firm.sample$cyrsSum - firm.sample$cyrs + 1)
ggplot(firm.sample, aes(x = localization)) + 
    geom_density(aes(fill = SEZ), alpha = 0.2)
ggplot(firm.sample, aes(x = localization)) + geom_histogram(aes(fill = city))

#t检验

# select two subsets: SEZ==1 \ SEZ==0
firm.outSEZ = subset(firm.sample, SEZ==0)
firm.inSEZ = subset(firm.sample, SEZ==1)

tfplp.inSEZ = firm.inSEZ$tfplp
tfplp.outSEZ = firm.outSEZ$tfplp


# draw a graph
df_for_bar_in_out <- data.frame(type = c("inSEZ", "outSEZ"), 
                                meantfplp = c(mean(tfplp.inSEZ, na.rm=T), mean(tfplp.outSEZ, na.rm=T)))
ggplot(data=df_for_bar_in_out, aes(type, meantfplp)) + geom_col()



# t-test for tfplp is significant
tfplp.inSEZ = firm.inSEZ$tfplp
tfplp.outSEZ = firm.outSEZ$tfplp
t.test(tfplp.outSEZ, tfplp.inSEZ, var.equal = TRUE, alternative = "less")


#去除过大或过小的公司影响
summary(firm.sample$cyrs)
cyrs_mean <- mean(firm.sample$cyrs,na.rm = 1)
cyrs_sd <- sd(firm.sample$cyrs,na.rm = 1)

firm.sample.subset <- subset(firm.sample, cyrs >0 &cyrs <= cyrs_mean + 2.5 * cyrs_sd)



#产值过大
summary(firm.sample.subset$gyzczxjxgd)
gyzczxjxgd_mean <- mean(firm.sample$gyzczxjxgd,na.rm = 1)
gyzczxjxgd_sd <- sd(firm.sample$gyzczxjxgd,na.rm = 1)
firm.sample.subset <- subset(firm.sample.subset, gyzczxjxgd >0 &gyzczxjxgd <= gyzczxjxgd_mean + 2.5 * gyzczxjxgd_sd)

#生成人均产值
firm.sample.subset$pro_pc <- firm.sample.subset$gyzczxjxgd/firm.sample.subset$cyrs

#生成新产品销售占比
firm.sample.subset <- subset(firm.sample.subset, firm.sample.subset$xcpcz >0)
firm.sample.subset$newsalesrat <- firm.sample.subset$xcpcz/firm.sample.subset$gyzczxjxgd

#生成补贴虚拟变量
firm.sample.subset$bt_dummy <- as.factor(ifelse(firm.sample.subset$btsr <= 0,0,1))

#回归
firm.sample.subset <- subset(firm.sample.subset, firm.sample.subset$tfplp >= 0)

firm.sample.subset$pro_pcsd <- scale(firm.sample.subset$pro_pc)
firm.sample.subset$newsalesratsd <- scale(firm.sample.subset$newsalesrat)
firm.sample.subset$localizationsd <- scale(firm.sample.subset$localization)


tfp.lm <- lm(tfplp~pro_pcsd+bt_dummy+SEZ+newsalesratsd,firm.sample.subset)

summary(tfp.lm)

