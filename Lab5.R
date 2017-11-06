library(stargazer)
library(sjPlot)
library(gmodels)
library(RCurl)
mydata = read.csv("https://raw.githubusercontent.com/TonyHuaHowell/DataScienceClass2017/master/probit_insurance.csv")
mydata$retire = factor(mydata$retire)
mydata$married = factor(mydata$married)
mydata$hisp = factor(mydata$hisp)

#Question 1
mydata$incCat = ifelse(mydata$hhincome < 17, 'Low income', 
                      ifelse(mydata$hhincome >= 17 & mydata$hhincome < 45, 'Lower middle income',
                             ifelse(mydata$hhincome >= 45 & mydata$hhincome < 53, 'Upper middle income','High income')))
mydata$incCat = factor(mydata$incCat)
olsreg = lm(ins ~ retire + married + hisp + incCat, mydata)
logit = glm(ins ~ retire + married + hisp + incCat, mydata, 
            family = binomial(link = 'logit'))
summary(logit)
probit = glm(ins ~ retire + married + hisp + incCat, mydata, 
             family = binomial(link = 'probit'))
summary(probit)
stargazer(list(olsreg, logit, probit), digits = 3, type = 'text')

#Question 2
mydata$EducCat = ifelse(mydata$educyear <= 5, 'Elementary school', 
                        ifelse(mydata$educyear <= 8, 'Middle school', 
                               ifelse(mydata$educyear <= 11, 'Some high school', 
                                      'At least high school degree')))
mydata$EducCat = factor(mydata$EducCat)
olsreg.2 = lm(ins ~ retire + married + hisp + EducCat, mydata)
logit.2 = glm(ins ~ retire + married + hisp + EducCat, mydata, 
            family = binomial(link = 'logit'))
summary(logit.2)
probit.2 = glm(ins ~ retire + married + hisp + EducCat, mydata, 
             family = binomial(link = 'probit'))
summary(probit.2)
stargazer(list(olsreg.2, logit.2, probit.2), digits = 3, type = 'text')

#Question 3
olsreg.3 = lm(ins ~ retire + married + hisp + incCat + EducCat, mydata)
logit.3 = glm(ins ~ retire + married + hisp + incCat + EducCat, mydata, 
              family = binomial(link = 'logit'))
summary(logit.3)
probit.3 = glm(ins ~ retire + married + hisp + incCat + EducCat, mydata, 
               family = binomial(link = 'probit'))
summary(probit.3)
stargazer(list(olsreg.3, logit.3, probit.3), digits = 3, type = 'text')
exp(logit.3$coefficients)
exp(probit.3$coefficients)
plot_model(logit.3, title = "Odds Ratios for Purchase vs Not Purchase Insurance- Logit Model", 
           sort.est = TRUE, show.p = TRUE, show.values = TRUE, 
           vline.color = 'black')
plot_model(probit.3, title = "Odds Ratios for Purchase vs Not Purchase Insurance- Probit Model", 
           sort.est = TRUE, show.p = TRUE, show.values = TRUE, 
           vline.color = 'black')
LogitScalar = mean(dlogis(predict(logit.3, type = "link")))
LogitScalar * coef(logit.3)
ProbitScalar = mean(dnorm(predict(probit.3, type = "link")))
ProbitScalar * coef(probit.3)
MarEff = as.data.frame(cbind(LogitScalar * coef(logit.3), ProbitScalar * coef(probit.3)))
colnames(MarEff) = c("Logit","Probit")
stargazer(MarEff, summary = FALSE, digits = 3, type = 'text')

#Question 4
dat = expand.grid(hisp = levels(factor(mydata$hisp)), married = levels(factor(mydata$married)), 
                  retire = levels(factor(mydata$retire)), incCat = levels(factor(mydata$incCat)), 
                  EducCat = levels(factor(mydata$EducCat)))
fit = predict(logit, newdat = dat, type = "response")
fitp = predict(probit, newdat = dat, type = "response")
dat$fitted.prob.lrm = round(fit, 3)
dat$fitted.prob.pro = round(fitp, 3)
head(dat)
dat[which(dat$hisp == 1 & dat$EducCat == 'Some high school' & dat$incCat == 'Low income'), ]
dat[which(dat$hisp == 0 & dat$EducCat == 'At least high school degree' & dat$incCat == 'High income'), ]

#Question 5
insur.train = mydata[1:1603, ]
insur.test = mydata[1604:3206, ]
insur.train.fit = glm(ins ~ retire + married + hisp + hhincome + EducCat, 
                      family = binomial(link = 'logit'), data = insur.train)
summary(insur.train.fit)
insur.test$prob.ins = insur.train.fit$fitted.values
summary(insur.test$prob.ins)
insur.test$prob.insur = predict.glm(insur.train.fit, insur.test)
insur.test$prob.insur = plogis(insur.test$prob.insur)
summary(as.numeric(insur.test$prob.insur))
insur.test$insur.hat <- ifelse(insur.test$prob.insur >= 0.5, 1, 0)
head(insur.test)
insur.test$insur = ifelse(insur.test$ins == 1, 'Insured', 'Uninsured')
insur.test$insur.hat = ifelse(insur.test$insur.hat == 1, 'Insured', 'Uninsured')
CrossTable(insur.test$insur, insur.test$insur.hat, prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, 
           dnn = c('Actual', 'Predicted'))
#The performance of our model is still pretty poor.
