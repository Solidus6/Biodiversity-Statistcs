# Biodiversity-Statistcs
Linear regression, General linear models and General additive model r code

##   Usefull librarys   ##

library(car)
library(ResourceSelection)
library(ggplot2)
library(gam)
library(mgcv)
library(readxl)
library(gvlma)
library(randomForest)
library(pscl)
library(finalfit)
library(dplyr)
library(raster)

##  Read Data.Frames   ##

Data1km <- read_excel("filepath")
View(Dados1km)

Data5km <- read_excel("filepath")
View(Dados5km)

Data50km <- read_excel("filepath")
View(Data50km)

ST.1km <- stack(list.files(path="filepath",pattern='tif',full.names=TRUE))
ST.5km <- stack(list.files(path="filepath",pattern='tif',full.names=TRUE))
ST.50km <- stack(list.files(path="filepath",pattern='tif',full.names=TRUE))

##  Separate the data.frames by taxa/country and remove the taxa/country collumn  ##

AnfSw1km <- subset(Data1km, Taxa=='Amphibian'& Country=='Sweden')
AnfSw1km <- AnfSw1km[-9]
AnfSw1km <- AnfSw1km[-8]
View(AnfSw1km)

AnfSw5km <- subset(Data5km, Taxa=='Amphibian'& Country=='Sweden')
AnfSw5km <- AnfSw5km[-9]
AnfSw5km <- AnfSw5km[-8]
View(AnfSw5km)

AnfSw50km <- subset(Data50km, Taxa=='Amphibian'& Country=='Sweden')
AnfSw50km <- AnfSw50km[-9]
AnfSw50km <- AnfSw50km[-8]
View(AnfSw50km)

##transform data(log)
AnfSw1km_log <- AnfSw1km
AnfSw1km_log[,c(2:7)] <- log(AnfSw1km_log[,c(2:7)]+1)
AnfSw1km_log
AnfSw5km_log <- AnfSw5km
AnfSw5km_log[,c(2:7)] <- log(AnfSw5km_log[,c(2:7)]+1)
AnfSw5km_log
AnfSw50km_log <- AnfSw50km
AnfSw50km_log[,c(2:7)] <- log(AnfSw50km_log[,c(2:7)]+1)
AnfSw50km_log

## Linear_Regression_Anfphibian (transformed_data) ##

#1km
LR.AnfSw1km_log<- lm(Richness~., data=AnfSw1km_log)
summary(LR.AnfSw1km_log)
plot(LR.AnfSw1km_log)

#5km
LR.AnfSw5km_log<- lm(Richness~., data=AnfSw5km_log)
summary(LR.AnfSw5km_log)
plot(LR.AnfSw5km_log)

#50km
LR.AnfSw50km_log<- lm(Richness~., data=AnfSw50km_log)
summary(LR.AnfSw50km_log)
plot(LR.AnfSw50km_log)

# Residuals must be normal

par(mfrow=c(1,1))
hist(LR.AnfSw1km_log$residuals)
hist(LR.AnfSw5km_log$residuals)
hist(LR.AnfSw50km_log$residuals)

## GLM's (log data) ##

par(mfrow=c(2,2))
GLM.AnfSw1km_log<-glm(Richness~.,family=poisson,data=AnfSw1km_log)
GLM.AnfSw5km_log<-glm(Richness~.,family=poisson,data=AnfSw5km_log)
GLM.AnfSw50km_log<-glm(Richness~.,family=poisson,data=AnfSw50km_log)

summary(GLM.AnfSw1km_log)
plot(GLM.AnfSw1km_log)
pR2(GLM.AnfSw1km_log)

summary(GLM.AnfSw5km_log)
plot(GLM.AnfSw5km_log)
pR2(GLM.AnfSw5km_log)

summary(GLM.AnfSw50km_log)
plot(GLM.AnfSw50km_log)
pR2(GLM.AnfSw50km_log)

residualPlots(GLM.AnfSw1km_log)
residualPlots(GLM.AnfSw5km_log)
residualPlots(GLM.AnfSw50km_log)

## Residuals must be normal ##
par(mfrow=c(1,1))
hist(GLM.AnfSw1km_log$residuals)
hist(GLM.AnfSw5km_log$residuals)
hist(GLM.AnfSw50km_log$residuals)

## check if predictors are significant ##

# AnfSw1km
GLM.AnfSw1km_log.anova<-anova(GLM.AnfSw1km_log)
print(GLM.AnfSw1km_log.anova)

y_pred <- predict(GLM.AnfSw1km_log, data.frame(AnfSw1km))
ggplot(AnfSw1km, aes(x=AnfSw1km$Richness, y=y_pred)) + 
  geom_point() + 
  geom_smooth(method = "glm")

# AnfSw5km
GLM.AnfSw5km_log.anova<-anova(GLM.AnfSw5km_log)
print(GLM.AnfSw5km_log.anova)

y_pred <- predict(GLM.AnfSw5km_log, data.frame(AnfSw5km))
ggplot(AnfSw5km, aes(x=AnfSw5km$Richness, y=y_pred)) + 
  geom_point() + 
  geom_smooth(method = "glm")

# AnfSw50km
GLM.AnfSw50km_log.anova<-anova(GLM.AnfSw50km_log)
print(GLM.AnfSw50km_log.anova)

y_pred <- predict(GLM.AnfSw50km_log, data.frame(AnfSw50km))
ggplot(AnfSw50km, aes(x=AnfSw50km$Richness, y=y_pred)) + 
  geom_point() + 
  geom_smooth(method = "glm")

### GAM without smooth ### 

GAM.AnfSw1km <- mgcv::gam(Richness ~ Albedomean+LSTmean+NDVImean+NDWImean+TSFmean, data = AnfSw1km)
summary(GAM.AnfSw1km)

gam.check(GAM.AnfSw1km, k.Anf = 1000)

GAM.AnfSw5km <- mgcv::gam(Richness ~ Albedomean+LSTmean+NDVImean+NDWImean+TSFmean, family='poisson', data = AnfSw5km)
summary(GAM.AnfSw5km)

gam.check(GAM.AnfSw5km, k.Anf = 1000)

GAM.AnfSw50km <- mgcv::gam(Richness ~ Albedomean+LSTmean+NDVImean+NDWImean+TSFmean, family='poisson', data = AnfSw50km)
summary(GAM.AnfSw50km)

gam.check(GAM.AnfSw50km, k.Anf = 1000)

### GAM with smooth terms

## Cubic splines
GAM2.AnfSw1km <- mgcv::gam(Richness ~ s(Albedomean)+s(LSTmean)+s(NDVImean)+s(NDWImean)+(TSFmean), family='poisson',data = AnfSw1km)
summary(GAM2.AnfSw1km)
plot(GAM2.AnfSw1km, pages=1, residuals=T, pch=19, cex=0.25,
     scheme=1, col='#FF8000', shade=T,shade.col='gray90')

gam.check(GAM2.AnfSw1km, k.Anf = 1000)

GAM2.AnfSw5km <- mgcv::gam(Richness ~ s(Albedomean)+s(LSTmean)+s(NDVImean)+s(NDWImean)+s(TSFmean), family='poisson',data = AnfSw5km)
summary(GAM2.AnfSw5km)
plot(GAM2.AnfSw5km, pages=1, residuals=T, pch=19, cex=0.25,
     scheme=1, col='#FF8000', shade=T,shade.col='gray90')

gam.check(GAM2.AnfSw5km, k.Anf = 1000)

GAM2.AnfSw50km <- mgcv::gam(Richness ~ s(Albedomean)+s(LSTmean)+s(NDVImean)+s(NDWImean)+s(TSFmean), family='poisson', data = AnfSw50km)
summary(GAM2.AnfSw50km)
plot(GAM2.AnfSw50km, pages=1, residuals=T, pch=19, cex=0.25,
     scheme=1, col='#FF8000', shade=T,shade.col='gray90')

gam.check(GAM2.AnfSw50km, k.Anf = 1000)

## Tensor 
GAM3.AnfSw1km <- mgcv::gam(Richness ~ te(Albedomean)+te(LSTmean)+te(NDVImean)+te(NDWImean)+(TSFmean), family='poisson', data = AnfSw1km)
summary(GAM3.AnfSw1km)
plot(GAM3.AnfSw1km, pages=1, residuals=T, pch=19, cex=0.25,
     scheme=1, col='#FF8000', shade=T,shade.col='gray90')

gam.check(GAM3.AnfSw1km, k.Anf = 1000)

GAM3.AnfSw5km <- mgcv::gam(Richness ~ te(Albedomean)+te(LSTmean)+te(NDVImean)+te(NDWImean)+te(TSFmean)+te(RCmean), family='poisson', data = AnfSw5km)
summary(GAM3.AnfSw5km)
plot(GAM3.AnfSw5km, pages=1, residuals=T, pch=19, cex=0.25,
     scheme=1, col='#FF8000', shade=T,shade.col='gray90')

gam.check(GAM3.AnfSw5km, k.Anf = 1000)

GAM3.AnfSw50km <- mgcv::gam(Richness ~ te(Albedomean)+te(LSTmean)+te(NDVImean)+te(NDWImean)+te(TSFmean), family='poisson', data = AnfSw50km)
summary(GAM3.AnfSw50km)
plot(GAM3.AnfSw50km, pages=1, residuals=T, pch=19, cex=0.25,
     scheme=1, col='#FF8000', shade=T,shade.col='gray90')

gam.check(GAM3.AnfSw50km, k.Anf = 1000)
