#Import Datasets

#Model dataset

data_trees=read.csv2(file = "C://Users//Raph//Documents//Group1.csv", header = T, sep = ",")


#Validation dataset


data_trees_validation=read.csv2(file = "C://Users//Raph//Documents//validating_data.csv", header = T, sep = ",")


#Overview:
#Modelling data (13 columns in 150 rows,instances)
#Validation data (12 columns/field in the dataset, 1000 samples/rows)


#str(data_trees_validation)

#head(data_trees)  #tree
#--------------------------
#head(data_trees_validation)

#Check if there are na. No missing value
sum(is.na(data_trees))
sum(is.na(data_trees_validation))

#View features

colnames(data_trees)
# Observation: "X.1", "X", Are extra fields that are same/ 1 is therefore redundant. 
colnames(data_trees_validation)

#The column N is a distinct feature that is not mentioned in the experimental data. Meanwhile, variables X and X1 should not be used because there is no way to validate a model built on them. Also, N should be omitted as it is absent in the model.

#Data Cleaning

library(dplyr)

trees <- data_trees
treesv<-data_trees_validation

#Drop the columns of the dataframe 

trees<-select(trees,-c(X.1, X)) #Dont use these
treesv<-select(treesv,-c(N))#Dont use these

#trees<-subset(trees, select = -c(X.1, X) )
#treesv<-subset(treesv,select = -c(N) )

#Data check
length(trees) 
length(treesv)

colnames(trees)
colnames(treesv)

#Homogenize the column name,volume

colnames(trees)[which(colnames(trees) %in% c("TOTAL_VOLUME") )] <- c("VOLUME")

colnames(trees)
colnames(treesv)

#Summary statistics

library(psych) 

#create summary table. This gives beautiful table(dropped vars, trimmed,mad,range)
#Summary of modelling data

desct<-describe(trees)
desctv<-describe(treesv)

subset(desct, select = -c(vars, trimmed,mad,range))

#Summary of validation data

subset(desctv, select = -c(vars, trimmed,mad,range))

#From tables above, the age of the youngest and oldest tree is 18 and 269 years, whereas the average age for the trees is 66 years. Also, the shortest and tallest tree are 1m and 150m. Also, the smallest and largest diameters are 1cm and 150cm. The average annual rainfall is 328.90mm.

#Data type conversion

trees$BA<-as.double(trees$BA, replace=TRUE)
trees$H<-as.double(trees$H, replace=TRUE)
trees$D<-as.double(trees$D, replace=TRUE)
trees$D<-as.double(trees$D, replace=TRUE)
trees$D<-trees$D*0.01
trees$P0<-as.double(trees$P0, replace=TRUE)
trees$VOLUME<-as.double(trees$VOLUME, replace=TRUE)
trees$SP_GROUP<-as.factor(trees$SP_GROUP)
trees$FOREST_TYPE<-as.factor(trees$FOREST_TYPE)
trees$LAT<-as.double(trees$LAT, replace=TRUE)
trees$LONG<-as.double(trees$LONG, replace=TRUE)

#Validation data: treesv

treesv$BA<-as.double(treesv$BA, replace=TRUE)
treesv$H<-as.double(treesv$H, replace=TRUE)
treesv$D<-as.double(treesv$D, replace=TRUE)
treesv$D<-treesv$D*0.01
treesv$P0<-as.double(treesv$P0, replace=TRUE)
treesv$VOLUME<-as.double(treesv$VOLUME, replace=TRUE)
treesv$SP_GROUP<-as.factor(treesv$SP_GROUP)
treesv$FOREST_TYPE<-as.factor(treesv$FOREST_TYPE)
treesv$LAT<-as.double(treesv$LAT, replace=TRUE)
treesv$LONG<-as.double(treesv$LONG, replace=TRUE)

#Data Visualization 

library(GGally)
trees_sub<-subset(trees, select = -c(SP_GROUP, FOREST_TYPE) )
treesv_sub<-subset(treesv, select = -c(SP_GROUP, FOREST_TYPE) )

#Modelling data
ggpairs(trees_sub)

#Validation data
ggpairs(treesv_sub)

#Volume by species

par(mfrow=c(1,3))
ggplot(trees, aes(x=BA, y=VOLUME))+ geom_point(alpha = 0.5, aes(color=SP_GROUP))
ggplot(trees, aes(x=H, y=VOLUME))+ geom_point(alpha = 0.5, aes(color=SP_GROUP)) 
ggplot(trees, aes(x=D, y=VOLUME))+ geom_point(alpha = 0.5, aes(color=SP_GROUP))

#Scatter: plots against volume

par(mfrow=c(1,3))
plot( trees$BA, trees$VOLUME,xlab = "BA", ylab = 'VOLUME', main="Volume vs. Area")
plot( trees$H, trees$VOLUME,xlab = "H", ylab = 'VOLUME', main="Volume vs. Height")
plot( trees$D, trees$VOLUME,xlab = "D", ylab = 'VOLUME', main="Volume vs. Diameter")

#Histogram: distribution

par(mfrow=c(1,4))

hist(treesv$H, xlab = 'Height', main ='Height')
hist(treesv$D, xlab = 'Diameter',main ='Diameter')
hist(treesv$BA, xlab = 'Area', main ='Area')
hist(treesv$VOLUME, xlab = 'Volume', main = "Volume")

#Boxplot: features by species group

par(mfrow=c(1,4))

boxplot(trees$VOLUME~trees$SP_GROUP, xlab = 'Species', ylab = "Total Volume, m3 ha-1'",main="Volume")
boxplot(trees$BA~trees$SP_GROUP, xlab = 'Species', ylab = "Basal area",main="Area")
boxplot(trees$H~trees$SP_GROUP, xlab = 'Species', ylab = "Height", main="Height")
boxplot(trees$D~trees$SP_GROUP, xlab = 'Species', ylab = "Diameter",main="Diameter")

#Further analysis

library(tidyverse)
# Record of tallest tree

#trees[which.max(trees$H),]
#trees[trees$H==max(trees$H)]
trees%>% slice_max(H)

#Shortest tree
trees%>% slice_min(H)

# largest diameter
trees%>% slice_max(D)

#Smallest diameter
trees%>% slice_min(D)

#Oldest tree
trees%>% slice_max(AGE)
#Youngest
trees%>% slice_min(AGE)

#Highest location
trees%>% slice_max(LONG)

#Lowest location 
trees%>% slice_min(LONG)

# Largest area
trees%>% slice_max(BA)

# Largest volume
trees%>% slice_max(VOLUME)


#Further spread? boxplots
#Distribution of variables over SP_GROUP", "FOREST_TYPE

library(reshape2)
treesz<-melt(subset(trees, select = -c(YEAR)), id=c("SP_GROUP", "FOREST_TYPE") )

#Boxplot by species

ggplot(treesz, aes(SP_GROUP, y = value, fill=SP_GROUP)) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free_y")

#Boxplot by Forest types

ggplot(treesz, aes(SP_GROUP, y = value, fill=FOREST_TYPE)) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free_y")


#Predictive models

#The linear models and algorithms used here are from literature<sup>6</sup>
#0. Model lmForest0

library(MASS)
library(car)
lmForest0<-lm(VOLUME~BA+D+H+P0+ SP_GROUP+FOREST_TYPE+LAT+LONG+AGE, data = trees)

summary(lmForest0)
steplmForest0<-stepAIC(lmForest0, direction = "both")

summary(steplmForest0)
vif(steplmForest0)

#Plot of residuals
plot(trees$VOLUME, residuals(steplmForest0),xlab = 'Volume, m3 ha-1', ylab = 'Residuals')
#plot(trees$VOLUME, residuals(lmForest0),xlab = 'Volume, m3 ha-1', ylab = 'Residuals')

#1. Model lmForest1

lmForest1<-lm(VOLUME~ BA+D+H+SP_GROUP,data = trees) #Drop other variables that are poor and negatively correlated
summary(lmForest1)
vif(lmForest1)

#Plot of residuals
plot(trees$VOLUME, residuals(lmForest1),xlab = 'Volume, m3 ha-1', ylab = 'Residuals')

#Validate with test data: treesv. No validation with this search

VOLUME_Pred<-predict(lmForest1, treesv)
#predicted is VOLUME_Pred

#Add predicted volume as column to validation data(treesv)
treesv["Predicted"]<-VOLUME_Pred

#Comparism table
Volume_comp<-treesv[,c("VOLUME", "Predicted")]
#Volume_comp
#plot actual volume vs predicted volume

plot(treesv$VOLUME,treesv$Predicted, xlab = "Actual Volume", ylab = "Predicted Volume")
#plot(Volume_comp$VOLUME, Volume_comp$Predicted, xlab = "Actual Volume", ylab = "Predicted Volume") #same
#Add trendline
abline(lm(treesv$Predicted~treesv$VOLUME))

#Comparison
library(Metrics)
bias(actual =Volume_comp[,1] ,predicted =Volume_comp[,2] ) 
rmse(actual =Volume_comp[,1] ,predicted =Volume_comp[,2]) 

#II Linear mixed effect model
#2. Model lmForest2

library(lme4)
library(sjPlot)

lmForest2<-lmer(VOLUME~BA+D+H+ (1|SP_GROUP), data = trees)
summary(lmForest2)
tab_model(lmForest2)
vif(lmForest2)

#Plot of residuals
plot(trees$VOLUME, residuals(lmForest2),xlab = 'Volume, m3 ha-1', ylab = 'Residuals')
#Validate with test data: treesv

VOLUME_Pred<-predict(lmForest2, treesv)
#predicted is VOLUME_Pred

#Add predicted volume as column to validation data(treesv)
treesv["Predicted"]<-VOLUME_Pred

#Comparism table
Volume_comp<-treesv[,c("VOLUME", "Predicted")]
#Volume_comp
#plot actual volume vs predicted volume

plot(treesv$VOLUME,treesv$Predicted, xlab = "Actual Volume", ylab = "Predicted Volume")

#Add trendline
abline(lm(treesv$Predicted~treesv$VOLUME))

#Comparison
bias(actual =Volume_comp[,1] ,predicted =Volume_comp[,2] ) 
rmse(actual =Volume_comp[,1] ,predicted =Volume_comp[,2]) 

#III Log model 
#3. Model lmForest3

lmForest3<-lm(log(VOLUME)~ log(BA)+log(D)+log(H)+SP_GROUP,data = trees) #Drop other variables that are poor and negatively correlated
summary(lmForest3)

#steplmForest3<-stepAIC(lmForest3, direction = "both")

summary(lmForest3)
vif(lmForest3)

#Plot of residuals
plot(trees$VOLUME, residuals(lmForest3),xlab = 'Volume, m3 ha-1', ylab = 'Residuals')

#Validate with test data: treesv

VOLUME_Pred<-exp(predict(lmForest3, treesv))
#predicted is VOLUME_Pred


#Add predicted volume as column to validation data(treesv)
treesv["Predicted"]<-VOLUME_Pred

#Comparism table
Volume_comp<-treesv[,c("VOLUME", "Predicted")]
#Volume_comp
#plot actual volume vs predicted volume

plot(treesv$VOLUME,treesv$Predicted, xlab = "Actual Volume", ylab = "Predicted Volume")
#plot(Volume_comp$VOLUME, Volume_comp$Predicted) #same
#Add trendline
abline(lm(treesv$Predicted~treesv$VOLUME))

#Volume comparison table for the first five rows
head(Volume_comp)

#4. Model lmForest4

# No D: to investigate if the supposed multicolinearity between H&D is an issue
lmForest4<-lm(log(VOLUME)~ log(BA)+log(H)+SP_GROUP,data = trees) #Drop other variables that are poor and negatively correlated
summary(lmForest3)

#steplmForest4<-stepAIC(lmForest4, direction = "both")

summary(lmForest4)
vif(lmForest4)

#Plot of residuals
plot(trees$VOLUME, residuals(lmForest4),xlab = 'Volume, m3 ha-1', ylab = 'Residuals')

#Validate with test data: treesv

VOLUME_Pred<-exp(predict(lmForest4, treesv))
#predicted is VOLUME_Pred

#Add predicted volume as column to validation data(treesv)
treesv["Predicted"]<-VOLUME_Pred

#Comparism table
Volume_comp<-treesv[,c("VOLUME", "Predicted")]
#Volume_comp
#plot actual volume vs predicted volume

plot(treesv$VOLUME,treesv$Predicted, xlab = "Actual Volume", ylab = "Predicted Volume")
#plot(Volume_comp$VOLUME, Volume_comp$Predicted) #same
#Add trendline
abline(lm(treesv$Predicted~treesv$VOLUME))

#Comparison
bias(actual =Volume_comp[,1] ,predicted =Volume_comp[,2] ) 
rmse(actual =Volume_comp[,1] ,predicted =Volume_comp[,2]) 

#Comparison
# RMSE Determination based on the best log model

bias(actual =Volume_comp[,1] ,predicted =Volume_comp[,2] ) 
rmse(actual =Volume_comp[,1] ,predicted =Volume_comp[,2]) 

#Volume comparison table for the first five rows
head(Volume_comp)


#Discussion

Links: 
  
  #https://wiki.uef.fi/download/attachments/44434211/Group1.csv?version=1&modificationDate=1479667748000&api=v2
  
  #https://wiki.uef.fi/download/attachments/44434211/validating_data.csv?version=1&modificationDate=1479668013000&api=v2
  
  #https://wiki.uef.fi/display/RESMET/Assignment
  
  #Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer
  
  
  
