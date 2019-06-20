install.packages("ggplot2")
install.packages("sqldf")
install.packages("gridExtra")
install.packages("corrgram")
install.packages("corrplot")
install.packages("caret")
install.packages("usdm")
library(ggplot2)
library(sqldf)
library(gridExtra)
library(corrgram)
library(corrplot)
library(caret)
library(usdm)
library(rpart)
library(MASS)

rm(list = ls())
setwd('path')
getwd()

day_data = read.csv("day.csv")
str(day_data)

#Exploring the data
day_data$registered = NULL
day_data$casual = NULL
day_data$dteday = NULL
day_data$instant =NULL

day_data$season = as.factor(day_data$season)
day_data$yr = as.factor(day_data$yr)
day_data$holiday = as.factor(day_data$holiday)
day_data$workingday = as.factor(day_data$workingday)
day_data$weathersit = as.factor(day_data$weathersit)

str(day_data)

#Fetching average count of bikes by season
season_summary = sqldf('select  season, weekday, avg(cnt) as count from day_data group by season, weekday')
ggplot(day_data, aes(x=weekday, y=count, color=season))+
  geom_point(data = season_summary, aes(group = season))+
  geom_line(data = season_summary, aes(group = season))+
  ggtitle("Bikes Rent By Season")+ 
  scale_colour_hue('Season',breaks = levels(day_data$season), labels=c('spring', 'summer', 'fall', 'winter'))


#Fetching average count of bikes by weather situation

weather_summary = sqldf('select weathersit, weekday, avg(cnt) as count from day_data group by weathersit, weekday')
ggplot(day_data, aes(x=weekday, y=count, color=weathersit))+
  geom_point(data = weather_summary, aes(group = weathersit))+
  geom_line(data = weather_summary, aes(group = weathersit))+
  ggtitle("Bikes Rent By Weather Situation")+ 
  scale_colour_hue('Weather',breaks = levels(day_data$weathersit), labels=c('1- Good',
                                                                            '2 - Bad',
                                                                            '3 - Worse'))


#Missing Value Analysis
MissingValue = data.frame(apply(day_data, 2, function(f){sum(is.na(f))}))

##Outlier Analysis
NumericDataIndex = sapply(day_data, is.numeric)
NumericData = day_data[,NumericDataIndex]
NumericData = NumericData[,-7]
ColumnNames = colnames(NumericData)

for(i in 1:length(ColumnNames))
{
  assign(paste0("boxplot",i), ggplot(aes_string(y = (ColumnNames[i]), x = "cnt"), 
                                           data = subset(day_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=ColumnNames[i],x="cnt")+
           ggtitle(paste("Box plot of Count for",ColumnNames[i])))
}

gridExtra::grid.arrange(boxplot1,boxplot2,boxplot3,ncol=3)
gridExtra::grid.arrange(boxplot4,boxplot5,boxplot6,ncol=3)


#Removing Outliers
for(i in ColumnNames)
{
  print(i)
  val = day_data[,i][day_data[,i] %in% boxplot.stats(day_data[,i])$cnt]
  #print(length(val))
  day_data = day_data[which(!day_data[,i] %in% val),]
}

##Correlation Analysis
corrplot(cor(NumericData), method = 'color', addCoef.col = "grey")

#Dimension Reduction
day_data_deleted = subset(day_data,select = -c(temp))
str(day_data_deleted)

#Check for MultiCollinearity
NumericIndex = sapply(day_data_deleted, is.numeric)
NumericData_Coll = day_data_deleted[,NumericIndex]
NumericData_Coll = NumericData_Coll[,-6]
vifcor(NumericData_Coll, th = 0.9)

#Splitting the data into train and test
set.seed(1234)
Index = createDataPartition(day_data_deleted$cnt, p = .80, list = FALSE)
TrainData = day_data_deleted[Index,]
TestData = day_data_deleted[-Index,]

##########################################Model Development##########################################

##Linear Regression Model
#Train Data
regTrain = lm(cnt ~ ., data = TrainData)
summary(regTrain)
plot(regTrain)

#Test Data
regpredict = predict(regTrain, TestData[,1:10])
summary(regpredict)

##Performance Evaluation
#Mean Absolute Percentage Error
MAPE = function(actual,predicted){
  mean(abs((actual-predicted)/actual))
}

MAPE(TestData[,11], regpredict) 

#Error Rate for Linear Regression: 19.46%
#Accuracy for Linear Regression: 80.54%

##Decision Tree
#Train Data
fit = rpart(cnt ~ ., data = TrainData, method = "anova")

#Test Data
regpredict_DT = predict(fit, TestData[,-11])
summary(regpredict_DT)

#Performance Evaluation
MAPE(TestData[,11], regpredict_DT)

#Error Rate of Decision Tree: 27.03%
#Accuracy for Decision Tree: 72.97%
