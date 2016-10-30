library('caret')
library('dplyr')
library('VIM')
library('mice')
library('Metrics')
library('xgboost')
library('randomForest')
library('Boruta')
library('scales')

train<-read.csv("Train_xyqdbho.csv",stringsAsFactors = T)
test<-read.csv("Test_pyI9Owa.csv",stringsAsFactors = T)

train$Date<-as.Date(train$Date,"%d-%m-%Y")
test$Date<-as.Date(test$Date,"%d-%m-%Y")

train$Location_Type<-as.factor(train$Location_Type)
test$Location_Type<-as.factor(test$Location_Type)

all<-bind_rows(train, test)
str(all)

mice_plot <- aggr(all, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(all), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


all$Average_Atmospheric_Pressure[is.na(all$Average_Atmospheric_Pressure)] <- median(all$Average_Atmospheric_Pressure, na.rm=TRUE)
all$Max_Atmospheric_Pressure[is.na(all$Max_Atmospheric_Pressure)] <- median(all$Max_Atmospheric_Pressure, na.rm=TRUE)
all$Min_Atmospheric_Pressure[is.na(all$Min_Atmospheric_Pressure)] <- median(all$Min_Atmospheric_Pressure, na.rm=TRUE)
all$Min_Ambient_Pollution[is.na(all$Min_Ambient_Pollution)] <- median(all$Min_Ambient_Pollution, na.rm=TRUE)
all$Max_Ambient_Pollution[is.na(all$Max_Ambient_Pollution)] <- median(all$Max_Ambient_Pollution, na.rm=TRUE)
all$Var1[is.na(all$Var1)] <- median(all$Var1, na.rm=TRUE)
all$Max_Breeze_Speed[is.na(all$Max_Breeze_Speed)] <- median(all$Max_Breeze_Speed, na.rm=TRUE)
all$Min_Breeze_Speed[is.na(all$Min_Breeze_Speed)] <- median(all$Min_Breeze_Speed, na.rm=TRUE)
all$Direction_Of_Wind[is.na(all$Direction_Of_Wind)] <- median(all$Direction_Of_Wind, na.rm=TRUE)
all$Average_Breeze_Speed[is.na(all$Average_Breeze_Speed)] <- median(all$Average_Breeze_Speed, na.rm=TRUE)
all$Average_Moisture_In_Park[is.na(all$Average_Moisture_In_Park)] <- median(all$Average_Moisture_In_Park, na.rm=TRUE)
all$Max_Moisture_In_Park[is.na(all$Max_Moisture_In_Park)] <- median(all$Max_Moisture_In_Park, na.rm=TRUE)
all$Min_Moisture_In_Park[is.na(all$Min_Moisture_In_Park)] <- median(all$Min_Moisture_In_Park, na.rm=TRUE)

all$D<-as.numeric(format(all$Date, "%d"))
all$M<-as.numeric(format(all$Date, "%m"))
all$Y<-as.numeric(format(all$Date, "%Y"))

#Australian Weather
#Summer	1 December	28/29 February
#Autumn	1 March	31 May
#Winter	1 June	31 August
#Spring	1 September	30 November

all$Season<-as.factor('N')

all$Season <- ifelse(all$M==12,'S',all$Season)
all$Season <- ifelse(all$M==1,'S',all$Season)
all$Season <- ifelse(all$M==2,'S',all$Season)
all$Season <- ifelse(all$M==3,'A',all$Season)
all$Season <- ifelse(all$M==4,'A',all$Season)
all$Season <- ifelse(all$M==5,'A',all$Season)
all$Season <- ifelse(all$M==6,'W',all$Season)
all$Season <- ifelse(all$M==7,'W',all$Season)
all$Season <- ifelse(all$M==8,'W',all$Season)
all$Season <- ifelse(all$M==9,'P',all$Season)
all$Season <- ifelse(all$M==10,'P',all$Season)
all$Season <- ifelse(all$M==11,'P',all$Season)

all$Season<-as.factor(all$Season)


all$Breeze_Speed_diff<-all$Max_Breeze_Speed-all$Min_Breeze_Speed 
all$Atmospheric_Pressure_diff<-all$Max_Atmospheric_Pressure-all$Min_Atmospheric_Pressure 
all$Ambient_Pollution_diff<-all$Max_Ambient_Pollution-all$Min_Ambient_Pollution 
all$Moisture_In_Park_diff<-all$Max_Moisture_In_Park-all$Min_Moisture_In_Park 

all$Park_ID<-as.factor(all$Park_ID)

all$Day <- weekdays(as.Date(all$Date))
all$Day<-as.factor(all$Day)


all$Direction<-as.factor('NULL')

all$Direction<-cut(all$Direction_Of_Wind, c(0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360),labels = c('S1','SW','W','NW','N','NE','E','SE','S2'))
levels(all$Direction)<-c("S", "SW", "W",  "NW", "N",  "NE", "E",  "SE", "S")


all$Park_ID<-as.numeric(all$Park_ID)
all$Location_Type<-as.numeric(all$Location_Type)

#Treating outliers
all$Min_Moisture_In_Park <- squish(all$Min_Moisture_In_Park, round(quantile(all$Min_Moisture_In_Park, c(.10, .90))))
all$Average_Breeze_Speed <- squish(all$Average_Breeze_Speed, round(quantile(all$Average_Breeze_Speed, c(.10, .90))))
all$var1 <- squish(all$Var1, round(quantile(all$Var1, c(.10, .90))))
all$Max_Ambient_Pollution <- squish(all$Max_Ambient_Pollution, round(quantile(all$Max_Ambient_Pollution, c(.10, .90))))
all$Max_Atmospheric_Pressure <- squish(all$Max_Atmospheric_Pressure, round(quantile(all$Max_Atmospheric_Pressure, c(.10, .90))))



tr<-all[1:114539,]
te<-all[114540:153959,]

outcomeName <- c('Footfall')
predictors <- c('M','Direction','Season','Min_Moisture_In_Park','Average_Breeze_Speed','D','Park_ID','var1','Max_Ambient_Pollution','Max_Atmospheric_Pressure')


all$M<-as.factor(all$M)
all$D<-as.factor(all$D)
all$Y<-as.factor(all$Y)

a<-c('Footfall','Min_Moisture_In_Park','ID','Date','var1')
predictors <- names(tr)[!names(tr) %in% a]
model_gbm <- train(tr[,predictors], tr[,outcomeName], method='gbm')

predictions <- predict(object=model_gbm, te[,predictors])

soln<-te[,c('ID','Footfall')]
soln$Footfall<-predictions
write.csv(soln, file = 'soln.csv', row.names = F)
#119
