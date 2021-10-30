#---------------------------------------------------------------------------------------------------------
# This file contains the codes to read and analyze the meteorological data for
# estimation of the parameters for the optimization and statistical models.

# Note: make the current directory ./paper as the working directory
#---------------------------------------------------------------------------------------------------------
library("FieldReadiness")
require(discretizeGaussian)
require(httr)
require(jsonlite)
require(RDaisy)
require(processx)
require(data.table)

#---------------------------------------------------------------------------------------------------------
# Specifying the time slot of growing cycle by user
# 2020
t_count <-as.integer(readline(prompt="Enter the day: " ))  # t_count = 1,2,3,4,5.

# t_count=1 shows that we are at 12nd of September,2016 (First day of scheduling).
# In this case, field readiness will be determined from 13th of Sep. 2016 to 26th Sep. 2016.
# .
# .
# .
# t_count=5 shows that we are at 16th of September, 2016.
# In this case, field readiness will be determined from 17th of Sep. to 30rd Sep. 2016.

# The MDP model predicts the field readiness for 14 days ahead from t.
#---------------------------------------------------------------------------------------------------------
# Reading the weather data between the period 2012-2021 for a real farm in Foulum (2012-2021)

# Reading the historical weather data from 2012 to 2021
FoulumDgnMetData_1997_2021 <- read.table(file = "C:/Users/au520279/Desktop/mdpTillage_Master/FoulumDøgnMetData_1997-2021_ver02.csv", sep = ",", header = TRUE)
FoulumDgnMetData_1997_2021 <- subset(FoulumDgnMetData_1997_2021, select = c(Dato, Temp, TempMin,	TempMax, WV2, Prec.B, W.m2))

Numeric_dates=as.matrix(as.Date(FoulumDgnMetData_1997_2021[,1], format = "%m/%d/%Y"))

# 2020
Num_days_2012 <- as.Date(c("1997-07-31", "2011-12-31"))
Num_days_2020 <- as.Date(c("1997-07-31", "2020-09-12"))

Start_cell_Aug <- as.numeric(Num_days_2012[2]-Num_days_2012[1]+1)
End_cell_Aug <- as.numeric(Num_days_2020[2]-Num_days_2020[1])

FoulumHist1MetData_2012_2020 <- FoulumDgnMetData_1997_2021[Start_cell_Aug:End_cell_Aug,]  # metData

# Omitting the non-value and missing data
FoulumHist1MetData_2012_2020 <- na.omit(FoulumHist1MetData_2012_2020)

# Reading the historical weather data from 13th to 30rd September,2020
Num_days_1 <- as.Date(c("1997-07-31", "2020-09-12"))
Num_days_2 <- as.Date(c("1997-07-31", "2020-09-30"))

Start_cell_Sep <- as.numeric(Num_days_1[2]-Num_days_1[1]+1)
End_cell_Sep <- as.numeric(Num_days_2[2]-Num_days_2[1])

FoulumHist2MetData_2012_2020 <- FoulumDgnMetData_1997_2021[Start_cell_Sep:End_cell_Sep,]
FoulumHistMetData_2012_2020 <- rbind(FoulumHist1MetData_2012_2020, FoulumHist2MetData_2012_2020)

# Reading the prediction of weather data from 13th to 30rd of September,2020
FoulumPredMetData_Sep.2020 <- read.table(file = "C:/Users/au520279/Desktop/mdpTillage_Master/WeatherForecastData_2020.csv", sep = ",", header = TRUE)
#------------------------------------------------------------------------------------------------------------------------------
# Setting MDP parameters
param<-setParam()

# Setting GSSM parameters
mod<-setModel(param)

#---------------------------------------------------------------------------------------------------------
# Calculating the average soil-water content from the period 2012 to 14 days ahead from time t in 2016 by running Dasiy App.

WeatherData <- WeatherInfo(t_count, param, FoulumHist1MetData_2012_2020, FoulumHist2MetData_2012_2020, FoulumPredMetData_Sep.2020)
DWF_File(WeatherData)
DaisyOutputs <- Daisy(WeatherData)
MeanDaisyOutputs<- Mean_Daisy(DaisyOutputs)

MeanDaisyOutputs$Mean_SoilWaterContent[2] <- MeanDaisyOutputs$Mean_SoilWaterContent[2]*100  # %

#------------------------------------------------------------------------------------------------------------------------------
# Reading the weather data and estimate parameters for distributions related to the weather info.

metData <- FoulumHist1MetData_2012_2020   # 2020

# Selecting the weather data for two weeks in September 2012 from dataset
days_2012 <- seq(from=as.Date("2012-09-13"), to=as.Date("2012-09-30"), by='days')
days_2012 <- format(as.Date(seq(from=as.Date(days_2012[1]), to=as.Date(days_2012[length(days_2012)]), by='days')), "%m/%d/%Y")
days_2012 <- gsub('(?<=\\b|-)0', '', days_2012, perl=TRUE)

wedData3 <- as.data.frame( matrix(nrow = length(days_2012), ncol = 7))
colnames(wedData3) <- c("Dato", "Temp", "TempMin", "TempMax", "Prec.B", "W.m2", "WindSpeed")

for (i in seq_along(days_2012)){
  wedDataFilter<- metData[grep(days_2012[i], metData$Dato),]
  wedData3$Dato[i] <- paste(days_2012[i])
  wedData3$Temp[i] <- wedDataFilter$Temp
  wedData3$TempMin[i] <- wedDataFilter$TempMin
  wedData3$TempMax[i] <- wedDataFilter$TempMax
  wedData3$Prec.B[i] <- wedDataFilter$Prec.B
  wedData3$W.m2[i] <- wedDataFilter$W.m2
  wedData3$WindSpeed[i] <- wedDataFilter$WV2
}

# Selecting the weather data for two weeks in September 2013 from dataset
days_2013 <- seq(from=as.Date("2013-09-13"), to=as.Date("2013-09-30"), by='days')
days_2013 <- format(as.Date(seq(from=as.Date(days_2013[1]), to=as.Date(days_2013[length(days_2013)]), by='days')), "%m/%d/%Y")
days_2013 <- gsub('(?<=\\b|-)0', '', days_2013, perl=TRUE)

wedData4 <-  as.data.frame( matrix(nrow = length(days_2013), ncol = 7))
colnames(wedData4) <- c("Dato", "Temp", "TempMin", "TempMax", "Prec.B", "W.m2", "WindSpeed")

for (i in seq_along(days_2013)){
  wedDataFilter<- metData[grep(days_2013[i], metData$Dato),]
  wedData4$Dato[i] <- paste(days_2013[i])
  wedData4$Temp[i] <- wedDataFilter$Temp
  wedData4$TempMin[i] <- wedDataFilter$TempMin
  wedData4$TempMax[i] <- wedDataFilter$TempMax
  wedData4$Prec.B[i] <- wedDataFilter$Prec.B
  wedData4$W.m2[i] <- wedDataFilter$W.m2
  wedData4$WindSpeed[i] <- wedDataFilter$WV2
}

# Selecting the weather data for two-weeks in September 2014 from dataset
days_2014 <- seq(from=as.Date("2014-09-13"), to=as.Date("2014-09-30"), by='days')
days_2014 <- format(as.Date(seq(from=as.Date(days_2014[1]), to=as.Date(days_2014[length(days_2014)]), by='days')), "%m/%d/%Y")
days_2014 <- gsub('(?<=\\b|-)0', '', days_2014, perl=TRUE)

wedData5 <- as.data.frame( matrix(nrow = length(days_2014), ncol = 7))
colnames(wedData5) <- c("Dato", "Temp", "TempMin", "TempMax", "Prec.B", "W.m2", "WindSpeed")

for (i in seq_along(days_2014)){
  wedDataFilter<- metData[grep(days_2014[i], metData$Dato),]
  wedData5$Dato[i] <- paste(days_2014[i])
  wedData5$Temp[i] <- wedDataFilter$Temp
  wedData5$TempMin[i] <- wedDataFilter$TempMin
  wedData5$TempMax[i] <- wedDataFilter$TempMax
  wedData5$Prec.B[i] <- wedDataFilter$Prec.B
  wedData5$W.m2[i] <- wedDataFilter$W.m2
  wedData5$WindSpeed[i] <- wedDataFilter$WV2
}

# Selecting the weather data for two weeks in September 2015 from dataset
days_2015 <-  seq(from=as.Date("2015-09-13"), to=as.Date("2015-09-30"), by='days')
days_2015 <- format(as.Date(seq(from=as.Date(days_2015[1]), to=as.Date(days_2015[length(days_2015)]), by='days')), "%m/%d/%Y")
days_2015 <- gsub('(?<=\\b|-)0', '', days_2015, perl=TRUE)

wedData1 <- as.data.frame( matrix(nrow = length(days_2015), ncol = 7))
colnames(wedData1) <- c("Dato", "Temp", "TempMin", "TempMax", "Prec.B", "W.m2", "WindSpeed")

for (i in seq_along(days_2015)){
  wedDataFilter<- metData[grep(days_2015[i], metData$Dato),]
  wedData1$Dato[i] <- paste(days_2015[i])
  wedData1$Temp[i] <- wedDataFilter$Temp
  wedData1$TempMin[i] <- wedDataFilter$TempMin
  wedData1$TempMax[i] <- wedDataFilter$TempMax
  wedData1$Prec.B[i] <- wedDataFilter$Prec.B
  wedData1$W.m2[i] <- wedDataFilter$W.m2
  wedData1$WindSpeed[i] <- wedDataFilter$WV2
}

# Selecting the weather data for two weeks in September 2016 from dataset
days_2016 <-  seq(from=as.Date("2016-09-13"), to=as.Date("2016-09-30"), by='days')
days_2016 <- format(as.Date(seq(from=as.Date(days_2016[1]), to=as.Date(days_2016[length(days_2016)]), by='days')), "%m/%d/%Y")
days_2016 <- gsub('(?<=\\b|-)0', '', days_2016, perl=TRUE)

wedData2 <- as.data.frame( matrix(nrow = length(days_2016), ncol = 7))
colnames(wedData2) <- c("Dato", "Temp", "TempMin", "TempMax", "Prec.B", "W.m2", "WindSpeed")

for (i in seq_along(days_2016)){
  wedDataFilter<- metData[grep(days_2016[i], metData$Dato),]
  wedData2$Dato[i] <- paste(days_2016[i])
  wedData2$Temp[i] <- wedDataFilter$Temp
  wedData2$TempMin[i] <- wedDataFilter$TempMin
  wedData2$TempMax[i] <- wedDataFilter$TempMax
  wedData2$Prec.B[i] <- wedDataFilter$Prec.B
  wedData2$W.m2[i] <- wedDataFilter$W.m2
  wedData2$WindSpeed[i] <- wedDataFilter$WV2
}
# Selecting the weather data for two weeks in September 2017 from dataset
days_2017 <-  seq(from=as.Date("2017-09-13"), to=as.Date("2017-09-30"), by='days')
days_2017 <- format(as.Date(seq(from=as.Date(days_2017[1]), to=as.Date(days_2017[length(days_2017)]), by='days')), "%m/%d/%Y")
days_2017 <- gsub('(?<=\\b|-)0', '', days_2015, perl=TRUE)

wedData6 <- as.data.frame( matrix(nrow = length(days_2017), ncol = 7))
colnames(wedData6) <- c("Dato", "Temp", "TempMin", "TempMax", "Prec.B", "W.m2", "WindSpeed")

for (i in seq_along(days_2017)){
  wedDataFilter<- metData[grep(days_2017[i], metData$Dato),]
  wedData6$Dato[i] <- paste(days_2017[i])
  wedData6$Temp[i] <- wedDataFilter$Temp
  wedData6$TempMin[i] <- wedDataFilter$TempMin
  wedData6$TempMax[i] <- wedDataFilter$TempMax
  wedData6$Prec.B[i] <- wedDataFilter$Prec.B
  wedData6$W.m2[i] <- wedDataFilter$W.m2
  wedData6$WindSpeed[i] <- wedDataFilter$WV2
}
# Selecting the weather data for two weeks in September 2018 from dataset
days_2018 <-  seq(from=as.Date("2018-09-13"), to=as.Date("2018-09-30"), by='days')
days_2018 <- format(as.Date(seq(from=as.Date(days_2018[1]), to=as.Date(days_2018[length(days_2018)]), by='days')), "%m/%d/%Y")
days_2018 <- gsub('(?<=\\b|-)0', '', days_2018, perl=TRUE)

wedData7 <- as.data.frame( matrix(nrow = length(days_2018), ncol = 7))
colnames(wedData7) <- c("Dato", "Temp", "TempMin", "TempMax", "Prec.B", "W.m2", "WindSpeed")

for (i in seq_along(days_2018)){
  wedDataFilter<- metData[grep(days_2018[i], metData$Dato),]
  wedData7$Dato[i] <- paste(days_2018[i])
  wedData7$Temp[i] <- wedDataFilter$Temp
  wedData7$TempMin[i] <- wedDataFilter$TempMin
  wedData7$TempMax[i] <- wedDataFilter$TempMax
  wedData7$Prec.B[i] <- wedDataFilter$Prec.B
  wedData7$W.m2[i] <- wedDataFilter$W.m2
  wedData7$WindSpeed[i] <- wedDataFilter$WV2
}

# Selecting the weather data for two weeks in September 2019 from dataset
days_2019 <-  seq(from=as.Date("2019-09-13"), to=as.Date("2019-09-30"), by='days')
days_2019 <- format(as.Date(seq(from=as.Date(days_2019[1]), to=as.Date(days_2019[length(days_2019)]), by='days')), "%m/%d/%Y")
days_2019 <- gsub('(?<=\\b|-)0', '', days_2019, perl=TRUE)

wedData8 <- as.data.frame( matrix(nrow = length(days_2019), ncol = 7))
colnames(wedData8) <- c("Dato", "Temp", "TempMin", "TempMax", "Prec.B", "W.m2", "WindSpeed")

for (i in seq_along(days_2019)){
  wedDataFilter<- metData[grep(days_2019[i], metData$Dato),]
  wedData8$Dato[i] <- paste(days_2019[i])
  wedData8$Temp[i] <- wedDataFilter$Temp
  wedData8$TempMin[i] <- wedDataFilter$TempMin
  wedData8$TempMax[i] <- wedDataFilter$TempMax
  wedData8$Prec.B[i] <- wedDataFilter$Prec.B
  wedData8$W.m2[i] <- wedDataFilter$W.m2
  wedData8$WindSpeed[i] <- wedDataFilter$WV2
}

# Calculating the average mean and variance of temperature for wet and dry days
avgTem12<-(wedData3$TempMax + wedData3$TempMin)/2
avgTem12Pre<-(wedData3[wedData3$Prec.B!=0,]$TempMax + wedData3[wedData3$Prec.B!=0,]$TempMin)/2
avgTem12Dry<-(wedData3[wedData3$Prec.B==0,]$TempMax + wedData3[wedData3$Prec.B==0,]$TempMin)/2
avgGRd12Pre<-(wedData3[wedData3$Prec.B!=0,]$W.m2)
avgGRd12Dry<-(wedData3[wedData3$Prec.B==0,]$W.m2)
avgWind12Pre<-(wedData3[wedData3$Prec.B!=0,]$WindSpeed)
avgWind12Dry<-(wedData3[wedData3$Prec.B==0,]$WindSpeed)

meanTem12Pre<-mean(avgTem12Pre)
varTem12Pre<-var(avgTem12Pre)
meanTem12Dry<-mean(avgTem12Dry)
varTem12Dry<-var(avgTem12Dry)

meanGRd12Pre<-mean(avgGRd12Pre)
varGRd12Pre<-var(avgGRd12Pre)
meanGRd12Dry<-mean(avgGRd12Dry)
varGRd12Dry<-var(avgGRd12Dry)

meanWind12Pre<-mean(avgWind12Pre)
varWind12Pre<-var(avgWind12Pre)
meanWind12Dry<-mean(avgWind12Dry)
varWind12Dry<-var(avgWind12Dry)

avgTem13<-(wedData4$TempMax + wedData4$TempMin)/2
avgTem13Pre<-(wedData4[wedData4$Prec.B!=0,]$TempMax + wedData4[wedData4$Prec.B!=0,]$TempMin)/2
avgTem13Dry<-(wedData4[wedData4$Prec.B==0,]$TempMax + wedData4[wedData4$Prec.B==0,]$TempMin)/2
avgGRd13Pre<-(wedData4[wedData4$Prec.B!=0,]$W.m2)
avgGRd13Dry<-(wedData4[wedData4$Prec.B==0,]$W.m2)
avgWind13Pre<-(wedData4[wedData4$Prec.B!=0,]$WindSpeed)
avgWind13Dry<-(wedData4[wedData4$Prec.B==0,]$WindSpeed)

meanTem13Pre<-mean(avgTem13Pre)
varTem13Pre<-var(avgTem13Pre)
meanTem13Dry<-mean(avgTem13Dry)
varTem13Dry<-var(avgTem13Dry)

meanGRd13Pre<-mean(avgGRd13Pre)
varGRd13Pre<-var(avgGRd13Pre)
meanGRd13Dry<-mean(avgGRd13Dry)
varGRd13Dry<-var(avgGRd13Dry)

meanWind13Pre<-mean(avgWind13Pre)
varWind13Pre<-var(avgWind13Pre)
meanWind13Dry<-mean(avgWind13Dry)
varWind13Dry<-var(avgWind13Dry)

avgTem14<-(wedData5$TempMax + wedData5$TempMin)/2
avgTem14Pre<-(wedData5[wedData5$Prec.B!=0,]$TempMax + wedData5[wedData5$Prec.B!=0,]$TempMin)/2
avgTem14Dry<-(wedData5[wedData5$Prec.B==0,]$TempMax + wedData5[wedData5$Prec.B==0,]$TempMin)/2
avgGRd14Pre<-(wedData5[wedData5$Prec.B!=0,]$W.m2)
avgGRd14Dry<-(wedData5[wedData5$Prec.B==0,]$W.m2)
avgWind14Pre<-(wedData5[wedData5$Prec.B!=0,]$WindSpeed)
avgWind14Dry<-(wedData5[wedData5$Prec.B==0,]$WindSpeed)

meanTem14Pre<-mean(avgTem14Pre)
varTem14Pre<-var(avgTem14Pre)
meanTem14Dry<-mean(avgTem14Dry)
varTem14Dry<-var(avgTem14Dry)

meanGRd14Pre<-mean(avgGRd14Pre)
varGRd14Pre<-var(avgGRd14Pre)
meanGRd14Dry<-mean(avgGRd14Dry)
varGRd14Dry<-var(avgGRd14Dry)

meanWind14Pre<-mean(avgWind14Pre)
varWind14Pre<-var(avgWind14Pre)
meanWind14Dry<-mean(avgWind14Dry)
varWind14Dry<-var(avgWind14Dry)

avgTem15<-(wedData1$TempMax + wedData1$TempMin)/2
avgTem15Pre<-(wedData1[wedData1$Prec.B!=0,]$TempMax + wedData1[wedData1$Prec.B!=0,]$TempMin)/2
avgTem15Dry<-(wedData1[wedData1$Prec.B==0,]$TempMax + wedData1[wedData1$Prec.B==0,]$TempMin)/2

avgGRd15Pre<-(wedData1[wedData1$Prec.B!=0,]$W.m2)
avgGRd15Dry<-(wedData1[wedData1$Prec.B==0,]$W.m2)
avgWind15Pre<-(wedData1[wedData1$Prec.B!=0,]$WindSpeed)
avgWind15Dry<-(wedData1[wedData1$Prec.B==0,]$WindSpeed)

meanTem15Pre<-mean(avgTem15Pre)
varTem15Pre<-var(avgTem15Pre)
meanTem15Dry<-mean(avgTem15Dry)
varTem15Dry<-var(avgTem15Dry)

meanGRd15Pre<-mean(avgGRd15Pre)
varGRd15Pre<-var(avgGRd15Pre)
meanGRd15Dry<-mean(avgGRd15Dry)
varGRd15Dry<-var(avgGRd15Dry)

meanWind15Pre<-mean(avgWind15Pre)
varWind15Pre<-var(avgWind15Pre)
meanWind15Dry<-mean(avgWind15Dry)
varWind15Dry<-var(avgWind15Dry)

avgTem16<-(wedData2$TempMax + wedData2$TempMin)/2
avgTem16Pre<-(wedData2[wedData2$Prec.B!=0,]$TempMax + wedData2[wedData2$Prec.B!=0,]$TempMin)/2
avgTem16Dry<-(wedData2[wedData2$Prec.B==0,]$TempMax + wedData2[wedData2$Prec.B==0,]$TempMin)/2
avgGRd16Pre<-(wedData2[wedData2$Prec.B!=0,]$W.m2)
avgGRd16Dry<-(wedData2[wedData2$Prec.B==0,]$W.m2)
avgWind16Pre<-(wedData2[wedData2$Prec.B!=0,]$WindSpeed)
avgWind16Dry<-(wedData2[wedData2$Prec.B==0,]$WindSpeed)

meanTem16Pre<-mean(avgTem16Pre)
varTem16Pre<-var(avgTem16Pre)
meanTem16Dry<-mean(avgTem16Dry)
varTem16Dry<-var(avgTem16Dry)

meanGRd16Pre<-mean(avgGRd16Pre)
varGRd16Pre<-var(avgGRd16Pre)
meanGRd16Dry<-mean(avgGRd16Dry)
varGRd16Dry<-var(avgGRd16Dry)

meanWind16Pre<-mean(avgWind16Pre)
varWind16Pre<-var(avgWind16Pre)
meanWind16Dry<-mean(avgWind16Dry)
varWind16Dry<-var(avgWind16Dry)

#
avgTem17<-(wedData6$TempMax + wedData6$TempMin)/2
avgTem17Pre<-(wedData6[wedData6$Prec.B!=0,]$TempMax + wedData6[wedData6$Prec.B!=0,]$TempMin)/2
avgTem17Dry<-(wedData6[wedData6$Prec.B==0,]$TempMax + wedData6[wedData6$Prec.B==0,]$TempMin)/2
avgGRd17Pre<-(wedData6[wedData6$Prec.B!=0,]$W.m2)
avgGRd17Dry<-(wedData6[wedData6$Prec.B==0,]$W.m2)
avgWind17Pre<-(wedData6[wedData6$Prec.B!=0,]$WindSpeed)
avgWind17Dry<-(wedData6[wedData6$Prec.B==0,]$WindSpeed)

meanTem17Pre<-mean(avgTem17Pre)
varTem17Pre<-var(avgTem17Pre)
meanTem17Dry<-mean(avgTem17Dry)
varTem17Dry<-var(avgTem17Dry)

meanGRd17Pre<-mean(avgGRd17Pre)
varGRd17Pre<-var(avgGRd17Pre)
meanGRd17Dry<-mean(avgGRd17Dry)
varGRd17Dry<-var(avgGRd17Dry)

meanWind17Pre<-mean(avgWind17Pre)
varWind17Pre<-var(avgWind17Pre)
meanWind17Dry<-mean(avgWind17Dry)
varWind17Dry<-var(avgWind17Dry)

#
avgTem18<-(wedData7$TempMax + wedData7$TempMin)/2
avgTem18Pre<-(wedData7[wedData7$Prec.B!=0,]$TempMax + wedData7[wedData7$Prec.B!=0,]$TempMin)/2
avgTem18Dry<-(wedData7[wedData7$Prec.B==0,]$TempMax + wedData7[wedData7$Prec.B==0,]$TempMin)/2
avgGRd18Pre<-(wedData7[wedData7$Prec.B!=0,]$W.m2)
avgGRd18Dry<-(wedData7[wedData7$Prec.B==0,]$W.m2)
avgWind18Pre<-(wedData7[wedData7$Prec.B!=0,]$WindSpeed)
avgWind18Dry<-(wedData7[wedData7$Prec.B==0,]$WindSpeed)

meanTem18Pre<-mean(avgTem18Pre)
varTem18Pre<-var(avgTem18Pre)
meanTem18Dry<-mean(avgTem18Dry)
varTem18Dry<-var(avgTem18Dry)

meanGRd18Pre<-mean(avgGRd18Pre)
varGRd18Pre<-var(avgGRd18Pre)
meanGRd18Dry<-mean(avgGRd18Dry)
varGRd18Dry<-var(avgGRd18Dry)

meanWind18Pre<-mean(avgWind18Pre)
varWind18Pre<-var(avgWind18Pre)
meanWind18Dry<-mean(avgWind18Dry)
varWind18Dry<-var(avgWind18Dry)

avgTem19<-(wedData8$TempMax + wedData8$TempMin)/2
avgTem19Pre<-(wedData8[wedData8$Prec.B!=0,]$TempMax + wedData8[wedData8$Prec.B!=0,]$TempMin)/2
avgTem19Dry<-(wedData8[wedData8$Prec.B==0,]$TempMax + wedData8[wedData8$Prec.B==0,]$TempMin)/2
avgGRd19Pre<-(wedData8[wedData8$Prec.B!=0,]$W.m2)
avgGRd19Dry<-(wedData8[wedData8$Prec.B==0,]$W.m2)
avgWind19Pre<-(wedData8[wedData8$Prec.B!=0,]$WindSpeed)
avgWind19Dry<-(wedData8[wedData8$Prec.B==0,]$WindSpeed)

#meanTem19Pre<-mean(avgTem19Pre)
meanTem19Pre<-0
#varTem19Pre<-var(avgTem19Pre)
varTem19Pre<-0
meanTem19Dry<-mean(avgTem19Dry)
varTem19Dry<-var(avgTem19Dry)

#meanGRd19Pre<-mean(avgGRd19Pre)
meanGRd19Pre<-0
#varGRd19Pre<-var(avgGRd19Pre)
varGRd19Pre<-0
meanGRd19Dry<-mean(avgGRd19Dry)
varGRd19Dry<-var(avgGRd19Dry)

#meanWind19Pre<-mean(avgWind19Pre)
meanWind19Pre<-0
#varWind19Pre<-var(avgWind19Pre)
varWind19Pre<-0
meanWind19Dry<-mean(avgWind19Dry)
varWind19Dry<-var(avgWind19Dry)

meanTemPre<-(meanTem16Pre+meanTem17Pre+meanTem18Pre+meanTem19Pre+meanTem15Pre+meanTem14Pre+meanTem13Pre+meanTem12Pre)/8
varTemPre<-(varTem16Pre+varTem17Pre+varTem18Pre+varTem19Pre+varTem15Pre+varTem14Pre+varTem13Pre+varTem12Pre)/8
meanTemDry<-(meanTem16Dry+meanTem17Dry+meanTem18Dry+meanTem19Dry+meanTem15Dry+meanTem14Dry+meanTem13Dry+meanTem12Dry)/8
varTemDry<-(varTem16Dry+varTem17Dry+varTem18Dry+varTem19Dry+varTem15Dry+varTem14Dry+varTem13Dry+varTem12Dry)/8
meanGRdPre<-(meanGRd16Pre+meanGRd17Pre+meanGRd18Pre+meanGRd19Pre+meanGRd15Pre+meanGRd14Pre+meanGRd13Pre+meanGRd12Pre)/8
varGRdPre<-(varGRd16Pre+varGRd17Pre+varGRd18Pre+varGRd19Pre+varGRd15Pre+varGRd14Pre+varGRd13Pre+varGRd12Pre)/8
meanGRdDry<-(meanGRd16Dry+meanGRd17Dry+meanGRd18Dry+meanGRd19Dry+meanGRd15Dry+meanGRd14Dry+meanGRd13Dry+meanGRd12Dry)/8
varGRdDry<-(varGRd16Dry+varGRd17Dry+varGRd18Dry+varGRd19Dry+varGRd15Dry+varGRd14Dry+varGRd13Dry+varGRd12Dry)/8
meanWindPre<-(meanWind16Pre+meanWind17Pre+meanWind18Pre+meanWind19Pre+meanWind15Pre+meanWind14Pre+meanWind13Pre+meanWind12Pre)/8
varWindPre<-(varWind16Pre+varWind17Pre+varWind18Pre+varWind19Pre+varWind15Pre+varWind14Pre+varWind13Pre+varWind12Pre)/8
meanWindpDry<-(meanWind16Dry+meanWind17Dry+meanWind18Dry+meanWind19Dry+meanWind15Dry+meanWind14Dry+meanWind13Dry+meanWind12Dry)/8
varWindDry<-(varWind16Dry+varWind17Dry+varWind18Dry+varWind19Dry+varWind15Dry+varWind14Dry+varWind13Dry+varWind12Dry)/8

#------------------------------------------------------------------------------------------------------------------------------
# Calculating the shape and scale of the gamma distribution related to precipitation.
meanPre12<-mean(wedData3$Prec.B)
varPre12<-var(wedData3$Prec.B)
scale12<-varPre12/meanPre12
shape12<-meanPre12/scale12

meanPre13<-mean(wedData4$Prec.B)
varPre13<-var(wedData4$Prec.B)
scale13<-varPre13/meanPre13
shape13<-meanPre13/scale13

meanPre14<-mean(wedData5$Prec.B)
varPre14<-var(wedData5$Prec.B)
scale14<-varPre14/meanPre14
shape14<-meanPre14/scale14

meanPre15<-mean(wedData1$Prec.B)
varPre15<-var(wedData1$Prec.B)
scale15<-varPre15/meanPre15
shape15<-meanPre15/scale15

meanPre16<-mean(wedData2$Prec.B)
varPre16<-var(wedData2$Prec.B)
scale16<-varPre16/meanPre16
shape16<-meanPre16/scale16

meanPre17<-mean(wedData6$Prec.B)
varPre17<-var(wedData6$Prec.B)
scale17<-varPre17/meanPre17
shape17<-meanPre17/scale17

meanPre18<-mean(wedData7$Prec.B)
varPre18<-var(wedData7$Prec.B)
scale18<-varPre18/meanPre18
shape18<-meanPre18/scale18

meanPre19<-mean(wedData8$Prec.B)
varPre19<-var(wedData8$Prec.B)
scale19<-varPre19/meanPre19
shape19<-meanPre19/scale19

meanPre<-(meanPre16+meanPre17+meanPre18+meanPre19+meanPre12+meanPre13+meanPre14+meanPre15)/8
varPre<-(varPre16+varPre17+varPre18+varPre19+varPre12+varPre13+varPre14+varPre15)/8
scalePre<-varPre/meanPre
shapePre<-meanPre/scalePre

#------------------------------------------------------------------------------------------------------------------------------
# Estimating Wiebull distribution parameters (shape factor (k) and scale factor (c)) for wind speed

wedData <- rbind(wedData3, wedData4, wedData5, wedData1, wedData2, wedData6, wedData7, wedData8)

wedData_Windspeed_Wet <- rbind(wedData[wedData$Prec.B!=0,]$WindSpeed)
wedData_Windspeed_Dry <- as.matrix(rbind(wedData[wedData$Prec.B==0,]$WindSpeed))

WindShape_Wet <- 1  # Initial guess
k_fit <- 0
n <- length(wedData_Windspeed_Wet)

while (abs(k_fit-WindShape_Wet)>0.05){
  k_fit <- WindShape_Wet
  sum1 <- 0
  sum2 <- 0
  sum3 <- 0
  for (i in 1:n){
    sum1 <- sum1+(wedData_Windspeed_Wet[i])^k_fit*log(wedData_Windspeed_Wet[i])
    sum2 <- sum2+(wedData_Windspeed_Wet[i])^k_fit
    sum3 <- sum3+log(wedData_Windspeed_Wet[i])
  }
  WindShape_Wet <- (sum1/sum2-sum3/n)^-1
}

sum <- 0
for (i in 1:n){
  sum <- sum+(wedData_Windspeed_Wet[i])^WindShape_Wet
}

WindScale_Wet <- ((1/n)*sum)^(1/WindShape_Wet)

WindShape_Dry <- 1  # Initial guess
k_fit <- 0
n <- length(wedData_Windspeed_Dry)

while (abs(k_fit-WindShape_Dry)>0.05){
  k_fit <- WindShape_Dry
  sum1 <- 0
  sum2 <- 0
  sum3 <- 0
  for (i in 1:n){
    sum1 <- sum1+(wedData_Windspeed_Dry[i])^k_fit*log(wedData_Windspeed_Dry[i])
    sum2 <- sum2+(wedData_Windspeed_Dry[i])^k_fit
    sum3 <- sum3+log(wedData_Windspeed_Dry[i])
  }
  WindShape_Dry <- (sum1/sum2-sum3/n)^-1
}

sum <- 0
for (i in 1:n){
  sum <- sum+(wedData_Windspeed_Dry[i])^WindShape_Dry
}

WindScale_Dry <- ((1/n)*sum)^(1/WindShape_Dry)

#------------------------------------------------------------------------------------------------------------------------------
# Calculating the probability of wet day following a dry day and probability of wet day following a wet day

counterDry<-0
for(i in 1:(dim(wedData3)[1]) )
  if( (wedData3$Prec.B[i]==0) ) counterDry<-counterDry+1

counterWet<-0
for(i in 1:(dim(wedData3)[1]) )
  if( (wedData3$Prec.B[i]!=0) ) counterWet<-counterWet+1

counterDryWet<-0
for(i in 1:(dim(wedData3)[1]-1) )
  if( (wedData3$Prec.B[i]==0) && (wedData3$Prec.B[i+1]!=0) ) counterDryWet<-counterDryWet+1

counterWetWet<-0
for(i in 1:(dim(wedData3)[1]-1) )
  if( (wedData3$Prec.B[i]!=0) && (wedData3$Prec.B[i+1]!=0) ) counterWetWet<-counterWetWet+1

prDryWet12<-counterDryWet/counterDry
prWetWet12<-counterWetWet/counterWet

counterDry<-0
for(i in 1:(dim(wedData4)[1]) )
  if( (wedData4$Prec.B[i]==0) ) counterDry<-counterDry+1

counterWet<-0
for(i in 1:(dim(wedData4)[1]) )
  if( (wedData4$Prec.B[i]!=0) ) counterWet<-counterWet+1

counterDryWet<-0
for(i in 1:(dim(wedData4)[1]-1) )
  if( (wedData4$Prec.B[i]==0) && (wedData4$Prec.B[i+1]!=0) ) counterDryWet<-counterDryWet+1

counterWetWet<-0
for(i in 1:(dim(wedData4)[1]-1) )
  if( (wedData4$Prec.B[i]!=0) && (wedData4$Prec.B[i+1]!=0) ) counterWetWet<-counterWetWet+1

prDryWet13<-counterDryWet/counterDry
prWetWet13<-counterWetWet/counterWet

counterDry<-0
for(i in 1:(dim(wedData5)[1]) )
  if( (wedData5$Prec.B[i]==0) ) counterDry<-counterDry+1

counterWet<-0
for(i in 1:(dim(wedData5)[1]) )
  if( (wedData5$Prec.B[i]!=0) ) counterWet<-counterWet+1

counterDryWet<-0
for(i in 1:(dim(wedData5)[1]-1) )
  if( (wedData5$Prec.B[i]==0) && (wedData5$Prec.B[i+1]!=0) ) counterDryWet<-counterDryWet+1

counterWetWet<-0
for(i in 1:(dim(wedData5)[1]-1) )
  if( (wedData5$Prec.B[i]!=0) && (wedData5$Prec.B[i+1]!=0) ) counterWetWet<-counterWetWet+1

prDryWet14<-counterDryWet/counterDry
prWetWet14<-counterWetWet/counterWet

counterDry<-0
for(i in 1:(dim(wedData1)[1]) )
  if( (wedData1$Prec.B[i]==0) ) counterDry<-counterDry+1

counterWet<-0
for(i in 1:(dim(wedData1)[1]) )
  if( (wedData1$Prec.B[i]!=0) ) counterWet<-counterWet+1

counterDryWet<-0
for(i in 1:(dim(wedData1)[1]-1) )
  if( (wedData1$Prec.B[i]==0) && (wedData1$Prec.B[i+1]!=0) ) counterDryWet<-counterDryWet+1

counterWetWet<-0
for(i in 1:(dim(wedData1)[1]-1) )
  if( (wedData1$Prec.B[i]!=0) && (wedData1$Prec.B[i+1]!=0) ) counterWetWet<-counterWetWet+1

prDryWet15<-counterDryWet/counterDry
prWetWet15<-counterWetWet/counterWet

counterDry<-0
for(i in 1:(dim(wedData2)[1]) )
  if( (wedData2$Prec.B[i]==0) ) counterDry<-counterDry+1

counterWet<-0
for(i in 1:(dim(wedData2)[1]) )
  if( (wedData2$Prec.B[i]!=0) ) counterWet<-counterWet+1

counterDryWet<-0
for(i in 1:(dim(wedData2)[1]-1) )
  if( (wedData2$Prec.B[i]==0) && (wedData2$Prec.B[i+1]!=0) ) counterDryWet<-counterDryWet+1

counterWetWet<-0
for(i in 1:(dim(wedData2)[1]-1) )
  if( (wedData2$Prec.B[i]!=0) && (wedData2$Prec.B[i+1]!=0) ) counterWetWet<-counterWetWet+1

prDryWet16<-counterDryWet/counterDry
prWetWet16<-counterWetWet/counterWet

counterDry<-0
for(i in 1:(dim(wedData6)[1]) )
  if( (wedData6$Prec.B[i]==0) ) counterDry<-counterDry+1

counterWet<-0
for(i in 1:(dim(wedData6)[1]) )
  if( (wedData6$Prec.B[i]!=0) ) counterWet<-counterWet+1

counterDryWet<-0
for(i in 1:(dim(wedData6)[1]-1) )
  if( (wedData6$Prec.B[i]==0) && (wedData6$Prec.B[i+1]!=0) ) counterDryWet<-counterDryWet+1

counterWetWet<-0
for(i in 1:(dim(wedData6)[1]-1) )
  if( (wedData6$Prec.B[i]!=0) && (wedData6$Prec.B[i+1]!=0) ) counterWetWet<-counterWetWet+1

prDryWet17<-counterDryWet/counterDry
prWetWet17<-counterWetWet/counterWet

counterDry<-0
for(i in 1:(dim(wedData7)[1]) )
  if( (wedData7$Prec.B[i]==0) ) counterDry<-counterDry+1

counterWet<-0
for(i in 1:(dim(wedData7)[1]) )
  if( (wedData7$Prec.B[i]!=0) ) counterWet<-counterWet+1

counterDryWet<-0
for(i in 1:(dim(wedData7)[1]-1) )
  if( (wedData7$Prec.B[i]==0) && (wedData7$Prec.B[i+1]!=0) ) counterDryWet<-counterDryWet+1

counterWetWet<-0
for(i in 1:(dim(wedData7)[1]-1) )
  if( (wedData7$Prec.B[i]!=0) && (wedData7$Prec.B[i+1]!=0) ) counterWetWet<-counterWetWet+1

prDryWet18<-counterDryWet/counterDry
prWetWet18<-counterWetWet/counterWet

counterDry<-0
for(i in 1:(dim(wedData8)[1]) )
  if( (wedData8$Prec.B[i]==0) ) counterDry<-counterDry+1

counterWet<-0
for(i in 1:(dim(wedData8)[1]) )
  if( (wedData8$Prec.B[i]!=0) ) counterWet<-counterWet+1

counterDryWet<-0
for(i in 1:(dim(wedData1)[1]-1) )
  if( (wedData8$Prec.B[i]==0) && (wedData8$Prec.B[i+1]!=0) ) counterDryWet<-counterDryWet+1

counterWetWet<-0
for(i in 1:(dim(wedData1)[1]-1) )
  if( (wedData8$Prec.B[i]!=0) && (wedData8$Prec.B[i+1]!=0) ) counterWetWet<-counterWetWet+1

prDryWet19<-counterDryWet/counterDry
prWetWet19<-counterWetWet/counterWet
prWetWet19<-0

prDryWet<-(prDryWet19+prDryWet18+prDryWet17+prDryWet16+prDryWet15+prDryWet14+prDryWet13+prDryWet12)/8
prWetWet<-(prWetWet19+prWetWet18+prWetWet17+prWetWet16+prWetWet15+prWetWet14+prWetWet13+prWetWet12)/8

#------------------------------------------------------------------------------------------------------------------------------
# Calculating the center points for continuous state variables in MDP model from historical data

days_Sep. <- MeanDaisyOutputs$Mean_MatricWaterPotential_hPa[,1]
days_Sep. <- format(as.Date(seq(from=as.Date(days_Sep.[1]), to=as.Date(days_Sep.[length(days_Sep.)]), by='days')), "%m/%d/%Y")
days_Sep. <- as.data.frame(gsub('(?<=\\b|-)0', '', days_Sep., perl=TRUE))

count=1
MoisDataFilter=data.frame(nrow = nrow(wedData), ncol =2)
MoisDataFilter_hPa=data.frame(nrow = nrow(wedData), ncol =2)

for (i in 1:(dim(days_Sep.)[1]-3)) {
  index <- length(which(as.character(days_Sep.[i,1])==wedData$Dato))
  if (index!=0){
    MoisDataFilter[count,] <- MeanDaisyOutputs$Mean_SoilWaterContent[i,]
    MoisDataFilter_hPa[count,] <- MeanDaisyOutputs$Mean_MatricWaterPotential_hPa[i,]
    count=count+1
  }
}

Mean_Obs <- mean(MoisDataFilter[,2])
sum <- 0

for (i in 1:dim(MoisDataFilter)[1]){
  sum <- sum + (MoisDataFilter[i,2]-Mean_Obs)^2
}
GSSMV <- sum/dim(MoisDataFilter)[1]

# Center points for matric water potential state variable
interval_MatricPotential=7
Limits_MatricPotential=as.data.frame(matrix(nrow=interval_MatricPotential, ncol = 2))
Min_MatricPotential=min(MoisDataFilter_hPa[,2])
Max_MatricPotential=max(MoisDataFilter_hPa[,2])
d_MatricPotential=(Max_MatricPotential-Min_MatricPotential)/interval_MatricPotential
Limits_MatricPotential[1,1]=Min_MatricPotential
Limits_MatricPotential[1,2]=Min_MatricPotential+d_MatricPotential

i=2

while(i < interval_MatricPotential){
  L=Limits_MatricPotential[i-1,2]
  U=Limits_MatricPotential[i-1,2]+d_MatricPotential
  Limits_MatricPotential[i,1]=L
  Limits_MatricPotential[i,2]=U
  i=i+1
}
Limits_MatricPotential[i,1]=Max_MatricPotential-d_MatricPotential
Limits_MatricPotential[i,2]=Max_MatricPotential
centerPointsAvgMat=as.data.frame(matrix(nrow=1, ncol=7))

for (j in 1:nrow(Limits_MatricPotential)) {
  centerPointsAvgMat[1,j]=((Limits_MatricPotential[j,1]+Limits_MatricPotential[j,2])/2)
}

centerPointsAvgMat <- as.numeric(centerPointsAvgMat)

# Center points for soil-water content state variable
interval_Mois=7
Limits_Mois=as.data.frame(matrix(nrow=interval_Mois, ncol = 2))
Min_Mois=min(MoisDataFilter[,2])
Max_Mois=max(MoisDataFilter[,2])
d_Mois=(Max_Mois-Min_Mois)/interval_Mois
Limits_Mois[1,1]=Min_Mois
Limits_Mois[1,2]=Min_Mois+d_Mois

i=2

while(i < interval_Mois){
  L=Limits_Mois[i-1,2]
  U=Limits_Mois[i-1,2]+d_Mois
  Limits_Mois[i,1]=L
  Limits_Mois[i,2]=U
  i=i+1
}
Limits_Mois[i,1]=Max_Mois-d_Mois
Limits_Mois[i,2]=Max_Mois
centerPointsAvgWat=as.data.frame(matrix(nrow=1, ncol=7))

for (j in 1:nrow(Limits_Mois)) {
  centerPointsAvgWat[1,j]=(Limits_Mois[j,1]+Limits_Mois[j,2])/2
}
centerPointsAvgWat <- as.numeric(centerPointsAvgWat)

# Center points for ??? state variables
centerPointsSdWat=seq(1,1, by=1)

# Center points for weather information state variables: Temprature, Precipitation, Global Radiation, and Wind Speed
# Center points for temprature
interval_Tem=4
Limits_Tem=as.data.frame(matrix(nrow=interval_Tem, ncol = 2))
Min_Tem=min(wedData[,2])
Max_Tem=max(wedData[,2])
d_Tem=(Max_Tem-Min_Tem)/interval_Tem
Limits_Tem[1,1]=Min_Tem
Limits_Tem[1,2]=Min_Tem+d_Tem

i=2

while(i < interval_Tem){
  L=Limits_Tem[i-1,2]
  U=Limits_Tem[i-1,2]+d_Tem
  Limits_Tem[i,1]=L
  Limits_Tem[i,2]=U
  i=i+1
}
Limits_Tem[i,1]=Max_Tem-d_Tem
Limits_Tem[i,2]=Max_Tem
centerPointsTem=as.data.frame(matrix(nrow=1, ncol=4))

for (j in 1:nrow(Limits_Tem)) {
  centerPointsTem[1,j]=(Limits_Tem[j,1]+Limits_Tem[j,2])/2
}
centerPointsTem <- as.numeric(centerPointsTem)

# Center points for precipitation
interval_Pre=4
Limits_Pre=as.data.frame(matrix(nrow=interval_Pre, ncol = 2))
Min_Pre=min(wedData[,5])
Max_Pre=max(wedData[,5])
d_Pre=(Max_Pre-Min_Pre)/interval_Pre
Limits_Pre[1,1]=Min_Pre
Limits_Pre[1,2]=Min_Pre+d_Pre

i=2

while(i < interval_Pre){
  L=Limits_Pre[i-1,2]
  U=Limits_Pre[i-1,2]+d_Pre
  Limits_Pre[i,1]=L
  Limits_Pre[i,2]=U
  i=i+1
}
Limits_Pre[i,1]=Max_Pre-d_Pre
Limits_Pre[i,2]=Max_Pre
centerPointsPre=as.data.frame(matrix(nrow=1, ncol=4))

for (j in 1:nrow(Limits_Pre)) {
  centerPointsPre[1,j]=(Limits_Pre[j,1]+Limits_Pre[j,2])/2
}

centerPointsPre <- as.numeric(centerPointsPre)
centerPointsPre <- c(c(0,param$dryDayTh), centerPointsPre)

# Center points for global radiation
interval_GlobRd=4
Limits_GlobRd=as.data.frame(matrix(nrow=interval_GlobRd, ncol = 2))
Min_GlobRd=min(wedData[,6])
Max_GlobRd=max(wedData[,6])
d_GlobRd=(Max_GlobRd-Min_GlobRd)/interval_GlobRd
Limits_GlobRd[1,1]=Min_GlobRd
Limits_GlobRd[1,2]=Min_GlobRd+d_GlobRd

i=2

while(i < interval_GlobRd){
  L=Limits_GlobRd[i-1,2]
  U=Limits_GlobRd[i-1,2]+d_GlobRd
  Limits_GlobRd[i,1]=L
  Limits_GlobRd[i,2]=U
  i=i+1
}
Limits_GlobRd[i,1]=Max_GlobRd-d_GlobRd
Limits_GlobRd[i,2]=Max_GlobRd
centerPointsGlobRd=as.data.frame(matrix(nrow=1, ncol=4))

for (j in 1:nrow(Limits_GlobRd)) {
  centerPointsGlobRd[1,j]=(Limits_GlobRd[j,1]+Limits_GlobRd[j,2])/2
}
centerPointsGlobRd <- as.numeric(centerPointsGlobRd)

# Center points for wind speed
interval_WindSp=4
Limits_WindSp=as.data.frame(matrix(nrow=interval_WindSp, ncol = 2))
Min_WindSp=min(wedData[,7])
Max_WindSp=max(wedData[,7])
d_WindSp=(Max_WindSp-Min_WindSp)/interval_WindSp
Limits_WindSp[1,1]=Min_WindSp
Limits_WindSp[1,2]=Min_WindSp+d_WindSp

i=2

while(i < interval_WindSp){
  L=Limits_WindSp[i-1,2]
  U=Limits_WindSp[i-1,2]+d_WindSp
  Limits_WindSp[i,1]=L
  Limits_WindSp[i,2]=U
  i=i+1
}
Limits_WindSp[i,1]=Max_WindSp-d_WindSp
Limits_WindSp[i,2]=Max_WindSp
centerPointsWind=as.data.frame(matrix(nrow=1, ncol=4))

for (j in 1:nrow(Limits_WindSp)) {
  centerPointsWind[1,j]=(Limits_WindSp[j,1]+Limits_WindSp[j,2])/2
}
centerPointsWind <- as.numeric(centerPointsWind)

# Center points for posterior mean and variance of SSM
centerPointsSdPos=c(0.005,0.01,0.05,0.1)
centerPointsMeanPos=seq(0.8,1.2,by=0.07)
#------------------------------------------------------------------------------------------------------------------------------
# calculating the coefficient of global radiation
n <- dim(wedData)[1]
sum <- 0

for (i in 1:n){
  sum <- sum + pow(wedData[i,6]-mean(wedData[,6]),2)
}

SD_Rd <- sqrt(sum/(n-1))
coefRd <- SD_Rd/mean(wedData[,6])

#------------------------------------------------------------------------------------------------------------------------------
#Estimate the system variance in the Gaussian SSM using EM algorithm

# Implementting the EM algorithm for z=1000 iterations :
z<-1000
fdlm<-list()
sdlm<-list()
We<-array(NA,dim=z)
Ve<-array(NA,dim=z)
We1<-0
Ve1<-0

soilWatTrue <- MeanDaisyOutputs$Mean_SoilWaterContent[2]

soilWatObs<- MeanDaisyOutputs$Mean_SoilWaterContent[2]

for(u in 1:z){
  if(u==1){
    fdlm[[u]]<-DLMfilter(param,mod,soilWatObs,soilWatTrue,mod$W,mod$V)
    sdlm[[u]]<-Smoother(mod,fdlm[[u]],mod$W)
    We[u]<-EM(mod,fdlm[[u]],sdlm[[u]],mod$W)[[1]]
  }
  else{
    fdlm[[u]]<-DLMfilter(param,mod,soilWatObs,soilWatTrue,We[u-1],mod$V)
    sdlm[[u]]<-Smoother(mod,fdlm[[u]],We[u-1])
    We[u]<-EM(mod,fdlm[[u]],sdlm[[u]],We[u-1])[[1]]
  }
}
We1<-We[z] + We1

#------------------------------------------------------------------------------------------------------------------------------
#Calculating the upper and lower limits for workability criterion

# Based on the two papers: \url{http://www.sciencedirect.com/science/article/pii/S0167198700001549} and \url{https://pure.au.dk/portal/files/129330768/1_s2.0_S0167198718304434_main.pdf}
# The optimal tillage limit, upper tillage limit, and lower tillage limit are calculated as follows:
hydroWatS <- param$hydroWatS/(param$hydroBulkDensity/1)
hydroWatR <- param$hydroWatR/(param$hydroBulkDensity/1)

# Optimum water content for tillage operation
opt <- (param$hydroWatS - param$hydroWatR) * pow(1 + 1/param$hydroVanM, -param$hydroVanM ) + param$hydroWatR  # eq. (6) in Dexter and Bird(2000)- (kg/kg)
# Upper (wet) tillage limit for workability criterian
upper <- opt + 0.4*(param$hydroWatS - opt)  # eq. (11) in Dexter and Bird (2000)- (kg/kg)
# Lower (dry) tillage limit for workability criterian
h_DTL <- (2/param$hydroVanAlpha) * pow(1/(1-1/param$hydroVanN),1/param$hydroVanN) * pow(param$hydroVanN,1.1)  # eq. (5) in Obour et al. (2018), Module of matric potential for dry limit of water content-equations
lower <- (param$hydroWatS - param$hydroWatR)* pow(1+pow(param$hydroVanAlpha*h_DTL, param$hydroVanN),-param$hydroVanM) + param$hydroWatR

# converting the workability limits from gravimetric (kg/kg) to  volumetric (cm3/cm3)

opt <- (opt*param$hydroBulkDensity/1)
upper <- (upper*param$hydroBulkDensity/1)
lower <- (lower*param$hydroBulkDensity/1)
#------------------------------------------------------------------------------------------------------------------------------




