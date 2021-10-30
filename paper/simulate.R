
# Read the optimal policy based on basic weights for trafficability, workability, and completion criteria
policy <- read.csv2("C:/Users/au520279/Documents/FieldReadiness/paper/Policies/based_weight/policyMDP.csv", stringsAsFactors = F)
#policy <- read.csv2("C:/Users/au520279/Documents/FieldReadiness/paper/Policies/weight_high_work/policyMDP.csv", stringsAsFactors = F)
#policy <- read.csv2("C:/Users/au520279/Documents/FieldReadiness/paper/Policies/weight_high_traf/policyMDP.csv", stringsAsFactors = F)

# Define the coefficients for the probability of a rainy days in three scenarios

coefPre1 <- 0.33
coefPre2 <- 0.61
coefPre3 <- 0.89

coefPre1_Shape <- 0.12
coefPre2_Shape <- 0.59
coefPre3_Shape <- 0.98

coefPre1_Scale <- 16.6
coefPre2_Scale <- 5.9
coefPre3_Scale <- 5

# Define the coefficients for the average daily temperature in three scenarios

coefTem1_Mean <- 13.2
coefTem2_Mean <- 12.2
coefTem3_Mean <- 10.4

coefTem1_sd <- 1.8
coefTem2_sd <-  1.8
coefTem3_sd <- 1.9

# Define the coefficients for the average daily Global Radiation in three scenarios

coefRd1_Mean <- 108
coefRd2_Mean <- 104
coefRd3_Mean <- 87

coefRd1_sd <- 44.84
coefRd2_sd <- 40.88
coefRd3_sd <- 42.02

# Define the coefficients for the average shape and scale daily wind speed in three scenarios

coefWs1_Shape <- 5.51
coefWs2_Shape <- 6.44
coefWs3_Shape <- 6.12

coefWs1_Scale <- 2.39
coefWs2_Scale <- 4.06
coefWs3_Scale <- 3.11

# Generate a set of random numbers to specify wet and dry days

set.seed(156)
rndValues <- runif(n = (param$tMax+3), min = 0, max = 1)

# Initial soil water content at the begining of time cycling period

temData1 <- simWeather(param,coefPre1,coefPre1_Shape,coefPre1_Scale,coefTem1_Mean,coefTem1_sd,coefRd1_Mean,coefRd1_sd,coefWs1_Shape,coefWs1_Scale,rndValues)$temData
temData2 <- simWeather(param,coefPre2,coefPre2_Shape,coefPre2_Scale,coefTem2_Mean,coefTem2_sd,coefRd2_Mean,coefRd2_sd,coefWs2_Shape,coefWs2_Scale,rndValues)$temData
temData3 <- simWeather(param,coefPre3,coefPre3_Shape,coefPre3_Scale,coefTem3_Mean,coefTem3_sd,coefRd3_Mean,coefRd3_sd,coefWs3_Shape,coefWs3_Scale,rndValues)$temData

precData1 <- simWeather(param,coefPre1,coefPre1_Shape,coefPre1_Scale,coefTem1_Mean,coefTem1_sd,coefRd1_Mean,coefRd1_sd,coefWs1_Shape,coefWs1_Scale,rndValues)$precData
precData2 <- simWeather(param,coefPre2,coefPre2_Shape,coefPre2_Scale,coefTem2_Mean,coefTem2_sd,coefRd2_Mean,coefRd2_sd,coefWs2_Shape,coefWs2_Scale,rndValues)$precData
precData3 <- simWeather(param,coefPre3,coefPre3_Shape,coefPre3_Scale,coefTem3_Mean,coefTem3_sd,coefRd3_Mean,coefRd3_sd,coefWs3_Shape,coefWs3_Scale,rndValues)$precData

GlobRdData1 <- simWeather(param,coefPre1,coefPre1_Shape,coefPre1_Scale,coefTem1_Mean,coefTem1_sd,coefRd1_Mean,coefRd1_sd,coefWs1_Shape,coefWs1_Scale,rndValues)$GlobRdData
GlobRdData2 <- simWeather(param,coefPre2,coefPre2_Shape,coefPre2_Scale,coefTem2_Mean,coefTem2_sd,coefRd2_Mean,coefRd2_sd,coefWs2_Shape,coefWs2_Scale,rndValues)$GlobRdData
GlobRdData3 <- simWeather(param,coefPre3,coefPre3_Shape,coefPre3_Scale,coefTem3_Mean,coefTem3_sd,coefRd3_Mean,coefRd3_sd,coefWs3_Shape,coefWs3_Scale,rndValues)$GlobRdData

WindData1 <- simWeather(param,coefPre1,coefPre1_Shape,coefPre1_Scale,coefTem1_Mean,coefTem1_sd,coefRd1_Mean,coefRd1_sd,coefWs1_Shape,coefWs1_Scale,rndValues)$WindData
WindData2 <- simWeather(param,coefPre2,coefPre2_Shape,coefPre2_Scale,coefTem2_Mean,coefTem2_sd,coefRd2_Mean,coefRd2_sd,coefWs2_Shape,coefWs2_Scale,rndValues)$WindData
WindData3 <- simWeather(param,coefPre3,coefPre3_Shape,coefPre3_Scale,coefTem3_Mean,coefTem3_sd,coefRd3_Mean,coefRd3_sd,coefWs3_Shape,coefWs3_Scale,rndValues)$WindData

iniTrueWat <- 20  # Change it manually
givenWatInfo <- FALSE

library(MDP2)
# Simulate the weather data and find optimal actions in three scenarios.

dataOptimal1 <- optimalSearch(t_count, param = param, policy = policy, WatObs, temData = temData1 , precData = precData1, GlobRdData = GlobRdData1 ,WindData = WindData1 ,iniTrueWat = iniTrueWat, givenWatInfo = givenWatInfo )
dataOptimal2 <- optimalSearch(t_count, param = param, policy = policy, WatObs, temData = temData2 , precData = precData2, GlobRdData = GlobRdData2 ,WindData = WindData2 ,iniTrueWat = iniTrueWat, givenWatInfo = givenWatInfo )
dataOptimal3 <- optimalSearch(t_count, param = param, policy = policy, WatObs, temData = temData3 , precData = precData3, GlobRdData = GlobRdData3 ,WindData = WindData3 ,iniTrueWat = iniTrueWat, givenWatInfo = givenWatInfo )

# Store the results in three .csv files

datS1 <- dataOptimal1
datS2 <- dataOptimal2
datS3 <- dataOptimal3
datS1$scenario <- 1
datS2$scenario <- 2
datS3$scenario <- 3

# Write on the .csv files

datS1$optAction=as.character(datS1$optAction)
write.csv2(datS1, file ="C:/Users/au520279/Documents/FieldReadiness/paper/results_data_paper/datS1.csv",row.names=FALSE)
#
datS2$optAction=as.character(datS2$optAction)
write.csv2(datS2, file ="C:/Users/au520279/Documents/FieldReadiness/paper/results_data_paper/datS2.csv",row.names=FALSE)
#
datS3$optAction=as.character(datS3$optAction)
write.csv2(datS3, file ="C:/Users/au520279/Documents/FieldReadiness/paper/results_data_paper/datS3.csv",row.names=FALSE)


