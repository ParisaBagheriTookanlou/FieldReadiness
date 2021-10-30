
#' Set the parameters used when build the MDP model
#'
#' @param opNum Number of operations for scheduling
#' @param tMax Maximum length of growing cycle
#' @param StartDay The first day of growing cycle
#' @param LastDay The last day of growing cycle
#' @param opSeq A vector showing operations indexes and sequence
#' @param opE A vector showing earliest start time of operations
#' @param opL A vector showing latest start time of operationss
#' @param opD A vector showing the time needed to complete operation (obtained by machine capacity and the field area)
#' @param opDelay A vector including the delay times after finishing  the operations (except the last operation)
#' @param opFixCost A vector including the fixed costs of tillage operations
#' @param watTh A vector including threshold values used as the optimal level of soil-water content for performing tillage operations
#' @param coefLoss Coeficiant showing yield reduction when soil-water content is not appropriate
#' @param priceYield Price per kg yield (DDK)
#' @param machCap A vector showing the machine capacity for tillage operations
#' @param yieldHa Estimated yield per hetar in field (kg)
#' @param fieldArea Area of field (ha)
#' @param coefTimeliness Timeliness Coeficiant showing yield reduction resulting from postponing a tillage operation.
#' @param costSkip Cost of skipping tillage operations for the current cropping period
#' @param watUpper Upper limits of water content for workability criterion calculated based on the method in \url{http://www.sciencedirect.com/science/article/pii/S0167198700001549}
#' @param watLower Lower limits of water content for workability criterion calculated based on the method in \url{http://www.sciencedirect.com/science/article/pii/S0167198700001549}
#' @param weightCompletion Weight of the completion criterion used in the reward function of the MDP.
#' @param weightWorkable Weight of the workability criterion used in the reward function of the MDP.
#' @param weightTraffic Weight of the trafficability criterion used in the reward function of the MDP.
#' @param minOpt The lower tail of the interval related to the best time period for finishing tillage operations.
#' @param maxOpt The upper tail of the interval related to the best time period for finishing tillage operations.
#' @param centerPointsAvgWat Center points for discritization of estimated mean of soil-water content using historical weather data.
#' @param centerPointsAvgMat Center points for discritization of estimated mean of matric water potential using historical weather data.
#' @param centerPointsSdWat Center points for discritization of estimated standard deviation of soil-water content
#' @param centerPointsSdPos Center points for discritization of estimated standard deviation of posterior distribution in SSM
#' @param centerPointsMeanPos Center points for discritization of estimated mean of posterior distribution in SSM
#' @param centerPointsTem Center points for discritization of weather forecast data regarding air temperature
#' @param centerPointsPre Center points for discritization of weather forecast data regarding precipitation
#' @param centerPointsGlobRd  Center points for discritization of weather forecast data regarding global radiation
#' @param centerPointsWind Center points for discritization of weather forecast data regarding wind speed
#' @param temMeanDry Mean of air temprature in dry days (\url{http://link.springer.com/article/10.1007/BF00142466}).
#' @param temMeanWet Mean of air temprature in wet days (\url{http://link.springer.com/article/10.1007/BF00142466}).
#' @param temVarDry Variance of air temprature in dry days (\url{http://link.springer.com/article/10.1007/BF00142466}).
#' @param temVarWet Variance of air temprature in wet days (\url{http://link.springer.com/article/10.1007/BF00142466}).
#' @param GRdMeanDry Mean of global radiation in dry days
#' @param GRdMeanWet Mean of global radiation in wet days
#' @param GRdVarDry Variance of global radiation in dry days
#' @param GRdVarWet Variance of global radiation in wet days
#' @param WindMeanDry Mean of evatranspiration in dry days
#' @param WindMeanWet Mean of evatranspiration in wet days
#' @param WindVarDry Variance of evatranspiration in dry days
#' @param WindVarWet Variance of evatranspiration in wet days
#' @param observedSoilMois observed soil moisture
#' @param dryDayTh Threshold of precipitation amount for being a dry day (\url{http://link.springer.com/article/10.1007/BF00142466}).
#' @param precShape Shape parameter of gamma distribution for precipitation amount (\url{http://link.springer.com/article/10.1007/BF00142466}).
#' @param precScale Scale parameter of gamma distribution for precipitation amount (\url{http://link.springer.com/article/10.1007/BF00142466}).
#' @param WindShape_Wet Shape parameter of Weibull distribution of Wind speed amount for wet day
#' @param WindScale_Wet Scale parameter of Weibull distribution of Wind speed amount for wet day
#' @param WindShape_Dry Shape parameter of Weibull distribution of Wind speed amount for dry day
#' @param WindScale_Dry Scale parameter of Weibull distribution of Wind speed amount for dry day
#' @param prDryWet Probability of a wet day when the previous day is dry (\url{http://link.springer.com/article/10.1007/BF00142466}).
#' @param prWetWet Probability of a wet day when the previous day is wet (\url{http://link.springer.com/article/10.1007/BF00142466}).
#' @param hydroWatR Residual soil moisture value (\url{http://onlinelibrary.wiley.com/doi/10.1002/hyp.6629/abstract}).
#' @param hydroWatS Soil moisture value at saturation (\url{http://onlinelibrary.wiley.com/doi/10.1002/hyp.6629/abstract}).
#' @param hydroVanAlpha Parameter used in van Genuchten equation to calculate the matric water potensial (\url{http://www.sciencedirect.com/science/article/pii/S0016706198001323})
#' @param hydroVanM Parameter used in van Genuchten equation to calculate the matric water potensial (\url{http://www.sciencedirect.com/science/article/pii/S0016706198001323})
#' @param hydroVanN Parameter used in van Genuchten equation to calculate the matric water potensial (\url{http://www.sciencedirect.com/science/article/pii/S0016706198001323})
#' @param hydroBulkDensity Parameter used to change the unit of soil-water content from kg/kg to cm^3/cm^3(\url{http://www.sciencedirect.com/science/article/pii/S0016706198001323})
#' @param gSSMW System variance of Gaussian SSM.
#' @param gSSMV Observation variance of Gaussian SSM.
#' @param gSSMm0 Initial posterior mean of Gaussian SSM.
#' @param gSSMc0 Initial posterior variance of Gaussian SSM.
#' @param nGSSMm0 Initial posterior mean of non-Gaussian SSM.
#' @param nGSSMc0 Initial posterior mean of non-Gaussian SSM.
#' @param nGSSMK Number of observations in non-Gaussian SSM.
#' @param rewRisk A boolean variable specifing to calculate the reward based on cost parameters or the satisfaction level for trafficability, workability and completion criteria.
#' @param check Check model e.g. do trans pr sum to one.
#'
#' @export
#' @return A list containing all the parameters used in three-level MDP
setParam <- function(
  opNum=2,
  tMax=14,
  StartDay=13,
  LastDay=30,
  opSeq=c(1,2),
  opD=c(6,4),
  opE=c(1,1+opD[1]),
  opL=c(tMax-opD[2],tMax),
  opDelay=c(0,0),
  opFixCost=c(1000,1000),
  watTh=c(50,50),
  coefLoss=0.3,
  priceYield=10,
  machCap=50,
  yieldHa=100,
  fieldArea=100,
  coefTimeliness=0.01,
  costSkip=80000,

  watUpper=c(0.3390464,0.3390464)*100, # %
  watLower=c(0.2084356,0.2084356)*100, # %
  weightCompletion=1,
  weightWorkable=1,
  weightTraffic=1,
  minOpt=18,
  maxOpt=23,

# 2016
 centerPointsAvgWat=c(22.26053, 24.52712, 26.79373, 29.06032, 31.32693, 33.59352, 35.86013),
 centerPointsAvgMat=c(29.89776,  45.31459,  60.73142,  76.14826,  91.56509, 106.98192, 122.39876),
 centerPointsSdWat=seq(1,1, by=1),
 centerPointsSdPos=c(0.005,0.1),
 centerPointsMeanPos=seq(0.8,1.2, by=0.12),
 centerPointsTem=c(8.717964, 10.676148, 12.634332, 14.592516),
 centerPointsPre= c(0.0000,  0.2500,  3.0375,  9.1125, 15.1875, 21.2625),
 centerPointsGlobRd=c(39.73461,  75.43570, 111.13678, 146.83787),
 centerPointsWind=c(0.9471526, 2.0897995, 3.2324465, 4.3750934),

 temMeanDry=11.96235,  #It is estimated based on histrocial data.
 temMeanWet=11.42029,  #It is estimated based on histrocial data.
 temVarDry= 7.780112,  #It is estimated based on histrocial data.
 temVarWet=3.441829,   #It is estimated based on histrocial data.
 GRdMeanDry=119.9671,  #It is estimated based on histrocial data.
 GRdMeanWet=90.46169,  #It is estimated based on histrocial data.
 GRdVarDry=1640.874,   #It is estimated based on histrocial data.
 GRdVarWet=1468.409,   #It is estimated based on histrocial data.
 WindMeanDry=1.672297, #It is estimated based on histrocial data.
 WindMeanWet=2.643447, #It is estimated based on histrocial data.
 WindVarDry=0.9678253, #It is estimated based on histrocial data.
 WindVarWet=0.9314723, #It is estimated based on histrocial data.
 dryDayTh=0.25,        #It is estimated based on histrocial data.
 precShape=0.4183805,  #It is estimated based on histrocial data.
 precScale=7.366368,   #It is estimated based on histrocial data.
 prDryWet=0.4494048,   #It is estimated based on histrocial data.
 prWetWet=0.7750947,   #It is estimated based on histrocial data.
 WindShape_Wet=3.059134,   #It is estimated based on histrocial data.
 WindScale_Wet=2.994871,   #It is estimated based on histrocial data.
 WindShape_Dry=2.084098,   #It is estimated based on histrocial data.
 WindScale_Dry=1.989019,   #It is estimated based on histrocial data.

# 2020
#  centerPointsAvgWat=c(15.51984, 18.82241, 22.12498, 25.42755, 28.73012, 32.03269, 35.33526),
#  centerPointsAvgMat=c(78.73775, 191.70698, 304.67621, 417.64544, 530.61467, 643.58390, 756.55313),
#  centerPointsSdWat=seq(1,1, by=1),
#  centerPointsSdPos=c(0.005,0.1),
#  centerPointsMeanPos=seq(0.8,1.2, by=0.12),
#  centerPointsTem=c(9.34062, 12.54412, 15.74761, 18.95111),
#  centerPointsPre=c(0.0000,  0.2500,  3.0375,  9.1125, 15.1875, 21.2625),
#  centerPointsGlobRd=c(38.25177,  79.57014, 120.88850, 162.20686),
#  centerPointsWind=c(1.063851, 2.439893, 3.815936, 5.191979),

# temMeanDry=12.37325,  #It is estimated based on histrocial data.
# temMeanWet=10.71889,  #It is estimated based on histrocial data.
# temVarDry=6.873857,   #It is estimated based on histrocial data.
# temVarWet=3.381453,   #It is estimated based on histrocial data.
# GRdMeanDry=119.6411,  #It is estimated based on histrocial data.
# GRdMeanWet=79.07292,  #It is estimated based on histrocial data.
# GRdVarDry=1600.626,   #It is estimated based on histrocial data.
# GRdVarWet=1059.53,    #It is estimated based on histrocial data.
# WindMeanDry=2.117543, #It is estimated based on histrocial data.
# WindMeanWet=2.53611,  #It is estimated based on histrocial data.
# WindVarDry=1.020088,  #It is estimated based on histrocial data.
# WindVarWet=0.7630564, #It is estimated based on histrocial data.
# dryDayTh=0.25,
# precShape=0.2982603,  #It is estimated based on histrocial data.
# precScale=6.9151,     #It is estimated based on histrocial data.
# prDryWet=0.2949405,   #It is estimated based on histrocial data.
# prWetWet=0.5920928,   #It is estimated based on histrocial data.
# WindShape_Wet=3.055488,   #It is estimated based on histrocial data.
# WindScale_Wet=3.158224,   #It is estimated based on histrocial data.
# WindShape_Dry=1.983153,   #It is estimated based on histrocial data.
# WindScale_Dry=2.715127,   #It is estimated based on histrocial data.

  # Sandy loam
  hydroWatR=0.041,  #   cm3/cm3
  hydroWatS= 0.412, #   cm3/cm3
  hydroVanAlpha=0.068,
  hydroVanM=0.2436,
  hydroVanN=1.322,
  hydroBulkDensity=1.53,   # (g/cm3)- The value of the bulk density that is related to A horizon

# 2016
  gSSMW=0.1528,
# 2020
# gSSMW=0.1704,

# 2016
  gSSMV=12.81,
# 2020
# gSSMV=41.66,

  gSSMm0=1,
  gSSMc0=0.001,
  nGSSMm0=4,
  nGSSMc0=2,
  nGSSMK=20,

  rewRisk=TRUE,

  check=FALSE
){
  model <- list(opNum=opNum)
  model$opSeq <- opSeq
  model$tMax <- tMax
  model$StartDay <- StartDay
  model$LastDay <- LastDay
  model$opE <- opE
  model$opL <- opL
  model$opD <- opD
  model$opDelay <- opDelay
  model$opFixCost <- opFixCost
  model$watTh <- watTh
  model$coefLoss <- coefLoss
  model$priceYield <- priceYield
  model$machCap <- machCap
  model$yieldHa <- yieldHa
  model$fieldArea <- fieldArea
  model$coefTimeliness <- coefTimeliness
  model$costSkip <- costSkip

  model$watUpper <- watUpper
  model$watLower <- watLower
  model$weightCompletion <- weightCompletion
  model$weightWorkable <- weightWorkable
  model$weightTraffic <- weightTraffic
  model$minOpt <- minOpt
  model$maxOpt <- maxOpt

  model$temMeanDry <- temMeanDry
  model$temMeanWet <- temMeanWet
  model$temVarDry <- temVarDry
  model$temVarWet <- temVarWet
  model$GRdMeanDry <- GRdMeanDry
  model$GRdMeanWet <- GRdMeanWet
  model$WindMeanDry <- WindMeanDry
  model$WindMeanWet <- WindMeanWet
  model$GRdVarDry <- GRdVarDry
  model$GRdVarWet <- GRdVarWet
  model$WindVarDry <- WindVarDry
  model$WindVarWet <- WindVarWet

  model$dryDayTh <- dryDayTh
  model$precShape <- precShape
  model$precScale <- precScale
  model$prDryWet <- prDryWet
  model$prWetWet <- prWetWet

  model$WindShape_Wet <- WindShape_Wet
  model$WindScale_Wet <- WindScale_Wet
  model$WindShape_Dry <- WindShape_Dry
  model$WindScale_Dry <- WindScale_Dry


  model$hydroWatR <- hydroWatR
  model$hydroWatS <- hydroWatS

  model$hydroVanAlpha <- hydroVanAlpha
  model$hydroVanM <- hydroVanM
  model$hydroVanN <- hydroVanN
  model$hydroBulkDensity <- hydroBulkDensity

  model$gSSMW <- gSSMW
  model$gSSMV <- gSSMV
  model$gSSMm0 <- gSSMm0
  model$gSSMc0 <- gSSMc0
  model$nGSSMm0 <- nGSSMm0
  model$nGSSMc0 <- nGSSMc0
  model$nGSSMK <- nGSSMK

  model$rewRisk <- rewRisk
  model$check <- check

  model$centerPointsAvgWat <- centerPointsAvgWat
  model$centerPointsAvgMat <- centerPointsAvgMat
  model$centerPointsSdWat <- centerPointsSdWat
  model$centerPointsSdPos <- centerPointsSdPos
  model$centerPointsMeanPos <- centerPointsMeanPos
  model$centerPointsTem <- centerPointsTem
  model$centerPointsPre <- centerPointsPre
  model$centerPointsGlobRd <- centerPointsGlobRd
  model$centerPointsWind <- centerPointsWind

  #Discritization of continious states:
  obj <- discretizeGaussian::Discretize()
  disAvgWat <- matrix()
  disAvgMat <- matrix()
  disSdWat <- matrix()
  disSdPos <- matrix()
  disMeanPos <- matrix()
  disTem <- matrix()
  disPre <- matrix()
  disGR <- matrix()
  disWind <- matrix()

  disAvgWat <- as.matrix(obj$discretize1DVec(centerPointsAvgWat, mInf=-1, inf=100, asDF=F), ncol=3)
  disAvgMat <- as.matrix(obj$discretize1DVec(centerPointsAvgMat, mInf=-1, inf=100, asDF=F), ncol=3)
  disSdWat <- as.matrix(obj$discretize1DVec(centerPointsSdWat, inf=100, mInf=0.01, asDF=F), ncol=3)
  disSdPos <- as.matrix(obj$discretize1DVec(centerPointsSdPos, inf=100, mInf=0, asDF=F), ncol=3)
  disMeanPos <- as.matrix(obj$discretize1DVec(centerPointsMeanPos, inf=100, asDF=F), ncol=3)
  disTem <- as.matrix(obj$discretize1DVec(centerPointsTem, inf=100, asDF=F), ncol=3)
  disPre <- as.matrix(obj$discretize1DVec(centerPointsPre, inf=50, mInf=0, asDF=F), ncol=3)
  disGR <- as.matrix(obj$discretize1DVec(centerPointsGlobRd, inf=250, mInf=0, asDF=F), ncol=3)
  disWind <- as.matrix(obj$discretize1DVec(centerPointsWind, inf=10, mInf=0, asDF=F), ncol=3)

  model$disAvgWat <- disAvgWat
  model$disAvgMat <- disAvgMat
  model$disSdWat <- disSdWat
  model$disSdPos <- disSdPos
  model$disMeanPos <- disMeanPos
  model$disTem <- disTem
  model$disPre <- disPre
  model$disGR <- disGR
  model$disWind <- disWind

  #Soil water contents based on weather info. from centerpoints:

  Combn_centrpoints <- as.matrix(expand.grid(disTem[,1], disPre[,1], disGR[,1], disWind[,1]))
  combinations <- rbind(rbind(rbind(0,Combn_centrpoints),0),0)

  days <- format(as.Date(seq(from=as.Date("2015-09-06"), to=as.Date("2016-09-26"), by='days')), "%m/%d/%Y")  # The number of days is based on the number of combinations of centerpoints
  days <- gsub('(?<=\\b|-)0', '', days, perl=TRUE)
  wed_data <- as.data.frame(matrix(nrow = length(days) , ncol = 5))
  colnames(wed_data) <- c("datetime", "Temperature", "Precipitation", "GlobalRadiation", "WindSpeed")

  for (i in 1:length(days)){
    date <- as.Date(days[i],'%m/%d/%Y')
    wed_data$datetime[i] <- as.numeric(date)
    if (i==1){
      wed_data$Temperature[i] <- 0
      wed_data$Precipitation[i] <- 0
      wed_data$GlobalRadiation[i] <- 0
      wed_data$WindSpeed[i] <- 0
    } else{
     wed_data$Temperature[i] <- round(as.matrix(combinations[i,1]), digits = 6)
     wed_data$Precipitation[i] <- round(as.matrix(combinations[i,2]), digits = 4)
     wed_data$GlobalRadiation[i] <- round(as.matrix(combinations[i,3]), digits = 1)
     wed_data$WindSpeed[i] <- round(as.matrix(combinations[i,4]), digits = 4)
    }
  }

  Mois_WedcentrePoints <- matrix()
  DWF_File(wed_data)
  DaisyOutputs <- Daisy(wed_data)
  Mean_DaisyOutputs <- Mean_Daisy(DaisyOutputs)

  Mean_DaisyOutputs$Mean_SoilWaterContent[2] <- Mean_DaisyOutputs$Mean_SoilWaterContent[2]*100

  wed_data <- wed_data[c(-1, -(dim(wed_data)[1]-1), -dim(wed_data)[1]),]

  temp_wed_data <- as.data.frame(matrix(nrow = dim(wed_data)[1] , ncol = 5))
  colnames(temp_wed_data) <- c("datetime", "Temperature", "Precipitation", "GlobalRadiation", "WindSpeed")

  for (i in 1:dim(wed_data)[1]){
    temp_wed_data[i,]<- wed_data[i,]
  }

  wed_data <- temp_wed_data

  dim <- dim(Mean_DaisyOutputs$Mean_SoilWaterContent)[1]
  Mois_WedcentrePoints <- as.data.frame(matrix(nrow = dim , ncol = 2))
  colnames(Mois_WedcentrePoints) <- c("datetime", "Moisture")

  for (j in 1:dim(Mois_WedcentrePoints)[1]){
     date <- as.Date(Mean_DaisyOutputs$Mean_SoilWaterContent[j,1])
     Mois_WedcentrePoints[j,1] <- as.numeric(date)
     Mois_WedcentrePoints[j,2] <- as.numeric(Mean_DaisyOutputs$Mean_SoilWaterContent[j,2])
  }

  wed_data <- as.matrix(wed_data)
  Mois_WedcentrePoints <- as.matrix(Mois_WedcentrePoints)

  model$wed_data <- wed_data
  model$Mois_WedcentrePoints <- Mois_WedcentrePoints

  SCIout <- matrix(nrow = dim(disAvgMat)[1], ncol=1)

  for (i in 1:dim(disAvgMat)[1]){
    SCIout[i,1] <- Terranimo_SCI(disAvgMat[i,1])[5,1]
  }

  model$SCIout <- SCIout
  return(model)
}




