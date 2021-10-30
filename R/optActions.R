
#' A function for determining the optimal actions and updated information obtained by Gaussian SSM
#'
#' @param t_count The time period of observing weather data
#' @param param Parameter values given in R function \code{setParam}
#' @param policy The optimal policy of the MDP
#' @param WatObs Given soil water content data (if givenWatInfo==TRUE)
#' @param temData Temperature data
#' @param precData Precipitation data
#' @param GlobRdData Global Radiation data
#' @param WindData Wind speed data
#' @param givenWatInfo A boolean value
#' @param iniTrueWat Initial soil-water content at day t=1
#'
#' @return A data frame containing the optimal actions and updated information obtained by Gaussian SSM
#' @export
optimalSearch <- function(t_count, param, policy, WatObs, temData, precData, GlobRdData, WindData, iniTrueWat, givenWatInfo){

  sim_Mois <- simMois(t_count, temData, precData, GlobRdData, WindData, iniTrueWat, param)

  soilWatObs <- c()
  soilWatTrue <- c()
  if(!givenWatInfo){
    soilWatObs <- sim_Mois$soilMoisObs
    soilWatTrue <- sim_Mois$soilMoisTrue
      }else{
     soilWatObs <- WatObs
  }

  paramGSSM <- setModel(param)

  temData <- temData[2:(param$tMax+1)]
  precData <- precData[2:(param$tMax+1)]
  GlobRdData <- GlobRdData[2:(param$tMax+1)]
  WindData <- WindData[2:(param$tMax+1)]
  soilWatObs <- soilWatObs[2:(param$tMax+1)]
  soilWatTrue <- soilWatTrue[2:(param$tMax+1)]

  meanPos <- DLMfilter(param, mod = paramGSSM,soilWatObs ,soilWatTrue, W = paramGSSM$W, V = paramGSSM$V)$meanPos
  varPos <- DLMfilter(param, mod = paramGSSM, soilWatObs ,soilWatTrue, W = paramGSSM$W, V = paramGSSM$V)$varPos
  sdPos <- sqrt(varPos)

  optAction <- c()
  weight <- c()
  operation <- c()
  dayLeft <- c()
  idxMW <- c()
  idxSW <- c()
  idxMP <- c()
  idxSP <- c()
  idxT <- c()
  idxP <- c()
  idxGR <- c()
  idxWind <- c()

  for(t in 1:param$tMax){
    if(t==1){
      opeNext=1
      dLNext=param$opD[opeNext]
      idxMW[t] <- findIndex(iniTrueWat,param$disAvgWat)
      idxSW[t] <- 0
      idxMP[t] <- findIndex(param$gSSMm0,param$disMeanPos)
      idxSP[t] <- findIndex(sqrt(param$gSSMc0),param$disSdPos)
      idxT[t] <- findIndex(temData[t],param$disTem)
      idxP[t] <- findIndex(precData[t],param$disPre)
      idxGR[t] <- findIndex(GlobRdData[t],param$disGR)
      idxWind[t] <- findIndex(WindData[t],param$disWind)
    }else{
      idxMW[t] <- findIndex(soilWatObs[t],param$disAvgWat)
      idxSW[t] <- 0
      idxMP[t] <- findIndex(meanPos[t],param$disMeanPos)
      idxSP[t] <- findIndex(sdPos[t],param$disSdPos)
      idxT[t] <- findIndex(temData[t],param$disTem)
      idxP[t] <- findIndex(precData[t],param$disPre)
      idxGR[t] <- findIndex(GlobRdData[t],param$disGR)
      idxWind[t] <- findIndex(WindData[t],param$disWind)
    }

    ope <- opeNext
    dL <- dLNext
    operation[t] <- ope
    dayLeft[t] <- dL
    optAction[t] <- subset(policy, day==t & op==ope & d==dL & iMW==idxMW[t] & iSW==idxSW[t] & iMP==idxMP[t] & iSP==idxSP[t] & iT==idxT[t] & iP==idxP[t] & iG==idxGR[t] & iE==idxWind[t]) ["optAction"]
    weight[t] <- as.numeric(subset(policy, day==t & op==ope & d==dL & iMW==idxMW[t] & iSW==idxSW[t] & iMP==idxMP[t] & iSP==idxSP[t] & iT==idxT[t] & iP==idxP[t] & iG==idxGR[t] & iE==idxWind[t]) ["weight"] )

    if(optAction[t]=="pos."){
      dLNext=dL
      opeNext=ope
    }
    if( ( (optAction[t]=="do.") || (optAction[t]=="doF.") ) && (dL>1) ){
      dLNext=dL-1
      opeNext=ope
    }
    if( ( (optAction[t]=="do.") || (optAction[t]=="doF.") ) && (dL==1) && (ope<param$opNum) ){
      dLNext=param$opD[ope+1]
      opeNext=ope+1
    }
    if( ( (optAction[t]=="do.") || (optAction[t]=="doF.") ) && (dL==1) && (ope==param$opNum) ){
      tTerm <- t
      break
    }

  }

  dat <- data.table(t=1:(param$tMax))
  dat$MW <- soilWatObs[1:param$tMax]
  dat$MP <- meanPos[1:param$tMax]
  dat$SP <- sdPos[1:param$tMax]
  dat$Tem <- temData[1:param$tMax]
  dat$Pre <- precData[1:param$tMax]
  dat$GR <- GlobRdData[1:param$tMax]
  dat$Wind <- WindData[1:param$tMax]

  dat$iMW <- idxMW[1:param$tMax]
  dat$iMP <- idxMP[1:param$tMax]
  dat$iSP <- idxSP[1:param$tMax]
  dat$iTem <- idxT[1:param$tMax]
  dat$iPre <- idxP[1:param$tMax]
  dat$iGR <- idxGR[1:param$tMax]
  dat$iWind <- idxWind[1:param$tMax]

  dat$operation <- operation[1:param$tMax]
  dat$dayLeft <- dayLeft[1:param$tMax]
  dat$optAction <- optAction[1:param$tMax]
  dat$weight <- weight[1:param$tMax]

  return(dat)
}

