#include "mdp.h"


// ===================================================

const double MDPV::ZERO = 1e-10; //4.656613e-10;   // equal 1/2147483647 (limit of int since trans pr is stored as int when build the hgf)

// ===================================================

MDPV::MDPV(const List paramModel){

  List rParam(paramModel);       // Get parameters in params
  opNum=as<int>(rParam["opNum"]);
  tMax=as<int>(rParam["tMax"]);
  StartDay=as<int>(rParam["StartDay"]);
  LastDay=as<int>(rParam["LastDay"]);
  opSeq=as<arma::vec>(rParam["opSeq"]);
  opE=as<arma::vec>(rParam["opE"]);
  opL=as<arma::vec>(rParam["opL"]);
  opD=as<arma::vec>(rParam["opD"]);
  opDelay=as<arma::vec>(rParam["opDelay"]);
  opFixCost=as<arma::vec>(rParam["opFixCost"]);
  watTh=as<arma::vec>(rParam["watTh"]);
  coefLoss=as<double>(rParam["coefLoss"]);
  priceYield=as<double>(rParam["priceYield"]);
  machCap=as<double>(rParam["machCap"]);
  yieldHa=as<double>(rParam["yieldHa"]);
  fieldArea=as<double>(rParam["fieldArea"]);
  coefTimeliness=as<double>(rParam["coefTimeliness"]);
  costSkip=as<double>(rParam["costSkip"]);

  watUpper=as<arma::vec>(rParam["watUpper"]);
  watLower=as<arma::vec>(rParam["watLower"]);
  weightCompletion=as<double>(rParam["weightCompletion"]);
  weightWorkable=as<double>(rParam["weightWorkable"]);
  weightTraffic=as<double>(rParam["weightTraffic"]);
  minOpt=as<int>(rParam["minOpt"]);
  maxOpt=as<int>(rParam["maxOpt"]);

  temMeanDry=as<double>(rParam["temMeanDry"]);
  temMeanWet=as<double>(rParam["temMeanWet"]);
  GRdMeanWet=as<double>(rParam["GRdMeanWet"]);
  GRdMeanDry=as<double>(rParam["GRdMeanDry"]);
  WindMeanWet=as<double>(rParam["WindMeanWet"]);
  WindMeanDry=as<double>(rParam["WindMeanDry"]);
  temVarDry=as<double>(rParam["temVarDry"]);
  temVarWet=as<double>(rParam["temVarWet"]);
  GRdVarWet=as<double>(rParam["GRdVarWet"]);
  GRdVarDry=as<double>(rParam["GRdVarDry"]);
  WindVarWet=as<double>(rParam["WindVarWet"]);
  WindVarDry=as<double>(rParam["WindVarDry"]);
  dryDayTh=as<double>(rParam["dryDayTh"]);
  precShape=as<double>(rParam["precShape"]);
  precScale=as<double>(rParam["precScale"]);
  prDryWet=as<double>(rParam["prDryWet"]);
  prWetWet=as<double>(rParam["prWetWet"]);
  WindScale_Dry=as<double>(rParam["WindScale_Dry"]);
  WindShape_Dry=as<double>(rParam["WindShape_Dry"]);
  WindScale_Wet=as<double>(rParam["WindScale_Wet"]);
  WindShape_Wet=as<double>(rParam["WindShape_Wet"]);


  hydroWatR=as<double>(rParam["hydroWatR"]);
  hydroWatS=as<double>(rParam["hydroWatS"]);

  gSSMW=as<double>(rParam["gSSMW"]);
  gSSMV=as<double>(rParam["gSSMV"]);
  gSSMm0=as<double>(rParam["gSSMm0"]);
  gSSMc0=as<double>(rParam["gSSMc0"]);
  nGSSMm0=as<double>(rParam["nGSSMm0"]);
  nGSSMc0=as<double>(rParam["nGSSMc0"]);
  nGSSMK=as<double>(rParam["nGSSMK"]);

  check = as<bool>(rParam["check"]);
  rewRisk = as<bool>(rParam["rewRisk"]);

  Mois_WedcentrePoints = as<arma::mat>(rParam["Mois_WedcentrePoints"]);
  wed_data = as<arma::mat>(rParam["wed_data"]);
  SCIout = as<arma::mat>(rParam["SCIout"]);

  dMP = as<arma::mat>(rParam["disMeanPos"]);
  dSP = as<arma::mat>(rParam["disSdPos"]);
  dMW = as<arma::mat>(rParam["disAvgWat"]);
  dMM = as<arma::mat>(rParam["disAvgMat"]);
  dSW = as<arma::mat>(rParam["disSdWat"]);
  dT = as<arma::mat>(rParam["disTem"]);
  dP = as<arma::mat>(rParam["disPre"]);
  dG = as<arma::mat>(rParam["disGR"]);
  dE = as<arma::mat>(rParam["disWind"]);

  sMP = as<arma::vec>(rParam["centerPointsMeanPos"]);
  sSP = as<arma::vec>(rParam["centerPointsSdPos"]);
  sMW = as<arma::vec>(rParam["centerPointsAvgWat"]);
  sMM = as<arma::vec>(rParam["centerPointsAvgMat"]);
  sSW = as<arma::vec>(rParam["centerPointsSdWat"]);
  sT = as<arma::vec>(rParam["centerPointsTem"]);
  sP = as<arma::vec>(rParam["centerPointsPre"]);
  sG = as<arma::vec>(rParam["centerPointsGlobRd"]);
  sE = as<arma::vec>(rParam["centerPointsWind"]);

  sizeSMP = sMP.size();
  sizeSSP = sSP.size();
  sizeSMW = sMW.size();
  sizeSMM = sMM.size();
  sizeSSW = sSW.size();
  sizeST = sT.size();
  sizeSP = sP.size();
  sizeSG = sG.size();
  sizeSE = sE.size();

  // matrices for filling the rewards and transition probabilities before running the HMDP:
 prMW = vector< vector< vector< vector< vector< vector< vector< vector<double> > > > > > > >(sizeSMW,
        vector< vector< vector< vector< vector< vector< vector<double> > > > > > >(sizeSMP,
        vector< vector< vector< vector< vector <vector<double> > > > > >(sizeSSP,
        vector< vector< vector< vector< vector<double> > > > >(sizeST,
        vector< vector< vector< vector<double> > > >(sizeSG,
        vector< vector< vector<double> > >(sizeSE,
        vector< vector<double> >(sizeSP,
        vector<double>(sizeSMW) ) ) ) ) ) ) ); //prMW[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMW]


  prMP = vector< vector< vector< vector< vector< vector< vector< vector<double> > > > > > > >(sizeSMW,
         vector< vector< vector< vector< vector< vector< vector<double> > > > > > >(sizeSMP,
         vector< vector< vector< vector< vector< vector<double> > > > > >(sizeSSP,
         vector< vector< vector< vector< vector<double> > > > >(sizeST,
         vector< vector< vector< vector<double> > > >(sizeSG,
         vector< vector< vector<double> > >(sizeSE,
         vector< vector<double> >(sizeSP,
         vector<double>(sizeSMP) ) ) ) ) ) ) ); //prMP[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMP]


  prSP = vector<vector<vector<vector<vector<vector<double> > > > > >(sizeSSP,
         vector<vector<vector<vector<vector<double> > > > >(sizeST,
         vector<vector<vector<vector<double> > > >(sizeSG,
         vector<vector<vector<double> > >(sizeSE,
         vector<vector<double> >(sizeSP,
         vector<double>(sizeSSP) ) ) ) ) ); // prSP[iSPt][iTt][iGt][iEt][iPt][iSP]

  prSW = vector <vector <vector<double> > >(tMax+1,
         vector <vector<double> >(sizeSSW,
         vector<double>(sizeSSW) ) ); //prSW[t][iSWt][iSW]

  prT =  vector<vector<vector<double> > >(sizeST,
         vector<vector<double> >(sizeSP,
         vector<double>(sizeST) ) ); //prT[iTt][iPt][iT]


  prG =  vector<vector<vector<double> > >(sizeSG,
         vector<vector<double> >(sizeSP,
         vector<double>(sizeSG) ) ); //prG[iGt][iPt][iG]

  prE =  vector<vector<vector<double> > >(sizeSE,
         vector<vector<double> >(sizeSP,
         vector<double>(sizeSE) ) ); //prE[iEt][iPt][iE]

  prP = vector<vector<double> >(sizeSP,
        vector<double>(sizeSP) ); //prP[iPt][iP]

  rewDo = vector<vector<vector<vector<double> > > >(opNum,
          vector<vector<vector<double> > >(sizeSMW,
          vector<vector<double> >(sizeSMM,
          vector<double>(sizeSSW) ) ) ); //rewDo[op][iMWt][iMMt][iSWt]


  int opDMax = arma::max(opD);

  mapL1Vector = vector <vector <vector <vector <vector<vector<vector<vector<vector<vector<int> > > > > > > > > >(opNum,
                vector <vector <vector <vector <vector<vector<vector<vector<vector<int> > > > > > > > >(opDMax+1,
                vector <vector <vector <vector <vector<vector<vector<vector<int> > > > > > > >(sizeSMW,
                vector <vector <vector <vector <vector<vector<vector<int> > > > > > >(sizeSSW,
                vector <vector <vector <vector <vector<vector<int> > > > > >(sizeSMP,
                vector <vector <vector <vector<vector<int> > > > >(sizeSSP,
                vector <vector <vector <vector<int> > > >(sizeST,
                vector <vector <vector<int> > >(sizeSG,
                vector <vector<int> >(sizeSE,
                vector<int>(sizeSP) ) ) ) ) ) ) ) ) ); //mapL1Vector[op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP];

  valueFun =  vector< vector< vector< vector< vector< vector< vector< vector< vector< vector< vector<double> > > > > > > > > > >(tMax+1,
              vector< vector< vector< vector< vector< vector< vector< vector< vector< vector<double> > > > > > > > > >(opNum,
              vector< vector< vector< vector< vector< vector< vector< vector< vector<double> > > > > > > > >(opDMax+1,
              vector< vector< vector< vector< vector< vector< vector< vector<double> > > > > > > >(sizeSMW,
              vector< vector< vector< vector< vector< vector< vector<double> > > > > > >(sizeSSW,
              vector< vector< vector< vector< vector< vector<double> > > > > >(sizeSMP,
              vector< vector< vector< vector< vector<double> > > > >(sizeSSP,
              vector< vector< vector< vector<double> > > >(sizeST,
              vector< vector< vector<double> > >(sizeSG,
              vector< vector<double> >(sizeSE,
              vector<double>(sizeSP) ) ) ) ) ) ) ) ) ) ) ; //valueFun[t][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP];

  valFunDummy = vector<double>(tMax+1);

  optAction =   vector<vector<vector<vector<vector<vector<vector<vector<vector<vector<vector<string> > > > > > > > > > >(tMax+1,
                vector<vector<vector<vector<vector<vector<vector<vector<vector<vector<string> > > > > > > > > >(opNum,
                vector<vector<vector<vector<vector<vector<vector<vector<vector<string> > > > > > > > >(opDMax+1,
                vector<vector<vector<vector<vector<vector<vector<vector<string> > > > > > > >(sizeSMW,
                vector<vector<vector<vector<vector<vector<vector<string> > > > > > >(sizeSSW,
                vector<vector<vector<vector<vector<vector<string> > > > > >(sizeSMP,
                vector<vector<vector<vector<vector<string> > > > >(sizeSSP,
                vector<vector<vector<vector<string> > > >(sizeST,
                vector<vector<vector<string> > >(sizeSG,
                vector<vector<string> >(sizeSE,
                vector<string>(sizeSP) ) ) ) ) ) ) ) ) ) ) ; //optAction[t][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP];

}

// ===================================================================================================================

void MDPV::Preprocess() {
  Rcout << "Build the MDP ... \n\nStart preprocessing ...\n"<<endl;
  CalcTransPrMW();
  CalcTransPrSW();
  CalcTransPrMP();
  CalcTransPrSP();
  CalcTransPrT();
  CalcTransPrP();
  CalcTransPrG();
  CalcTransPrE();
  CalcRewaerdDo();

  Rcout << "... finished preprocessing.\n";
  Rcout << "weights: " << weightCompletion << endl;
}

// ===================================================================================================================

SEXP MDPV::SolveMDP(){
  Preprocess();
  int t, op, iMW, iMM, iSW, iMP, iSP, iT, iP, iG, iE, d;
  double valueDo, valuePos;

  int counter=0;

  for(t=tMax; t>=1; --t){
    if(t==tMax){
      if (rewRisk) valFunDummy[tMax]=0; else valFunDummy[tMax]=priceYield*yieldHa*fieldArea;
      continue;
    }
    Rcout<<" day: "<< t <<endl;
    for(op=0; op<opNum; op++){
      if( (opE[op]>t) || (opL[op]<=t) ) continue;
      for(d=1; d<=opD[op]; d++){
        if(opD[op]-t+opE(op)>d) continue;
        if(opL[op]-t<d) continue;
        for(iMW=0; iMW<sizeSMW; iMW++){
          for(iMM=0; iMM<sizeSMM; iMM++){
            for(iSW=0; iSW<sizeSSW; iSW++){
              for(iMP=0; iMP<sizeSMP; iMP++){
                for(iSP=0; iSP<sizeSSP; iSP++){
                  for(iT=0; iT<sizeST; iT++){
                    for (iG=0; iG<sizeSG; iG++){
                      for (iE=0; iE<sizeSE; iE++){
                        for(iP=0; iP<sizeSP; iP++){
                          if ( d<opL[op]-t ){
                            valuePos=WeightPos(op,d,iMW,iSW,iMP,iSP,iT,iP,iG,iE,t); counter = counter+1;
                            valueDo=WeightDo(op,d,iMW,iMM,iSW,iMP,iSP,iT,iP,iG,iE,t); counter = counter+1;

                            if(valueDo>valuePos){
                              valueFun[t][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP]=valueDo; optAction[t][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP]="do.";
                            }else{
                              valueFun[t][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP]=valuePos; optAction[t][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP]="pos.";
                            }
                          }
                          if( d==(opL[op]-t) ){
                            valueDo=WeightDo(op,d,iMW,iMM,iSW,iMP,iSP,iT,iP,iG,iE,t); counter = counter+1;
                            valueFun[t][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP]=valueDo; optAction[t][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP]="doF.";
                          }
                          valFunDummy[t]=0+valFunDummy[t+1]; //IS IT TRUE?
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  Rcout<<" Number of actions: "<< counter << endl;
  totalRew=weightIni();
  printPolicy();
  return( wrap( List::create(Named("totalRew") = totalRew) ) );
  // return( wrap( List::create(Named("weights") = valueFun, Named("optAction") = optAction, Named("totalRew") = totalRew) ) );

}

// ===================================================================================================================

double MDPV::WeightPos(int & opt, int & dt, int & iMWt, int & iSWt, int & iMPt, int & iSPt, int & iTt, int & iPt, int & iGt, int & iEt, int & t) {
  double pr4, prS, reward;
  double weightFu=0;
  int op,d,iMW,iSW,iMP,iSP,iT,iP,iG,iE,tN;
  op=opt;
  d=dt;
  tN=t+1;

  pr.clear();
  for(iMW=0; iMW<sizeSMW; iMW++){
    for(iSW=0; iSW<sizeSSW; iSW++){
     for(iMP=0; iMP<sizeSMP; iMP++){
        for(iSP=0; iSP<sizeSSP; iSP++){
          for(iT=0; iT<sizeST; iT++){
            for(iG=0; iG<sizeSG; iG++){
              for(iE=0; iE<sizeSE; iE++){
                for(iP=0; iP<sizeSP; iP++){
                  prS = prSP[iSPt][iTt][iGt][iEt][iPt][iSP];
                  pr4 = prS*exp(prMW[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMW] + prSW[t][iSWt][iSW] + prMP[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMP]
                                  + prT[iTt][iPt][iT] + prP[iPt][iP]+ prG[iGt][iPt][iG]+ prE[iEt][iPt][iE]);
                  if (pr4>ZERO) {
                    weightFu = weightFu + pr4*valueFun[tN][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP];
                    pr.push_back(pr4);
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  if(rewRisk) reward=0; else  reward=-coefTimeliness*priceYield*yieldHa*fieldArea;

 if (check) {
  arma::vec tmp(pr);
   if (!Equal(sum(tmp),1,1e-8)) {
   Rcout << "Warning sum pr!=1 in WeightsTransPrPos - diff = " << 1-sum(tmp) << " op = " << op << " action = pos. " << " index:" << endl; //vec2String<int>(index) << " pr:" << vec2String<flt>(pr) << endl;
   }
 }
  return(reward+weightFu);
}

// ===================================================================================================================

double MDPV::WeightDo(int & opt, int & dt, int & iMWt, int & iMMt , int & iSWt, int & iMPt, int & iSPt, int & iTt, int & iPt, int & iGt, int & iEt, int & t) {

  double pr4, prS, reward;
  double weightFu=0;
  double completionCri=0;
  int op,d,iMW,iSW,iMP,iSP,iT,iP,iG,iE,tN;

  pr.clear();
  if( (dt>1) ){
    d=dt-1;
    op=opt;
    tN=t+1;
    for(iMW=0; iMW<sizeSMW; iMW++){
      for(iSW=0; iSW<sizeSSW; iSW++){
        for(iMP=0; iMP<sizeSMP; iMP++){
          for(iSP=0; iSP<sizeSSP; iSP++){
            for(iT=0; iT<sizeST; iT++){
              for(iG=0; iP<sizeSG; iG++){
                for(iE=0; iP<sizeSE; iE++){
                  for(iP=0; iP<sizeSP; iP++){
                    prS = prSP[iSPt][iTt][iGt][iEt][iPt][iSP];
                    pr4 = prS*exp(prMW[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMW] + prSW[t][iSWt][iSW] + prMP[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMP]
                                    + prT[iTt][iPt][iT] + prP[iPt][iP]+ prG[iGt][iPt][iG] + prE[iEt][iPt][iE]);
                    if (pr4>ZERO) {
                      weightFu = weightFu + pr4*valueFun[tN][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP];
                      pr.push_back(pr4);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    if(rewRisk) reward = rewDo[opt][iMWt][iMMt][iSWt]; else reward=rewDo[opt][iMWt][iMMt][iSWt];

  }

  if( (dt==1) & (opt<(opNum-1)) ){
    d=opD[opt+1];
    op=opt+1;
    tN=t+1;
    for(iMW=0; iMW<sizeSMW; iMW++){
      for(iSW=0; iSW<sizeSSW; iSW++){
        for(iMP=0; iMP<sizeSMP; iMP++){
          for(iSP=0; iSP<sizeSSP; iSP++){
            for(iT=0; iT<sizeST; iT++){
              for(iG=0; iG<sizeSG; iG++){
                for(iE=0; iE<sizeSE; iE++){
                  for(iP=0; iP<sizeSP; iP++){
                    prS = prSP[iSPt][iTt][iGt][iEt][iPt][iSP];
                    pr4 = prS*exp(prMW[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMW] + prSW[t][iSWt][iSW] + prMP[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMP]
                                    + prT[iTt][iPt][iT] + prP[iPt][iP] + prG[iGt][iPt][iG] +prE[iEt][iPt][iE]);
                    if (pr4>ZERO) {
                      weightFu = weightFu + pr4*valueFun[tN][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP];
                      pr.push_back(pr4);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    if(rewRisk) reward = rewDo[opt][iMWt][iMMt][iSWt]; else reward=rewDo[opt][iMWt][iMMt][iSWt];

  }
  if( (dt==1) & (opt==(opNum-1)) ){
    pr4=1;
    pr.push_back(pr4);
    tN=t+1;
    weightFu = weightFu + pr4*valFunDummy[tN];
    if( (tN>=minOpt) & (tN<=maxOpt) ) completionCri = 1;
    if( tN<minOpt ) completionCri = (double)(minOpt-tN)/(double)(minOpt);
    if( tN>maxOpt ) completionCri = (double)(tN-maxOpt)/(double)(tN);

    if(rewRisk) reward = ( rewDo[opt][iMWt][iMMt][iSWt] +  weightCompletion*(completionCri) ); else reward=rewDo[opt][iMWt][iMMt][iSWt];
  }

  if (check) {
    arma::vec tmp(pr);
    if (!Equal(sum(tmp),1,1e-8)) {
      Rcout << "Warning sum pr!=1 in WeightsTransPrDo - diff = " << 1-sum(tmp) << " op = " << op << " action = Do. " << endl; // " index:" << vec2String<int>(index) << " pr:" << vec2String<flt>(pr) << endl;
    }
  }
  return(reward+weightFu);
}

// ===================================================================================================================

double MDPV::weightIni() {

  return(0);
}


// ===================================================================================================================

void MDPV::CalcRewaerdDo(){
  cpuTime.Reset(0); cpuTime.StartTime(0);
  int iMW,iMM, iSW, op;
  double workCri,trafiCriteria;

  for(op=0; op<opNum; op++){
    for(iMW=0; iMW<sizeSMW; iMW++){
     for(iMM=0;iMM<sizeSMM; iMM++){
      for(iSW=0;iSW<sizeSSW;iSW++){
        if(rewRisk){
          if( (dMW(iMW,0)>=watLower[op] ) & (dMW(iMW,0)<=watUpper[op] ) ) workCri=1;
          if( dMW(iMW,0)<watLower[op] ) workCri=1-(double)(watLower[op]-dMW(iMW,0))/(double)(watLower[op]);
          if( dMW(iMW,0)>watUpper[op] ) workCri=1-(double)(dMW(iMW,0)-watUpper[op])/(double)(dMW(iMW,0));

          if(SCIout(iMM,0)<0.1)  trafiCriteria=1;
          if(SCIout(iMM,0)>=0.1) trafiCriteria=0;

          rewDo[op][iMW][iMM][iSW] = weightWorkable*workCri + weightTraffic*trafiCriteria;
        }else{
          rewDo[op][iMW][iMM][iSW]= -coefLoss*priceYield*yieldHa*machCap*(1- R::pnorm(watTh[op],dMW(iMW,0),dSW(iSW,0),1,0) );
        }
      }
    }
  }
}
}

// ===================================================================================================================

void MDPV::CalcTransPrMW(){   //prMW[iMWt][iMPt][iSPt][iTt][iPt][iMW]
  cpuTime.Reset(0); cpuTime.StartTime(0);
  int iMWt, iMPt,iSPt,iTt,iPt, iGt, iEt, iMW;
  double lower, upper, mt, ct, ft,rt,qt;

  for(iMWt=0; iMWt<sizeSMW; iMWt++){
    for(iMPt=0;iMPt<sizeSMP;iMPt++){
      for(iSPt=0;iSPt<sizeSSP;iSPt++){
        for(iTt=0;iTt<sizeST;iTt++){
            for(iGt=0;iGt<sizeSG;iGt++){
              for(iEt=0;iEt<sizeSE;iEt++){
                for(iPt=0;iPt<sizeSP;iPt++){
                ft=Hydro(Mois_WedcentrePoints , wed_data , dT(iTt,0),dP(iPt,0), dG(iGt,0), dE(iEt,0));
                rt=pow(dSP(iSPt,0),2) + gSSMW;
                qt=pow(ft,2)*rt + gSSMV;
                mt=ft*dMP(iMPt,0); ct=qt;
                for(iMW=0; iMW<sizeSMW; iMW++){
                  lower= dMW(iMW,1); upper=dMW(iMW,2);
                  prMW[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMW] = log( R::pnorm(upper,mt,sqrt(ct),1,0) - R::pnorm(lower,mt,sqrt(ct),1,0)  );
                }
              }
            }
          }
        }
      }
    }
  }
}

// ===================================================================================================================

void MDPV::CalcTransPrSW(){  //prSW[t][iSWt][iSW]
  cpuTime.Reset(0); cpuTime.StartTime(0);
  int t,iSWt,iSW;

  for(t=1; t<tMax; t++){
    for(iSWt=0; iSWt<sizeSSW; iSWt++){
      for(iSW=0; iSW<sizeSSW; iSW++){
        prSW[t][iSWt][iSW] = log(1);//log(PrNGSSM( t, pow(dSW(iSW,1),2), pow(dSW(iSW,2),2), pow(dSW(iSWt,0),2) ) );
      }
    }
  }
}

// ===================================================================================================================

void MDPV::CalcTransPrMP(){   //prMP[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMP]
  cpuTime.Reset(0); cpuTime.StartTime(0);
  int iMWt, iMPt,iSPt,iTt,iPt, iGt, iEt, iMP;
  double lower, upper, mt, ct, ft, rt, qt;

  for(iMWt=0;iMWt<sizeSMW;iMWt++) {
  for(iMPt=0;iMPt<sizeSMP;iMPt++){
    for(iSPt=0;iSPt<sizeSSP;iSPt++){
      for(iTt=0;iTt<sizeST;iTt++){
          for (iGt=0;iGt<sizeSG;iGt++){
            for (iEt=0;iEt<sizeSE;iEt++){
              for(iPt=0;iPt<sizeSP;iPt++){
              ft = Hydro (Mois_WedcentrePoints, wed_data, dT(iTt,0),dP(iPt,0), dG(iGt,0), dE(iEt,0));
              rt= pow(dSP(iSPt,0),2) + gSSMW;
              qt=pow(ft,2)*rt + gSSMV;
              mt=dMP(iMPt,0); ct= pow(rt,2)*pow(ft,2)/qt;
              for(iMP=0; iMP<sizeSMP; iMP++){
                lower= dMP(iMP,1); upper=dMP(iMP,2);
                prMP[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMP] = log( R::pnorm(upper,mt,sqrt(ct),1,0) - R::pnorm(lower,mt,sqrt(ct),1,0)  );
              }
            }
          }
        }
      }
    }
  }
}
}

// ===================================================================================================================

void MDPV::CalcTransPrSP(){ //prSP[iSPt][iTt][iGt][iEt][iPt][iSP]
  cpuTime.Reset(0); cpuTime.StartTime(0);
  int iSPt,iTt,iPt,iGt, iEt, iSP;
  double ft, ct, cN;

  for(iSPt=0;iSPt<sizeSSP;iSPt++){
    for(iTt=0;iTt<sizeST;iTt++){
      for (iGt=0;iGt<sizeSG;iGt++){
        for (iEt=0;iEt<sizeSE;iEt++){
          for(iPt=0;iPt<sizeSP;iPt++){
            ft=Hydro (Mois_WedcentrePoints, wed_data, dT(iTt,0),dP(iPt,0), dG(iGt,0), dE(iEt,0));
            ct=pow(dSP(iSPt,0),2);
            cN=( (ct+gSSMW)*gSSMV )/( pow(ft,2)*(ct+gSSMW) + gSSMV);
            for(iSP=0;iSP<sizeSSP;iSP++){
              if((cN>pow(dSP(iSP,1),2)) & (cN<=pow(dSP(iSP,2),2))){
                prSP[iSPt][iTt][iGt][iEt][iPt][iSP]=1;
              }else{
                prSP[iSPt][iTt][iGt][iEt][iPt][iSP]=0;
              }
            }
          }
        }
      }
    }
  }
}

// ===================================================================================================================

void MDPV::CalcTransPrT(){ //prT[iTt][iPt][iT]
  cpuTime.Reset(0); cpuTime.StartTime(0);
  int iTt,iT,iPt;
  double lower, upper, mt, ct;

  for(iTt=0; iTt<sizeST; iTt++){
    for(iPt=0; iPt<sizeSP; iPt++){
      if(dP(iPt,0)<=dryDayTh){
        mt=temMeanDry; ct=temVarDry;
      }else{
        mt=temMeanWet; ct=temVarWet;
      }
      for(iT=0; iT<sizeST; iT++){
        lower=dT(iT,1); upper=dT(iT,2);
        prT[iTt][iPt][iT] = log( R::pnorm(upper,mt,sqrt(ct),1,0) - R::pnorm(lower,mt,sqrt(ct),1,0) );
      }
    }
  }
}
// ===================================================================================================================

void MDPV::CalcTransPrP(){ //prP[iPt][iP]
  cpuTime.Reset(0); cpuTime.StartTime(0);
  int iPt,iP;
  double lower, upper;

  for(iPt=0; iPt<sizeSP; iPt++){
    for(iP=0; iP<sizeSP; iP++){
      lower=dP(iP,1); upper=dP(iP,2);
      if( (dP(iPt,0)<=dryDayTh) & (dP(iP,0)<=dryDayTh) ) prP[iPt][iP]= log(1-prDryWet);
      if( (dP(iPt,0)>dryDayTh) & (dP(iP,0)<=dryDayTh) ) prP[iPt][iP]= log(1-prWetWet);
      if( (dP(iPt,0)<=dryDayTh) & (dP(iP,0)>dryDayTh)  ) { if(iP==1)lower=0; prP[iPt][iP]= log(prDryWet*( R::pgamma(upper,precShape,precScale,1,0) - R::pgamma(lower,precShape,precScale,1,0)) ); }
      if( (dP(iPt,0)>dryDayTh) & (dP(iP,0)>dryDayTh) ) { if(iP==1)lower=0; prP[iPt][iP]= log(prWetWet*( R::pgamma(upper,precShape,precScale,1,0) - R::pgamma(lower,precShape,precScale,1,0) ) ); }
    }
  }
}

// ===================================================================================================================

void MDPV::CalcTransPrG(){ //prG[iGt][iPt][iG]

  cpuTime.Reset(0); cpuTime.StartTime(0);
  int iGt,iG,iPt;
  double lower, upper, mt, ct;

  for(iGt=0; iGt<sizeSG; iGt++){
    for(iPt=0; iPt<sizeSP; iPt++){
      if(dP(iPt,0)<=dryDayTh){
        mt=GRdMeanDry; ct=GRdVarDry;
      }else{
        mt=GRdMeanWet; ct=GRdVarWet;
      }
      for(iG=0; iG<sizeSG; iG++){
        lower=dG(iG,1); upper=dG(iG,2);
        prG[iGt][iPt][iG] = log( R::pnorm(upper,mt,sqrt(ct),1,0) - R::pnorm(lower,mt,sqrt(ct),1,0) );
      }
    }
 }
}

// ===================================================================================================================

void MDPV::CalcTransPrE(){ //prE[iEt[iPt][iE]

  cpuTime.Reset(0); cpuTime.StartTime(0);
  int iEt,iE,iPt;
  double lower, upper, WindScale, WindShape;

  for(iEt=0; iEt<sizeSE; iEt++){
    for(iPt=0; iPt<sizeSP; iPt++){
      if(dP(iPt,0)<=dryDayTh){
        WindScale=WindScale_Dry; WindShape=WindShape_Dry;
      }else{
        WindScale=WindScale_Wet; WindShape=WindShape_Wet;
      }
      for(iE=0; iE<sizeSE; iE++){
        lower=dE(iE,1); upper=dE(iE,2);
        prE[iEt][iPt][iE] = log( R::pweibull(upper,WindScale,WindShape,1,0) - R::pweibull(lower,WindScale,WindShape,1,0) );
      }
    }
 }
}

// ===================================================================================================================

 double MDPV::PrNGSSM(int t, double lower, double upper, double var) { //SOLVED[Reza] : Based on the formulatiom for this probability in the paper, I changed "n" to "nf" (nf is the sample size).

  double probSd, xUpper, xLower;
  double a, s, alpha, gamma, beta;
  double G=1;

  double oShape = (double) (nGSSMK-1)/(2);   //shape parameter of observation distribution
  double iShape = (double) (nGSSMK-1)/(2);   //(double) (numSample-3)/(numSample-5); //("shape parameter of prior at t=1"): c_1 in the paper

  a = (double) (G * var * ( iShape + oShape*t ) ) / ( iShape + oShape*(t+1) ) ; // location
  s = a; // scale
  alpha = oShape; // shape 1
  gamma = iShape + oShape*t + 1; // shape 2
  beta =1 ;  // Weibul parameter
  xUpper = (double) (1)/( 1 + pow ( (double)(upper-a)/(s),-beta ) );
  xLower = (double) (1)/( 1 + pow ( (double)(lower-a)/(s),-beta ) );

  probSd= R::pbeta(xUpper, alpha, gamma,1, 0) - R::pbeta(xLower, alpha, gamma,1, 0);
  if( ( R::pbeta(xUpper, alpha, gamma,1, 0) - R::pbeta(xLower, alpha, gamma,1, 0) )<0 ) DBG4("error_minus"<<endl)
    //if(probSd!=probSd)  DBG4(endl << " Error" << " lower=" << lower << " upper=" << upper << " centerp=" << var <<" t: "<<t<< endl)
    return (probSd);  // Rf_pgamma(q, shape, scale, lower.tail, log.p)
}


// ===================================================================================================================

int MDPV::countStatesMDP(){
  int x, t, iMW, iSW, iMP, iSP, iT, iP, iG, iE, op, d;
  x=0;

  for(t=tMax; t>=1; --t){
    if(t == tMax)  continue;
    for(op=0; op<opNum; op++){
      if( (opE[op]>t) || (opL[op]<=t) ) continue;
      for(d=1; d<=opD[op]; d++){
        if(opD[op]-t+opE(op)>d) continue;
        if(opL[op]-t<d) continue;
        for(iMW=0; iMW<sizeSMW; iMW++){
          for(iSW=0; iSW<sizeSSW; iSW++){
            for(iMP=0; iMP<sizeSMP; iMP++){
              for(iSP=0; iSP<sizeSSP; iSP++){
                for(iT=0; iT<sizeST; iT++){
                  for(iG=0; iG<sizeSG; iG++){
                    for(iE=0; iE<sizeSE; iE++){
                      for(iP=0; iP<sizeSP; iP++){
                        x++;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  return(x+tMax);
}

// ===================================================================================================================

void MDPV::printPolicy(){
  int t, op, iMW, iSW, iMP, iSP, iT, iP, iG, iE, d;

  //Store the results in the .csv files:
  ofstream  myFile;
  myFile.open("C:\\Users\\au520279\\Documents\\FieldReadiness\\paper\\policyMDP.csv", ios::trunc);
  //myFile.open("policyMDP.csv", ios::trunc);
  myFile << "statLbl" << ";" << "day" << ";" << "op" << ";" << "d" << ";" << "iMW" << ";" << "iSW" << ";" << "iMP" << ";" << "iSP" << ";" << "iT" << ";" << "iG" << ";" << "iE" << ";" << "iP" << ";" << "optAction" << ";" << "weight" <<endl;

  for(t=tMax; t>=1; --t){
    if(t==tMax){valFunDummy[tMax]=priceYield*yieldHa*fieldArea; continue;}
    for(op=0; op<opNum; op++){
      if( (opE[op]>t) || (opL[op]<=t) ) continue;
      for(d=1; d<=opD[op]; d++){
        if(opD[op]-t+opE(op)>d) continue;
        if(opL[op]-t<d) continue;
        for(iMW=0; iMW<sizeSMW; iMW++){
          for(iSW=0; iSW<sizeSSW; iSW++){
            for(iMP=0; iMP<sizeSMP; iMP++){
              for(iSP=0; iSP<sizeSSP; iSP++){
                for(iT=0; iT<sizeST; iT++){
                  for(iG=0; iG<sizeSG; iG++){
                    for(iE=0; iE<sizeSE; iE++){
                      for(iP=0; iP<sizeSP; iP++){
                        label = getLabel(op,d,iMW,iSW,iMP,iSP,iT,iG,iE,iP,t);
                        myFile << label << ";" << t << ";" << op + 1 << ";" << d << ";" << iMW << ";" <<
                          iSW << ";" << iMP << ";" << iSP << ";" << iT << ";" << iG << ";" << iE << ";" << iP << ";" <<
                            optAction[t][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP] << ";" <<
                            valueFun[t][op][d][iMW][iSW][iMP][iSP][iT][iG][iE][iP] <<endl;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  myFile.close();
}

// ===================================================================================================================

int MDPV::findIndex(double st, arma::mat dis){
  for(int i=0; i<dis.n_rows; i++){
    if( ( st>=dis(i,1) ) & ( st<dis(i,2) )  )
      return(i);
  }
  cout<<"error in index: "<< st << " dis " << dis << endl;
  return(-1);
}

// ===================================================================================================================

double MDPV::Hydro(arma::mat Mois_WedcentrePoints, arma::mat wed_data, double & Tt, double & Pt, double & Gt, double & Et){ // ssm.hydro(arma::mat Mois_WedcentrePoints, arma::mat wed_data, dT(iTt,0),dP(iPt,0), dG(iGt,0),dE(iEt,0));

 double Mois, row1;
 int dim;
 double wed_data1, wed_data2, wed_data3, wed_data4;

 Tt = (int)(Tt * 10 + .5);
 Tt = (double)Tt / 10;

 Pt = (int)(Pt * 10 + .5);
 Pt = (double)Pt / 10;

 Gt = (int)(Gt * 10 + .5);
 Gt = (double)Gt / 10;

 Et = (int)(Et * 10 + .5);
 Et = (double)Et / 10;

//dim=sizeof(wed_data) / sizeof(wed_data[0]);
 dim=384;

 for (int i=0;  i<dim; i++){

   wed_data1 = (int)(wed_data(i,1)*10 + .5);
   wed_data1 = (double)wed_data1 / 10;

   wed_data2 = (int)(wed_data(i,2)*10 + .5);
   wed_data2 = (double)wed_data2 / 10;

   wed_data3 = (int)(wed_data(i,3)*10 + .5);
   wed_data3 = (double)wed_data3 / 10;

   wed_data4 = (int)(wed_data(i,4)*10 + .5);
   wed_data4 = (double)wed_data4 / 10;

   row1 = 0;

   if ((wed_data1==Tt) && (wed_data2==Pt) && (wed_data3==Gt) && (wed_data4==Et)){
     row1 = i;
     break;
   }
  }
  Mois = Mois_WedcentrePoints(row1,1);
  return(Mois);
}
// ===================================================================================================================
