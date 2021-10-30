
#ifndef MDPV_HPP
#define MDPV_HPP

#include "RcppArmadillo.h"    // we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "binaryMDPWriter.h"
#include "time.h"

using namespace Rcpp;
using namespace std;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do

// [[Rcpp::depends(RcppArmadillo)]]

// ===================================================

/**
* Class for solving an MDP model using value iteration algorithm for scheduling tillage operations.
*
* @author Reza Pourmoayed
*/
class MDPV
{
  public:  // methods

    /** Constructor. Store the parameters.
    *
    * @param paramModel A list for model parameters related to HMDP and SSMs created using \code{setParam} in R.
    */
    MDPV(const List paramModel);


    /** Build the MDP (to binary files). Use "shared linking".
    *
    *  Build a 3 level HMDP saved in binary files by using the
    *  binaryMDPWriter (c++ version).
    *
    *  @return Build log (string)
    *  @export
    */
    SEXP SolveMDP();


    /** Count the number of states in the HMDP */
    int countStatesMDP();


private:

  /** Calculate and fill arrays with rewards and trans pr. */
  void Preprocess();



  /** Calculate the value function for action "pos." related to postpone tillage operation.
  *
  * @param op Tillage operation under consideration
  * @param dt Index of state for remaining days for finishing operation op
  * @param iMWt Index of state for estimated mean of soil water content
  * @param iSWt Index of state for estimated standard deviation of soil water content
  * @param iMPt Index of state for estimated posterior mean of latent variable in Gaussian SSM (error factor)
  * @param iSPt Index of state for estimated posterior standard deviation of latent variable in Gaussian SSM (error factor)
  * @param iTt Index of state for weather forecast regarding air temprature.
  * @param iPt Index of state for weather forecast regarding precipitation.
  * @param iGt Index of state for weather forecast regarding global radiation.
  * @param iEt Index of state for weather forecast regarding wind data.
  * @param t Current day.
  *
  */
  double WeightPos(int & op, int & dt, int & iMWt, int & iSWt, int & iMPt, int & iSPt, int & iTt, int & iPt, int & iGt, int & iEt, int & t);


  /** Calculate the value function for action "do." related to performing a tillage operation.
  *
  * @param opt Tillage operation under consideration
  * @param dt Index of state for remaining days for finishing operation op
  * @param iMWt Index of state for estimated mean of soil water content
  * @param iMMt Index of state for estimated mean of matric water potential
  * @param iSWt Index of state for estimated standard deviation of soil water content
  * @param iMPt Index of state for estimated posterior mean of latent variable in Gaussian SSM (error factor)
  * @param iSPt Index of state for estimated posterior sandard deviation of latent variable in Gaussian SSM (error factor)
  * @param iTt Index of state for weather forecast regarding air temprature.
  * @param iPt Index of state for weather forecast regarding precipitation.
  * @param iGt Index of state for weather forecast regarding global radiation.
  * @param iEt Index of state for weather forecast regarding wind data.
  * @param t Current day.
  *
  */
  double WeightDo(int & opt, int & dt, int & iMWt, int & iMMt, int & iSWt, int & iMPt, int & iSPt, int & iTt, int & iPt, int & iGt, int & iEt, int & t);


  /** Calculate the total reward of the model (value function at the stage 0)
   */
  double weightIni();


  /** Calculate the reward values under action Do.
  *
  *  Values are stored in the vector \var(rewDo[op][iMW][iMM][iSW]).
  */
  void CalcRewaerdDo();

  /** Calculate the transition probability values for estimated mean of soil water content.
  *
  *  Values are stored in the vector \var(prMW[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMW]).
  */
  void CalcTransPrMW();


  /** Calculate the transition probability values for estimated standard deviation of soil water content.
  *
  *  Values are stored in the vector \var(prSW[t][iSWt][iSW]).
  */
  void CalcTransPrSW();


  /** Calculate the transition probability values for posterior mean of latent variable (error factor) in Gaussian SSM.
  *
  *  Values are stored in the vector \var(prMP[iMWt][iMPt][iSPt][iTt][iGt][iEt][iPt][iMP]).
  */
  void CalcTransPrMP();


  /** Calculate the transition probability values for posterior standard deviation of latent variable (error factor) in Gaussian SSM.
  *
  *  Values are stored in the vector \var(prSP[iSPt][iTt][iGt][iEt][iPt][iSP]).
  */
  void CalcTransPrSP();


  /** Calculate the transition probability values for weather information regarding air temprature.
  *
  *  Values are stored in the vector \var(prT[iTt][iPt][iT]).
  */
  void CalcTransPrT();


  /** Calculate the transition probability values for weather information regarding global radiation.
  *
  *  Values are stored in the vector \var(prG[iGt][iPt][iG]).
  */
  void CalcTransPrG();


  /** Calculate the transition probability values for weather information regarding evatranspiration.
   *
   *  Values are stored in the vector \var(prE[iEt][iPt][iE]).
   */

  void CalcTransPrE();



 /** Calculate the transition probability values for weather information regarding precipitation.
  *
  *  Values are stored in the vector \var(prP[iPt][iP]).
  */
  void CalcTransPrP();

  /** Print the optimal policy with optimal value functions in a .csv file.
   *
   */


  void printPolicy();


  /** Calculate the future soil water content based on a rainfall-runoff model given in \url(http://onlinelibrary.wiley.com/doi/10.1002/hyp.6629/abstract).
  *
  * @param Mois_WedcentrePoints Soil water contents based on centerpoints defined for weather information.
  * @param wed_data A matrix containing Prediction weather information during growing cycle.
  * @param Tt Prediction of average air temprature for the next day.
  * @param Pt Prediction of total precipitation for the next day.
  * @param Gt Prediction of total global radiation for the next day.
  * @param Et Prediction of total wind data for the next day.
  *
  * @return A predition of soil water content at the next day.
  */
  double  Hydro(arma::mat Mois_WedcentrePoints, arma::mat wed_data, double & Tt, double & Pt, double & Gt, double & Et);


  /** Calculate the transition probability for variance component (posterior mean of latent variable) in a non-Gaussian SSM baesd on Theorem 4 in \url(http://www.sciencedirect.com/science/article/pii/S0377221715008802).
  *
  * @param t current day.
  * @param lower Lower limit of a variance component in the next day.
  * @param upper Upper limit of a variance component in the next day.
  * @param var Center point of the variance component in the current day.
  *
  * @return Transition probability for the variance component at the next day given variance var.
  */
  double PrNGSSM(int t, double lower, double upper, double var);

  /** Convert integers into a string. */
  string getLabel(const int & a, const int & b, const int & c, const int & d, const int & e, const int & f, const int & g, const int & h, const int & i, const int & j) {
    std::ostringstream s;
    s << "(" << a << "," << b << "," << c << "," << d << "," << e << "," << f << "," << g << "," << h << "," << i << "," << j << ")";
    return s.str();
  }

  /** Convert integers into a string. */
  string getLabel(const int & a, const int & b, const int & c, const int & d, const int & e, const int & f, const int & g, const int & h, const int & i, const int & j, const int & k) {
    std::ostringstream s;
    s << "(" << a << "," << b << "," << c << "," << d << "," << e << "," << f << "," << g << "," << h << "," << i << "," << j << "," << k << ")";
    return s.str();
  }

  /** Find the index of a given state based on the discritization matrix .
   *
   * @param st A value in a given discretized matrix
   * @param dis A matrix containing discretized values of a continuous state variable.
   *
   * @return The index number of a specefic interval in dis that includes st.
   */
  int findIndex(double st, arma::mat dis);

  //----------------------------------------------------------------------------------------------------------------------------------

  private:   // variables

    static const double ZERO;  // trans pr below are considered as ZERO

    arma::vec opSeq;
    arma::vec opE;
    arma::vec opL;
    arma::vec opD;
    arma::vec opDelay;
    arma::vec opFixCost;
    arma::vec watTh;

    arma::vec watUpper;
    arma::vec watLower;
    double weightCompletion;
    double weightWorkable;
    double weightTraffic;
    double BetaM1;
    double Gama;
    double Lambda;
    int minOpt;
    int maxOpt;
    int StartDay;
    int LastDay;

    int opNum;
    int tMax;
    double coefLoss;
    double priceYield;
    double machCap;
    double yieldHa;
    double fieldArea;
    double coefTimeliness;
    double costSkip;

    double temMeanDry;
    double temMeanWet;
    double temVarDry;
    double temVarWet;
    double GRdMeanWet;
    double GRdMeanDry;
    double WindMeanWet;
    double WindMeanDry;
    double GRdVarWet;
    double GRdVarDry;
    double WindVarWet;
    double WindVarDry;
    double dryDayTh;
    double precShape;
    double precScale;
    double prDryWet;
    double prWetWet;
    double WindScale_Dry;
    double WindShape_Dry;
    double WindScale_Wet;
    double WindShape_Wet;

    double hydroWatR;
    double hydroWatS;

    double gSSMW;
    double gSSMV;
    double gSSMm0;
    double gSSMc0;
    double nGSSMm0;
    double nGSSMc0;
    double nGSSMK;

    bool check;
    bool rewRisk;

    arma::mat SCIout;
    arma::mat Mois_WedcentrePoints;
    arma::mat wed_data;

    arma::mat dMP;
    arma::mat dSP;
    arma::mat dMW;
    arma::mat dMM;
    arma::mat dSW;
    arma::mat dT;
    arma::mat dP;
    arma::mat dG;
    arma::mat dE;

    arma::vec sMP;
    arma::vec sSP;
    arma::vec sMW;
    arma::vec sMM;
    arma::vec sSW;
    arma::vec sT;
    arma::vec sP;
    arma::vec sG;
    arma::vec sE;

    int sizeSMP;
    int sizeSSP;
    int sizeSMW;
    int sizeSMM;
    int sizeSSW;
    int sizeST;
    int sizeSP;
    int sizeSG;
    int sizeSE;

    double totalRew;

    vector<double> pr;

    vector< vector< vector< vector< vector< vector< vector< vector<double> > > > > > > > prMW;
    vector< vector< vector< vector< vector< vector< vector< vector<double> > > > > > > > prMP;
    vector< vector< vector< vector< vector< vector<double> > > > > > prSP;
    vector< vector< vector<double> > > prSW;
    vector< vector< vector<double> > > prT;
    vector< vector< vector<double> > > prG;
    vector< vector< vector<double> > > prE;
    vector< vector<double> > prP;
    vector< vector< vector< vector<double> > > >rewDo;
    vector< vector< vector< vector< vector< vector< vector< vector <vector< vector<int> > > > > > > > > >mapL1Vector;
    vector< vector< vector< vector< vector< vector< vector< vector< vector< vector< vector<double> > > > > > > > > > >valueFun;
    vector< vector< vector< vector< vector< vector< vector< vector< vector< vector< vector<string> > > > > > > > > > >optAction;
    vector<double> valFunDummy;

    TimeMan cpuTime;
    string label;
};


#endif
