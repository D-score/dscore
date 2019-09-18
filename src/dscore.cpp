// C++ functions for the d-score package
// Author: Arjan Huizing

#include "RcppArmadillo.h" // automatically calls Rcpp.
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

//' Normalize distribution
//'
//' Normalizes the distribution so that the total mass equals 1.
//' @aliases normalize
//' @param d A vector with \code{length(qp)} elements representing
//' the unscaled density at each quadrature point.
//' @param qp Vector of equally spaced quadrature points.
//' @return A \code{vector} of \code{length(d)} elements with 
//' the prior density estimate at each quadature point.
//' @examples 
//' dscore:::normalize(c(5, 10, 5), qp = c(0, 1, 2))
//' 
//' sum(dscore:::normalize(rnorm(5), qp = 1:5))
// [[Rcpp::export]]
NumericVector normalize(NumericVector d, NumericVector qp) {
  if(d.size() != qp.size()){
    Rcpp::stop("Arguments `d` and  `qp` of different length");
  } 
  NumericVector normalized = (d/sum(d)) / (qp(1) - qp(0));
  return(normalized);
}

//' Calculate posterior for one item given score, difficulty and prior
//'
//' @details
//' This function assumes that the difficulties have been estimated by 
//' a binary Rasch model (e.g. by 
//' \code{sirt::rasch.pairwise.itemcluster()}). 
//' 
//' @aliases posterior
//' @param score Integer, either 0 (fail) and 1 (pass)
//' @param tau Numeric, difficulty parameter
//' @param prior Vector of prior values on quadrature points \code{qp}
//' @param qp vector of equally spaced quadrature points
//' @return A vector of length \code{length(prior)}
//' @author Stef van Buuren, Arjan Huizing, 2019
//' @seealso \code{\link{dscore}}, 
//' \code{\link[sirt]{rasch.pairwise.itemcluster}}
// [[Rcpp::export]]
NumericVector posterior(int score, double tau, 
                        NumericVector prior, 
                        NumericVector qp){
  
  NumericVector cpc;
  score += 1;
  if(score < 1 | score > 2){stop("score out-of-range.");}
  
  // compute category respones probability under 1PL model
  NumericVector p = plogis(qp, tau);
  
  if(score == 1){
    cpc = 1.0 - p;
    cpc = cpc/sum(cpc);
  } else if(score == 2){
    cpc = p;
    cpc = cpc/sum(cpc);
  } else if(score == 3){
    cpc = NA_REAL;
  }
  
  // clean missings
  LogicalVector prior_miss = is_na(prior);
  prior[prior_miss] = 0;
  
  // calc posterior per category
  NumericVector postcat = cpc * prior;
  postcat = normalize(postcat, qp);
  
  return(postcat);
}

double wmean(NumericVector x, NumericVector w) {
  int n = x.size();
  double total = 0, total_w = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i] * w[i];
    total_w += w[i];
  }
  return total / total_w;
}

//' Calculate posterior of ability
//' 
//' @inheritParams dscore
//' @param scores A vector with PASS/FAIL observations. 
//' Scores are coded numerically as \code{pass = 1} and \code{fail = 0}. 
//' Alternatively, \code{pass = TRUE} and \code{fail = FALSE} may be used. 
//' @param tau A vector containing the item difficulties for the item scores in \code{scores}
//' estimated from the Rasch model in the prefferred metric/scale.
//' @param mu Numeric scalar. The mean of the prior.
//' @author Stef van Buuren, Arjan Huizing, 2019
//' @param sd Numeric scalar. Standard deviation of the prior.
// [[Rcpp::export]]
List calculate_posterior(NumericVector scores, 
                          NumericVector tau, 
                          NumericVector qp,
                          double mu, double sd){
  
  List fullpost = List::create(Named("eap",NumericVector({NA_REAL})),
                               Named("start",R_NilValue),
                               Named("qp",qp),
                               Named("posterior",R_NilValue));
  int m = scores.length();
  int k = 0; // valid item score counter
  int score;
  double tauj;
  NumericVector prior;
  NumericVector post;
  
  for(int j = 0; j < m; j++){ // loop over item scores
    score = scores[j];
    tauj = tau[j];
    if (!arma::is_finite(score) | !arma::is_finite(tauj)){
      continue;
    }
    k += 1;
    
    // For the first item, get age-dependent prior, else old prior
    if(k == 1){
      prior = dnorm(qp, mu, sd);
      prior = normalize(prior, qp);
      fullpost["start"] = prior;
    } else
      prior = post;
    
    // calculate posterior
    post = posterior(score, tauj, prior, qp);
    fullpost["posterior"] = post;
    
    // overwrite old eap estimate
    fullpost["eap"] = wmean(qp, post);
    
  } // end for loop
  return fullpost;
}