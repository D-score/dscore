// C++ functions for the dscore package
// Author: Arjan Huizing

#include "RcppArmadillo.h" // automatically calls Rcpp.
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

//' Normalize distribution
//'
//' Normalizes the distribution so that the total mass equals 1.
//' @aliases normalize
//' @param d A vector with `length(qp)` elements representing
//' the unscaled density at each quadrature point.
//' @param qp Vector of equally spaced quadrature points.
//' @return A vector of `length(d)` elements with
//' the prior density estimate at each quadature point.
//' @note: Internal function
//' @examples
//' dscore:::normalize(c(5, 10, 5), qp = c(0, 1, 2))
//'
//' sum(dscore:::normalize(rnorm(5), qp = 1:5))
// [[Rcpp::export]]
NumericVector normalize(NumericVector d, NumericVector qp) {
  if(d.size() != qp.size()){
    Rcpp::stop("Arguments `d` and  `qp` of different length");
  }
  NumericVector normalized = d/sum(d);
  return(normalized);
}

//' Calculate posterior for one item given score, difficulty and prior
//'
//' @details
//' This function assumes that the difficulties have been estimated by
//' a binary Rasch model, e.g. by `rasch.pairwise.itemcluster()` of
//' the `sirt` package.
//'
//' @aliases posterior
//' @param score Integer, either 0 (fail) and 1 (pass)
//' @param tau Numeric, difficulty parameter
//' @param prior Vector of prior values on quadrature points `qp`
//' @param qp vector of equally spaced quadrature points
//' @param scale expansion relative to the logit scale
//' @return A vector of length `length(prior)`
//' @author Stef van Buuren, Arjan Huizing, 2020
//' @note: Internal function
//' @seealso [dscore()]
// [[Rcpp::export]]
NumericVector posterior(int score, double tau,
                        NumericVector prior,
                        NumericVector qp,
                        double scale) {

  NumericVector cpc;
  score += 1;
  if((score < 1) | (score > 2)){stop("score out-of-range.");}

  // compute category respones probability under 1PL model
  NumericVector p = plogis(qp, tau, scale);

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
//' If the tauj is not within the range rello - relhi from the
//' dynamic EAP, the procedure ignores the score of item j.
//' @param scores A vector with PASS/FAIL observations.
//' Scores are coded numerically as `pass = 1` and `fail = 0`.
//' @param tau A vector containing the item difficulties for the item
//' scores in `scores` estimated from the Rasch model in the
//' preferred metric/scale.
//' @param qp Numeric vector of equally spaced quadrature points.
//' @param scale Scale expansion
//' @param mu Numeric scalar. The mean of the prior.
//' @param sd Numeric scalar. Standard deviation of the prior.
//' @param relhi Positive numeric scalar. Upper end of the relevance interval
//' @param rello Negative numeric scalar. Lower end of the relevance interval
//' @author Stef van Buuren, Arjan Huizing, 2020
//' @return A `list` with three elements:
//'
//' | Name | Label |
//' | --- | --------- |
//' `eap` | Mean of the posterior
//' `gp`  | Vector of quadrature points
//' `posterior` | Vector with posterior distribution.
//'
//' Since `dscore V40.1` the function does not return the `"start"` element.
// [[Rcpp::export]]
List calculate_posterior(NumericVector scores,
                          NumericVector tau,
                          NumericVector qp,
                          double scale,
                          double mu, double sd,
                          double relhi, double rello){

  List fullpost = List::create(Named("eap",NumericVector({NA_REAL})),
                               Named("qp",qp),
                               Named("posterior",R_NilValue));
  int m = scores.length();
  int score;
  double tauj;
  double eap;
  NumericVector prior;
  NumericVector post;

  // initialize prior, posterior and EAP
  prior = dnorm(qp, mu, sd);
  prior = normalize(prior, qp);
  post = prior;
  fullpost["posterior"] = post;
  fullpost["eap"] = wmean(qp, post);

  for(int j = 0; j < m; j++){ // loop over item scores
    score = scores[j];
    tauj = tau[j];
    eap = fullpost["eap"];
    if (!arma::is_finite(score) || !arma::is_finite(tauj)){
      continue;
    }
    if (((tauj - eap) > relhi) || ((tauj - eap) < rello)) {
      continue;
    }

    // calculate posterior
    prior = post;
    post = posterior(score, tauj, prior, qp, scale);

    // store posterior and eap estimate
    fullpost["posterior"] = post;
    fullpost["eap"] = wmean(qp, post);

  } // end for loop
  return fullpost;
}