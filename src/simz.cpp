// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>

using namespace arma;

// [[Rcpp::export]]
arma::imat simz_pcount(arma::umat y, arma::mat lam_post, arma::cube p_post,
                       unsigned K, arma::uvec Kmin, arma::uvec kvals){

  int M = y.n_rows;
  int J = y.n_cols;
  int nsamples = lam_post.n_cols;

  vec kprob(K+1);
  double pp, bp;

  imat Zpost(M, nsamples);

  for (unsigned i=0; i < nsamples; i++){
    for (unsigned m=0; m < M; m++){
      if(!is_finite(lam_post(m,i))){
        Zpost(m,i) = NA_INTEGER;
        continue;
      }
      kprob.zeros();
      for (unsigned k=Kmin(m); k < (K+1); k++){
        pp = R::dpois(k, lam_post(m, i), 0);
        bp = 1.0;
        for (unsigned j=0; j < J; j++){
          if(!is_finite(p_post(m,j,i))){
            continue;
          }
          bp *= R::dbinom(y(m,j), k, p_post(m,j,i), 0);
        }
        kprob(k) = pp * bp;
      }
      kprob = kprob / sum(kprob);
      Zpost(m,i) = Rcpp::RcppArmadillo::sample(kvals, 1, false, kprob)(0);
    }
  }

  return Zpost;

}

// [[Rcpp::export]]
arma::imat simz_occuRN(arma::umat y, arma::mat lam_post, arma::cube r_post,
                       unsigned K, arma::uvec Kmin, arma::uvec kvals){

  int M = y.n_rows;
  int J = y.n_cols;
  int nsamples = lam_post.n_cols;

  vec kprob(K+1);
  double pp, bp;

  imat Zpost(M, nsamples);

  cube q = 1 - r_post;
  double p;

  for (unsigned i=0; i < nsamples; i++){
    for (unsigned m=0; m < M; m++){
      if(!is_finite(lam_post(m,i))){
        Zpost(m,i) = NA_INTEGER;
        continue;
      }
      kprob.zeros();
      for (unsigned k=Kmin(m); k < (K+1); k++){
        pp = R::dpois(k, lam_post(m, i), 0);
        bp = 1.0;
        for (unsigned j=0; j < J; j++){
          p = 1 - pow(q(m,j,i), k);
          if(!is_finite(p)){
            continue;
          }
          bp *= R::dbinom(y(m,j), 1, p, 0);
        }
        kprob(k) = pp * bp;
      }
      kprob = kprob / sum(kprob);
      Zpost(m,i) = Rcpp::RcppArmadillo::sample(kvals, 1, false, kprob)(0);
    }
  }

  return Zpost;

}

//This should be split out into its own file eventually
double dmultinom(arma::urowvec x, arma::rowvec prob){
  double out;
  double logout;
  logout = lgamma(sum(x) + 1) + sum(x % log(prob) - lgamma(x + 1));
  out = exp(logout);
  return out;
}


// [[Rcpp::export]]
arma::imat simz_multinom(arma::umat y, arma::mat lam_post, arma::cube p_post,
                         unsigned K, arma::uvec Kmin, arma::uvec kvals){

  int M = y.n_rows;
  int J = y.n_cols;
  int nsamples = lam_post.n_cols;

  vec kprob(K+1);
  umat yblank(M, 1);
  umat yexpand = join_rows(y, yblank);

  double pp, bp;

  umat not_observed(M, K+1);

  for (unsigned m=0; m < M; m++){
    for (unsigned k=Kmin(m); k < (K+1); k++){
      not_observed(m, k) = k - sum(y.row(m));
    }
  }

  rowvec psub(J+1);
  imat Zpost(M, nsamples);

  for (unsigned i=0; i < nsamples; i++){
    for (unsigned m=0; m < M; m++){
      if(!is_finite(lam_post(m,i))){
        Zpost(m,i) = NA_INTEGER;
        continue;
      }
      kprob.zeros();
      for (unsigned k=Kmin(m); k < (K+1); k++){
        pp = R::dpois(k, lam_post(m, i), 0);
        bp = 1.0;
        yexpand(m, J) = not_observed(m, k);
        psub = p_post(span(m),span(),span(i));
        bp = dmultinom(yexpand.row(m), psub);
        kprob(k) = pp * bp;
      }
      kprob = kprob / sum(kprob);
      Zpost(m,i) = Rcpp::RcppArmadillo::sample(kvals, 1, false, kprob)(0);
    }
  }

  return Zpost;

}
