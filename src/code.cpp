#include <Rcpp.h>
using namespace Rcpp;

//' Calculate euclidean distance for the safety distances
//'
//' @param A data.matrix produced in the simulation
//' @export
// [[Rcpp::export]]
NumericVector sim_dist(NumericMatrix A) {
  int nr=A.nrow();
  NumericVector distance(nr);
  for (int i=0; i<nr; i++) {
    distance[i] = sqrt(pow(A(i,0) - A(i,2),2.0) + pow(A(i,1) - A(i,3),2.0));
  }
  return distance;
}

//' Calculate smallest distance between points not in same zone
//'
//' @param A data.matrix produced from solve_ga function with obj == "SAFE"
//' @export
// [[Rcpp::export]]
double safe_dist(NumericMatrix A) {
  int nr=A.nrow();
  NumericVector dist_temp(nr);
  NumericVector distance(nr);
  for (int i=0; i<nr; i++) {
    for (int j=0; j<nr; j++) {
      //     // if (result$`Centroid id`[i] == result$`Centroid id`[j]) {dist_temp[j] <- Inf}
      if (A(i,1) == A(j,1)) {
        dist_temp[j] = R_PosInf;
        //     // else dist_temp[j] <- euclid_norm(c(result$x[i],result$y[i])-c(result$x[j],result$y[j]))
      } else {
        dist_temp[j] = sqrt(pow(A(i,3) - A(j,3),2.0) + pow(A(i,4) - A(j,4),2.0));
      }
    }
    distance[i] = min(dist_temp);
  }
  return min(distance);
}
