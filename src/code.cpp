#include <Rcpp.h>
using namespace Rcpp;

//' Calculate euclidean distance for the safety distances
//'
//' @param A A data.matrix produced in the simulation
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
