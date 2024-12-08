#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix gibbs_sampling(int m, int n, double a, double b) {
  // 创建一个 m 行 2 列的矩阵来存储样本 (x, y)
  NumericMatrix x(m, 2);
  // 初始值
  double xt_1 = 0.5; 
  double xt_2 = 0.5;
  // Gibbs抽样循环
  for (int i = 1; i < m; ++i) {
    // 从条件分布Binomial(n, y)抽样更新 x
    xt_1 = R::rbinom(n, xt_2);
    // 从条件分布Beta(x + a, n - x + b)抽样更新 y
    xt_2 = R::rbeta(xt_1 + a, n - xt_1 + b);
    // 存储更新后的样本
    x(i, 0) = xt_1;
    x(i, 1) = xt_2;
  }
  return x;
}
