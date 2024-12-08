#include <Rcpp.h>
using namespace Rcpp;
//' @title stratified_train_test_split
//' @name stratified_train_test_split
//' @description Performs stratified sampling to split a dataset into training and test sets.
//' @param X A numeric matrix or data frame containing the features, where rows are samples and columns are features.
//' @param y A vector of class labels, where each element corresponds to the class of the sample in `X`.
//' @param test_ratio A numeric value between 0 and 1 specifying the proportion of the dataset to include in the test set.
//' @import Rcpp
//'@return A list containing:
//'\itemize{
//' \item \code{X_train}: A numeric matrix or data frame of features for the training set.
//' \item \code{y_train}: A vector of class labels for the training set.
//'  \item \code{X_test}: A numeric matrix or data frame of features for the test set.
//' \item \code{y_test}: A vector of class labels for the test set.
//' }
//' @details This function performs stratified sampling to split the dataset into training and test sets while ensuring that each class in `y` is proportionally represented in both the training and test sets.
//' @examples
//' \dontrun{
//' set.seed(123)
//' X <- matrix(rnorm(1000), ncol = 10)  # 100 samples, 10 features
//' y <- sample(1:3, 100, replace = TRUE)  # 3 classes
//' result <- stratified_train_test_split(X, y, test_ratio = 0.2)
//'
//' # Access the training and testing sets
//' X_train <- result$X_train
//' y_train <- result$y_train
//'X_test <- result$X_test
//' y_test <- result$y_test
//' }
//' @export

// [[Rcpp::export]]
List stratified_train_test_split(NumericMatrix X, IntegerVector y, double test_ratio) {
  int n = X.nrow();  // 样本数量
  std::map<int, std::vector<int>> strata_map;
  
  // 根据y的值，将样本按类别分组
  for (int i = 0; i < n; ++i) {
    strata_map[y[i]].push_back(i);  // 使用样本索引作为分组依据
  }
  
  // 用于存储训练集和测试集的索引
  std::vector<int> train_indices;
  std::vector<int> test_indices;
  
  // 对每个类别进行分层抽样
  for (auto& entry : strata_map) {
    int stratum_size = entry.second.size();
    int test_size = std::round(stratum_size * test_ratio);  // 计算测试集的样本数
    
    // 随机抽取测试集的样本索引
    IntegerVector indices = wrap(entry.second);  // 获取当前类别的所有样本索引
    IntegerVector sampled_indices = Rcpp::sample(indices, test_size, false);
    
    // 将抽取的样本加入测试集
    for (int i = 0; i < sampled_indices.size(); ++i) {
      test_indices.push_back(sampled_indices[i]);
    }
    
    // 将剩余的样本加入训练集
    std::set<int> sampled_set(sampled_indices.begin(), sampled_indices.end());
    for (int i = 0; i < stratum_size; ++i) {
      if (sampled_set.find(entry.second[i]) == sampled_set.end()) {
        train_indices.push_back(entry.second[i]);
      }
    }
  }
  
  // 创建训练集和测试集的数据
  NumericMatrix X_train(train_indices.size(), X.ncol());
  NumericMatrix X_test(test_indices.size(), X.ncol());
  IntegerVector y_train(train_indices.size());
  IntegerVector y_test(test_indices.size());
  
  for (int i = 0; i < train_indices.size(); ++i) {
    int idx = train_indices[i];
    X_train(i, _) = X(idx, _);  // 提取训练集特征
    y_train[i] = y[idx];        // 提取训练集标签
  }
  
  for (int i = 0; i < test_indices.size(); ++i) {
    int idx = test_indices[i];
    X_test(i, _) = X(idx, _);  // 提取测试集特征
    y_test[i] = y[idx];        // 提取测试集标签
  }
  
  // 返回训练集和测试集
  return List::create(
    Named("X_train") = X_train,
    Named("y_train") = y_train,
    Named("X_test") = X_test,
    Named("y_test") = y_test
  );
}
