#' @title Logistic regression with elasticnet
#' @name elasticnet_logistic
#' @description The 'elasticnet_logistic' function implements a logistic regression model based on ElasticNet regularization, and selects the best ' alpha ' and ' lambda ' parameters through cross-validation. Firstly, the function selects the optimal model parameters by cross-validating different ' alpha ' values ( controlling the L1 and L2 regularization ratios ). Then, the final model is trained with the best parameters, and the prediction and evaluation are carried out on the test set, and the accuracy, precision, recall, F1 score and support are calculated. At the same time, the function also draws the ROC curve and calculates the AUC value to evaluate the classification performance of the model. Finally, the results of optimal model, model coefficient, cross validation score, test set performance index and ROC curve are returned. This process helps to optimize model performance in classification tasks and provides comprehensive performance evaluation.
#' @param X_train Training set of explanatory variables
#' @param y_train Training set (vector) of explanatory variables
#' @param X_test Test set of explanatory variables
#' @param y_test Test set (vector) of explanatory variables
#' @import glmnet
#' @import caret
#' @import pROC
#' @importFrom graphics legend
#' @importFrom stats coef predict sd
#' @return Solution results and a range of model evaluation results \code{n}
#' @examples
#' \dontrun{
#'     set.seed(42)
#'     X_train <- matrix(rnorm(100 * 20), ncol = 20)
#'     y_train <- sample(c(0, 1), 100, replace = TRUE)
#'     y_test <- sample(c(0, 1), 50, replace = TRUE)
#'     results <- elasticnet_logistic(X_train, y_train, X_test, y_test)
#' }
#' @export
library(glmnet);library(caret);library(pROC)
elasticnet_logistic <- function(X_train, y_train, X_test, y_test, seed = 42) {
  set.seed(seed)
  best_alpha <- NULL;  best_model <- NULL; best_lambda <- NULL
  best_cv_score <- -Inf  
  
  # l1_ratio(0 = Ridge, 1 = Lasso)
  alphas <- seq(0, 1, length.out = 10)
  # Loop traversing different alpha values for cross validation
  for (alpha in alphas) {
    cv_model <- cv.glmnet(X_train, y_train, alpha = alpha, family = "binomial")
    # Get the best score and lambda value for cross-validation
    cv_score <- min(cv_model$cvm)  
    best_lambda_temp <- cv_model$lambda.min  
    # Compare current cross-validation scores and select the optimal alpha and lambda combination
    if (cv_score > best_cv_score) {
      best_cv_score <- cv_score
      best_alpha <- alpha
      best_lambda <- best_lambda_temp
      best_model <- cv_model  
    }
  }
  ## best situation
  CV_scores=mean(best_model$cvm)
  cat('regularization parameter:','best_alpha')
  # prediction
  prob_pred <- predict(best_model, X_test, type = "response", s = best_lambda)
  class_pred <- ifelse(prob_pred > 0.5, 1, 0)
  class_pred <- factor(class_pred, levels = c(0, 1))
  y_test <- factor(y_test, levels = c(0, 1))
  # Ensure predicted and true label lengths are consistent
  if (length(class_pred) != length(y_test)) {
    stop("The length of the predicted and real labels do not match!")
  }
  # test accuracy
  test_accuracy <- mean(class_pred == y_test)
  # model_coef
  model_coef <- coef(best_model, s = best_lambda)
  #Converts the coefficients into a common matrix form for output
  coef_matrix <- as.matrix(model_coef)
  # Calculate and output precision, recall, F1 score, and support.
  confusion_matrix <- confusionMatrix(class_pred, y_test)
  precision <- confusion_matrix$byClass["Pos Pred Value"]
  recall <- confusion_matrix$byClass["Sensitivity"]
  f1_score <- confusion_matrix$byClass["F1"]
  support <- sum(confusion_matrix$table)
  # ROC
  prob_pred <- as.vector(prob_pred)  
  roc_curve <- roc(y_test, prob_pred)
  plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
  auc_value <- auc(roc_curve)
  legend("bottomright", legend = paste("AUC =", round(auc_value, 3)),
         col = "blue", lwd = 2,cex=0.7,bty = "n")
  
  return(list(model = best_model,
              coef=coef_matrix,   
              best_alpha=best_alpha, 
              CV_scores=CV_scores,  
              test_accuracy=test_accuracy,   
              precision = precision, 
              recall = recall, 
              f1_score = f1_score,
              support = support, 
              roc_curve = roc_curve))
}
