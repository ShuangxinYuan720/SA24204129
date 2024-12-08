## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=TRUE----------------------------------------------------------------
library(SA24204129)
seed=42;set.seed(seed)

## -----------------------------------------------------------------------------
## 加载数据集
library(mlbench)
data(PimaIndiansDiabetes)
data <- PimaIndiansDiabetes
data$diabetes <- ifelse(data$diabetes == 'pos', 1, 0)
#summary(data)
X <- data[, -ncol(data)] 
y <- data$diabetes

## -----------------------------------------------------------------------------
processed_X <- preprocess(X)
# 打印预处理后的数据
print("预处理后的数据：")
print(head(processed_X))

## -----------------------------------------------------------------------------
p_X=as.matrix(processed_X)
## stratified_train_test_split函数分层划分80%训练集和20%测试集
result <- stratified_train_test_split(p_X, y, test_ratio = 0.2)
X_train <- result$X_train
y_train <- result$y_train
X_test <- result$X_test
y_test <- result$y_test
## 查看分层结果来验证比例
table(y_train)
table(y_test)

## -----------------------------------------------------------------------------
## 使用elasticnet_logistic进行训练和评估
results <- elasticnet_logistic(X_train, y_train, X_test, y_test,seed=seed)
# 查看模型评估结果
print(results)

