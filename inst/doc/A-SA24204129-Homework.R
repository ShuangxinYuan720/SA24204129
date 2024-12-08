## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=FALSE---------------------------------------------------------------
####——————【【数据模拟】】——————
# 种子设置
set.seed(6543)

# 设置参数
n <- 1000000   # 样本数
p <- 10    # 变量数

# 生成V_1和V_2的随机数
V_1 <- rnorm(n, mean = 0, sd = sqrt(290))
V_2 <- rnorm(n, mean = 0, sd = sqrt(300))

# 计算V_3
e <- rnorm(n)
V_3 <- -0.3 * V_1 + 0.925 * V_2 + e

# 至此,三个隐藏因子构造完毕

# 初始化矩阵X，使用零矩阵确保结构正确
X <- matrix(0, nrow = n, ncol = p)  # 确保是一个n行p列的零矩阵

# 构造X矩阵的前4列
for (i in 1:4) {
  e1 <- rnorm(n)  # 随机误差
  X[, i] <- V_1 + e1  # 确保每列的长度一致
}

# 构造X矩阵的第5至8列
for (i in 5:8) {
  e2 <- rnorm(n)  # 随机误差
  X[, i] <- V_2 + e2  # 确保每列的长度一致
}

# 构造X矩阵的第9和第10列
for (i in 9:10) {
  e3 <- rnorm(n)  # 随机误差
  X[, i] <- V_3 + e3  # 确保每列的长度一致
}

# 检查矩阵 X 的维度
dim(X)  # 确保它是 n x p 的矩阵

# 对X进行标准化
X_standardized <- scale(X)   # 对X标准化-零均值&单位方差
# 或者按列逐一标准化:
# X_standardized <- apply(X, 2, scale)

####——————【【PCA】】——————
# 进行主成分分析（PCA）
pca_res <- prcomp(X_standardized, scale. = TRUE)  

# 载荷（loadings）
pca_loadings <- pca_res$rotation    

# 方差贡献率（%）
pca_variance_contrib <- pca_res$sdev^2 / sum(pca_res$sdev^2) * 100  

# 累积贡献率（%）
pca_cum_contrib <- cumsum(pca_variance_contrib)  

# 【输出结果】
combined_mat <- rbind(pca_loadings[,1:3], pca_variance_contrib[1:3], pca_cum_contrib[1:3])    

# 转化为表格
table <- as.data.frame(combined_mat)

# 给行加名字
rownames(table) <- c('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','方差贡献率(%)','累积贡献率(%)')

# 输出表格
knitr::kable(table, format = "markdown", digits = 3)


## ----iris, echo=FALSE---------------------------------------------------------
# 提取 Sepal.Length 数据
sepal_length = iris$Sepal.Length

# 正态性检验：绘制 QQ 图
qqnorm(sepal_length, main="QQ Plot of Sepal Length",col="deeppink")
qqline(sepal_length, col="deepskyblue")

## ----sepal_length, echo=FALSE-------------------------------------------------
# 因为 Sepal.Length 是一个连续变量，我们可以将其按种类分组进行 t 检验
# 提取每个种类的数据
setosa <- iris$Sepal.Length[iris$Species == "setosa"]
versicolor <- iris$Sepal.Length[iris$Species == "versicolor"]
virginica <- iris$Sepal.Length[iris$Species == "virginica"]

# 独立样本 t 检验：Setosa vs Versicolor
t_test_setosa_versicolor <- t.test(setosa, versicolor, var.equal = TRUE)
print(t_test_setosa_versicolor)

# 独立样本 t 检验：Setosa vs Virginica
t_test_setosa_virginica <- t.test(setosa, virginica, var.equal = TRUE)
print(t_test_setosa_virginica)

# 独立样本 t 检验：Versicolor vs Virginica
t_test_versicolor_virginica <- t.test(versicolor, virginica, var.equal = TRUE)
print(t_test_versicolor_virginica)

## ----iris sepal_length, echo=FALSE--------------------------------------------

boxplot(Sepal.Length ~ Species, data=iris, main="Boxplot of Sepal Length by Species", 
        col=c("darkgray",'gold','deepskyblue'), border=c('deeppink','purple','red'))
        


## ----women,echo=FALSE---------------------------------------------------------
plot(women$height, women$weight,
xlab = "Height (in inchs)", ylab = "Weight (in pounds)",col='deeppink',pch=19)
mo=lm(women$weight ~ women$height)
a=mo$coefficients[1]
b=mo$coefficients[2]
mo_result=summary(mo)
squ_h= I((women$height)^2)


## ----mo_result,echo=FALSE-----------------------------------------------------
mo_result

## ----mo , fig.width=7, fig.height=5,echo=FALSE--------------------------------
par(mfrow=c(2,2)) 
plot(mo)
par(mfrow=c(1,1)) # 恢复设置

## ----women squ_h, echo=FALSE--------------------------------------------------
mo_1=lm(women$weight ~ women$height + squ_h)
summary(mo_1)
plot(women$height, women$weight,
xlab = "Height (in inchs)", ylab = "Weight (in pounds)",col='deeppink')
lines(women$height, col='deepskyblue',fitted(mo_1))

## ----echo=TRUE,fig.width=7, fig.height=5--------------------------------------
# 设置随机种子 可复现
set.seed(1234)

# 定义生成瑞利分布样本的函数
rayleigh_samples = function(sigma, n) {
  U = runif(n) #生成[0,1]
  samples = sigma * sqrt(-2 * log(U)) #逆变换
  return(samples)
}

# 选取一系列Rayleigh分布的参数sigma
sigmas=c(0.5,1,2,4)
n=8000  # 样本数量

# 循环生成样本并绘制直方图
par(mfrow = c(2, 2))  # 设置图形布局2×2
for (sigma in sigmas) 
  {
  samples = rayleigh_samples(sigma, n)
  # 绘制直方图
  hist(samples, breaks = 30, main = paste("Rayleigh Distribution (sigma =", sigma, ")"), 
       xlab = "Value", col = "lightblue", border = "black", prob = TRUE)
  # 添加理论密度函数曲线
  curve((x / sigma^2) * exp(-x^2 / (2 * sigma^2)), add = TRUE, col = "deeppink", lwd = 2)

  # 添加众数线
  abline(v = sigma, col = "blue", lwd = 2, lty = 2)  # 理论众数
  legend("topright", legend = c('Mode', "Density"), col = c("blue", "deeppink"), lwd = 2)
}
par(mfrow = c(1, 1)) # 还原图形布局

## ----echo=TRUE,fig.width=7, fig.height=5--------------------------------------
set.seed(1234)
# 创建一个函数模拟并生成直方图
mix_hist=function(n,p1, co='lightblue') 
  #参数n为生成随机样本数,p1为N(0,1)的混合概率,co为绘制颜色
{
  p=rbinom(n,1,prob = p1)
  #生成两个正态分布随机数 N(0,1)和N(3,1)
  x1=rnorm(n) 
  x2=rnorm(n,3,1)
  x=p*x1+(1-p)*x2 #混合
  #绘制直方图
  title=paste('p1 = ',p1)
  hist(x,probability = T,main = title,col = co)
  #理论密度函数
  densityplot=function(x)
    {
      p1*dnorm(x)+(1-p1)*dnorm(x,3,1)
  }
  y=seq(-3,6,0.1)
  lines(y,densityplot(y),col='deeppink',lwd=2)
} 

mix_hist(50000,0.75)#p1=0.75的情况

par(mfrow = c(3, 3))  # 设置图形布局3×3
for (p1 in seq(0.1,0.9,0.1)) #p1取值为0.1,0.2,……,0.9
  {
  mix_hist(50000,p1,'gray')
  }
par(mfrow = c(1, 1)) # 还原图形布局

## ----echo=TRUE----------------------------------------------------------------

set.seed(1234)
# 函数一：复合泊松-伽马过程模拟函数
com_poisson_gamma = function(lambda_rate, alpha, beta, T, num) 
  {
  X_T = numeric(num)  # 初始化结果向量
  
  for (i in 1:num) {
    # 生成泊松过程中的事件数量
    N_T = rpois(1, lambda_rate * T)
    # 生成伽马分布的随机变量并求和
    if (N_T > 0) 
      {
      Y = rgamma(N_T, shape = alpha, rate = beta)
      X_T[i] = sum(Y)} 
    else {
      X_T[i] = 0}
  }
  return(X_T)
}

# 函数2：估计平均值和方差
mean_var = function(X_T, lambda_rate, alpha, beta, T) {  
  emp_mean = mean(X_T)  
  emp_var = var(X_T)  
  theo_mean = lambda_rate * T * (alpha / beta)  
  theo_var = lambda_rate * T * ((alpha * (alpha + 1)) / beta^2 - (alpha / beta)^2)  
  return(list(emp_mean = emp_mean,  
              emp_var = emp_var,  
              theo_mean = theo_mean,  
              theo_var = theo_var))  
} 

# 参数设置
lambda_rate = 2.0  # 泊松率参数
alpha = 2.0        # 伽马分布形状参数
beta = 1.0         # 伽马分布率参数
T = 10             # 时间
num = 50000        # 模拟次数

# 运行模拟
X_T = com_poisson_gamma(lambda_rate, alpha, beta, T, num)

# 估计平均值和方差
results = mean_var(X_T, lambda_rate, alpha, beta, T)
# 计算实验的平均值和标准差
emp_mean = results$emp_mean
emp_sd = sqrt(results$emp_var)


# 创建一个数据框以便输出为表格
result_table = data.frame(
  Statistic = c("实验平均值", "理论平均值", "实验方差", "理论方差"),
  Value = c(results$emp_mean, results$theo_mean, results$emp_var, results$theo_var)
)
knitr::kable(result_table, caption = "复合泊松-伽马过程的模拟统计结果")

## ----X_T ,echo=TRUE,fig.width=7, fig.height=5---------------------------------
# 绘制直方图
hist(X_T, breaks = 50, main = "Histogram of X(10)", 
     xlab = "X(10) values", col = "lightblue", probability = TRUE)
# 绘制核密度估计曲线  
density_curve = density(X_T, adjust = 1)  
lines(density_curve, col = "deeppink", lwd = 2)  

text(x = max(density_curve$x) * 0.9, y = max(density_curve$y) * 1.1,   
     labels = "Kernel Density Estimate", pos = 4, col = "red")  

## -----------------------------------------------------------------------------
# 加载必要的包
library(ggplot2)
# 设置参数
n = 10000  # 蒙特卡罗样本数量
a = 3      # Beta 分布的第一个形状参数
b = 3      # Beta 分布的第二个形状参数

# Monte Carlo 估计 CDF 的函数
mc.cdf.beta = function(p, n, shape1, shape2) 
  {
  if (p <= 0 || p >= 1) return(0)  # 确保 p 在 (0, 1) 范围内
  samples = rbeta(n, shape1, shape2)  # 生成 n 个 Beta 分布的样本
  return(mean(samples <= p))  # 计算小于等于 p 的比例
}

# 要估计的 x 值
x_values = seq(0.1, 0.9, by = 0.1)

# 计算 Monte Carlo 估计和精确 CDF
mc_estimates = sapply(x_values, function(x) mc.cdf.beta(x, n, a, b))
exact_values = sapply(x_values, function(x) pbeta(x, a, b))

# 输出结果
result = data.frame(
  x = x_values,
  Monte_Carlo_Estimate = mc_estimates,
  Exact_Value = exact_values
)
knitr::kable(result, caption = "Monte Carlo 估计和精确 CDF比较表",digits = 3)

# 绘制对比图
ggplot(result, aes(x = x)) +
  geom_line(aes(y = Monte_Carlo_Estimate, color = "蒙特卡罗估计"), linewidth = 1.2) +
  geom_point(aes(y = Monte_Carlo_Estimate, color = "蒙特卡罗估计"), size = 3) +
  geom_line(aes(y = Exact_Value, color = "精确值"), linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = Exact_Value, color = "精确值"), size = 3) +
  labs(title = "Beta(3, 3) CDF 蒙特卡罗估计与精确值对比",
       x = "x 值",
       y = "CDF 值") +
  scale_color_manual(values = c("蒙特卡罗估计" = "deepskyblue", "精确值" = "deeppink")) +
  theme_minimal(base_size = 15) +  # 增加基础字体大小
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # 标题居中并加粗
    axis.title.x = element_text(face = "bold"),  # X轴标题加粗
    axis.title.y = element_text(face = "bold")   # Y轴标题加粗
  )



## -----------------------------------------------------------------------------
# 函数：生成Rayleigh分布样本
rayleigh_sample <- function(sigma, n_samples) {
  U <- runif(n_samples)  # 生成均匀分布随机数
  X1 <- sigma * sqrt(-2 * log(U))       # 使用U生成X1
  X2 <- sigma * sqrt(-2 * log(1 - U))   # 使用(1-U)生成X2
  return(list(X1 = X1, X2 = X2))
}

# 函数：计算方差减少并绘制图形
variance_reduction_and_plot <- function(sigma, n_samples) 
  {
  samples <- rayleigh_sample(sigma, n_samples)
  X1 <- samples$X1
  X2 <- samples$X2
  
  # 计算对立变量的平均
  X_plus_antithetic <- (X1 + X2) / 2
  
  # 独立变量生成
  independent_X1 <- sigma * sqrt(-2 * log(runif(n_samples)))
  independent_X2 <- sigma * sqrt(-2 * log(runif(n_samples)))
  X_plus_independent <- (independent_X1 + independent_X2) / 2
  
  # 计算方差
  var_antithetic <- var(X_plus_antithetic)
  var_independent <- var(X_plus_independent)
  
  # 计算方差减少百分比
  percent_reduction <- (var_independent - var_antithetic) / var_independent * 100
  
  # 绘制直方图
  par(mfrow = c(1, 2))  # 设置图形区域为1行2列
  
  hist(X_plus_independent, breaks = 30, col = 'deepskyblue', 
       main = '独立变量生成的Rayleigh样本', 
       xlab = '样本值', 
       ylab = '频率', 
       xlim = c(0, max(X_plus_independent, X_plus_antithetic)),
       probability = TRUE)
  
  hist(X_plus_antithetic, breaks = 30, col = 'deeppink', 
       main = '对立变量生成的Rayleigh样本', 
       xlab = '样本值', 
       ylab = '频率', 
       xlim = c(0, max(X_plus_independent, X_plus_antithetic)),
       probability = TRUE)

  legend("topright", legend = c("独立变量", "对立变量"), 
         fill = c("deepskyblue", "deeppink"))
  
  return(percent_reduction)
}

# 示例使用
sigma <- 1.0  # 参数
n_samples <- 10000  # 样本数量
reduction <- variance_reduction_and_plot(sigma, n_samples)
title(main = paste("\nσ =", sigma), outer = TRUE)
cat(sprintf("方差减少百分比: %.2f%%\n", reduction))

## -----------------------------------------------------------------------------
# 加载必要的库
library(ggplot2)
# 设置随机种子以确保结果可重复
set.seed(1234)
# 定义目标分布 g(x)
g <- function(x) {
  (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2)
}
# 定义重要性分布 f1 (Gamma 分布) 和 f2 (正态分布)
f1 <- function(x) {
  (x^2 * exp(-x)) / 2 # Gamma(3, 1) 的 PDF
}
f2 <- function(x) {
  dnorm(x, mean = 2, sd = 1) # N(2, 1) 的 PDF
}
# 重要性采样函数
importance_sampling <- function(g, f, n) {
  samples <- numeric(n)
  if (identical(f, f1)) {
    samples <- rgamma(n, shape = 3, rate = 1)
  } else if (identical(f, f2)) {
    samples <- rnorm(n, mean = 2, sd = 1)
  }
  # 计算权重
  weights <- g(samples) / sapply(samples, f)
  # 计算估计值和方差
  estimate <- mean(weights)
  variance <- var(weights) / n
  return(list(estimate = estimate, variance = variance, samples = samples))
}

# 执行重要性采样
n_samples <- 10000
result_f1 <- importance_sampling(g, f1, n_samples)
result_f2 <- importance_sampling(g, f2, n_samples)

results_df <- data.frame(
  Distribution = c("Gamma", "Normal"),
  Estimate = c(result_f1$estimate, result_f2$estimate),
  Variance = c(result_f1$variance, result_f2$variance)
)

# 输出结果为表格
knitr::kable(results_df, caption = "重要性采样结果")

# 可视化目标分布和重要性分布
x_vals <- seq(0, 6, length.out = 100)
target_density <- g(x_vals)
gamma_density <- f1(x_vals)
normal_density <- f2(x_vals)

# 创建数据框用于 ggplot
data_plot <- data.frame(
  x = rep(x_vals, 3),
  density = c(target_density, gamma_density, normal_density),
  distribution = rep(c("Target (g)", "Gamma (f1)", "Normal (f2)"), each = length(x_vals))
)

# 绘制图像
ggplot(data_plot, aes(x = x, y = density, color = distribution)) +
  geom_line(linewidth = 1.2) +  
  labs(title = "Comparison of Target and Importance Distributions",
       x = "x", y = "Density") +
  theme_minimal() +
  scale_color_manual(values = c("deepskyblue", "deeppink", "darkolivegreen3")) +
  theme(legend.title = element_blank())

## -----------------------------------------------------------------------------
rm(list=ls())
#设置种子
set.seed(12345)
# 参数设定
N=1000  # 总假设数
num0=950  # 零假设数
num1=50  # 备择假设数
m=10000  # 模拟次数
alpha=0.1  # 显著性水平

# 初始化存储结果的表格
results=matrix(0, nrow=m, ncol=6)
# 模拟
for (i in 1:m) {
  ## 生成p值
  # 零假设使用均匀分布，备择假设使用贝塔分布
  p_values=c(runif(num0), rbeta(num1, 0.1, 1)) 
  
  ## 校正
  bo_adjusted=p.adjust(p_values, method = "bonferroni")
  bh_adjusted=p.adjust(p_values, method = "BH")
  
  #计算拒绝数目
  rejected_bo=sum(bo_adjusted < alpha)
  rejected_bh=sum(bh_adjusted < alpha)
  
  # 真实拒绝的数目
  true_positive_bo=sum((p_values[(num0 + 1):N] < alpha) & (bo_adjusted < alpha))
  true_positive_bh=sum((p_values[(num0 + 1):N] < alpha) & (bh_adjusted < alpha))
  
  ## FWER
  fwer_bo=rejected_bo / num0
  fwer_bh=rejected_bh / num0
  
  ## FDR
  fdr_bo=ifelse(rejected_bo > 0, (num0 - true_positive_bo) / rejected_bo, 0)
  fdr_bh=ifelse(rejected_bh > 0, (num0 - true_positive_bh) / rejected_bh, 0)
  
  ## TPR
  tpr_bo=true_positive_bo / num1
  tpr_bh=true_positive_bh / num1
  
  # 存储每次模拟的结果
  results[i, ]=c(fwer_bo, fdr_bo, tpr_bo, fwer_bh, fdr_bh, tpr_bh)
}

# 转换为数据框并计算平均值
results1=as.data.frame(results)
colnames(results1)=c("fwer_bo", "fdr_bo", "tpr_bo",
                     "FWER_BH", "FDR_BH", "TPR_BH")

# 计算每列的平均值
results_mean=colMeans(results1)

# 将平均值整理为3×2表格
table=matrix(results_mean, nrow=3, byrow=FALSE)
rownames(table)=c("FWER", "FDR", "TPR")
colnames(table)=c("Bonferroni校正", "B-H校正")
table=round(table, 3)  # 保留三位小数

# 显示结果
knitr::kable(table)


## -----------------------------------------------------------------------------
rm(list=ls())
# 加载空调故障时间数据
library(boot)
hours=aircondit$hours
# 设置Bootstrap参数
set.seed(12345)
B=1e4  # 设置Bootstrap次数为10000次

## 计算lambda的 MLE：n/sum(xi)
lambda_hat=length(hours)/sum(hours)
# 初始化存储向量
boot_lambda_hats=numeric(B)
## 进行Bootstrap抽样
for (b in 1:B) {
  boot_hours=sample(hours, replace=TRUE)
  # 计算Bootstrap样本的 MLE
  boot_lambda_hats[b]=length(boot_hours)/sum(boot_hours)
}
## 计算偏差
boot_lambda_hats_mean=mean(boot_lambda_hats)
bias=boot_lambda_hats_mean - lambda_hat
## 计算标准误差
std_error=sd(boot_lambda_hats)

# 结果输出
results=round(c(MLE=lambda_hat,bias=bias, se.boot=std_error, 
                se.samp=sd(hours)/sqrt(length(hours))), 4)
knitr::kable(results)

# 绘制Bootstrap均值的直方图
hist(boot_lambda_hats, main="Bootstrap Estimates of Lambda", 
     xlab=expression(hat(lambda)), 
     breaks=30, col="lightblue", border="black")
abline(v=lambda_hat, col='red', lwd=2)

## -----------------------------------------------------------------------------
library(boot)
# 提取数据
hours=aircondit$hours
# 计算样本均值的倒数
mu=1 / mean(hours)
# 设置随机种子和参数
set.seed(12345)
m=1000  
R=999  

# 初始化置信区间矩阵
ci.norm=ci.basic=ci.perc=ci.bca=matrix(NA, m, 2)

# 定义fun算平均故障时间
mean_failure_time=function(hours, ind) {
  return(1/mean(hours[ind]))  # lambda的倒数为平均故障时间
}

# 执行自助法抽样
for (i in 1:m) {
  boot_results=boot(data = hours, statistic = mean_failure_time, R = R)
  # 计算各类型的置信区间
  ci=boot.ci(boot_results, type = c("norm", "basic", "perc", "bca"))
  ci.norm[i, ]=ci$norm[2:3]
  ci.basic[i, ]=ci$basic[4:5]
  ci.perc[i, ]=ci$percent[4:5]
  ci.bca[i, ]=ci$bca[4:5]
}

# 计算并打印每种置信区间的平均值
result=list(
  norm = round(colMeans(ci.norm), 4),
  basic = round(colMeans(ci.basic), 4),
  perc = round(colMeans(ci.perc), 4),
  bca = round(colMeans(ci.bca), 4)
)
print(result)
# 计算每种置信区间包含mu的比例
cat('norm =', mean(ci.norm[, 1] <= mu & ci.norm[, 2] >= mu), "\n",
    'basic =', mean(ci.basic[, 1] <= mu & ci.basic[, 2] >= mu), "\n",
    'perc =', mean(ci.perc[, 1] <= mu & ci.perc[, 2] >= mu), "\n",
    'BCa =', mean(ci.bca[, 1] <= mu & ci.bca[, 2] >= mu), "\n")

## -----------------------------------------------------------------------------
rm(list=ls()) #清除内存变量
# 导入数据
library(bootstrap)
scores=scor
# 计算 theta_hat
theta_hat=eigen(cov(scores))$values[1]/sum(eigen(cov(scores))$values)
# Jackknife 估计
n=nrow(scores)
theta_jack=sapply(1:n, function(i) {
  eigen(cov(scores[-i, ]))$values[1]/sum(eigen(cov(scores[-i, ]))$values) 
})
# 计算 Jackknife 偏差和标准误差
bias_jack=(n - 1) * (mean(theta_jack) - theta_hat)
se_jack=sqrt((n - 1) * mean((theta_jack - theta_hat)^2))
# 输出结果
results=data.frame(Metric=c("Original", "Bias", "Standard Error"),
  Value=c(round(mean(theta_jack),3), round(bias_jack,3), round(se_jack,3)))
knitr::kable(results, caption="Jackknife方法估计结果")

## -----------------------------------------------------------------------------
rm(list=ls())
# 加载数据集
library(DAAG)
data(ironslag)
# 定义变量
magnetic=ironslag$magnetic; chemical=ironslag$chemical
n=length(magnetic); e1=e2=e3=e4=numeric(n); a_R21=a_R22=a_R23=a_R24=numeric(n)

## ----echo=FALSE,fig.width=7, fig.height=5-------------------------------------
## 绘制模型图像
par(mfrow=c(2, 2))
a=seq(10, 40, 0.1)
# 线性模型
M1=lm(magnetic ~ chemical)
plot(chemical, magnetic, main="Linear")
lines(a, predict(M1, newdata=data.frame(chemical=a)), col='red',lwd=2)
# 二次模型
M2=lm(magnetic ~ poly(chemical, 2))
plot(chemical, magnetic, main="Quadratic")
lines(a, predict(M2, newdata=data.frame(chemical=a)), col='deepskyblue',lwd=2)
# 指数模型
M3=lm(log(magnetic) ~ chemical)
plot(chemical, magnetic, main="Exponential")
lines(a, exp(predict(M3, newdata=data.frame(chemical=a))), col='purple',lwd=2)
# 三次模型
M4=lm(magnetic ~ poly(chemical, 3))
plot(chemical, magnetic, main="Cubic")
lines(a, predict(M4, newdata=data.frame(chemical=a)), col='orange', lwd=2)

## -----------------------------------------------------------------------------
## leave-one-out交叉验证
for (k in 1:n) {
  y = magnetic[-k]
  x = chemical[-k]
  
  # 线性模型
  L1 = lm(y ~ x)
  yhat1 = predict(L1, newdata = data.frame(x = chemical[k]))
  e1[k] = magnetic[k] - yhat1
  a_R21[k] = summary(L1)$adj.r.squared
  
  # 二次模型
  L2 = lm(y ~ x + I(x^2))
  yhat2 = predict(L2, newdata = data.frame(x = chemical[k]))
  e2[k] = magnetic[k] - yhat2
  a_R22[k] = summary(L2)$adj.r.squared
  
  # 指数模型
  L3 = lm(log(y) ~ x)
  logyhat3 = predict(L3, newdata = data.frame(x = chemical[k]))
  yhat3 = exp(logyhat3)
  e3[k] = magnetic[k] - yhat3
  a_R23[k] = summary(L3)$adj.r.squared
  
  # 三次模型
  L4 = lm(y ~ x + I(x^2) + I(x^3))
  yhat4 = predict(L4, newdata = data.frame(x = chemical[k]))
  e4[k] = magnetic[k] - yhat4
  a_R24[k] = summary(L4)$adj.r.squared
}

## 计算均方误差和调整后R方(均值)
mse = c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))
adj_r_squared = c(mean(a_R21), mean(a_R22), mean(a_R23), mean(a_R24))

# 输出结果
results = data.frame(model_type = c("线性模型", "二次模型", "指数模型", "三次模型"),
                     mes = round(mse, 3),
                     adjR2 = round(adj_r_squared, 3))

knitr::kable(results, caption = "模型评估结果")

## ----echo=FALSE,fig.width=7, fig.height=5-------------------------------------
par(mfrow=c(2, 2))
plot(M2)

## -----------------------------------------------------------------------------
rm(list=ls())
# 加载数据
data(chickwts)
x=sort(as.vector(chickwts$weight[chickwts$feed == "soybean"]))
y=sort(as.vector(chickwts$weight[chickwts$feed == "linseed"]))
## 计算Cramér-von Mises统计量
cvm_statistic=function(x, y) {
  n=length(x); m=length(y)
  # 合并样本并计算经验分布
  pooled=c(x, y); Fn=ecdf(x)(pooled); Gm=ecdf(y)(pooled)
  # 计算 Cramér-von Mises 统计量
  W2=((n*m)/((n+m)^2))*(sum((Fn[1:n]-Gm[1:n])^2)+sum((Fn[(n+1):(n+m)]-Gm[(n+1):(n+m)])^2))
  return(W2)
}
t0=cvm_statistic(x, y) #原始统计量
## 置换检验
set.seed(23456)
R=999; z=c(x, y);n=length(x);K=1:length(z);reps=numeric(R)
for (i in 1:R) {
  k=sample(K, size=n, replace=FALSE)
  x1=z[k];  y1=z[-k]
  reps[i]=cvm_statistic(x1, y1)
}
## 计算p值
p=mean(abs(c(t0, reps)) >= abs(t0))
# 输出结果
cat(" Cramér-von Mises 统计量:", round(t0,3), "\n","p 值:", round(p,3), "\n")

## ----echo=FALSE,fig.width=7, fig.height=5-------------------------------------
hist(reps);abline(v=t0, col="red", lwd=2)
y_position=max(hist(reps, plot=FALSE)$counts)
text(t0+0.15,y_position,labels=paste(round(t0,3)),col="red")

## -----------------------------------------------------------------------------
rm(list=ls())
#加载数据
library(DAAG); data(ironslag)
magnetic=ironslag$magnetic; chemical=ironslag$chemical
# 计算Spearman相关系数和p值
spearman_test=cor.test(magnetic, chemical, method="spearman",exact = FALSE)
spearman_corr=spearman_test$estimate; p_value_cor=spearman_test$p.value
# 置换检验
set.seed(12345);R=999;comb=c(magnetic, chemical);n=length(magnetic)
reps=numeric(R);t0=spearman_corr
for (i in 1:R) {
    pp=sample(comb,replace = FALSE)
    x1=pp[1:n]; y1=pp[-(1:n)]
    reps[i]=cor(x1, y1, method="spearman")
}
# 计算置换检验p值
p_value_perm=mean(abs(c(t0, reps)) >= abs(t0))
# 输出结果
cat(" Spearman rank相关系数为:", round(spearman_corr,3),"\n",
    "cor.test的p值:",signif(p_value_cor,3),"\n",
    "置换检验的p值:", round(p_value_perm,3),"\n")

## -----------------------------------------------------------------------------
rm(list=ls())
## 柯西分布
theta = 1; eta = 0
df = function(x) {
  1/(theta*pi*(1+((x-eta)/theta)^2))}
## 正态分布-Proposal distribution
dg = function(x, df) {
  dnorm(x = x, mean = df) }
# Proposal分布中生成随机变量
rg = function(df) {
  rnorm(n = 1, mean = df) }
## M-H
mh = function (N, df, dg, rg) {
  x = numeric(N); k = 0
  x[1] = rg(1) # 从Proposal中产生X1
  u = runif(N)
  for (i in 2:N) {
    xt = x[i-1]; y = rg(xt)
    r = df(y)*dg(xt, y)/(df(xt)*dg(y, xt)) # 计算接受概率
    if (u[i] <= r) {
      #均匀随机数<=接受概率时接受
      x[i] = y } 
    else { 
      # 否则拒绝候选值，保留当前值
      x[i] = xt ; k = k + 1 }
  }
  print(round(k/N,3)); return(x) # 返回生成的随机变量序列
}
## 执行M-H算法
set.seed(1234); n = 20000; k = 3 # 链数
X = matrix(nrow = k, ncol = n)
for (i in 1:k) {
  X[i, ] = mh(n, df, dg, rg)
}

## -----------------------------------------------------------------------------
#绘制直方图
x=X; hist(x, probability = TRUE, breaks = 100,col="skyblue")
plot.x = seq(min(x), max(x), 0.01);lines(plot.x, df(plot.x),col="deeppink")
# 绘制QQ图
sample_quantiles = t(apply(X, 1, quantile, probs = seq(0, 1, by = 0.1)))# 样本分位数
theoretical_quantiles = qcauchy(seq(0, 1, by = 1/n), 1, lower.tail = FALSE)#理论分位数
qqplot(theoretical_quantiles, sample_quantiles, main = "QQ plot")
abline(1, 2, col="deeppink")

## -----------------------------------------------------------------------------
## 计算潜在尺度缩减因子
Gelman.Rubin = function(phi) {
  phi = as.matrix(phi)
  k = nrow(phi); n = ncol(phi)
  phi.means = rowMeans(phi)
  B = n * var(phi.means)
  phi.w = apply(phi, 1, var)
  W = mean(phi.w)
  v.hat = W * (n - 1) / n + B / n
  r.hat = v.hat / W
  return(r.hat)
}
## 绘制累积均值(phi)图
b=1000; phi = t(apply(X, 1, cumsum))
for (i in 1:k) {
  phi[i,] = phi[i,] / (1:ncol(phi))
}
for (i in 1:k) {
  if (i == 1) {
    plot((b+1):n, phi[i,(b+1):n], ylim = c(-4, 2),
         type = "l", xlab = 'Index', ylab = bquote(phi))
  } else { lines((b+1):n, phi[i, (b+1):n], col = i) }
}

## -----------------------------------------------------------------------------
## 绘制潜在尺度缩减因子图
rhat = rep(0, n)
for (j in (b+1):n) {
  rhat[j] = Gelman.Rubin(phi[, 1:j])
}
plot(rhat[(b+1):n], type = "l", xlab = "", ylab = "R",
     ylim = c(1, round(max(rhat[(b+1):n]),digits = 0)))
abline(h = 1.2, lty = 2,col='red')

## -----------------------------------------------------------------------------
rm(list=ls())
# 定义目标联合密度函数
n = 1000;a = 30;b = 60
df = function (x, y) {
  gamma(n+1)/(gamma(x+1)*gamma(n-x+1))*y^(x+a-1)*(1-y)^(n-x+b-1)}
m = 20000 ;burnin = 1000;set.seed(12345)
## Gibbs抽样循环
x = matrix(0, nrow = m, 2)
for (i in 2:m) {
  xt = x[i-1,]  # 获取当前迭代的样本值
  xt[1] = rbinom(1, n, xt[2])  # 从条件分布Binomial(n, y)中抽样
  xt[2] = rbeta(1, xt[1] + a, n - xt[1] + b)  # 从条件分布Beta(x + a, n - x + b)中抽样
  x[i,] = xt  # 更新样本值
}
plot(x[,1], x[,2], xlab = bquote(X[1]), ylab = bquote(X[2]),
     main = "", cex = 0.5, ylim = range(x[,2]))

## -----------------------------------------------------------------------------
## 绘制累积均值(phi)图
phi = t(t(apply(x, 1, cumsum)))
for (i in 1:nrow(phi)) {
  phi[i,] = phi[i,] / (1:ncol(phi))}
plot((burnin+1):m, phi[i, (burnin+1):m],
      type = "l", xlab = 'Index', ylab = bquote(phi))
# 计算潜在尺度缩减因子
Gelman.Rubin = function(phi) {
  phi = as.matrix(phi);  k = nrow(phi); n = ncol(phi)
  phi.means = rowMeans(phi); B = n * var(phi.means);
  phi.w = apply(phi, 1, var);  W = mean(phi.w)
  v.hat = W*(n-1)/n+B/n; r.hat = v.hat / W
  return(r.hat)
}
## 绘制潜在尺度缩减因子图
rhat = rep(0, n)
for (j in (b+1):n) {
  rhat[j] = Gelman.Rubin(phi[, 1:j])}
plot(rhat[(b+1):n], type = "l", xlab = "", ylab = "R")
abline(h = 1.2, lty = 2)

## -----------------------------------------------------------------------------
rm(list=ls())
## (a) 编写计算第k项的函数
calcu_kterm = function(a, k, d) {
  #对原式取对数--这里有一个log(k!)在0的位置需要定义一下
  if (k==0) log_factorial=0 else log_factorial=sum(log(1:k))
  log_term = 0+(2*k+2)*log(sum(a^2))+log(gamma((d+1)/2))+log(gamma(k+3/2))-
      log_factorial-k*log(2)-log((2*k+1)*(2*k+2))-log(gamma(k+d/2+1))
  term = ((-1)^k)*exp(log_term) # 还原
  return(term)
}
## (b) 计算并返回总和
# 最大计算次数为max_iter 当计算小于1e-10时停止计算
getSum = function(a,d,max_iter=1000,tol=1e-10){ 
  sum_value = 0; Final = calcu_kterm(a, 0, d)
  exp_sum_value = Final
  for (k in 1:max_iter) {
    current_term = calcu_kterm(a, k, d)
    sum_value = sum_value + current_term
    if (abs(current_term) < tol){
      break}
  }
  return(sum_value)
}
## (c) Evaluate the sum
a = c(1, 2) ; d = length(a)
result = getSum(a, d)
cat("计算结果约为:",result)
absolute_values <- sapply(0:40, calcu_kterm, a = a, d = d)
plot(1:40, sapply(1:40, calcu_kterm, a = a, d = d), type = "l", 
     main = "级数项随k的变化",xlab = "k",ylab="", col = "deeppink")

## -----------------------------------------------------------------------------
rm(list=ls())
solve.equation = function(k) {
  # 定义方程的左右两边
  term = function(n, a) {
    integral = function(u) (1+u^2/(n-1))^(-n/2)
    ck = sqrt(a^2*n/(n+1-a^2)) 
    p = 2/sqrt(pi*(n-1))*exp(lgamma(n/2)-lgamma((n-1)/2))
    return(p*integrate(integral, 0, ck)$value)  # 计算积分并返回
  }
  f = function(a){
    term(k, a)-term(k+1, a)}
  if (f(1.5)*f(2) < 0) {
    r = uniroot(f, c(0.5, 2))$root  # 求解方程
  } else r = NA #出现NA则通过绘制k对应图像调整区间
  return(r)
}
# 不同的k值求解
options(digits=4)
rs = sapply(c(4:25,100,500,1000), solve.equation)
print(rs)

## -----------------------------------------------------------------------------
rm(list=ls())
Y=c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
tau=1.0  # 删失阈值
# EM算法
EM=function(Y, tau, max_iter=100,tol=1e-6) {
  n=length(Y); lamb=1
  for (j in 1:max_iter) {
    expect=ifelse(Y<tau, Y, tau+1/lamb)  # E步
    new_lamb=1 / mean(expect)  # M步
    if (abs(new_lamb-lamb)<tol) {# 收敛判定
      break}
    lamb=new_lamb
  }
  return(lamb)
}
# 调用EM算法
lambda_EM=EM(Y, tau)
cat("EM算法估计的lambda：", lambda_EM, "\n")

## -----------------------------------------------------------------------------
# 负对数似然函数
neg_log_likelihood <- function(lambda, Y, tau) {
  # 对应删失和非删失数据的似然项
  term1 <- sum(log(lambda) - lambda * Y[Y < tau])  # 未删失数据
  term2 <- sum(log(1 - exp(-lambda * tau)) * (Y >= tau))  # 删失数据
  return(-(term1 + term2))
}
# MLE估计
MLE_lambda <- function(Y, tau) {
  # 最小化负对数似然
  result <- optimize(neg_log_likelihood, interval = c(0.0001, 10), Y = Y, tau = tau)
  return(result$minimum)
}
# 运行MLE估计
lambda_MLE <- MLE_lambda(Y, tau)
cat("最大似然估计的lambda：", lambda_MLE, "\n")

## -----------------------------------------------------------------------------
rm(list=ls())
library(lpSolve)
# 目标函数系数(min 4x+2y+9z)
objective=c(4, 2, 9)
# 约束条件
constr=rbind(
  c(2, 1, 1), #  2x+y+z <= 2
  c(1, -1, 3) #  x-y+3z <= 3
)
direction=c("<=", "<=")
rhs=c(2, 3)
# 定义变量的下界(x, y, z >= 0)
lower.bounds=c(0, 0, 0)
# 使用lp函数求解线性规划问题
result=lp("min", objective, constr, direction, rhs,all.int=FALSE)
# 输出结果
result$solution

## ----echo=FALSE---------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
formulas=list(
  mpg ~ disp,
  mpg ~ I(1/disp),
  mpg ~ disp+wt,
  mpg ~ I(1/disp)+wt)

## -----------------------------------------------------------------------------
# 加载数据集和所需包(microbenchmark测量两者的执行时间)
data(mtcars);library(microbenchmark)
## 一、使用 for 循环
time_for1=microbenchmark(
  for_loop={mod_for1=list()
    for(i in 1:length(formulas)) {
      mod_for1[[i]]=lm(formulas[[i]], data=mtcars)
    }},  times=10)
# 打印模型2结果
print(summary(mod_for1[[2]]))
## 二、使用 lapply()
time_lapply1=microbenchmark(
  lapply_loop={
    mod_lapply1=lapply(formulas, function(f) lm(f, data=mtcars))
  },times=10 )
# 打印模型2结果
print(summary(mod_lapply1[[2]]))
## 对比二者运行效率
print(time_for1)
print(time_lapply1)

## -----------------------------------------------------------------------------
bootstraps=lapply(1:10, function(i) {
  rows=sample(1:nrow(mtcars), replace=TRUE)
  mtcars[rows, ]})

## -----------------------------------------------------------------------------
library(microbenchmark)
## 一、使用 for 循环
time_for2=microbenchmark(
  for_loop={
    mod_for2=list()
    for(i in 1:length(bootstraps)) {
      mod_for2[[i]]=lm(mpg ~ disp, data=bootstraps[[i]])}
    },times=10)
## 二、使用 lapply()
fit_model=function(bootstrap_data) {# 定义拟合模型的函数
  lm(mpg ~ disp, data=bootstrap_data)}
time_lapply2=microbenchmark(
  lapply_loop={mod_lapply2=lapply(bootstraps, fit_model)},times=10)
## 比较两个方法的第一个模型结果
identical(coef(mod_for2[[1]]),coef(mod_lapply2[[1]]))#输出为True即相同
## 打印比较效率
print(time_for2);print(time_lapply2)

## -----------------------------------------------------------------------------
rsq=function(mod) summary(mod)$r.squared

## -----------------------------------------------------------------------------
options(digits=3)
## 练习1 
R1_1=lapply(mod_for1,rsq);R1_2=lapply(mod_lapply1,rsq) #提取R²值
df1=data.frame(mod_for=unlist(R1_1),mod_lapply=unlist(R1_2))
knitr::kable(t(df1),caption="练习一中四个模型的结果")
## 练习2
R2_1=lapply(mod_for2,rsq);R2_2=lapply(mod_lapply2,rsq) #提取R²值
df2=data.frame(mod_for=unlist(R2_1),mod_lapply=unlist(R2_2))
knitr::kable(t(df2),caption="练习二中模型结果")

## ----echo=FALSE---------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
trials=replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify=FALSE)

## -----------------------------------------------------------------------------
## 使用 sapply() 和匿名函数提取每次试验中的 p 值
p_values1=sapply(trials, function(x) x$p.value)

## -----------------------------------------------------------------------------
## 额外挑战：使用 sapply() 和 [[ 来提取 p.value
p_values2=sapply(trials, `[[`, "p.value")

## -----------------------------------------------------------------------------
m_v_lapply=function(..., FUN) {
  args=list(...)  # 获取输入的列表或向量
  result=Map(FUN, args[[1]], args[[2]]) # 将所有输入的参数传入FUN
  if (length(result) > 0) { # 获取第一个元素的类型来推断 FUN.VALUE 类型
    first_value=result[[1]]
    if (is.numeric(first_value)) {
      result=vapply(result, function(x) x, FUN.VALUE=numeric(1))
    } else if (is.character(first_value)) {
      result=vapply(result, function(x) x, FUN.VALUE=character(1))
    } else if (is.logical(first_value)) {
      result=vapply(result, function(x) x, FUN.VALUE=logical(1))
    } else if (is.list(first_value)) {
      result=vapply(result, function(x) x, FUN.VALUE=list())
    } else {stop("返回类型不可识别")}
  }
  return(result)}
## 运行示例1：加法
# 定义一个简单的函数：两个输入的乘法
multi_fun=function(x, y) {
  return(x*y)}
input1=1:5; input2=6:10
output1=m_v_lapply(input1, input2, FUN=multi_fun)
print(output1)

## ----echo=FALSE---------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
chisq_test=function(x, y) {
  if (!is.numeric(x) || !is.numeric(y)) {# 输入是否为数值向量
    stop("两个输入都必须是数值向量")}
  if (length(x) != length(y)) {# 向量长度是否相等
    stop("两个向量的长度必须相同") }
   if (any(is.na(x)) || any(is.na(y))) {# 是否有缺失值
    stop("向量中不允许有缺失值")}
    c_table=table(x, y) # 创建列联表
    # 计算期望频数矩阵
    row_sum=rowSums(c_table);col_sum=colSums(c_table);total_sum=sum(c_table)
    expected=outer(row_sum, col_sum, "*")/total_sum
    # 卡方统计量
    chi_squared_stat=sum((c_table-expected)^2/expected)
    # 自由度、p值
    df=(nrow(c_table)-1)*(ncol(c_table)-1)
    p_value=1-pchisq(chi_squared_stat, df)
    # 返回结果
    return(list(chi_squared_stat=chi_squared_stat,df=df,p_value=p_value))
}
## 示例使用
x=c(10,15,20,25,30); y=c(15,10,25,20,50)
result1=chisq_test(x, y)
print(result1)

## -----------------------------------------------------------------------------
fast_table=function(x, y) {
  x_levels=as.integer(factor(x))
  y_levels=as.integer(factor(y))
  # 初始化空的频数表
  counts=matrix(0, nrow=length(unique(x)), ncol=length(unique(y)))
  # 填充频数表
  for (i in seq_along(x)) {
    counts[x_levels[i], y_levels[i]]=counts[x_levels[i], y_levels[i]]+1
  }
  return(counts)}
# 使用之前定义的 chisq_test 函数,修改列联表定义的部分，进行卡方检验
chisq_test_fast=function(x, y) {
     c_table=fast_table(x, y) # 创建列联表
    # 计算期望频数矩阵
    row_sum=rowSums(c_table);col_sum=colSums(c_table);total_sum=sum(c_table)
    expected=outer(row_sum, col_sum, "*")/total_sum
    # 卡方统计量
    chi_squared_stat=sum((c_table-expected)^2/expected)
    # 自由度、p值
    df=(nrow(c_table)-1)*(ncol(c_table)-1)
    p_value=1-pchisq(chi_squared_stat, df)
    # 返回结果
    return(list(chi_squared_stat=chi_squared_stat,df=df,p_value=p_value))
}
## 示例使用
x=c(10,15,20,25,30); y=c(15,10,25,20,50)
result2=chisq_test_fast(x, y)
print(result2)
## 对比运行效率
library(microbenchmark)
time1=microbenchmark(chisq_test(x, y),times=50);print(time1)
time2=microbenchmark(chisq_test_fast(x, y),times=50);print(time2)

## ----eval=FALSE---------------------------------------------------------------
# #include <Rcpp.h>
# using namespace Rcpp;
# 
# // [[Rcpp::export]]
# NumericMatrix gibbs_sampling(int m, int n, double a, double b) {
#   NumericMatrix x(m, 2);
#   double xt_1=0.5;
#   double xt_2=0.5;
#   for (int i=1; i < m; ++i) {
#     xt_1=R::rbinom(n, xt_2);
#     xt_2=R::rbeta(xt_1 + a, n - xt_1 + b);
#     x(i, 0)=xt_1;
#     x(i, 1)=xt_2;
#   }
#   return x;
# }

## -----------------------------------------------------------------------------
rm(list=ls())
library(Rcpp);library(microbenchmark)
## 载入Rcpp函数
sourceCpp('../vignettes/gibbs.cpp')

## -----------------------------------------------------------------------------
# 设置参数和种子--Beta分布的参数与Gibbs抽样过程的迭代次数m
n=1000;a=30;b=60;m=10000
set.seed(1234)
## R编写Gibbs抽样函数
R_gibbs_sampling=function(m, n, a, b) {
  x=matrix(0, nrow=m, ncol=2)
  for (i in 2:m) {
    xt=x[i-1,]  # 当前迭代的样本值
    xt[1]=rbinom(1, n, xt[2])   # Binomial(n,y)中抽样
    xt[2]=rbeta(1,xt[1]+a,n-xt[1]+b) # Beta(x+a,n-x+b)中抽样
    x[i,]=xt # 更新样本值
  }
  return(x)
}

## 使用Rcpp的Gibbs抽样
x_rcpp=gibbs_sampling(m, n, a, b)
## 使用R编写的Gibbs抽样
x_r=R_gibbs_sampling(m, n, a, b)

## -----------------------------------------------------------------------------
## 在一张图上plot散点图
par(mfrow=c(1, 1))
plot(x_rcpp[,1], x_rcpp[,2], xlab = 'x1', ylab ='x2',main = "",
     cex = 1, ylim = range(x_rcpp[,2]),col = 'darkgray',pch = 1)
par(new=TRUE)
plot(x_r[,1], x_r[,2],cex = 0.5,xlab = '', ylab ='',main = "",
     xaxt = 'n',yaxt = 'n',col ='darkred',pch = 18)
legend("bottomright", legend = c("Rcpp Samples", "R Samples"),
       col = c("darkgray", "darkred"), pch = c(1, 18), cex = 0.8)

## -----------------------------------------------------------------------------
## QQ图比较
par(mfrow=c(1, 2))
qqplot(x_rcpp[,1], x_r[,1], main="X1的QQ图(Rcpp v.s. R)", 
       xlab="Rcpp-X1", ylab="R-X1",col = "darkgray")
abline(0, 1, col = "red")  # 添加y=x参考线
qqplot(x_rcpp[,2], x_r[,2], main="X2的QQ图(Rcpp v.s. R)", 
       xlab="Rcpp-X2", ylab="R-X2",col = "darkgray")
abline(0, 1, col = "red")  # 添加y=x参考线

## -----------------------------------------------------------------------------
## 两种方法的计算时间-重复10次结果
ts=microbenchmark(
  Rcpp=gibbs_sampling(m,n,a,b),
  R=R_gibbs_sampling(m,n,a,b),
  times=10) 
knitr::kable(summary(ts))

## -----------------------------------------------------------------------------
## 累积均值(phi)图
phi=function(x,c="darkred",y='n'){
  burnin=1000 # 不稳定的烧入期
  phi = t(t(apply(x, 1, cumsum)))
  for (i in 1:nrow(phi)) {
    phi[i,] = phi[i,] / (1:ncol(phi))}
  plot((burnin+1):m, phi[i, (burnin+1):m],
       type = "l", xlab = 'Index', ylab = bquote(phi),
       col=c,yaxt = y) 
}
phi(x_r,c="darkgray",y='s');par(new=TRUE);phi(x_rcpp)

## -----------------------------------------------------------------------------
## 绘制潜在尺度缩减因子图
rhat=function(x,c='darkred'){
  ## 计算潜在尺度缩减因子
  phi = t(t(apply(x, 1, cumsum)))
  Gelman.Rubin = function(phi) {
    phi = as.matrix(phi);  k = nrow(phi); n = ncol(phi)
    phi.means = rowMeans(phi); B = n * var(phi.means);
    phi.w = apply(phi, 1, var);  W = mean(phi.w)
    v.hat = W*(n-1)/n+B/n; r.hat = v.hat / W
    return(r.hat)
}
  rhat = rep(0, n)
  for (j in (b+1):n) {
    rhat[j] = Gelman.Rubin(phi[, 1:j])
    }
  plot(rhat[(b+1):n], type = "l", xlab = "", ylab = "R",col=c)
  abline(h = 1.2, lty = 2,col='red') # 小于1.2认为是收敛
}
par(mfrow=c(1, 2))
rhat(x_r,c="black");rhat(x_rcpp)

