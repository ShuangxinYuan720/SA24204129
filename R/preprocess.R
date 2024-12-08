#' @title Data preprocessing functions
#' @name preprocess
#' @description Missing value filling and near normalisation of data
#' @param data Input data (data frame or matrix)
#' @return standardized_data \code{n}
#' @examples
#' \dontrun{
#'     data <- data.frame(
#'         Feature1 = c(1, 2, NA, 4, 5),
#'         Feature2 = c(NA, 2, 3, NA, 5),
#'         Feature3 = c(1, 2, 3, 4, 5))
#'     processed_data <- preprocess(data)
#'     print(processed_data)
#' }
#' @export


# 数据预处理函数：缺失值处理 + 标准化
preprocess <- function(data) {
  # 如果输入的数据不是数据框或矩阵，尝试转换为数据框
  if (!is.data.frame(data) && !is.matrix(data)) {
    message("Input data is neither a data frame nor a matrix. Converting to data frame.")
   data <- data.frame(data)
  }
  # 检查转换后的数据是否为数据框
  #if (!is.data.frame(data)) {
    #stop("Input data could not be converted to a data frame.")
  #}
  # 缺失值处理：用众数填充
  for (i in seq_along(data)) {
    if (is.numeric(data[[i]])) {
      mode_value <- as.numeric(names(sort(table(data[[i]]), decreasing = TRUE)[1]))  # 计算众数
      data[[i]][is.na(data[[i]])] <- mode_value  # 用众数填充缺失值
    }
  }
  
  # 标准化：计算均值和标准差
  means <- colMeans(data, na.rm = TRUE)
  sds <- apply(data, 2, sd, na.rm = TRUE)
  
  # 标准化：每列数据减去均值并除以标准差
  standardized_data <- sweep(data, 2, means, FUN = "-")  # 减去均值
  standardized_data <- sweep(standardized_data, 2, sds, FUN = "/")  # 除以标准差
  
  return(standardized_data)
}

