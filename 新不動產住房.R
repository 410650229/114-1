
######################
#######packages#######
######################
install.packages("readxl")  # 用於讀取 Excel 文件
install.packages("rvest")
install.packages("httr")
install.packages("gganimate")
install.packages("ggplot2")  # gganimate 基於 ggplot2



# 設定檔案路徑
file_path <- "C:\\Users\\User\\Desktop\\學\\大學\\資料探勘\\plvr_land_csv\\merged_data_BULI.csv"

# 匯入 CSV 文件
data <- read.csv(file_path, stringsAsFactors = FALSE)

# 查看數據
head(data)



################################
##########資料檢視:函數#########
################################

get_unique_values <- function(df, col_index) {
  # 檢查col_index是否有效
  if (col_index > ncol(df) || col_index < 1) {
    stop("col_index超出範圍")
  }
  
  # 取得唯一值
  unique_values <- unique(df[[col_index]])
  
  # 顯示所有唯一值
  print(unique_values)
}
count_value_occurrences <- function(df, col_index, value) {
  # 檢查col_index是否有效
  if (col_index > ncol(df) || col_index < 1) {
    stop("col_index超出範圍")
  }
  
  # 計算該值的出現次數
  count <- sum(df[[col_index]] == value, na.rm = TRUE)
  
  # 返回計數
  return(count)
}

get_unique_values(data, 2)
count_value_occurrences(data, 5, "住") 

######################
#######packages#######
######################
install.packages("readxl")  # 用於讀取 Excel 文件
install.packages("rvest")
install.packages("httr")
install.packages("gganimate")
install.packages("ggplot2")  # gganimate 基於 ggplot2



# 設定檔案路徑
file_path <- "C:\\Users\\User\\Desktop\\學\\大學\\資料探勘\\plvr_land_csv\\merged_data_BULI.csv"

# 匯入 CSV 文件
data <- read.csv(file_path, stringsAsFactors = FALSE)

# 查看數據
head(data)

# 顯示資料框的所有變數名稱
names(data)


################################
##########資料檢視:函數#########
################################

get_unique_values <- function(df, col_index) {
  # 檢查col_index是否有效
  if (col_index > ncol(df) || col_index < 1) {
    stop("col_index超出範圍")
  }
  
  # 取得唯一值
  unique_values <- unique(df[[col_index]])
  
  # 顯示所有唯一值
  print(unique_values)
}
count_value_occurrences <- function(df, col_index, value) {
  # 檢查col_index是否有效
  if (col_index > ncol(df) || col_index < 1) {
    stop("col_index超出範圍")
  }
  
  # 計算該值的出現次數
  count <- sum(df[[col_index]] == value, na.rm = TRUE)
  
  # 返回計數
  return(count)
}
#############都市土地使用分區、非都市土地使用分區、非都市土地使用編定
get_unique_values(data, 2)
get_unique_values(data, 5)
get_unique_values(data, 6)
get_unique_values(data, 7)
count_value_occurrences(data, 5, "住") 
count_value_occurrences(data, 6, "") 
count_value_occurrences(data, 6, "住宅區") 

get_unique_values(data, 13)
#####################
# 篩選出數值型變數
numeric_columns <- sapply(data, is.numeric)

# 計算每個數值型變數的最大和最小值
numeric_summary <- data[, numeric_columns]

# 顯示每個數值變數的最大值和最小值
for (col in names(numeric_summary)) {
  cat("變數:", col, "\n")
  cat("最小值:", min(numeric_summary[[col]], na.rm = TRUE), "\n")
  cat("最大值:", max(numeric_summary[[col]], na.rm = TRUE), "\n\n")
}

# 查看第13個變數的出現次數
table(data[[13]])
