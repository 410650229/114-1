setwd("C:\\Users\\User\\Desktop\\學\\大學\\資料探勘")


# 匯入 CSV 文件
merged_data <- read.csv("merged_data.csv")
merged_data_RLC <- merged_data[!(merged_data[[2]] %in% c("土地", "車位")), ]

# 查看第13個變數的出現次數
table(merged_data_RLC[[13]])
garb = c("見其他登記事項","商業用","工業用","見使用執照","停車空間",
         "工商用","農業用","見其它登記事項","列管標準廠房","辦公室","店舖",
         "市場攤位","自設停車空間","店屋","防空避難室兼停車場","共有部份",
         "店鋪","旅館、防空避難室","地面一層：停車空間；地面二至五層：一般服務業",
         "一般零售場所、停車空間","店舖、辦公室","保養所、辦公室","自由職業事務所",
         "預拌混凝土場及瀝青拌合場","防空避難室、店舖","一般事務所","工廠","廠房",
         "托兒所","補習班","店舖、停車空間",
         "防空避難室，旅館","停車空間，民俗文物館","餐廳",
         "車庫、保齡球館、遊樂場、服務區","共有部分","飲食店","辦公廳",
         "店舖診所類場所","辦公室（廳）","畜牧設施（室內養雞場）","攤販中心",
         "辦公用","其他","商辦用","廠房、辦公室","溫室及植物環控栽培設施",
         "事務所及工商服務業","作業廠房","日用品零售業、室內停車",
         "一般零售場所","農業設施","包裝場、辦公室、倉庫、裝車柵及公廁","工廠〈Ｃ－２〉",
         "管理室、雞舍","作業廠房、附屬辦公室")
merged_data_RLCt13 <- merged_data_RLC[!(merged_data_RLC[[13]] %in% garb), ]
merged_data_RLCt13[merged_data_RLCt13 == ""] <- NA

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

count_value_occurrences(merged_data_RLCt13, 5, "住") 
get_unique_values(merged_data_RLC, 13)
##########################
##########合併!###########
##########################
merge_columns_if_possible <- function(data, var1, var2,nvar) {
  # 確保變數存在於資料集中
  if (!all(c(var1, var2) %in% names(data))) {
    stop(paste("指定的變數名稱不存在於資料集中:", 
               paste(setdiff(c(var1, var2), names(data)), collapse = ", ")))
  }
  
  # 檢查同時非遺失值的觀察值
  both_non_missing <- which(!is.na(data[[var1]]) & !is.na(data[[var2]]))
  
  if (length(both_non_missing) > 0) {
    cat("以下觀察值同時存在非遺失值，無法合併：\n")
    print(data[both_non_missing, c(var1, var2)])
    stop("無法合併，因為指定的變數存在同時非遺失值的觀察值")
  }
  
  # 合併變數
  merged_name <- nvar
  data[[merged_name]] <- ifelse(!is.na(data[[var1]]), data[[var1]], data[[var2]])
  
  # 刪除原始變數
  data[[var1]] <- NULL
  data[[var2]] <- NULL
  
  # 確認完成合併
  cat(paste("成功合併變數:", var1, "與", var2, "為", merged_name, "\n"))
  
  # 回傳更新後的資料框
  return(data)
}

# 使用函數
merged_data_RLCt13_C2 <- merge_columns_if_possible(
  merged_data_RLCt13, 
  "車位移轉總面積平方公尺", 
  "車位移轉總面積.平方公尺.",
  "車位移轉總面積p方公尺"
)

##########################
##########################
##########################
names(merged_data_RLCt13_C2)

get_unique_values(merged_data_RLCt13_C2, 5)
get_unique_values(merged_data_RLCt13_C2, 6)
get_unique_values(merged_data_RLCt13_C2, 7)


#####################
##篩選出數值型變數###
#####################
numeric_columns <- sapply(merged_data_RLCt13_C2, is.numeric)

# 計算每個數值型變數的最大和最小值
numeric_summary <- merged_data_RLCt13_C2[, numeric_columns]

# 顯示每個數值變數的最大值和最小值
for (col in names(numeric_summary)) {
  cat("變數:", col, "\n")
  cat("最小值:", min(numeric_summary[[col]], na.rm = TRUE), "\n")
  cat("最大值:", max(numeric_summary[[col]], na.rm = TRUE), "\n\n")
}

##########################
# 計算每個變數的空值數量##
##########################
# 定義計算 NA 比例的函數
calculate_na_ratio <- function(data) {
  # 計算每個變數的 NA 比例
  na_ratio <- sapply(data, function(x) sum(is.na(x)))
  
  # 將結果轉換為資料框
  na_ratio_df <- data.frame(
    Variable = names(na_ratio),
    NA_Proportion = na_ratio
  )
  
  # 按照 NA 比例降序排序
  na_ratio_df <- na_ratio_df[order(-na_ratio_df$NA_Proportion), ]
  
  # 回傳結果
  return(na_ratio_df)
}
##########################
##########################
##########################
na_result <- calculate_na_ratio(merged_data_RLCt13_C2)
###############
# 將資料框匯出為 CSV 檔案
write.csv(merged_data_RLCt13_C2, "RLCt13C2_merged_data.csv", row.names = FALSE)


