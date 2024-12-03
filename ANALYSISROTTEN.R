setwd("C:\\Users\\AA\\Desktop\\PYYYY")
install.packages("tidyr")
install.packages("dplyr")
movies_final =read.csv("movies_final.csv")
# 假設資料集名稱為 movies_final
movies_final <- movies_final[, -27]
colnames(movies_final)[7] <- "Name"
# 將所有空值或空白轉為 NA
movies_final[movies_final == "" | movies_final == " "] <- NA

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

library(dplyr)
library(tidyr)

# 假設資料集名稱為 movies_final
# 假設第12個變數名稱為 'content_metadataGenres'

# 1. 清理 'content_metadataGenres' 欄位，將字串轉換為向量格式
movies_final$content_metadataGenres <- gsub("\\[|\\]|'", "", movies_final$content_metadataGenres)  # 移除方括號與單引號
movies_final$content_metadataGenres <- strsplit(as.character(movies_final$content_metadataGenres), ",\\s*")  # 根據逗號分割

# 2. 提取所有唯一的 TAG
all_tags <- unique(unlist(movies_final$content_metadataGenres))  # 取出所有出現過的TAG

# 3. 創建布林變數，表示每部作品是否擁有某一 TAG
for (tag in all_tags) {
  tag_name <- gsub(" ", "_", tag)  # 替換 TAG 中的空格為底線
  movies_final[[tag_name]] <- sapply(movies_final$content_metadataGenres, function(x) tag %in% x)
}

# 檢查結果
colnames(movies_final)
# 重新調整變數順序
movies_final <- movies_final %>%
  select(Name, Birthday, Birthplace, Age, credits, everything())  # 'everything()' 是保留剩下的欄位

head(movies_final[13])
##############################################
################################################
library(dplyr)
library(tidyr)

# 假設 movies_final 是資料框
# 首先清理 'content_metadataProps' 欄位
movies_final$content_metadataProps <- gsub("\\[|\\]|'", "", movies_final$content_metadataProps)  # 移除方括號與單引號
movies_final$content_metadataProps <- strsplit(as.character(movies_final$content_metadataProps), ",\\s*")  # 根據逗號分割

# 定義電影分級，包括電影和電視劇的分級
valid_grades <- c("G", "PG", "PG-13", "R", "NC-17",
                  "TV-Y", "TV-Y7", "TV-G", "TV-PG",
                  "TV-14", "TV-MA")

# 根據 'overlay_mediaType' 來分配正確的元素
movies_final <- movies_final %>%
  rowwise() %>%
  mutate(
    # 根據 mediaType 分配分級
    grade = ifelse("Movie" %in% overlay_mediaType && content_metadataProps[[1]][1] %in% valid_grades, 
                   content_metadataProps[[1]][1], 
                   ifelse("TvSeries" %in% overlay_mediaType && content_metadataProps[[1]][1] %in% valid_grades, 
                          content_metadataProps[[1]][1], NA)),
    
  ) %>%
  ungroup()

###########################################
###########################################
###########################################
# 轉換 list 類型欄位為字串
movies_final[] <- lapply(movies_final, function(x) {
  if (is.list(x)) {
    return(sapply(x, paste, collapse = ", "))  # 將 list 轉換為以逗號分隔的字串
  } else {
    return(x)
  }
})

# 使用正則表達式提取 year 變數的前四個數字作為 fir_year
movies_final$fir_year <- sub("^(\\d{4}).*", "\\1", movies_final$year)
movies_final$fir_year <- as.numeric(movies_final$fir_year)  # 轉換為數字型別

# 將 movies_final 資料框寫入 CSV 檔案
write.csv(movies_final, "movies_final.csv", row.names = FALSE)


get_unique_values(movies_final,1)
colnames(movies_final)
