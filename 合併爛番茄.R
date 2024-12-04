R.Version()
install.packages("dplyr")
install.packages("tidyr")
# 設定工作目錄（可選）
setwd("C:\\Users\\User\\Desktop\\大學\\四上\\PYTHON")


# 取得所有 CSV 檔案的檔名
csv_files <- list.files(pattern = "*.csv")

# 迴圈讀取每個 CSV 檔案並將其存入不同的資料框中
for (file in csv_files) {
  # 讀取 CSV 檔案
  temp_data =read.csv(file)
  
  # 創建資料框名稱，去掉副檔名
  df_name <- gsub(".csv", "", file)
  
  # 將資料框存入環境中
  assign(df_name, temp_data)
  
  # 可選：輸出檔案名稱和資料框名稱
  print(paste("Imported:", file, "as", df_name))
  
}


#######################
#######################
#######################
#######################
names(directors_info)

names( movies_from_directors)

names(movies_and_tv_cleaned)
head(movies_and_tv_cleaned)
#######################
#######################
#######################
#######################
library(dplyr)

# 將 movies_and_tv_cleaned 的 Work.Title 改為 title 以便合併
movies_and_tv_cleaned <- movies_and_tv_cleaned %>%
  rename(title = Work.Title)
movies_and_tv_cleaned <- movies_and_tv_cleaned %>%
  distinct(title, .keep_all = TRUE)

# 將 movies_and_tv_cleaned 左外連結到 movies_from_directors
movies_combined <- movies_from_directors %>%
  left_join(movies_and_tv_cleaned, by = "title")



# 移除 directors_info 中的 Director.URL 並基於 Director.Name 進行合併
movies_final <- directors_info %>%
  select(-Director.URL) %>%
  left_join(movies_combined, by = "Director.Name")

# 調整變數順序，將導演名稱設為第一個變數
movies_final <- movies_final %>%
  relocate(Director.Name, .before = everything())

# 移除不必要的變數
movies_final <- movies_final %>%
  select(
    -iconic_fallback, 
    -iconic_fallbackDesktop, 
    -iconic_fallbackMobile, 
    -content_title, 
    -fallback, 
    -hideAudienceScore, 
    -overlay_audienceTitle, 
    -overlay_audienceVerified_title, 
    -audienceScore_title, 
    -overlay_criticsAll_title, 
    -overlay_criticsTitle, 
    -overlay_criticsTop_title, 
    -overlay_learnMoreUrl, 
    -primaryImageUrl,
    -category,
    -overlay_criticsAll_scoreLinkText,
    -overlay_criticsTop_scoreLinkText
  )



# 確保資料轉換正確
numeric_movies <- movies_final %>%
  mutate(across(where(is.numeric), as.numeric)) %>% # 保留數值型
  mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>% # 類別轉數值
  mutate(across(where(~ !is.numeric(.) && !is.factor(.)), ~ as.numeric(gsub("[^0-9.]", "", .)))) # 處理字串

# 確認所有變數均為數值型
str(numeric_movies)

# 計算相關性矩陣
cor_matrix <- cor(numeric_movies, use = "pairwise.complete.obs")



# 找到完全相關的變數 (相關係數為 1)
related_vars <- which(cor_matrix == 1, arr.ind = TRUE)

# 保留上三角形的唯一組合
related_vars <- related_vars[related_vars[, 1] < related_vars[, 2], ]

# 獲取要刪除的變數名稱
vars_to_remove <- unique(colnames(cor_matrix)[related_vars[, 2]])

# 刪除完全相關的變數，只保留最前面的變數
movies_final <- movies_final %>%
  select(-all_of(vars_to_remove))

# 確認刪除結果
print(vars_to_remove)
colnames(movies_final)
#######################################

# 將所有空值或空白轉為 NA
movies_final[movies_final == "" | movies_final == " "] <- NA

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
  select(Director.Name , Birthday, Birthplace, Age, credits, everything())  # 'everything()' 是保留剩下的欄位


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



