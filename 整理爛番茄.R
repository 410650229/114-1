R.Version()
install.packages("dplyr")
# 設定工作目錄（可選）
setwd("C:\\Users\\AA\\Desktop\\PYYYY")


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

# 假設 directors_info 和 movies_from_directors 已經存在

# 刪除 "Movie.Title" 和 "Director.URL" 並合併資料
movies_from_directors_info <- movies_from_directors %>%
  left_join(
    directors_info %>%
      select(-Movie.Title, -Director.URL), 
    by = "Director.Name"
  )

# 檢視合併後的資料
head(movies_from_directors_info)
#######################
#######################
#######################
#######################
library(dplyr)

# 將 "Work.Title" 改為 "title"
movies_and_tv_cleaned <- movies_and_tv_cleaned %>%
  rename(title = Work.Title)

# 檢查變數是否已成功更改
colnames(movies_and_tv_cleaned)
# 移除指定變數
movies_and_tv_cleaned <- movies_and_tv_cleaned %>%
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
    -primaryImageUrl
  )

# 確認變數是否成功移除
colnames(movies_and_tv_cleaned)

# 接受多對多關係
movies_final <- movies_from_directors_info %>%
  left_join(movies_and_tv_cleaned, by = "title", relationship = "many-to-many")
library(dplyr)

# 刪除指定變數
movies_final <- movies_final %>%
  select(
    -category,
    -overlay_criticsAll_scoreLinkText,
    -overlay_criticsTop_scoreLinkText
  )

# 檢查變數是否成功刪除



library(dplyr)

# 轉換資料集為數值類型
# 非數值變數將被編碼為因子，然後轉換為數值
numeric_movies <- movies_final %>%
  mutate(across(everything(), as.numeric))

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

# 確認結果
print(vars_to_remove)
colnames(movies_final)
write.csv(movies_final, "movies_final.csv", row.names = FALSE)

