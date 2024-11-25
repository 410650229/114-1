install.packages("dplyr")
library(dplyr)

# 設定工作目錄（可選）
setwd("C:/Users/User/Desktop/學/大學/python")

# 匯入 CSV 檔案
movie_titles_and_urls <- read.csv("movie_titles_and_urls.csv")
head(movie_titles_and_urls)

directors_info <- read.csv("directors_info.csv")
head(directors_info)


movies_from_directors <- read.csv("movies_from_directors.csv")
head(movies_from_directors)


movies_details<- read.csv("movies_details.csv")
head(movies_details)



# 假設資料集為 movies_from_directors
movies_from_directors <- movies_from_directors %>%
  mutate(box_office = ifelse(category == "Movie" & !is.na(box_office) & box_office != "", 
                             box_office, NA))

#基於導演合併電影列表與導演資訊
dir_merged_data <- merge(directors_info, movies_from_directors, by = "Director.Name")
# 刪除變數
dir_merged_data  <- dir_merged_data [ , !names(dir_merged_data ) %in% c("Age","Director.URL","Movie.Title")]



# 定義需要清理的欄位
columns_to_clean <- c("tomatometer", "audience_score", "box_office")

# 對每個欄位進行清理
for (col in columns_to_clean) {
  # 先將 "No Score Yet" 轉換為 NA
  dir_merged_data[[col]][dir_merged_data[[col]] == "No Score Yet"] <- NA
  
  # 去掉百分比符號並轉為數值
  dir_merged_data[[col]] <- as.numeric(gsub("%", "", dir_merged_data[[col]]))
}
# 假設 dir_merged_data 是你的數據框
# columns_to_clean 是你想要清理的列的名稱

# 使用 summary() 取得數據摘要
summary_output <- summary(dir_merged_data[columns_to_clean])

# 將 summary 輸出轉換為資料框
summary_df <- as.data.frame(summary_output)

# 如果需要，可以重設資料框的行名
summary_df <- tibble::rownames_to_column(summary_df, var = "Statistics")

# 檢視轉換後的資料框
print(summary_df)


#########查看職位類別#########
unique(dir_merged_data$credits)
table(dir_merged_data$credits)

unique(dir_merged_data$title)
######數字化類別變數電影或電視影集#######
dir_merged_data$category01 <- ifelse(dir_merged_data$category == "Movie", 1, 0)
table(dir_merged_data$category01)
# 保存為 CSV 檔案
write.csv(dir_merged_data, file = "C:/Users/User/Desktop/學/大學/python/dir_merged_data.csv", row.names = FALSE)
#######################################################
############查看電影與影集數量(重複&NO重複)############
#######################################################
table(dir_merged_data$category)
cont_data <- dir_merged_data[, c("title", "category","tomatometer", "audience_score", "box_office")]
library(dplyr)

# 刪除重複的 title，確認實際考慮了幾部電影與電視劇
dir_merged_data_unique <- cont_data  %>%
  distinct(title, .keep_all = TRUE)
table(dir_merged_data_unique$category)

summary(dir_merged_data_unique)

################################
#########再合併#################
################################
