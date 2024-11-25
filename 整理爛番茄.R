R.Version()
# 設定工作目錄（可選）
setwd("C:\\Users\\AA\\Desktop\\PYYYY")


# 取得所有 CSV 檔案的檔名
csv_files <- list.files(pattern = "*.csv")

# 迴圈讀取每個 CSV 檔案並將其存入不同的資料框中
for (file in csv_files) {
  # 讀取 CSV 檔案
  temp_data <- read.csv(file)
  
  # 創建資料框名稱，去掉副檔名
  df_name <- gsub(".csv", "", file)
  
  # 將資料框存入環境中
  assign(df_name, temp_data)
  
  # 可選：輸出檔案名稱和資料框名稱
  print(paste("Imported:", file, "as", df_name))
}
######################
######################
######################
summary(movies_and_tv_enriched_text_mining)