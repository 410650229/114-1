setwd("C:\\Users\\User\\Desktop\\學\\大學\\資料探勘")
data <- read.csv("merged_data_RLCt13_C2_t12_RC_c26_RY_PROP.csv")
colnames(data)
# 去除 "last_prop_date" 和 "Event" 欄位結尾的引號
data$last_prop_date <- gsub('"$', '', data$last_prop_date)  # 去除結尾的引號
data$Event <- gsub('"$', '', data$Event)  # 去除結尾的引號

# 加載 ggplot2 套件
library(ggplot2)

# 建立直方圖
ggplot(data, aes(x = 順序)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) + 
  labs(title = "直方圖：順序變數", x = "順序", y = "頻數") +
  theme_minimal()
library(dplyr)

# 1. 篩選出 "順序" 在 1 到 10 之間的樣本數量
sample_count_1_to_10 <- nrow(data %>% filter(順序 >= -1 & 順序 <= 10))

# 顯示篩選出的樣本數量
cat("順序 1~10 的樣本數量：", sample_count_1_to_10, "\n")

library(dplyr)

# 過濾掉 "順序" 在 1 到 10 之間的樣本
CL_data <- data %>%
  filter(順序 < -1 | 順序 > 10)  # 保留 "順序" 小於 1 或大於 10 的樣本
colnames(CL_data)
library(dplyr)

# 取得 "順序" 的唯一值，並命名為 AUCL_data
AUCL_data <- CL_data %>%
  distinct(順序, .keep_all = TRUE)  # 保留 "順序" 唯一值的樣本
library(dplyr)

# 排序並計算前後兩期 "last_prop_date" 之間的天數差
AUCL_data <- AUCL_data %>%
  arrange(順序) %>%  # 確保 "順序" 是排序過的
  mutate(
    prev_last_prop_date = lag(last_prop_date),  # 前一期的 "last_prop_date"
    next_last_prop_date = lead(last_prop_date), # 後一期的 "last_prop_date"
    prev_days_diff = as.numeric(difftime(last_prop_date, prev_last_prop_date, units = "days")),  # 前一期到當期的天數差
    next_days_diff = as.numeric(difftime(next_last_prop_date, last_prop_date, units = "days"))  # 當期到後一期的天數差
  )

# 顯示結果，檢查每期與前後兩期的天數差
colnames(AUCL_data)
# 保留 "last_prop_date" 和 "next_days_diff" 這兩個欄位
AUCL_data <- AUCL_data %>%
  select(last_prop_date, next_days_diff)
CL_data <- CL_data %>%
  left_join(AUCL_data, by = "last_prop_date")

####################
library(dplyr)
# 篩選 "順序" 11 到 67 之間的資料
filtered_data <- CL_data %>% 
  filter(順序 >= 11 & 順序 <= 66)
colnames(filtered_data)
# 計算每個 "順序" 的頻率
freq_data <- filtered_data %>%
  group_by(順序) %>%
  summarise(freq = n()) %>%
  ungroup()

# 取每個 "順序" 的第一個 "next_days_diff"
first_next_days_diff <- filtered_data %>%
  group_by(順序) %>%
  summarise(first_next_days_diff = first(next_days_diff)) %>%
  ungroup()

# 步驟 1: 計算並儲存 ACL_data，並將 freq_per_next_days_diff 改名為 日均交易量
ACL_data <- freq_data %>%
  left_join(first_next_days_diff, by = "順序") %>%
  mutate(日均交易量 = freq / first_next_days_diff) %>%
  select(順序, 日均交易量)

# 步驟 2: 基於 "順序" 合併 CL_data 中的 "last_prop_date" 到 ACL_data
ACL_data <- ACL_data %>%
  left_join(CL_data %>%
              select(順序, last_prop_date) %>%
              distinct(), by = "順序")
# 基於 "順序" 合併 CL_data 中的 "next_days_diff" 到 ACL_data
ACL_data <- ACL_data %>%
  left_join(CL_data %>%
              select(順序, next_days_diff) %>%
              distinct(), by = "順序")
colnames(ACL_data)
summary(ACL_data)
#############################
########交易量直方圖#########
#############################
# 加载所需的包
library(ggplot2)
library(dplyr)
library(lubridate)

# 假设CL_data是您的数据框
# 先将交易年月日从民国年转成西元年，并格式化为YYYY-MM-DD
ERCL_data <- CL_data %>%
  mutate(
    西元年月日 = as.Date(paste0(as.integer(substr(交易年月日, 1, 3)) + 1911, substr(交易年月日, 4, 7)), format = "%Y%m%d")
  )
# 计算每_的数量分布并转换为日平均样本量
ERCL_data_aggregated <- ERCL_data %>%
  mutate(
    年月 = floor_date(西元年月日, "1 months")
  ) %>%
  group_by(年月) %>%
  summarise(
    數量 = n(),
    天數 = as.numeric(max(西元年月日) - min(西元年月日)) + 1, # 计算每2个月内的天数
    日平均樣本量 = 數量 / 天數 # 计算日平均样本量
  )

# 绘制直方图
ggplot(ERCL_data_aggregated, aes(x = 年月, y = 日平均樣本量)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "12 months", date_labels = "%Y-%m") +
  labs(title = "每1個月的日平均交易量分布直方圖", x = "年月", y = "日平均交易量") +
  theme_minimal()
# 繪製時間數列圖
ggplot(ERCL_data_aggregated, aes(x = 年月, y = 日平均樣本量)) +
  geom_line(color = "blue") + # 使用線圖表示時間數列
  geom_point(color = "red") + # 添加數據點以方便觀察
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(title = "日平均樣本量的時間數列圖", x = "年月", y = "日平均樣本量") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 旋轉X軸標籤方便閱讀
#####################
#####################
# 確保 "last_prop_date" 是日期格式，並計算每個色塊的結束時間
ACL_data <- ACL_data %>%
  mutate(
    last_prop_date = as.Date(last_prop_date),  # 確保日期格式
    end_date = last_prop_date + next_days_diff  # 計算結束時間
  )

# 繪製日均交易量的直方圖，使用 "last_prop_date" 和 "end_date" 來作為色塊
ggplot(ACL_data, aes(x = last_prop_date, xend = end_date, y = 日均交易量, yend = 日均交易量)) +
  geom_rect(aes(xmin = last_prop_date, xmax = end_date, ymin = 0, ymax = 日均交易量), 
            fill = "skyblue", color = "black", alpha = 0.5) +  # 繪製色塊
  scale_x_date(
    labels = scales::date_format("%Y-%m-%d"),  # 設定x軸的顯示格式
    breaks = scales::date_breaks("8 month"),   # 設定每個刻度間隔為1個月
    limits = c(min(ACL_data$last_prop_date), max(ACL_data$end_date))  # 設定x軸的範圍
  ) +
  labs(x = "時間", y = "日平均交易量", 
       title = "政策時間切割日平均交易量直方圖") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x 軸文字旋轉，避免重疊


###############
###############
###############
colnames(ACL_data)


# 確保資料按照 "順序" 排序
ACL_data <- ACL_data %>%
  arrange(順序)

# 計算 "日均交易量" 的前後差異
ACL_data <- ACL_data %>%
  mutate(
    abs_diff_daily_volume = abs(日均交易量 - lag(日均交易量)), # 計算與前一個值的差異，取絕對值
    diff_daily_volume = 日均交易量 - lag(日均交易量)
  )

# 載入必要套件
library(ggplot2)
library(dplyr)

# 確保 next_days_diff 是數值型別
ACL_data$next_days_diff <- as.numeric(ACL_data$next_days_diff)

# 按照 next_days_diff 從大到小進行排序
ACL_data_sorted <- ACL_data %>% 
  arrange(desc(next_days_diff))

# 繪製長條圖並加入數值標籤
ggplot(ACL_data_sorted, aes(x = reorder(factor(順序), -next_days_diff), y = next_days_diff)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", alpha = 0.7) +
  geom_text(aes(label = round(next_days_diff, 1)), 
            vjust = -0.5,  # 調整標籤位置，放在長條上方
            size = 3.5) +  # 調整標籤文字大小
  labs(title = "政策時間", 
       x = "順序", y = "政策時間") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # 調整y軸範圍，為數值標籤留出空間
  scale_y_continuous(limits = function(x) c(min(x), max(x) * 1.1))

# 計算 "順序" 的頻數
freq_dataset <- CL_data %>%
  group_by(順序) %>%
  summarise(頻數 = n()) %>%
  ungroup()
###############################################################
#####################################"單位平方公尺金額"########
###############################################################
library(dplyr)
library(ggplot2)
library(lubridate)

# 假設 filtered_data 是您的資料框

# 修改日期格式為西元日期
filtered_data <- filtered_data %>%
  mutate(
    西元日期 = as.Date(as.character(交易年月日 + 19110000), format = "%Y%m%d"),  # 將民國轉換為西元
    年份 = year(西元日期)  # 提取年份
  )

# 計算每年的單位平方公尺金額中位數
annual_median_data <- filtered_data %>%
  group_by(年份) %>%
  summarise(
    年中位數價格 = median(單位平方公尺金額, na.rm = TRUE)
  )

# 繪製年中位數的直方圖
ggplot(annual_median_data, aes(x = 年份, y = 年中位數價格)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  labs(
    title = "全台年度住房單位平方公尺金額中位數直方圖(2012與2024年資料不完整)",
    x = "年份",
    y = "單位平方公尺金額的中位數"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


library(dplyr)
library(ggplot2)
library(lubridate)

# 假設 filtered_data 是您的資料框


# 按縣市和年份計算單位平方公尺金額的年中位數，並移除連江縣
annual_median_data <- filtered_data %>%
  filter(縣市名 != "連江縣") %>%  # 排除連江縣
  group_by(縣市名, 年份) %>%
  summarise(
    年中位數價格 = median(單位平方公尺金額, na.rm = TRUE),
    .groups = "drop"
  )

# 繪製多重折線圖
ggplot(annual_median_data, aes(x = 年份, y = 年中位數價格, color = 縣市名, group = 縣市名)) +
  geom_line(size = 1) +  # 繪製折線
  geom_point(size = 2) +  # 繪製節點
  geom_text(
    data = annual_median_data %>% 
      group_by(縣市名) %>%
      filter(年份 == 2024),  # 找到每條線的終點
    aes(label = 縣市名),
    hjust = -0.2, vjust = 0.5, size = 3
  ) +  # 在線條結尾標註縣市名
  labs(
    title = "全台縣市年度住房單位平方公尺金額中位數多重折線圖\n(2012與2024年資料不完整)",
    x = "年份",
    y = "單位平方公尺金額的中位數",
    color = "縣市"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",  # 隱藏圖例
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

###################
###################
###################
colnames(CL_data)

colnames(ACL_data)
# 改名變數
BCL_data <- CL_data %>%
  rename(
    city = 縣市名,
    district = 鄉鎮市區,
    unit_price = 單位平方公尺金額,
    PROP_orders = 順序,
    days_diff = next_days_diff
  )

# 計算每個"縣市名"的日均交易量與單位平方公尺金額中位數
summary_data <- BCL_data %>%
  group_by(city) %>%
  summarise(
    daily_transaction_volume = sum(days_diff) / n(),  # 計算日均交易量
    median_unit_price = median(unit_price, na.rm = TRUE)  # 計算單位平方公尺金額的中位數
  )

# 繪製日均交易量與單位平方公尺金額中位數的散布圖，並標註每個縣市
ggplot(summary_data, aes(x =median_unit_price, y =  daily_transaction_volume)) +
  geom_point(aes(color = city), size = 3) +  # 散布圖，根據縣市名來上色
  geom_text(aes(label = city), vjust = -0.5, size = 3) +  # 顯示每個城市名稱
  labs( title ="實價登錄後各城市日均交易量與單位平方公尺價格中位數",
    x = "單位平方公尺金額的中位數" ,
    y = "日均交易量"
  ) +
  theme_minimal() +
  theme(legend.position = "top")



#################
#####分群分析####
#################

#####################
# 定義篩選資料的函數#
#####################
######################
######2級行政區#######
######################
calculate_and_plot_summary_city <- function(data, PROP_orders_values, save_data_name = NULL) {
  
  # 篩選出 PROP_orders 為指定數字的樣本
  filtered_data <- data %>% 
    filter(PROP_orders %in% PROP_orders_values)
  
  # 計算每個 city 的中位數和日均交易量
  summary_data <- filtered_data %>%
    group_by(city) %>%
    summarise(
      median_unit_price = median(unit_price, na.rm = TRUE),
      daily_transaction_volume = n() / sum(unique(days_diff))
    ) %>%
    ungroup()
  
  # 創建散布圖
  scatter_plot <- ggplot(summary_data, 
                         aes(x = median_unit_price, 
                             y = daily_transaction_volume,
                             color = city,
                             label = city)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_text(hjust = -0.2, vjust = 0.5, size = 3) + # 新增城市名稱標籤
    labs(
      title = paste("Median Unit Price vs Daily Transaction Volume\n(PROP_orders in", 
                    paste(PROP_orders_values, collapse = ", "), ")"),
      x = "每平方公尺價格中位數",
      y = "日均交易量",
      color = "城市"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # 如果沒有提供 save_data_name，則自動生成名稱
  if (is.null(save_data_name)) {
    save_data_name <- paste0("c_", paste(PROP_orders_values, collapse = ""))
  }
  
  # 如果提供了 save_data_name，則將數據保存到指定名稱的變量中
  assign(save_data_name, summary_data, envir = .GlobalEnv)
  
  # 顯示圖表
  print(scatter_plot)
  
  # 回傳數據和圖表
  invisible(list(
    summary_data = summary_data,
    plot = scatter_plot
  ))
}

####################################
####################################
# 先根據 next_days_diff 從大到小排序
sorted_acl_data <- ACL_data %>%
  arrange(desc(next_days_diff))
# 篩選掉 next_days_diff < 19 的順序
sorted_acl_data_filtered <- sorted_acl_data %>%
  filter(next_days_diff >= 19)

# 篩選掉 next_days_diff < 19 的順序
sorted_acl_data_B <- sorted_acl_data %>%
  filter(next_days_diff <= 19)
##66~11##
####c(66,65),c(60,59,58),c(51,50) ,c(45,44) ,c(38,37,36,35),c(26,25), c(18,17),c(12,11)

# 已知的合併順序組合
predefined_groups <- list(
  c(66, 65), c(60, 59, 58), c(51, 50), c(45, 44),
  c(38, 37, 36, 35), c(26, 25), c(18, 17), c(12, 11)
)

# 先將已知的合併順序添加到 order_list 中
order_list <- predefined_groups

# 獲取 11 到 66 範圍內的所有順序
all_order_values <- 11:66

# 從排序資料中獲取所有順序，並排除已經在 predefined_groups 中的數字
existing_values <- unlist(predefined_groups)
remaining_values <- setdiff(all_order_values, existing_values)

# 將剩餘的順序數字單獨添加到 order_list 中
for (value in remaining_values) {
  order_list <- append(order_list, list(value))
}

# 遍歷順序列表並依次傳遞給函數
for (order_value_set in order_list) {
  calculate_and_plot_summary_city(BCL_data, order_value_set)
}
# 列出當前環境中的所有物件
all_objects <- ls()

# 篩選出那些為空的資料集或物件
empty_objects <- all_objects[sapply(all_objects, function(x) {
  obj <- get(x)  # 取得物件
  is.data.frame(obj) && nrow(obj) == 0  # 檢查是否為空的資料框
  # 如果是其他類型的空物件，可以根據需要進行檢查
})]

# 移除所有空的資料集
rm(list = empty_objects)
colnames(c_27)
##############################################################################
###############################簡單圖#########################################
##############################################################################
library(dplyr)

# 定義一個包含每個資料集名稱的列表
data_sets <- list(c_1211, c_13, c_14, c_15, c_16, c_1817, c_19, c_20, c_21, c_22, 
                  c_24, c_2625, c_27, c_29, c_30, c_31, c_32, c_33, c_34, c_38373635, 
                  c_39, c_40, c_41, c_42, c_43, c_4544, c_46, c_47, c_48, c_49, c_5150, 
                  c_52, c_53, c_54, c_55, c_57, c_605958, c_61, c_62, c_63, c_64, c_6665)

# 將所有資料集合併成一個資料框，並為每個資料集標註時間順序
data_combined <- bind_rows(
  lapply(seq_along(data_sets), function(i) {
    df <- data_sets[[i]]
    df$time_order <- i  # 添加對應的時間順序
    return(df)
  })
)

# 篩選出不為NA的城市
data_combined <- data_combined %>%
  filter(!is.na(city))  # 排除沒有城市的資料

# 繪製每個城市的路徑圖
ggplot(data_combined, aes(x = median_unit_price, y = daily_transaction_volume, group = city, color = city)) +
  geom_line(size = 1) +  # 繪製路徑
  geom_point(size = 2) +  # 標註每個時間點
  labs(
    title = "各縣市政策分段住房市場變化動向分布圖",
    x = "單位平方公尺金額的中位數",
    y = "日均交易量",
    color = "City",
    shapes ="city"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )
###############################################
##################REDBLUE######################
###############################################
library(ggplot2)
library(dplyr)

# 定義一個包含每個資料集名稱的列表
data_sets <- list(c_1211, c_13, c_14, c_15, c_16, c_1817, c_19, c_20, c_21, c_22, 
                  c_24, c_2625, c_27, c_29, c_30, c_31, c_32, c_33, c_34, c_38373635, 
                  c_39, c_40, c_41, c_42, c_43, c_4544, c_46, c_47, c_48, c_49, c_5150, 
                  c_52, c_53, c_54, c_55, c_57, c_605958, c_61, c_62, c_63, c_64, c_6665)

# 將所有資料集合併成一個資料框，並為每個資料集標註時間順序
data_combined <- bind_rows(
  lapply(seq_along(data_sets), function(i) {
    df <- data_sets[[i]]
    df$time_order <- i  # 添加對應的時間順序
    return(df)
  })
)

# 篩選出不為NA的城市
data_combined <- data_combined %>%
  filter(!is.na(city))  # 排除沒有城市的資料

# 找到每個城市最早的點
first_points <- data_combined %>%
  group_by(city) %>%
  filter(time_order == min(time_order)) %>%
  ungroup()

# 繪製每個城市的路徑圖，使用顏色漸變來顯示時間順序，並在最早點加上標註
ggplot(data_combined, aes(x = median_unit_price, y = daily_transaction_volume, group = city, color = time_order, shape = city)) +
  geom_line(size = 1) +  # 繪製路徑
  geom_point(size = 2) +  # 標註每個時間點
  scale_color_gradient(low = "blue", high = "red") +  # 顏色漸變，時間越近為藍色，越遠為紅色
  geom_text(data = first_points, aes(label = city), vjust = -1, hjust = 0.5, size = 3) +  # 在最早的點上添加標註
  labs(
    title =  "各縣市政策分段住房市場變化動向分布圖",
    x ="單位平方公尺金額的中位數",
    y = "單位平方公尺金額的中位數",
    color ="政策分段",
    shape = "城市"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

############################################
##################彩虹######################
############################################
# 假設 city 是字符型，將優先城市重新排序
priority_cities <- c("台北市", "新北市", "桃園市", "高雄市", "台中市", "台南市")

# 將 city 欄位轉換為 factor 並重新排序，優先城市排在前面
data_combined$city <- factor(data_combined$city, levels = c(priority_cities, setdiff(unique(data_combined$city), priority_cities)))

# 繪製每個城市的路徑圖，字顏色使用彩虹漸變，形狀優先分配給特定城市
ggplot(data_combined, aes(x = median_unit_price, y = daily_transaction_volume, group = city, color = time_order, shape = city)) +
  geom_line(size = 1) +  # 繪製路徑
  geom_point(size = 2) +  # 標註每個時間點
  scale_color_gradientn(colors = c("purple3", "green3", "yellow3", "red3")) +  # 彩虹色顏色漸變
  geom_text(data = first_points, aes(label = city, color = time_order), vjust = -1, hjust = 0.5, size = 3) +  # 在最早的點上添加標註，顏色使用時間順序的漸變
  labs(
    title = "各縣市政策分段住房市場變化動向分布圖",
    x ="單位平方公尺金額的中位數",
    y = "單位平方公尺金額的中位數",
    color ="政策分段",
    shape = "城市"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

############################################
#################六都彩虹###################
############################################
# 定義六都的城市名稱
cities_of_interest <- c("台北市", "新北市", "桃園市", "高雄市", "台中市", "台南市")

# 篩選只保留六都的資料
data_combined_six_cities <- data_combined %>%
  filter(city %in% cities_of_interest)  # 只保留六都

# 找到每個城市最早的點
first_points_six_cities <- data_combined_six_cities %>%
  group_by(city) %>%
  filter(time_order == min(time_order)) %>%
  ungroup()

# 繪製每個城市的路徑圖，使用顏色漸變來顯示時間順序，並在最早點加上標註
ggplot(data_combined_six_cities, aes(x = median_unit_price, y = daily_transaction_volume, group = city, color = time_order, shape = city)) +
  geom_line(size = 1) +  # 繪製路徑
  geom_point(size = 2) +  # 標註每個時間點
  scale_color_gradientn(colors = c("purple3", "green3", "yellow3", "red3")) +  # 顏色漸變，時間越近為藍色，越遠為紅色
  geom_text(data = first_points_six_cities, aes(label = city, color =time_order), vjust = -1, hjust = 0.5, size = 3) +  # 在最早的點上添加標註
  labs(
    title = "六都政策分段住房市場變化動向分布圖",
    x ="單位平方公尺金額的中位數",
    y = "單位平方公尺金額的中位數",
    color ="政策分段",
    shape = "城市"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

#########################################################################
#########################################################################
#########################################################################
# 計算每個城市的後期減前期（一階差分）
data_diff <- data_combined_six_cities %>%
  group_by(city) %>%
  arrange(city, time_order) %>%  # 確保按時間順序排序
  mutate(
    diff_median_unit_price = median_unit_price - lag(median_unit_price),
    diff_daily_transaction_volume = daily_transaction_volume - lag(daily_transaction_volume)
  ) %>%
  ungroup()

# 計算標準化（z-score），即 (X - mean) / sd
data_diff_standardized <- data_diff %>%
  group_by(city) %>%
  mutate(
    z_median_unit_price = scale(diff_median_unit_price, center = TRUE, scale = TRUE),
    z_daily_transaction_volume = scale(diff_daily_transaction_volume, center = TRUE, scale = TRUE)
  ) %>%
  ungroup()

# 找到每個城市最早的標準化點
first_points_six_cities_standardized <- data_diff_standardized %>%
  group_by(city) %>%
  filter(time_order == min(time_order)) %>%
  ungroup()

# 繪製標準化後的圖表
ggplot(data_diff_standardized, aes(x = z_median_unit_price, y = z_daily_transaction_volume, group = city, color = time_order, shape = city)) +
  geom_line(size = 1) +  # 繪製路徑
  geom_point(size = 2) +  # 標註每個時間點
  scale_color_gradientn(colors = c("purple3", "green3", "yellow3", "red3")) +  # 顏色漸變，時間越近為藍色，越遠為紅色
  geom_text(data = first_points_six_cities_standardized, aes(label = city), color = "black", vjust = -1, hjust = 0.5, size = 3) +  # 在最早的點上添加標註
  labs(
    title = "Standardized City-wise Path of Median Unit Price and Daily Transaction Volume (First Difference)",
    x = "Standardized (Difference in Median Unit Price)",
    y = "Standardized (Difference in Daily Transaction Volume)",
    color = "Time Order",
    shape = "City"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )
#########################################################################
#########################################################################
#########################################################################
# 計算每個城市的後期減前期（一階差分）
data_diff <- data_combined_six_cities %>%
  group_by(city) %>%
  arrange(city, time_order) %>%  # 確保按時間順序排序
  mutate(
    diff_median_unit_price = median_unit_price - lag(median_unit_price),
    diff_daily_transaction_volume = daily_transaction_volume - lag(daily_transaction_volume)
  ) %>%
  ungroup()

# 找到每個城市最早的點
first_points_six_cities_diff <- data_diff %>%
  group_by(city) %>%
  filter(time_order == min(time_order)) %>%
  ungroup()

# 繪製後期減前期的圖表
ggplot(data_diff, aes(x = diff_median_unit_price, y = diff_daily_transaction_volume, group = city, color = time_order, shape = city)) +
  geom_line(size = 1) +  # 繪製路徑
  geom_point(size = 2) +  # 標註每個時間點
  scale_color_gradientn(colors = c("purple3", "green3", "yellow3", "red3")) +  # 顏色漸變，時間越近為藍色，越遠為紅色
  geom_text(data = first_points_six_cities_diff, aes(label = city), color = "black", vjust = -1, hjust = 0.5, size = 3) +  # 在最早的點上添加標註
  labs(
    title = "City-wise Path of First Difference in Median Unit Price and Daily Transaction Volume (Six Major Cities)",
    x = "First Difference in Median Unit Price",
    y = "First Difference in Daily Transaction Volume",
    color = "Time Order",
    shape = "City"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )
#######################################################################
##########################二級分群資料差異#############################
#######################################################################
compare_and_plot_datasets_city <- function(data1_name, data2_name) {
  # 從全局環境獲取數據
  data1 <- get(data1_name, envir = .GlobalEnv)
  data2 <- get(data2_name, envir = .GlobalEnv)
  
  # 找出相同的 city 和 district
  common_areas <- merge(
    data1, 
    data2, 
    by = "city",
    suffixes = c("_1", "_2")
  )
  
  # 計算差異
  comparison_data <- common_areas %>%
    mutate(
      price_diff = median_unit_price_1 - median_unit_price_2,
      volume_diff = daily_transaction_volume_1 - daily_transaction_volume_2
    )
  
  # 創建散布圖
  scatter_plot <- ggplot(comparison_data, 
                         aes(x = price_diff, 
                             y = volume_diff,
                             color = city,
                             label = city
                         )) +
    geom_point(size = 3, alpha = 0.7) +
    geom_text(hjust = -0.2, vjust = 0, size = 3) +
    labs(
      title = paste("Differences in Metrics between", data1_name, "and", data2_name),
      x = "單位平方公尺金額的中位數差異",
      y = "日均交易量差異",
      color = "城市"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # 顯示圖表
  print(scatter_plot)
  
  # 將比較結果保存到全局環境
  result_name <- paste0(data1_name, "M", data2_name)
  assign(result_name, comparison_data, envir = .GlobalEnv)
  
  # 回傳比較數據
  invisible(comparison_data)
}
#########################################################################
#########################################################################
colnames(BCL_data)
# 找出每個唯一 Event 對應的記錄
unique_events_data <- BCL_data %>%
  group_by(Event) %>% 
  slice(1) %>%  # 每個 Event 僅保留第一筆記錄
  ungroup()
unique_events <- unique(BCL_data$Event)

# 查看結果
print(unique_events)
##############################################################################################################
#############"青安貸款額度提高至800萬\"---房地合一稅1.0上路，奢侈稅同步退場" 發生#############################
###後與之前 "台北市「囤房稅」增訂防錯殺條款","金管會取消銀行不動產貸款限制措施"的差異 ########################
##############################################################################################################
#######K-MEANS########
######################
compare_and_plot_datasets_city("c_27", "c_2625")
colnames(c_27Mc_2625)
# 加載必要的庫
# 加載必要的庫
required_packages <- c("ggplot2", "cluster", "factoextra")

# 檢查並安裝未安裝的套件
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
###############################
############K-means fun########
###############################
perform_kmeans_analysis <- function(data, city_col, features, max_k = 10, nstart = 9) {
  # 篩選所需數據，排除城市列中為 NA 或空白的行
  selected_data <- data[data[[city_col]] != "" & !is.na(data[[city_col]]), features]
  
  # 去除 NA 值
  selected_data <- na.omit(selected_data)
  
  # 檢查 selected_data 是否仍然有數據
  if (nrow(selected_data) == 0) {
    stop("After cleaning, there are no valid rows of data available for clustering.")
  }
  
  # 標準化數據
  selected_data_scaled <- scale(selected_data)
  
  # 繪製陡坡圖以選擇最佳 K 值
  wss <- sapply(1:max_k, function(k) {
    kmeans(selected_data_scaled, centers = k, nstart = nstart)$tot.withinss
  })
  
  # 陡坡圖
  plot(1:max_k, wss, type = "b", pch = 19, frame = FALSE,
       xlab = "群數", ylab = "群內總變異",
       main = "陡坡圖")
  
  # 確定最佳 K 值（可以手動根據圖形選擇）
  cat("Please examine the elbow plot to determine the best K.\n")
  
  # 選擇最佳 K，這裡假設選擇的 K 為 3（用戶需根據陡坡圖更改）
  best_k <- as.integer(readline(prompt = "Enter the optimal number of clusters (K): "))
  
  # 執行 K-means 並計算每次的距離測度和 Silhouette 指標
  kmeans_results <- lapply(1:nstart, function(i) {
    set.seed(i)
    kmeans_model <- kmeans(selected_data_scaled, centers = best_k, nstart = 1)
    
    # 計算 Silhouette 指標
    silhouette_values <- silhouette(kmeans_model$cluster, dist(selected_data_scaled))
    silhouette_avg <- mean(silhouette_values[, 3])
    
    list(
      model = kmeans_model,
      silhouette_avg = silhouette_avg,
      withinss = kmeans_model$tot.withinss
    )
  })
  
  # 顯示每次的距離測度和 Silhouette 指標
  cat("\nK-Means Results:\n")
  for (i in seq_along(kmeans_results)) {
    cat(sprintf("Run %d:\n", i))
    cat(sprintf("  - Total Within Sum of Squares: %.2f\n", kmeans_results[[i]]$withinss))
    cat(sprintf("  - Silhouette Score: %.4f\n\n", kmeans_results[[i]]$silhouette_avg))
  }
  
  # 返回最佳模型和結果
  best_run <- which.max(sapply(kmeans_results, function(x) x$silhouette_avg))
  cat(sprintf("Best run is Run %d with Silhouette Score %.4f.\n", best_run, kmeans_results[[best_run]]$silhouette_avg))
  
  # 顯示最佳分群結果的散點圖
  best_kmeans_model <- kmeans_results[[best_run]]$model
  cluster_data <- data.frame(selected_data, Cluster = as.factor(best_kmeans_model$cluster), City = data[[city_col]])
  
  # 繪製最佳分群結果的散點圖並標註城市名稱
  a <- ggplot(cluster_data, aes(x = selected_data[, 1], y = selected_data[, 2], color = Cluster)) +
    geom_point() +
    geom_text(aes(label = City), size = 3, vjust = -0.5, hjust = 0.5) + # 標註城市名稱
    labs(title = "K-Means Clustering Results", x = features[1], y = features[2], color = "Cluster") +
    theme_minimal()
  
  # 顯示結果
  print(a)
  
  return(kmeans_results[[best_run]])
}


#########################################
# 示例應用到 "c_27Mc_2625" 資料集########
# 使用函數進行分析#######################K=2
result <- perform_kmeans_analysis(data = c_27Mc_2625, city_col = "city", 
                                  features = c("price_diff", "volume_diff"))
############
#移除金門縣#
############K=2
Tc_27Mc_2625 <- c_27Mc_2625 %>%
filter(city != "金門縣")
result <- perform_kmeans_analysis(data = Tc_27Mc_2625, city_col = "city", 
                                  features = c("price_diff", "volume_diff"))
###############################
#############Hie###############
###############################
# 載入必要的套件
library(ggplot2)
library(stats)
# 載入所需的套件
library(cluster)  # 用於 Silhouette 分數計算

# 定義函數進行 Centroid linkage 的階層式分群，並計算 Silhouette 指標
centroid_hierarchical_clustering <- function(data, scale_data = TRUE,nm_clusters=3) {
  
  # 提取價格差異(price_diff)與量差異(volume_diff)特徵
  data_features <- data[, c("price_diff", "volume_diff")]
  
  # 如果 scale_data 為 TRUE，則標準化數據
  if (scale_data) {
    data_features <- scale(data_features)
  }
  
  # 計算距離矩陣
  dist_matrix <- dist(data_features, method = "euclidean")
  
  # 執行階層式分群（使用 Centroid linkage）
  hc_centroid <- hclust(dist_matrix, method = "centroid")
  
  # 繪製 Dendrogram，並且標籤對應到城市 (city)
  dendrogram_plot <- plot(hc_centroid, main = "樹狀圖以 Centroid linkage", 
                          labels = data$city, cex = 0.7, hang = -1)
  
  # 計算 Silhouette 分數
  # 假設我們選擇分群為 3 群，您可以根據 Dendrogram 來調整群數
  num_clusters <- nm_clusters
  clusters <- cutree(hc_centroid, k = num_clusters)
  silhouette_score <- silhouette(clusters, dist_matrix)
  
  # 繪製 Silhouette 分數圖
  plot(silhouette_score, main = "輪廓圖以 Centroid linkage")
  
  # 返回分群結果與 Silhouette 分數
  return(list(hc_centroid = hc_centroid, silhouette_score = silhouette_score))
}

# 使用範例資料進行測試
# 假設 c_27Mc_2625 是資料集，並且包含 "price_diff", "volume_diff", "city" 這些變數
# centroid_result <- centroid_hierarchical_clustering(c_27Mc_2625, scale_data = TRUE)

# 載入所需的套件
library(ggplot2)
library(stats)
library(cluster)  # 用於 Silhouette 分數計算

# 定義函數進行階層式分群，並計算 Silhouette 分數
hierarchical_clustering <- function(data, linkage_method = c("complete", "single", "average", "ward.D", "centroid"), scale_data = TRUE,m_clusters=3) {
  
  # 如果選擇了 "centroid"，則調用 centroid_hierarchical_clustering
  if (linkage_method == "centroid") {
    return(centroid_hierarchical_clustering(data, scale_data,nm_clusters=m_clusters))
  }
  
  # 提取價格差異(price_diff)與量差異(volume_diff)特徵
  data_features <- data[, c("price_diff", "volume_diff")]
  
  # 如果 scale_data 為 TRUE，則標準化數據
  if (scale_data) {
    data_features <- scale(data_features)
  }
  
  # 計算距離矩陣
  dist_matrix <- dist(data_features, method = "euclidean")
  
  # 設定 linkage 方法
  linkage_method <- match.arg(linkage_method)
  
  # 執行階層式分群
  hc <- hclust(dist_matrix, method = linkage_method)
  
  # 繪製 Dendrogram，並且標籤對應到城市 (city)
  dendrogram_plot <- plot(hc, main = paste("樹狀圖以", linkage_method, "linkage"), 
                          labels = data$city, cex = 0.7, hang = -1, las = 1, xlab = "城市", ylab = "距離")
  
  # 計算 Silhouette 分數
  # 設定分群數量，這裡選擇分為 3 群
  num_clusters <- m_clusters
  clusters <- cutree(hc, k = num_clusters)
  silhouette_score <- silhouette(clusters, dist_matrix)
  
  # 繪製 Silhouette 分數圖
  plot(silhouette_score, main = paste("輪廓圖以", linkage_method, "linkage"))
  
  # 返回分群結果與 Silhouette 分數
  return(list(hc = hc, silhouette_score = silhouette_score))
}



##################################
#######HEI_FUN####################
##################################

linkage_methods <- c("complete", "single", "average", "ward.D", "centroid")
for (method in linkage_methods) {
  hc_result <- hierarchical_clustering(c_27Mc_2625, linkage_method = method, scale_data = FALSE)
}
# 使用不同的 linkage 方法進行分群，並繪製 Dendrogram
for (method in linkage_methods) {
  hc_result <- hierarchical_clustering(c_27Mc_2625, linkage_method = method, scale_data = TRUE)
}
linkage_methods <- c("complete", "single", "average", "ward.D", "centroid")
for (method in linkage_methods) {
  hc_result <- hierarchical_clustering(c_27Mc_2625, linkage_method = method, scale_data = FALSE,m_clusters=4)
}
# 使用不同的 linkage 方法進行分群，並繪製 Dendrogram
for (method in linkage_methods) {
  hc_result <- hierarchical_clustering(c_27Mc_2625, linkage_method = method, scale_data = TRUE,m_clusters=4)
}
for (method in linkage_methods) {
  hc_result <- hierarchical_clustering(c_27Mc_2625, linkage_method = method, scale_data = FALSE,m_clusters=2)
}
# 使用不同的 linkage 方法進行分群，並繪製 Dendrogram
for (method in linkage_methods) {
  hc_result <- hierarchical_clustering(c_27Mc_2625, linkage_method = method, scale_data = TRUE,m_clusters=2)
}

##########################
#########移除金門縣#######
##########################

#########################
#########################
#########################
linkage_methods <- c("complete", "single", "average", "ward.D", "centroid")
for (method in linkage_methods) {
  hc_result <- hierarchical_clustering(Tc_27Mc_2625, linkage_method = method, scale_data = FALSE)
}
# 使用不同的 linkage 方法進行分群，並繪製 Dendrogram
for (method in linkage_methods) {
  hc_result <- hierarchical_clustering(Tc_27Mc_2625, linkage_method = method, scale_data = TRUE)
}
for (method in linkage_methods) {
  hc_result <- hierarchical_clustering(Tc_27Mc_2625, linkage_method = method, scale_data = FALSE,m_clusters=4)
}
# 使用不同的 linkage 方法進行分群，並繪製 Dendrogram
for (method in linkage_methods) {
  hc_result <- hierarchical_clustering(Tc_27Mc_2625, linkage_method = method, scale_data = TRUE,m_clusters=4)
}
for (method in linkage_methods) {
  hc_result <- hierarchical_clustering(Tc_27Mc_2625, linkage_method = method, scale_data = FALSE,m_clusters=2)
}
# 使用不同的 linkage 方法進行分群，並繪製 Dendrogram
for (method in linkage_methods) {
  hc_result <- hierarchical_clustering(Tc_27Mc_2625, linkage_method = method, scale_data = TRUE,m_clusters=2)
}
############################################
###################FC#######################
############################################
install.packages("e1071")
library(e1071)

# 假設數據集中有 median_unit_price 和 daily_transaction_volume 兩個變數
data_for_clustering <- data_combined_six_cities %>%
  select(median_unit_price, daily_transaction_volume)

# 執行模糊聚類（例如選擇3個聚類中心）
fcm_result <- cmeans(data_for_clustering, centers = 3)

# 查看聚類結果
fcm_result$membership  # 顯示每個點對各聚類的隸屬度
# 添加模糊聚類結果的隸屬度到原始數據
data_combined_six_cities$fuzzy_cluster <- apply(fcm_result$membership, 1, which.max)

# 添加模糊聚類的隸屬度（最大隸屬度）
data_combined_six_cities$fuzzy_membership <- apply(fcm_result$membership, 1, max)

ggplot(data_combined_six_cities, aes(x = median_unit_price, y = daily_transaction_volume, 
                                     group = city, color = as.factor(fuzzy_cluster), 
                                     size = fuzzy_membership)) +
  geom_line(size = 1) +  # 繪製路徑
  geom_point(size = 2) +  # 標註每個時間點
  scale_color_manual(values = c("red", "blue", "green")) +  # 根據聚類結果設定顏色
  labs(
    title = "各縣市房地產變化路徑模糊集群分析 ",
    x ="單位平方公尺金額的中位數",
    y =  "日均交易量",
    color = "集群",
    size = "Fuzzy Membership"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )



####

ggplot(data_combined_six_cities, aes(x = median_unit_price, y = daily_transaction_volume, 
                                     group = city, color = as.factor(fuzzy_cluster), 
                                     size = fuzzy_membership, shape = city)) + 
  geom_line(size = 1) +  # 繪製路徑
  geom_point(size = 2) +  # 標註每個時間點
  scale_color_manual(values = c("red2", "blue3", "green3")) +  # 根據聚類結果設定顏色
  scale_shape_manual(values = 1:length(unique(data_combined_six_cities$city))) +  # 依城市設定不同形狀
  labs(
    title = "各縣市房地產變化路徑模糊集群分析",
    x =  "單位平方公尺金額的中位數",
    y =  "日均交易量",
    color = "集群",
    size = "Fuzzy Membership",
    shape = "城市"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

ggplot(data_combined_six_cities, aes(x = median_unit_price, y = daily_transaction_volume, 
                                     group = city, color = city, 
                                     size = fuzzy_membership, shape = as.factor(fuzzy_cluster))) + 
  geom_line(size = 1) +  # 繪製路徑
  geom_point(size = 2) +  # 標註每個時間點
  scale_color_manual(values = c("red2", "blue3", "green3", "purple", "orange", "pink4")) +  # 根據縣市設定顏色
  scale_shape_manual(values = c(15, 20, 8)) +  # 根據聚類結果設定形狀（例如圓形、方形、三角形）
  labs(
    title = "各縣市房地產變化路徑模糊集群分析",
    x = "單位平方公尺金額的中位數",
    y = "日均交易量",
    color = "城市",
    size = "Fuzzy Membership",
    shape = "集群"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )
# 計算每個城市的平均最大隸屬度
city_membership_effect <- data_combined_six_cities %>%
  group_by(city) %>%
  summarise(
    avg_membership = mean(fuzzy_membership),  # 平均最大隸屬度
    membership_sd = sd(fuzzy_membership)      # 最大隸屬度的標準差
  )

# 輸出結果，按平均隸屬度排序
city_membership_effect <- city_membership_effect %>%
  arrange(desc(avg_membership))

# 顯示每個城市的分群效果
print(city_membership_effect)

ggplot(city_membership_effect, aes(x = reorder(city, avg_membership), y = avg_membership)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # 反轉x軸和y軸
  labs(
    title = "各城市平均最大隸屬度",
    x = "城市",
    y = "平均最大隸屬度"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # 調整標題字體大小
    axis.title.x = element_text(size = 14),  # 調整x軸標籤字體大小
    axis.title.y = element_text(size = 14),  # 調整y軸標籤字體大小
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # 調整x軸文字大小
    axis.text.y = element_text(size = 12)  # 調整y軸文字大小
  )
#########################
########3級行政區########
#########################
# 定義函數，篩選 PROP_orders 並創建散布圖
calculate_and_plot_summary <- function(data, PROP_orders_values, save_data_name = NULL) {
  # 將城市名稱統一處理
  data <- data %>%
    mutate(
      city = case_when(
        city %in% c("新竹市", "新竹縣") ~ "新竹",
        city %in% c("嘉義縣", "嘉義市") ~ "嘉義",
        TRUE ~ city
      )
    )
  
  # 篩選出 PROP_orders 為指定數字的樣本
  filtered_data <- data %>% 
    filter(PROP_orders %in% PROP_orders_values)
  
  # 計算每個 city 和 district 下的中位數和日均交易量
  summary_data <- filtered_data %>%
    group_by(city, district) %>%
    summarise(
      median_unit_price = median(unit_price, na.rm = TRUE),
      daily_transaction_volume = n() / sum(unique(days_diff))
    ) %>%
    ungroup()
  
  # 創建散布圖
  scatter_plot <- ggplot(summary_data, 
                         aes(x = median_unit_price, 
                             y = daily_transaction_volume,
                             color = city,
                             label = district
                         )) +
    geom_point(size = 3, alpha = 0.7) +
    geom_text(hjust = -0.2, vjust = 0, size = 3) +
    labs(
      title = paste("Median Unit Price vs Daily Transaction Volume\n(PROP_orders in", 
                    paste(PROP_orders_values, collapse = ", "), ")"),
      x = "單位平方公尺金額的中位數",
      y = "日均交易量",
      color = "城市"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  # 如果沒有提供 save_data_name，則自動生成名稱
  if (is.null(save_data_name)) {
    save_data_name <- paste0("r_", paste(PROP_orders_values, collapse = ""))
  }
  
  # 如果提供了 save_data_name，則將數據保存到指定名稱的變量中
  if (!is.null(save_data_name)) {
    assign(save_data_name, summary_data, envir = .GlobalEnv)
  }
  
  # 顯示圖表
  print(scatter_plot)
  
  # 回傳數據和圖表
  invisible(list(
    summary_data = summary_data,
    plot = scatter_plot
  ))
}
#######################
#######################
# 先根據 next_days_diff 從大到小排序
sorted_acl_data <- ACL_data %>%
  arrange(desc(next_days_diff))
# 篩選掉 next_days_diff < 19 的順序
sorted_acl_data_filtered <- sorted_acl_data %>%
  filter(next_days_diff >= 19)

# 篩選掉 next_days_diff < 19 的順序
sorted_acl_data_B <- sorted_acl_data %>%
  filter(next_days_diff <= 19)
##66~11##
####c(66,65),c(60,59,58),c(51,50) ,c(45,44) ,c(38,37,36,35),c(26,25), c(18,17),c(12,11)

# 已知的合併順序組合
predefined_groups <- list(
  c(66, 65), c(60, 59, 58), c(51, 50), c(45, 44),
  c(38, 37, 36, 35), c(26, 25), c(18, 17), c(12, 11)
)

# 先將已知的合併順序添加到 order_list 中
order_list <- predefined_groups

# 獲取 11 到 66 範圍內的所有順序
all_order_values <- 11:66

# 從排序資料中獲取所有順序，並排除已經在 predefined_groups 中的數字
existing_values <- unlist(predefined_groups)
remaining_values <- setdiff(all_order_values, existing_values)

# 將剩餘的順序數字單獨添加到 order_list 中
for (value in remaining_values) {
  order_list <- append(order_list, list(value))
}

# 遍歷順序列表並依次傳遞給函數
for (order_value_set in order_list) {
  calculate_and_plot_summary(BCL_data, order_value_set)
}
# 列出當前環境中的所有物件
all_objects <- ls()

# 篩選出那些為空的資料集或物件
empty_objects <- all_objects[sapply(all_objects, function(x) {
  obj <- get(x)  # 取得物件
  is.data.frame(obj) && nrow(obj) == 0  # 檢查是否為空的資料框
  # 如果是其他類型的空物件，可以根據需要進行檢查
})]

# 移除所有空的資料集
rm(list = empty_objects)
####################################
####################################

#######################################
# 定義比較兩個資料集並繪製散布圖的函數#
#######################################
compare_and_plot_datasets <- function(data1_name, data2_name) {
  # 從全局環境獲取數據
  data1 <- get(data1_name, envir = .GlobalEnv)
  data2 <- get(data2_name, envir = .GlobalEnv)
  
  # 找出相同的 city 和 district
  common_areas <- merge(
    data1, 
    data2, 
    by = c("city", "district"),
    suffixes = c("_1", "_2")
  )
  
  # 計算差異
  comparison_data <- common_areas %>%
    mutate(
      price_diff = median_unit_price_1 - median_unit_price_2,
      volume_diff = daily_transaction_volume_1 - daily_transaction_volume_2
    )
  
  # 創建散布圖
  scatter_plot <- ggplot(comparison_data, 
                         aes(x = price_diff, 
                             y = volume_diff,
                             color = city,
                             label = district
                         )) +
    geom_point(size = 3, alpha = 0.7) +
    geom_text(hjust = -0.2, vjust = 0, size = 3) +
    labs(
      title = paste("Differences in Metrics between", data1_name, "and", data2_name),
      x = "Difference in Median Unit Price",
      y = "Difference in Daily Transaction Volume",
      color = "City"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # 顯示圖表
  print(scatter_plot)
  
  # 將比較結果保存到全局環境
  result_name <- paste0(data1_name, "M", data2_name)
  assign(result_name, comparison_data, envir = .GlobalEnv)
  
  # 回傳比較數據
  invisible(comparison_data)
}
###########################
# 直接使用變量名稱作為參數#
###########################
compare_and_plot_datasets("r_27", "r_2625")
colnames(r_27Mr_2625)
library(ggplot2)
library(cluster)  # 用於分群的額外方法
library(dplyr)
######################################
# 定義分群函數###KM###################
######################################
library(ggplot2)
library(dplyr)
library(cluster)  # for pam (partitioning around medoids)

KMcluster_and_visualize <- function(data, price_col, volume_col, city_col, district_col, 
                                  max_k = 10, method = "kmeans", nstart = 9) {
  # 確保輸入的列存在
  if (!all(c(price_col, volume_col, city_col, district_col) %in% colnames(data))) {
    stop("One or more specified columns do not exist in the dataset.")
  }
  
  # 選取並標準化所需列
  cluster_data <- data %>%
    select(all_of(c(price_col, volume_col, city_col, district_col))) %>%
    mutate(
      price_diff_scaled = scale(get(price_col)),
      volume_diff_scaled = scale(get(volume_col))
    )
  
  # 准備分群資料
  scaled_data <- cluster_data %>%
    select(price_diff_scaled, volume_diff_scaled)
  
  # 使用肘部法選擇最佳 k
  wss <- sapply(1:max_k, function(k) {
    kmeans(scaled_data, centers = k, nstart = nstart)$tot.withinss
  })
  
  # 繪製肘部圖
  plot(1:max_k, wss, type = "b", pch = 19, frame = FALSE,
       xlab = "群數", ylab = "群內總變異",
       main = "陡坡圖")
  
  # 提示使用者查看肘部圖以確定最佳 K
  cat("Please examine the elbow plot to determine the best K.\n")
  best_k <- as.integer(readline(prompt = "Enter the optimal number of clusters (K): "))
  
  # 根據選擇的分群方法進行分群
  if (method == "kmeans") {
    # 計算每次的距離測度和 Silhouette 指標
    kmeans_results <- lapply(1:nstart, function(i) {
      set.seed(i)
      kmeans_model <- kmeans(scaled_data, centers = best_k, nstart = 1)
      
      # 計算 Silhouette 指標
      silhouette_values <- silhouette(kmeans_model$cluster, dist(scaled_data))
      silhouette_avg <- mean(silhouette_values[, 3])
      
      list(
        model = kmeans_model,
        silhouette_avg = silhouette_avg,
        withinss = kmeans_model$tot.withinss
      )
    })
    
    # 顯示每次的距離測度和 Silhouette 指標
    cat("\nK-Means Results:\n")
    for (i in seq_along(kmeans_results)) {
      cat(sprintf("Run %d:\n", i))
      cat(sprintf("  - Total Within Sum of Squares: %.2f\n", kmeans_results[[i]]$withinss))
      cat(sprintf("  - Silhouette Score: %.4f\n\n", kmeans_results[[i]]$silhouette_avg))
    }
    
    # 選擇最佳結果
    best_run <- which.max(sapply(kmeans_results, function(x) x$silhouette_avg))
    cat(sprintf("Best run is Run %d with Silhouette Score %.4f.\n", best_run, kmeans_results[[best_run]]$silhouette_avg))
    
    # 使用最佳模型進行分群
    best_kmeans_model <- kmeans_results[[best_run]]$model
    cluster_data$cluster <- as.factor(best_kmeans_model$cluster)
    
  } else if (method == "pam") {
    clustering <- pam(scaled_data, k = best_k)
    cluster_data$cluster <- as.factor(clustering$clustering)
  } else {
    stop("Unsupported clustering method. Please choose 'kmeans' or 'pam'.")
  }
  
  # 總體散點圖
  total_plot <- ggplot(cluster_data, aes(x = price_diff_scaled, y = volume_diff_scaled, color = cluster)) +
    geom_point(alpha = 0.7) +
    geom_text(aes(label = get(district_col)), hjust = -0.1, size = 3) +
    labs(
      title = "分群住房市場變化散點圖",
      x = "單位平方公尺金額的中位數" ,
      y = "日均交易量",
      color = "Cluster"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
  
  # 按城市的散點圖
  city_plot <- ggplot(cluster_data, aes(x = price_diff_scaled, y = volume_diff_scaled, color = cluster)) +
    geom_point(alpha = 0.7) +
    geom_text(aes(label = get(district_col)), hjust = -0.1, size = 3) +
    labs(
      title = "分群住房市場變化散點圖",
      x = "單位平方公尺金額的中位數" ,
      y = "日均交易量",
      color = "Cluster"
    ) +
    theme_minimal() +
    theme(legend.position = "right") +
    facet_wrap(vars(get(city_col)), scales = "free")
  
  # 顯示圖表
  print(total_plot)
  print(city_plot)
  
  # 回傳圖表
  invisible(list(
    total_plot = total_plot,
    city_plot = city_plot
  ))
}


################
#################K=4

KMcluster_and_visualize(
  data = r_27Mr_2625,
  price_col = "price_diff",
  volume_col = "volume_diff",
  city_col = "city",
  district_col = "district",
  max_k = 10,  # you can change the maximum k value to explore more clusters
  method = "kmeans"
)
###########################################################
#######################ENTROPY等高線#######################
###########################################################
library(ggplot2)
library(cluster)

fuzzy_entropy_analysis <- function(data, price_col, volume_col, max_k = 10, m_values = seq(1.1, 2.5, by = 0.1), scale_data = TRUE) {
  
  # 標準化數據
  if (scale_data == TRUE) {
    data_scaled <- data %>%
      mutate(
        price_scaled = scale(get(price_col)),
        volume_scaled = scale(get(volume_col))
      )
  } else {
    data_scaled <- data %>%
      mutate(
        price_scaled = get(price_col),
        volume_scaled = get(volume_col)
      )
  }
  
  # 選擇標準化後的數據
  cluster_data <- data_scaled %>%
    select(price_scaled, volume_scaled)
  
  # 準備計算不同max_k和m的模糊熵
  entropy_matrix <- matrix(NA, nrow = length(m_values), ncol = max_k - 1)
  
  # 遍歷不同的m和k計算模糊熵
  for (m in m_values) {
    for (k in 2:max_k) {
      # 使用模糊 C-means 進行聚類
      fcm_result <- cmeans(cluster_data, centers = k, m = m)
      
      # 計算模糊熵
      fuzzy_entropy <- -sum(fcm_result$membership * log(fcm_result$membership))
      
      # 儲存熵值
      entropy_matrix[which(m_values == m), k - 1] <- fuzzy_entropy
    }
  }
  
  # 建立熵矩陣的數據框
  entropy_df <- expand.grid(m = m_values, k = 2:max_k)
  entropy_df$entropy <- as.vector(entropy_matrix)
  
  # 畫出模糊熵的等高線圖，設定固定等高線線距
  p <- ggplot(entropy_df, aes(x = k, y = m, z = entropy)) +
    geom_tile(aes(fill = entropy)) +
    # 設定固定的等高線線距
    geom_contour(aes(z = entropy), color = "black", breaks = seq(min(entropy_df$entropy), max(entropy_df$entropy), by = 25)) + 
    scale_fill_gradient(low = "white", high = "blue") +
    labs(
      title = "模糊熵高線圖",
      x = "群數",
      y = "模糊指數 (m)",
      fill = "模糊熵"
    ) +
    theme_minimal()
  
  print(p)  # 確保圖形正確顯示
  
}
#########################
#########################
# 使用範例
result <- fuzzy_entropy_analysis(
  data = r_27Mr_2625,  # 替換為你的資料框
  price_col = "price_diff",
  volume_col = "volume_diff",
  max_k = 5,  # 最大聚類數
  m_values = seq(1.1, 2.5, by = 0.1),  # 測試的模糊指數範圍
  scale_data = FALSE  # 是否標準化
)
# 使用範例
result <- fuzzy_entropy_analysis(
  data = r_27Mr_2625,  # 替換為你的資料框
  price_col = "price_diff",
  volume_col = "volume_diff",
  max_k = 5,  # 最大聚類數
  m_values = seq(1.1, 2.5, by = 0.1),  # 測試的模糊指數範圍
  scale_data = TRUE  # 是否標準化
)

##################################################
######################FC##########################
##################################################
# 載入所需的包
library(e1071)
library(ggplot2)
library(dplyr)

# 定義函數來進行模糊聚類
fuzzy_cluster_analysis <- function(data, price_col, volume_col, city_col, district_col, max_k = 10, m = 1.5, scale_data) {
  # 確保輸入的列存在
  if (!all(c(price_col, volume_col, city_col, district_col) %in% colnames(data))) {
    stop("One or more specified columns do not exist in the dataset.")
  }
  
  # 標準化數據
  if (scale_data == FALSE) {
    data_scaled <- data %>%
      mutate(
        price_scaled = get(price_col),
        volume_scaled = get(volume_col)
      )
  } else if (scale_data == TRUE) {
    data_scaled <- data %>%
      mutate(
        price_scaled = scale(get(price_col)),
        volume_scaled = scale(get(volume_col))
      )
  } 
  
  

  # 選擇標準化後的數據
  cluster_data <- data_scaled %>%
    select(price_scaled, volume_scaled)
  
  # 使用模糊 C-means 進行聚類
  fcm_result <- cmeans(cluster_data, centers = max_k, m = m)
  
  # 目標函數值
  objective_function <- fcm_result$objective
  
  # 計算模糊熵
  fuzzy_entropy <- -sum(fcm_result$membership * log(fcm_result$membership))
  
  # 結果檢視
  print(paste("Objective Function: ", objective_function))
  print(paste("Fuzzy Entropy: ", fuzzy_entropy))
  
  # 把聚類結果加入原始數據
  cluster_data$cluster <- as.factor(fcm_result$cluster)
  cluster_data$membership <- fcm_result$membership
  cluster_data$city <- data[[city_col]]
  cluster_data$district <- data[[district_col]]
  
  # 繪製散點圖
  scatter_plot <- ggplot(cluster_data, aes(x = price_scaled, y = volume_scaled, color = cluster)) +
    geom_point(alpha = 0.7) +
    geom_text(aes(label = get(district_col)), hjust = -0.1, size = 3) +
    labs(
      title = "模糊分群住房市場變化散點圖",
      x = "單位平方公尺金額的中位數" ,
      y = "日均交易量",
      color = "Cluster"
    ) +
    theme_minimal()
  
  print(scatter_plot)
  # 按城市的散點圖
  city_plot <- ggplot(cluster_data, aes(x = price_scaled, y = volume_scaled, color = cluster)) +
    geom_point(alpha = 0.7) +
    geom_text(aes(label = get(district_col)), hjust = -0.1, size = 3) +
    labs(
      title = "模糊分群住房市場變化散點圖",
      x = "單位平方公尺金額的中位數" ,
      y = "日均交易量",
      color = "Cluster"
    ) +
    theme_minimal() +
    theme(legend.position = "right") +
    facet_wrap(vars(get(city_col)), scales = "free")
  
  # 顯示圖表
  print(city_plot)
  
  # 計算每個城市的平均最大隸屬度
  city_max_membership <- cluster_data %>%
    group_by(city) %>%
    summarise(
      avg_max_membership = mean(apply(membership, 1, max))
    ) %>%
    arrange(desc(avg_max_membership))  # 排序為降序
  
  # 繪製城市最大隸屬度的平均排名直方圖（降序）
  membership_histogram <- ggplot(city_max_membership, aes(x = reorder(city, -avg_max_membership), y = avg_max_membership)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(
      title = "城市平均最大隸屬度排名",
      x = "",
      y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))  # 可以調整文字角度
  
  print(membership_histogram)
  
  
  return(fcm_result)
}
#########
#########
#########

# 假設 "data" 是包含 "price_diff", "volume_diff", "city", "district" 的資料框
# 進行模糊聚類分析
result <- fuzzy_cluster_analysis(
  data = r_27Mr_2625,  # 替換為你的資料框
  price_col = "price_diff",
  volume_col = "volume_diff",
  city_col = "city",
  district_col = "district",
  max_k = 5,  # 最多最多的群集數
  m = 2,    # 模糊程度
  scale_data = FALSE  # 是否進行標準化
)
# 進行模糊聚類分析
result <- fuzzy_cluster_analysis(
  data = r_27Mr_2625,  # 替換為你的資料框
  price_col = "price_diff",
  volume_col = "volume_diff",
  city_col = "city",
  district_col = "district",
  max_k = 5,  # 最多最多的群集數
  m = 2,    # 模糊程度
  scale_data = TRUE  # 是否進行標準化
)