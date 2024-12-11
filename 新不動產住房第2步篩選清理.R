

install.packages("ggplot2")
install.packages("reshape2")
setwd("C:\\Users\\User\\Desktop\\學\\大學\\資料探勘")
# 匯入 CSV 文件
merged_data <- read.csv("merged_data.csv")
time_event=read.csv("time_event - time_propeve.csv")
# 將 merged_data 中所有空字串或僅包含空白的值替換為 NA
merged_data <- lapply(merged_data, function(column) {
  if (is.character(column)) {
    # 偵測並將空字串或僅有空白的字元替換為 NA
    column[grepl("^\\s*$", column)] <- NA
  }
  return(column)
})

# 將結果轉回資料框
merged_data <- as.data.frame(merged_data)

###############################################
###########交易內容、用途、篩選################
###############################################
merged_data_RLC <- merged_data[!(merged_data[[2]] %in% c("土地", "車位")), ]
merged_data_GLC <- merged_data[merged_data[[2]] %in% c("土地", "車位"), ]
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
merged_data_GLCt13 <- merged_data_RLC[merged_data_RLC[[13]] %in% garb, ]

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
# 建立函數以篩選包含指定關鍵字的樣本
filter_by_keyword <- function(data, column_name, keyword) {
  # 確保變數名稱存在於資料框中
  if (!column_name %in% colnames(data)) {
    stop("指定的變數名稱不存在於資料框中。")
  }
  
  # 使用 grepl 找出包含關鍵字的行
  filtered_data <- data[grepl(keyword, data[[column_name]], fixed = TRUE), ]
  
  return(filtered_data)
}


#############都市土地使用分區、非都市土地使用分區、非都市土地使用編定


########################################
##########前處理-合併車位轉移變數#######
########################################
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
  "車位移轉平方公尺"
)
get_unique_values(merged_data_RLCt13_C2,34)
########################################
###########篩選:建物型態################
########################################
get_unique_values(merged_data_RLCt13_C2,12)
# 排除的建物型態
exclude_types <- c("店面(店鋪)", "辦公商業大樓", "其他", "倉庫", "工廠", "廠辦")

# 生成新資料集，排除指定的建物型態
merged_data_RLCt13_C2_t12 <- merged_data_RLCt13_C2[!merged_data_RLCt13_C2$建物型態 %in% exclude_types, ]
colnames(merged_data_RLCt13_C2_t12)
########################################
###########移除:編號、移轉編號##########
########################################
merged_data_RLCt13_C2_t12_RC <- merged_data_RLCt13_C2_t12[, !(colnames(merged_data_RLCt13_C2_t12) %in% c("編號", "移轉編號"))]


#######################
##備註查看#############
#######################
get_unique_values(merged_data_RLCt13_C2_t12_RC ,26)

a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "親")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "租")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "預")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "增")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "加")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "臨")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "違")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "工")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "農")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "店")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "贈與")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "繼承")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "裝潢")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "精裝修")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "毛胚屋")
get_unique_values(a ,26)
a= filter_by_keyword(merged_data_RLCt13_C2_t12_RC, "備註", "店")
get_unique_values(a ,26)

a= NULL

#################################################
##################依目的判斷要篩選的備註篩選#####
#################################################
# 定義篩選資料的函數
filter_keyword <- function(data, column, keyword) {
  # 篩選出 column 中包含 keyword 的樣本
  data[grepl(keyword, data[[column]], ignore.case = TRUE), ]
}

# 將需要排除的樣本定義為關鍵字列表
keywords <- c("親", "租", "預", "增", "加", "臨", "違", "工", 
              "農", "店", "贈與", "繼承","政府機關標","父子","父女","母女","母子")

# 複製原始資料集
merged_data_RLCt13_C2_t12_RC_c26 <- merged_data_RLCt13_C2_t12_RC

# 逐步排除含有關鍵字的樣本
for (keyword in keywords) {
  filtered_data <- filter_keyword(merged_data_RLCt13_C2_t12_RC_c26, "備註", keyword)
  merged_data_RLCt13_C2_t12_RC_c26 <- merged_data_RLCt13_C2_t12_RC_c26[!(row.names(merged_data_RLCt13_C2_t12_RC_c26) %in% row.names(filtered_data)), ]
}
#####################################
################資料檔名變數處理#####
#####################################
# 使用tidyr的separate函數
library(tidyr)

# 將 source_file 拆解成 year_quarter 和 code 兩個欄位
merged_data_RLCt13_C2_t12_RC_c26 <- merged_data_RLCt13_C2_t12_RC_c26 %>%
  separate(source_file, into = c("year_quarter", "code", NA), sep = "_", extra = "drop", remove = FALSE)
# 移除 source_file 欄位
merged_data_RLCt13_C2_t12_RC_c26 <- merged_data_RLCt13_C2_t12_RC_c26[, !names(merged_data_RLCt13_C2_t12_RC_c26) %in% "source_file"]
#################################
############交易年月日合理篩選###
#################################
# 確保交易年月日是數值類型
merged_data_RLCt13_C2_t12_RC_c26$交易年月日 <- as.numeric(merged_data_RLCt13_C2_t12_RC_c26$交易年月日)

# 篩選資料：保留範圍內的觀察值
merged_data_RLCt13_C2_t12_RC_c26_RY <- merged_data_RLCt13_C2_t12_RC_c26 %>%
  filter(交易年月日 >= 601009 & 交易年月日 <= 1131128)

# 計算比例
original_count <- nrow(merged_data_RLCt13_C2_t12_RC_c26)  # 原始資料數量
filtered_count <- nrow(merged_data_RLCt13_C2_t12_RC_c26_RY) # 篩選後資料數量
removed_count <- original_count - filtered_count          # 被篩掉的資料數量

# 比例計算
filtered_ratio <- filtered_count / original_count
removed_ratio <- removed_count / original_count
original_count
removed_count
filtered_count
# 輸出結果
cat("篩選後保留的比例：",filtered_ratio * 100, "%\n")
cat("被篩掉的比例：", removed_ratio * 100, "%\n")


colnames(merged_data_RLCt13_C2_t12_RC_c26_RY)


############################################
########交易紀錄匹配交易時間前一個政策######
############################################
# 將 Date 欄位轉換為民國年月日格式
convert_to_republic_date <- function(date_column) {
  # 移除多餘的引號
  date_column <- gsub('"', '', date_column)
  
  # 檢查是否為日期格式
  if (!all(grepl("^\\d{4}-\\d{2}-\\d{2}$", date_column))) {
    stop("Date 欄位格式不正確，應為 YYYY-MM-DD。")
  }
  
  # 轉換日期
  republic_date <- sapply(date_column, function(x) {
    date <- as.Date(x) # 轉換為 Date 類型
    year <- as.numeric(format(date, "%Y")) - 1911 # 西元轉民國年
    month <- format(date, "%m")
    day <- format(date, "%d")
    paste0(year, month, day) # 組合民國年月日
  })
  
  return(as.numeric(republic_date))
}

# 將 time_event 的 Date 欄位轉換為民國日期格式
time_event$RepublicDate <- convert_to_republic_date(time_event$Date)

# 處理日期重疊的情況，將其他欄位以 "---" 連接
library(dplyr)
time_event <- time_event %>%
  group_by(RepublicDate) %>%
  summarise(across(everything(), ~ ifelse(is.character(.), paste(unique(.), collapse = "---"), unique(.)))) %>%
  ungroup()

library(dplyr)

# 新增「發展前」虛擬政策資料
time_event <- time_event %>%
  bind_rows(data.frame(
    RepublicDate = 1000,  # 虛擬民國日期
    Event = "發展前",     # 政策名稱
    順序 = 0,            # 順序號
    概要 = NA            # 其他補充說明
  ))
library(data.table)

# 將 merged_data 和 time_event 轉換為 data.table 格式
merged_data_dt <- as.data.table(merged_data_RLCt13_C2_t12_RC_c26_RY)
time_event_dt <- as.data.table(time_event)

# 確保 `RepublicDate` 和 `交易年月日` 都是數字類型，並排序
time_event_dt[, RepublicDate := as.numeric(RepublicDate)]
merged_data_dt[, 交易年月日 := as.numeric(交易年月日)]

# 在交易資料中新增一個區間 [交易年月日, 交易年月日]
merged_data_dt[, `:=`(start = 交易年月日, end = 交易年月日)]

# 在政策資料中新增一個區間 [RepublicDate, 無窮大]
time_event_dt[, `:=`(start = RepublicDate, end = Inf)]

# 使用 foverlaps 找到每筆交易之前的最近政策
setkey(time_event_dt, start, end)
setkey(merged_data_dt, start, end)

# 使用 foverlaps 執行匹配
result_dt <- foverlaps(
  merged_data_dt, 
  time_event_dt, 
  by.x = c("start", "end"), 
  by.y = c("start", "end"),
  nomatch = 0
)

# 根據 `start` 進行分組，並選擇最近的政策
result_dt <- result_dt[,
                       .SD[.N], by = .(i.start)  # 每筆交易找出最近的過去政策
]

# 合併政策資訊到原始交易資料中
result_dt <- merge(
  merged_data_dt, 
  result_dt, 
  by.x = "start", 
  by.y = "i.start", 
  all.x = TRUE
)

# 移除不必要的欄位，並解決 .x 和 .y 的問題
result_dt <- result_dt[, !grepl("\\.y$", colnames(result_dt)), with = FALSE]  # 移除 .y 欄位



# 重新命名欄位，移除 `.x`
setnames(result_dt, old = colnames(result_dt), new = gsub("\\.x$", "", colnames(result_dt)))

# 檢查欄位名稱
colnames(result_dt)


# 將結果轉回 data.frame（如果需要）
merged_data_RLCt13_C2_t12_RC_c26_RY_PROP <- as.data.frame(result_dt)
colnames(time_event )
colnames(merged_data_RLCt13_C2_t12_RC_c26_RY_PROP )


# 移除不需要的欄位
merged_data_RLCt13_C2_t12_RC_c26_RY_PROP <- merged_data_RLCt13_C2_t12_RC_c26_RY_PROP[, 
                                                                                     setdiff(names(merged_data_RLCt13_C2_t12_RC_c26_RY_PROP), c("稅", "貸款", "其他", "概要", "i.end"))]

# 合併 `time_event` 中的 `Event` 和 `順序` 兩個欄位，基於共同變數 "Date"
merged_data_RLCt13_C2_t12_RC_c26_RY_PROP <- merge(
  merged_data_RLCt13_C2_t12_RC_c26_RY_PROP, 
  time_event[, c("Date", "Event", "順序")], 
  by = "Date", 
  all.x = TRUE  # 確保保留所有交易資料中的記錄
)


########################################################
#############基於合併前資料集名稱新增縣市名欄位#########
########################################################
time_event_alfa_city <- read.csv("time_event - 字母與縣市.csv")
# 1. 移除 file 欄位
time_event_alfa_city <- time_event_alfa_city[, -1]

# 2. 檢查縣市名與 code 是否一對一
relation_check <- time_event_alfa_city %>%
  group_by(縣市名, code) %>%
  summarise(count = n(), .groups = 'drop')

# 檢查是否一對一
if (nrow(relation_check) == nrow(unique(relation_check[, c("縣市名", "code")]))) {
  # 3. 保留不重複的觀察值
  time_event_alfa_city_clean <- distinct(time_event_alfa_city)
} else {
  stop("縣市名與 code 並非一對一關係，請檢查資料！")
}

############################
############################
############################
library(dplyr)

# 1. 將 `code` 轉換為對應的縣市名
merged_data_RLCt13_C2_t12_RC_c26_RY_PROP <- merged_data_RLCt13_C2_t12_RC_c26_RY_PROP %>%
  left_join(time_event_alfa_city_clean, by = "code") %>%  # 匹配 code
  select(縣市名, everything())  # 將縣市名移動到第一欄位

# 按照 `交易年月日` 進行升序排列
merged_data_RLCt13_C2_t12_RC_c26_RY_PROP <- merged_data_RLCt13_C2_t12_RC_c26_RY_PROP %>%
  arrange(交易年月日)
# 將 Date 欄位名稱改為 last_prop_date
colnames(merged_data_RLCt13_C2_t12_RC_c26_RY_PROP)[colnames(merged_data_RLCt13_C2_t12_RC_c26_RY_PROP) == "Date"] <- "last_prop_date"

# 取得欄位名稱
cols <- colnames(merged_data_RLCt13_C2_t12_RC_c26_RY_PROP)

# 找出 'last_prop_date' 的位置
last_prop_date_index <- which(cols == "last_prop_date")

# 移除 'last_prop_date' 欄位
cols <- cols[cols != "last_prop_date"]

# 將 'last_prop_date' 移到倒數第三個位置
new_cols <- c(
  cols[1:(length(cols) - 2)],  # 保留前面欄位
  "last_prop_date",             # 插入 'last_prop_date' 在倒數第三位置
  cols[(length(cols) - 1):length(cols)]  # 保留剩下的欄位
)

# 根據新的欄位順序重排資料框
merged_data_RLCt13_C2_t12_RC_c26_RY_PROP <- merged_data_RLCt13_C2_t12_RC_c26_RY_PROP[, new_cols]

write.csv(merged_data_RLCt13_C2_t12_RC_c26_RY_PROP, "merged_data_RLCt13_C2_t12_RC_c26_RY_PROP.csv", row.names = FALSE)


























######################################################################################################
#########################已下為過時的探索式資料分析----資料集不是最新的###############################
######################################################################################################
############################
###總價元極端值檢查#########
############################
# 確保欄位是數值類型
merged_data_RLCt13_C2_t12_RC_c26_RY$`總價元` <- as.numeric(merged_data_RLCt13_C2_t12_RC_c26_RY$`總價元`)

# 計算平均值與標準差
mean_total_price <- mean(merged_data_RLCt13_C2_t12_RC_c26_RY$`總價元`, na.rm = TRUE)
sd_total_price <- sd(merged_data_RLCt13_C2_t12_RC_c26_RY$`總價元`, na.rm = TRUE)

# 設定篩選範圍
lower_bound <- mean_total_price - 3 * sd_total_price
upper_bound <- mean_total_price + 3 * sd_total_price

# 篩選符合範圍的觀察值
within_range <- merged_data_RLCt13_C2_t12_RC_c26_RY$`總價元` >= lower_bound & merged_data_RLCt13_C2_t12_RC_c26_RY$`總價元` <= upper_bound
count_within_range <- sum(within_range, na.rm = TRUE)

# 篩選超出範圍的觀察值
out_of_range <- !within_range
count_out_of_range <- sum(out_of_range, na.rm = TRUE)

# 顯示結果
cat("範圍內的觀察值數量：", count_within_range, "\n")
cat("範圍外的觀察值數量：", count_out_of_range, "\n")

###############################################################
###########################確認數值分布的######################
###############################################################
# 設定圖檔輸出資料夾
output_folder <- "distribution_plots"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# 資料分布繪圖函數
plot_distribution <- function(data, variable, output_folder) {
  # 確認變數存在於資料框
  if (!variable %in% colnames(data)) {
    warning(paste("變數", variable, "不存在於資料框中，略過。"))
    return(NULL)
  }
  
  # 檢查變數類型
  var_data <- data[[variable]]
  if (is.numeric(var_data)) {
    # 數值型變數分布
    plot <- ggplot(data, aes(x = var_data)) +
      geom_histogram(binwidth = diff(range(var_data, na.rm = TRUE)) / 30, fill = "skyblue", color = "black") +
      labs(
        title = paste("數值分布圖 -", variable),
        x = variable,
        y = "頻率"
      ) +
      theme_minimal()
  } else {
    # 類別型變數分布
    plot <- ggplot(data, aes(x = var_data)) +
      geom_bar(fill = "skyblue", color = "black") +
      labs(
        title = paste("類別分布圖 -", variable),
        x = variable,
        y = "頻率"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # 儲存圖檔
  plot_path <- file.path(output_folder, paste0(variable, "_distribution.png"))
  ggsave(plot_path, plot, width = 10, height = 6, dpi = 300)
  cat("已保存圖檔：", plot_path, "\n")
}

# 變數列表
variables <- c("交易年月日", "建築完成年月", "總價元")

# 批量繪製並儲存分布圖
for (var in variables) {
  plot_distribution(merged_data_RLCt13_C2_t12_RC_c26_RY, var, output_folder)
}

cat("所有分布圖已保存於資料夾：", output_folder, "\n")


#################################################################################################
###################################找小類別的####################################################
#################################################################################################

plot_relative_histogram <- function(data, variable, threshold = 0.01) {
  # 檢查變數是否存在
  if (!variable %in% colnames(data)) {
    stop("指定的變數在資料框中不存在。")
  }
  
  # 提取變數並計算次數與相對次數
  var_data <- data[[variable]]
  var_table <- as.data.frame(table(var_data))
  colnames(var_table) <- c("Category", "Count")
  var_table$RelativeFrequency <- var_table$Count / sum(var_table$Count)
  
  # 合併相對次數小於 threshold 的類別為 "其他"
  var_table$Category <- as.character(var_table$Category)  # 確保類別是字符型
  var_table$Category[var_table$RelativeFrequency < threshold] <- "其他"
  
  # 重新計算合併後的次數和相對次數
  final_table <- aggregate(cbind(Count, RelativeFrequency) ~ Category, data = var_table, sum)
  
  # 顯示各類別次數表格
  print(final_table)
  
  # 繪製直方圖
  library(ggplot2)
  ggplot(final_table, aes(x = reorder(Category, -RelativeFrequency), y = RelativeFrequency)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = Count), vjust = -0.5, size = 3.5) +  # 在柱狀圖上方顯示次數
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = paste("相對次數直方圖 -", variable),
      x = "類別",
      y = "相對次數"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 範例使用
colnames(merged_data_RLCt13_C2_t12_RC_c26_RY)
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY,  "交易標的")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "都市土地使用分區")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "非都市土地使用分區")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "非都市土地使用編定")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "交易筆棟數")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY,  "移轉層次")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "總樓層數")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "建物型態")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "主要用途")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "主要建材")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY,  "建物現況格局.房" )
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "建物現況格局.廳" )
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "建物現況格局.衛" )
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "建物現況格局.隔間")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "有無管理組織" )
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY,   "車位類別"  )
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY,  "車位總價元" )
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY,  "備註" )
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "附屬建物面積")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "陽台面積")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, "電梯")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY,  "車位移轉平方公尺")
plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY,  "順序")
# 設定圖檔輸出資料夾
output_folder <- "relative_histograms"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# 欄位列表
variables <- c(
  "交易標的", "都市土地使用分區", "非都市土地使用分區", "非都市土地使用編定", "交易筆棟數",
  "移轉層次", "總樓層數", "建物型態", "主要用途", "主要建材", "建物現況格局.房", 
  "建物現況格局.廳", "建物現況格局.衛", "建物現況格局.隔間", "有無管理組織",
  "車位類別", "車位總價元", "備註", "附屬建物面積", "陽台面積", "電梯", "車位移轉平方公尺"
)

# 批量生成圖檔和表格
for (var in variables) {
  try({
    # 圖檔路徑
    plot_path <- file.path(output_folder, paste0(var, "_histogram.png"))
    table_path <- file.path(output_folder, paste0(var, "_table.csv"))
    
    # 繪製並儲存直方圖
    png(filename = plot_path, width = 1000, height = 600)
    plot_relative_histogram(merged_data_RLCt13_C2_t12_RC_c26_RY, var)
    dev.off()
    
    # 儲存分類表為 CSV
    var_data <- merged_data_RLCt13_C2_t12_RC_c26_RY[[var]]
    var_table <- as.data.frame(table(var_data))
    colnames(var_table) <- c("Category", "Count")
    var_table$RelativeFrequency <- var_table$Count / sum(var_table$Count)
    write.csv(var_table, table_path, row.names = FALSE, fileEncoding = "UTF-8")
  }, silent = TRUE)
}

cat("所有圖表已生成並儲存在資料夾：", output_folder, "\n")




###################################################
####################篩選出數值型變數###############
###################################################
numeric_columns <- sapply(merged_data_RLCt13_C2_t12_RC_c26_RY, is.numeric)

# 計算每個數值型變數的最大和最小值
numeric_summary <- merged_data_RLCt13_C2_t12_RC_c26_RY[, numeric_columns]

# 顯示每個數值變數的最大值和最小值
for (col in names(numeric_summary)) {
  cat("變數:", col, "\n")
  cat("最小值:", min(numeric_summary[[col]], na.rm = TRUE), "\n")
  cat("最大值:", max(numeric_summary[[col]], na.rm = TRUE), "\n\n")
}
summary(numeric_summary)

########################################################################################################
###################################分割都市與非都市資料集###############################################
########################################################################################################
# 假設 merged_data_RLCt13_C2 是你的資料框

# 條件 1: 第五個變數不為遺失值的資料
data_cond1 <- merged_data_RLCt13_C2[!is.na(merged_data_RLCt13_C2[[5]]), ]

# 條件 2: 第六和第七個變數不為遺失值的資料
data_cond2 <- merged_data_RLCt13_C2[!is.na(merged_data_RLCt13_C2[[6]]) & !is.na(merged_data_RLCt13_C2[[7]]), ]

# 條件 3: 第五、六、七三個變數都為遺失值的資料
data_cond3 <- merged_data_RLCt13_C2[is.na(merged_data_RLCt13_C2[[5]]) & is.na(merged_data_RLCt13_C2[[6]]) & is.na(merged_data_RLCt13_C2[[7]]), ]

# 條件 4: 其他情形
data_cond4 <- merged_data_RLCt13_C2[!(1:nrow(merged_data_RLCt13_C2) %in% c(
  which(!is.na(merged_data_RLCt13_C2[[5]])),
  which(!is.na(merged_data_RLCt13_C2[[6]]) & !is.na(merged_data_RLCt13_C2[[7]])),
  which(is.na(merged_data_RLCt13_C2[[5]]) & is.na(merged_data_RLCt13_C2[[6]]) & is.na(merged_data_RLCt13_C2[[7]]))
)), ]
write.csv(data_cond1, "都市.csv", row.names = FALSE)
write.csv(data_cond2, "非都市.csv", row.names = FALSE)
write.csv(data_cond3, "未知.csv", row.names = FALSE)
write.csv(data_cond4, "非都市但有未知.csv", row.names = FALSE)





########################################################################################################
################################非都市列連表矩陣###########################################################
########################################################################################################
library(ggplot2)
library(reshape2)

# 建立列聯表 (第六個變數與第七個變數)
contingency_table <- table(merged_data_RLCt13_C2[[6]], merged_data_RLCt13_C2[[7]])

# 將列聯表轉換為長格式資料框
contingency_df <- as.data.frame(as.table(contingency_table))
colnames(contingency_df) <- c("Var6", "Var5", "Frequency")

# 繪製矩陣圖，標註頻率
ggplot(contingency_df, aes(x = Var5, y = Var6, fill = Frequency)) +
  geom_tile(color = "white") +  # 繪製矩形
  scale_fill_gradient(low = "lightblue", high = "blue", na.value = "red") +  # 設定漸層配色
  geom_text(aes(
    label = Frequency,
    color = ifelse(Frequency == 0, "red", "black")  # 頻率為 0 用紅色，非 0 用黑色
  ), size = 3) +
  scale_color_identity() +  # 保留顏色自動識別
  labs(
    title = "非都市土地使用編定*非都市土地使用分區列連表矩陣",
    x = "Variable 5",
    y = "Variable 6"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # 調整 X 軸標籤
  )

# 列出頻率不為 0 的組合
non_zero_combinations <- contingency_df[contingency_df$Frequency > 0, ]
print(non_zero_combinations)
########################################################################################################
###################################基於目的篩選後的#####################################################
########################################################################################################





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
##########################
##########################
##########################
# 确保第 33 个变量存在于数据框中
if (ncol(merged_data_RLCt13_C2) >= 33) {
  
  # 按第 33 个变量分组，并提取每组的第一行
  unique_values_with_first <- merged_data_RLCt13_C2 %>%
    group_by(.[[33]]) %>%  # 使用第 33 个变量进行分组
    slice(1) %>%           # 每组选择第一行
    ungroup()              # 解除分组
  
  # 输出结果
  print(unique_values_with_first)
  
} else {
  cat("The dataset does not have 33 variables.\n")
}
write.csv(unique_values_with_first, "unique_values_with_first.csv", row.names = FALSE)


########################################################################################################
########################################################################################################
########################################################################################################

# 將資料框匯出為 CSV 檔案
write.csv(merged_data_RLCt13_C2, "merged_data_RLCt13_C2.csv", row.names = FALSE)
########################################################################################################
#############################取樣merged_data_RLCt13_C2##################################################
########################################################################################################
# 假設 merged_data_RLCt13_C2 是你的資料框
set.seed(123)  # 設定隨機種子以確保結果可重現

# 計算取樣數量
sample_size <- ceiling(0.01 * nrow(merged_data_RLCt13_C2))

# 隨機取樣
test_merged_data_RLCt13_C2 <- merged_data_RLCt13_C2[sample(1:nrow(merged_data_RLCt13_C2), sample_size), ]

write.csv(test_merged_data_RLCt13_C2 , "test_merged_data_RLCt13_C2.csv ", row.names = FALSE)


########################################################################################################
#######################################地址修正#########################################################
########################################################################################################
install.packages(c("tidyverse","dplyr"))
library(tidyverse)
library(dplyr)
# 創建完整地址欄位
data <- data %>%
  mutate(Full_Address = paste(data[[1]], data[[3]], sep = ", "))

# 篩選需要地理編碼的地址
unique_addresses <- data %>%
  distinct(Full_Address) %>%
  filter(!is.na(Full_Address))  # 避免空地址

# 定義檢查與補救函數
check_address_complete <- function(address) {
  grepl("縣|市", address)
}

add_city_county <- function(address) {
  district <- sub(",.*", "", address)
  mapping <- list(
    "平鎮區" = "桃園市",
    "豐原區" = "台中市",
    "東區" = "新竹市",
    "佳里區" = "台南市",
    "永康區" = "台南市",
    "大寮區" = "高雄市"
  )
  city_county <- mapping[[district]]
  if (!is.null(city_county)) {
    paste(city_county, address, sep = ", ")
  } else {
    address  # 返回原地址作為預設
  }
}

# 檢查與補救地址
unique_addresses <- unique_addresses %>%
  mutate(
    Is_Complete = sapply(Full_Address, check_address_complete),
    Fixed_Address = ifelse(Is_Complete, Full_Address, 
                           mapply(add_city_county, Full_Address))
  )

# 分離成功與失敗的地址
valid_addresses <- unique_addresses %>%
  filter(!is.na(Fixed_Address))
head(valid_addresses,n=30)
# 保留需要的變數
valid_addresses <- valid_addresses %>%
  select(Is_Complete, Fixed_Address)

# 自訂修正函數
process_fixed_address <- function(is_complete, address) {
  if (is_complete) {
    # 當 Is_Complete 為 TRUE，刪除逗號與其之前的字元
    address <- sub(".*,\\s*", "", address)
  } else {
    # 當 Is_Complete 為 FALSE，移除逗號並檢查重複字元組
    address <- gsub(",", "", address) # 移除逗號
    # 移除重複的 "XX區XX區" 模式，保留後一組
    address <- sub("(\\S+區)\\1", "\\1", address)
  }
  return(address)
}

# 應用修正函數到 Fixed_Address
valid_addresses <- valid_addresses %>%
  mutate(Fixed_Address = mapply(process_fixed_address, Is_Complete, Fixed_Address))

# 查看結果
head(valid_addresses)
write.csv(valid_addresses, "valid_addresses.csv", row.names = FALSE)

#################################################################################################
#############################取樣valid_addresses#################################################
#################################################################################################
# 假設 valid_addresses 是你的資料框
set.seed(123)  # 設定隨機種子以確保結果可重現

# 計算取樣數量
sample_size <- ceiling(0.01 * nrow(valid_addresses))

# 隨機取樣
test_valid_addresses <-valid_addresses[sample(1:nrow(valid_addresses), sample_size), ]

write.csv(test_valid_addresses , "test_valid_addresses.csv ", row.names = FALSE)
