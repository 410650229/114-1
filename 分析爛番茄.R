setwd("C:\\Users\\AA\\Downloads")
# 安裝必要套件（如果尚未安裝）
if (!require("igraph")) install.packages("igraph")
if (!require("ggraph")) install.packages("ggraph")
if (!require("tidygraph")) install.packages("tidygraph")
if (!require("visNetwork")) install.packages("visNetwork")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggrepel")) install.packages("ggrepel")
# 載入套件
library(ggrepel)
library(igraph)
library(ggraph)
library(tidygraph)
library(visNetwork)
library(tidyverse)
movies_final =read.csv("movies_final.csv")

head(movies_final)
colnames(movies_final)

############################################
############################################
############################################
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
get_unique_values(movies_final,3)
get_unique_values(movies_final,5)
###################
## 載入必要的套件##
###################
library(dplyr)
#########################################
###############movies_clean##############
#########################################
# 檢查資料中是否有非數字（字串或其他類型），並重新命名變數
movies_clean <- movies_final %>%
  mutate(
    num_tomatometer = as.numeric(tomatometer),
    num_audience_score = as.numeric(audience_score),
    num_box_office = as.numeric(box_office),
    num_criticsScore_ratingCount = as.numeric(criticsScore_ratingCount) # 確保 criticsScore_ratingCount 是數字
  )

colnames(movies_clean)
################################
################################
################################
library(igraph)
library(ggraph)
library(dplyr)

# 假設 movies_clean 是你的資料框，包含 'Director.Name' 和 'title' 兩個欄位
# movies_clean <- read.csv("movies_clean.csv")  # 如有需要可讀取資料

# 創建一個包含導演和電影的邊資料框
edges <- movies_clean %>%
  select(Director.Name, title) %>%
  distinct()  # 去除重複的導演與電影組合

# 創建一個網絡圖對象
g <- graph_from_data_frame(edges, directed = FALSE)

# 計算節點的度數（即每個導演或電影的關聯數量）
V(g)$degree <- degree(g)


# 只顯示有多位導演的電影
edges_filtered <- edges %>%
  group_by(title) %>%
  filter(n() > 1)  # 只選擇有多個導演參與的電影

# 為所有節點初始化標籤為空字符串
V(g)$label <- ""

# 為電影節點添加標籤
V(g)$label[V(g)$name %in% edges_filtered$title] <- V(g)$name[V(g)$name %in% edges_filtered$title]

# 為導演節點添加標籤
V(g)$label[V(g)$name %in% edges$Director.Name] <- V(g)$name[V(g)$name %in% edges$Director.Name]
# 設定節點大小與顏色，根據度數（關聯強度）
V(g)$size <- pmin(V(g)$degree * 3, 100)  # 節點大小根據度數（設定最大大小）

# 設定節點顏色
V(g)$color <- ifelse(V(g)$name %in% edges$Director.Name, "lightcoral",  # 導演為淺紅
                     ifelse(V(g)$name %in% edges_filtered$title, "lightgreen",  # 電影為淺綠
                            "lightblue"))  # 其他為淺藍
# 可視化圖形
ggraph(g, layout = "fr") + 
  geom_edge_link(aes(alpha = 0.5), color = "gray") +  # 邊的顏色與透明度
  geom_node_point(aes(size = size, color = color)) +  # 節點的大小與顏色
  geom_node_text(aes(label = label), repel = TRUE, max.overlaps = 230, size = 3) +   # 節點的文字標籤
  theme_void() +
  ggtitle("Director-Movie Network")

################################
################################
################################
# 載入必要的包
library(igraph)
library(ggraph)
library(dplyr)

# 創建一個包含導演和電影的邊資料框
edges <- movies_clean %>%
  select(Director.Name, title) %>%
  distinct()  # 去除重複的導演與電影組合

# 計算每部電影參與的導演數量
movie_directors_count <- edges %>%
  group_by(title) %>%
  summarise(director_count = n_distinct(Director.Name)) %>%
  filter(director_count >= 2)  # 篩選出與至少兩位導演有關聯的電影

# 過濾原始邊資料框，只保留這些電影
edges_filtered <- edges %>%
  filter(title %in% movie_directors_count$title)

# 創建一個網絡圖對象
g <- graph_from_data_frame(edges_filtered, directed = FALSE)

# 計算節點的度數（即每個導演或電影的關聯數量）
V(g)$degree <- degree(g)

# 設定節點顏色與大小，根據度數（關聯強度）
V(g)$size <- V(g)$degree * 3  # 節點大小根據度數

# 設定顏色：導演節點為淺藍色，電影節點為淺紅色
V(g)$color <- ifelse(grepl(" ", V(g)$name), "lightblue", "lightcoral")  # 電影和導演的顏色區分

# 可視化圖形，並設置 max.overlaps = 230 以顯示更多標籤
ggraph(g, layout = "fr") + 
  geom_edge_link(aes(alpha = 0.5), color = "gray") +  # 邊的顏色與透明度
  geom_node_point(aes(size = size, color = color)) +  # 節點的大小與顏色
  geom_node_text(aes(label = name), repel = TRUE, max.overlaps = 230) +   # 節點的文字標籤
  theme_void() +
  ggtitle("Director-Movie Network (At least 2 Directors)")

#########################################
###############movies_clean##############
#########################################


# 分析每位人物的統計數據
Director.Name_analysis <- movies_clean %>%
  group_by(Director.Name) %>%
  summarise(
    # 計算參與作品數
    total_works = n(),
    
    # 計算各項平均值，忽略非數字或遺失值
    avg_tomatometer = mean(num_tomatometer, na.rm = TRUE),
    avg_audience_score = mean(num_audience_score, na.rm = TRUE),
    avg_box_office = mean(num_box_office, na.rm = TRUE),
    
    # 計算遺失值或非數字數量
    missing_tomatometer = sum(is.na(num_tomatometer)),
    missing_audience_score = sum(is.na(num_audience_score)),
    missing_box_office = sum(is.na(num_box_office)),
    
    # 計算 criticsScore_ratingCount 的總和
    total_criticsScore_ratingCount = sum(num_criticsScore_ratingCount, na.rm = TRUE),
    
    # 計算 total_criticsScore_ratingCount 除以 (total_works - missing_tomatometer)
    critics_per_work = total_criticsScore_ratingCount / (total_works - missing_tomatometer)
  ) %>%
  arrange(avg_tomatometer, avg_audience_score, avg_box_office)


write.csv(Director.Name_analysis, "Director.Name_analysis.csv", row.names = FALSE)

##########################
##########################
##########################
##########################
# 載入必要套件
library(dplyr)
library(tidyr)

# 分析每個人的媒體類型分布
media_type_analysis <- movies_final %>%
  group_by(Director.Name) %>%
  summarise(
    total_Movie = sum(overlay_mediaType == "Movie", na.rm = TRUE),
    total_TvSeries = sum(overlay_mediaType == "TvSeries", na.rm = TRUE),
    total_works = n() # 總作品數
  )

# 分析影劇分級的分布
grade_distribution <- movies_final %>%
  group_by(Director.Name, grade) %>%  # 按導演和分級分組
  tally() %>%                         # 計算每個組合的數量
  spread(key = grade, value = n, fill = 0)  # 將分級展平為寬表格，若某分級為 0，則填 0

# 計算從TAG變數的主題分布
topic_analysis <- movies_final %>%
  select(Director.Name, 23:55) %>%  # 選擇第29到第61個變數
  pivot_longer(cols = 2:ncol(.), names_to = "Tag", values_to = "Value") %>%  # 展開為長格式
  group_by(Director.Name, Tag) %>%  # 根據導演和Tag分組
  summarise(total = sum(Value, na.rm = TRUE), .groups = "drop") %>%  # 計算每個人的主題總計
  pivot_wider(names_from = Tag, values_from = total, values_fill = list(total = 0))  # 轉置Tag為變數

# 合併media_type_analysis與topic_analysis
combined_analysis <- media_type_analysis %>%
  left_join(topic_analysis, by = "Director.Name")

# 新增一個變數 represent，找出第五個到第三十七個變數最大值所在的變數名稱
combined_analysis <- combined_analysis %>%
  mutate(
    represent = apply(select(., 5:37), 1, function(x) colnames(select(., 5:37))[which.max(x)])
  )



