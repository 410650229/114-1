setwd("C:\\Users\\User\\Desktop\\大學\\四上\\PYTHON")

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
    tomatometer = as.numeric(tomatometer),
    audience_score = as.numeric(audience_score),
    box_office = as.numeric(box_office),
    criticsScore_ratingCount = as.numeric(criticsScore_ratingCount) # 確保 criticsScore_ratingCount 是數字
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
  geom_node_text(aes(label = label), repel = TRUE, max.overlaps = 230, size = 2) +   # 節點的文字標籤
  theme_void() +
  ggtitle("Director-Movie Network")+
  theme(legend.position = "none")

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

# 為節點新增類型標籤
edges_long <- edges_filtered %>%
  pivot_longer(cols = c(Director.Name, title), 
               names_to = "type", values_to = "name") %>%
  mutate(type = ifelse(type == "Director.Name", "Director", "Movie"))

# 創建一個網絡圖對象
g <- graph_from_data_frame(edges_filtered, directed = FALSE)

# 獲取節點名稱並合併類型標籤
V(g)$type <- edges_long %>%
  distinct(name, type) %>%
  arrange(match(name, V(g)$name)) %>%
  pull(type)

# 設定節點的度數（即每個導演或電影的關聯數量）
V(g)$degree <- degree(g)

# 設定節點顏色與大小，根據類型區分顏色，根據度數決定大小
V(g)$size <- V(g)$degree * 3  # 節點大小根據度數
V(g)$color <- ifelse(V(g)$type == "Director", "lightblue", "lightcoral")  # 導演為藍色，電影為紅色

# 可視化圖形，並設置 max.overlaps = 230 以顯示更多標籤
ggraph(g, layout = "fr") + 
  geom_edge_link(aes(alpha = 0.5), color = "gray") +  # 邊的顏色與透明度
  geom_node_point(aes(size = size, color = color)) +  # 節點的大小與顏色
  geom_node_text(aes(label = name), repel = TRUE, max.overlaps = 230) +   # 節點的文字標籤
  theme_void() +
  ggtitle("Director-Movie Network (At least 2 Directors)")+
  theme(legend.position = "none")

#########################################
###############movies_clean##############
#########################################

library(igraph)
library(ggraph)
library(dplyr)

# Assuming `movies_clean` is your data frame and `genre_columns` contains genre columns.
genre_columns <- names(movies_clean)[23:55]

# Filter top 3% movies based on `criticsScore_ratingCount`
threshold <- quantile(movies_clean$criticsScore_ratingCount, 0.97, na.rm = TRUE)
top_movies <- movies_clean %>%
  filter(criticsScore_ratingCount >= threshold) %>%
  distinct(title) %>%  # Ensure unique titles
  pull(title)

# 創建邊資料框，避免電影標題重複
edge_list <- do.call(rbind, lapply(unique(movies_clean$title), function(movie) {
  movie_data <- movies_clean[movies_clean$title == movie, ]
  genres <- genre_columns[which(movie_data[1, genre_columns] == TRUE)]  # 找出第一筆符合的主題類型
  if (length(genres) > 0) {
    return(data.frame(movie = movie, genre = genres))
  }
  NULL
}))

# 移除重複的邊
edge_list <- distinct(edge_list)


# Create igraph object
g <- graph_from_data_frame(d = edge_list, directed = FALSE)

# Set node attributes
V(g)$type <- ifelse(V(g)$name %in% movies_clean$title, "movie", "genre")
V(g)$color <- ifelse(V(g)$type == "movie", "lightblue", "lightgreen")
V(g)$shape <- ifelse(V(g)$type == "movie", "circle", "square")

V(g)$label <- ifelse(
  V(g)$type == "genre",  # 僅顯示主題類型
  V(g)$name,
  NA  # 其他節點不顯示標籤
)
# Visualize the graph
ggraph(g, layout = "fr") +
  geom_edge_link(aes(alpha = 0.5), color = "gray") +
  geom_node_point(aes(size = degree(g), color = color, shape = shape)) +
  geom_node_text(aes(label = label), repel = TRUE, max.overlaps = 700, size = 0) +
  scale_shape_manual(values = c(16, 15)) +
  theme_void() +
  ggtitle("Movie-Genre Network") +
  theme(legend.position = "none")

# Visualize the graph
ggraph(g, layout = "fr") +
  geom_edge_link(aes(alpha = 0.5), color = "gray") +
  geom_node_point(aes(size = degree(g), color = color, shape = shape)) +
  geom_node_text(aes(label = label), repel = TRUE, max.overlaps = 700, size = 3) +
  scale_shape_manual(values = c(16, 15)) +
  theme_void() +
  ggtitle("Movie-Genre Network") +
  theme(legend.position = "none")

########################################################
#######################十個獎項#############################
########################################################
# 計算導演的加權平均表現
director_performance <- movies_clean %>%
  group_by(Director.Name) %>%
  summarise(
    weighted_tomatometer = sum(tomatometer * criticsScore_ratingCount, na.rm = TRUE) / sum(criticsScore_ratingCount, na.rm = TRUE),
    weighted_audience = sum(audience_score * criticsScore_ratingCount, na.rm = TRUE) / sum(criticsScore_ratingCount, na.rm = TRUE),
    total_box_office = sum(box_office, na.rm = TRUE)
  ) %>%
  mutate(overall_score = (weighted_tomatometer + weighted_audience + total_box_office) / 3) %>%
  arrange(overall_score)

worst_director <- director_performance %>% slice(1)
print(worst_director)

# 計算差評率
director_disapproval <- movies_clean %>%
  group_by(Director.Name) %>%
  summarise(
    critics_disapproval_rate = sum(criticsScore_notLikedCount, na.rm = TRUE) / sum(criticsScore_ratingCount, na.rm = TRUE),
    audience_disapproval_rate = mean(audience_score < 30, na.rm = TRUE)
  ) %>%
  mutate(average_disapproval = (critics_disapproval_rate + audience_disapproval_rate) / 2) %>%
  arrange(desc(average_disapproval))

worst_director_disapproval <- director_disapproval %>% slice(1)
print(worst_director_disapproval)

# 計算導演的平均票房
director_box_office <- movies_clean %>%
  group_by(Director.Name) %>%
  summarise(avg_box_office = mean(box_office, na.rm = TRUE)) %>%
  arrange(avg_box_office)

worst_director_box_office <- director_box_office %>% slice(1)
print(worst_director_box_office)

# 計算爛電影比例
low_quality_directors <- movies_clean %>%
  group_by(Director.Name) %>%
  summarise(
    total_movies = n(),
    low_quality_movies = sum(Movie.Title != "", na.rm = TRUE),
    low_quality_ratio = low_quality_movies / total_movies
  ) %>%
  arrange(desc(low_quality_ratio))

worst_low_quality_director <- low_quality_directors %>% slice(1)
print(worst_low_quality_director)

# 計算導演在每個主題類型中的表現
genre_performance <- movies_clean %>%
  pivot_longer(cols = starts_with("Action"):starts_with("Anime"), names_to = "Genre", values_to = "HasGenre") %>%
  filter(HasGenre == TRUE) %>%
  group_by(Director.Name, Genre) %>%
  summarise(
    avg_tomatometer = mean(tomatometer, na.rm = TRUE),
    avg_audience_score = mean(audience_score, na.rm = TRUE)
  ) %>%
  arrange(avg_tomatometer, avg_audience_score)

print(genre_performance)



























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
  select(Director.Name, 23:55) %>%  
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



