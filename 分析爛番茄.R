setwd("C:\\Users\\User\\Desktop\\學\\大學\\PYTHON")

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

################################
##########網絡圖################
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

ggraph(g, layout = "fr") + 
  geom_edge_link(aes(alpha = 0.5), color = "gray") +  # 邊的顏色與透明度
  geom_node_point(aes(size = size, color = color)) +  # 節點大小與顏色
  geom_node_text(aes(label = name), repel = TRUE, max.overlaps = 100) +   # 節點標籤
  theme_void() +
  ggtitle("Director-Movie Network (At least 2 Directors)") +
  theme(legend.position = "none")
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
#######################分析評分#########################
########################################################
Director.Name_analysis <- movies_clean %>%
  group_by(Director.Name) %>%
  summarise(
    # 計算參與作品數
    total_works = n(),
    
    # 計算各項平均值，忽略非數字或遺失值
    avg_tomatometer = mean(tomatometer, na.rm = TRUE),
    avg_audience_score = mean(audience_score, na.rm = TRUE),
    avg_box_office = mean(box_office, na.rm = TRUE),
    
    # 計算遺失值或非數字數量
    missing_tomatometer = sum(is.na(tomatometer)),
    missing_audience_score = sum(is.na(audience_score)),
    missing_box_office = sum(is.na(box_office)),
    
    # 計算 criticsScore_ratingCount 的總和
    total_criticsScore_ratingCount = sum(criticsScore_ratingCount, na.rm = TRUE),
    
    # 計算 total_criticsScore_ratingCount 除以 (total_works - missing_tomatometer)
    critics_per_work = total_criticsScore_ratingCount / (total_works - missing_tomatometer)
  ) %>%
  arrange(avg_tomatometer, avg_audience_score, avg_box_office)

########################################################
#######################分析類別#########################
########################################################
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
  select(Director.Name, 22:54
         ) %>%  
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
#########################
#########################
colnames(combined_analysis)
# 繪製 represent 次數的柱狀圖，並按照次數排序
ggplot(combined_analysis, aes(x = reorder(represent, represent, FUN = function(x) -length(x)))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "人物主要參與類別", x = "represent", y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) 


##########################################################################
################人物-作品類別矩陣視覺化###################################
##########################################################################
# 加載必要套件
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: 計算每位導演拍攝的各類型作品數量
director_genre_matrix <- movies_clean %>%
  select(Director.Name, Action:Anime) %>% # 選取導演和類型列
  group_by(Director.Name) %>%
  summarise(across(Action:Anime, sum, na.rm = TRUE)) %>%
  ungroup()

# Step 2: 計算每個導演的總作品數量（按行總和排序）
director_genre_matrix <- director_genre_matrix %>%
  mutate(total_count = rowSums(select(., Action:Anime)))

# 排序導演，從最多作品數到最少
director_genre_matrix <- director_genre_matrix %>%
  arrange(desc(total_count))

# Step 3: 計算每個類型的總作品數量（按列總和排序）
genre_totals <- colSums(select(director_genre_matrix, Action:Anime))

# 排序類型，從最多作品數到最少
ordered_genres <- names(sort(genre_totals, decreasing = TRUE))

# Step 4: 將數據轉換為長格式以適應 ggplot
long_data <- director_genre_matrix %>%
  pivot_longer(cols = ordered_genres, 
               names_to = "Genre", 
               values_to = "Count")

# Step 5: 繪製排序後的熱圖
ggplot(long_data, aes(x = Genre, y = Director.Name, fill = Count)) +
  geom_tile(color = "white") + # 繪製熱圖
  scale_fill_gradient(low = "white", high = "steelblue") + # 設置顏色漸變
  theme_minimal() +
  labs(
    title = "Director-Genre Relationship Heatmap",
    x = "Genre",
    y = "Director",
    fill = "Count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # 旋轉類型標籤
    axis.text.y = element_text(size = 6) # 控制導演名字的大小
  )
###########################################################
######################各直方圖#############################
###########################################################
########################################################
#######################評分為0########################## 
########################################################
# 假設 'movies_clean' 是包含所有電影與類型資料的數據框
library(tidyverse)

# 1. 去除重複的作品（以作品名稱來判斷重複）
unique_movies <- movies_clean %>%
  distinct(Movie.Title, .keep_all = TRUE)

# 2. 把 'content_metadataGenres' 展開成多個類型，並計數每個類型的出現次數
genre_counts <- unique_movies %>%
  separate_rows(content_metadataGenres, sep = ",") %>%  # 分割每個作品的類型
  mutate(content_metadataGenres = str_trim(content_metadataGenres)) %>%  # 去除多餘空格
  count(content_metadataGenres)  # 計算每個類型的數量

# 3. 繪製類型分布直方圖
ggplot(genre_counts, aes(x = reorder(content_metadataGenres, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # 水平顯示
  labs(title = "Distribution of Movie Genres",
       x = "Genre",
       y = "Frequency") +
  theme_minimal()
################################################
####################全部作品####################
################################################
# 假設 'movies_clean' 是包含所有電影與類型資料的數據框
library(tidyverse)

# 1. 去除重複的作品（以作品名稱來判斷重複）
unique_movies <- movies_clean %>%
  distinct(title, .keep_all = TRUE)

# 2. 把 'content_metadataGenres' 展開成多個類型，並計數每個類型的出現次數
genre_counts <- unique_movies %>%
  separate_rows(content_metadataGenres, sep = ",") %>%  # 分割每個作品的類型
  mutate(content_metadataGenres = str_trim(content_metadataGenres)) %>%  # 去除多餘空格
  count(content_metadataGenres)  # 計算每個類型的數量

# 3. 繪製類型分布直方圖
ggplot(genre_counts, aes(x = reorder(content_metadataGenres, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # 水平顯示
  labs(title = "Distribution of Movie Genres",
       x = "Genre",
       y = "Frequency") +
  theme_minimal()
########################################
################平均評分################
########################################
####################人物#################
colnames(movies_clean)
# 2. 把 'Director.Name' 展開並計算平均評分

director_avg_scores <- movies_clean %>%
  group_by(Director.Name) %>%  # 按導演名稱分組
  summarise(
    avg_tomatometer = mean(tomatometer, na.rm = TRUE),  # 計算平均專家評分
    avg_audience_score = mean(audience_score, na.rm = TRUE)  # 計算平均觀眾評分
  ) %>%
  pivot_longer(cols = starts_with("avg"), names_to = "score_type", values_to = "avg_score")  # 長格式轉換

# 3. 繪製每個導演的平均評分直方圖
ggplot(director_avg_scores, aes(x = reorder(Director.Name, avg_score), y = avg_score, fill = score_type)) +
  geom_bar(stat = "identity", position = "dodge") +  # 條形圖
  coord_flip() +  # 水平顯示
  labs(title = "Average Ratings by Director",
       x = "Director Name",
       y = "Average Rating",
       fill = "Rating Type") +
  theme_minimal()

###########################電影類別###############################

# 假設 'movies_clean' 是包含所有電影與類型資料的數據框
library(tidyverse)

# 1. 去除重複的作品（以作品名稱來判斷重複）
unique_movies <- movies_clean %>%
  distinct(title, .keep_all = TRUE)

# 2. 把 'content_metadataGenres' 展開成多個類型，並保留相對應的評分
genre_avg_scores <- unique_movies %>%
  separate_rows(content_metadataGenres, sep = ",") %>%  # 分割每個作品的類型
  mutate(content_metadataGenres = str_trim(content_metadataGenres)) %>%  # 去除多餘空格
  group_by(content_metadataGenres) %>%  # 按類型分組
  summarise(
    avg_tomatometer = mean(tomatometer, na.rm = TRUE),  # 計算平均專家評分
    avg_audience_score = mean(audience_score, na.rm = TRUE)  # 計算平均觀眾評分
  ) %>%
  pivot_longer(cols = starts_with("avg"), names_to = "score_type", values_to = "avg_score")

# 3. 繪製每個類型的平均評分直方圖
ggplot(genre_avg_scores, aes(x = reorder(content_metadataGenres, avg_score), y = avg_score, fill = score_type)) +
  geom_bar(stat = "identity", position = "dodge") +  # 條形圖
  coord_flip() +  # 水平顯示
  labs(title = "Average Ratings by Movie Genre",
       x = "Genre",
       y = "Average Rating",
       fill = "Rating Type") +
  theme_minimal()
########################################
###############評分數量#################
########################################
library(tidyverse)

# 假設 'movies_clean' 是包含電影資料的數據框

# 1. 計算每位導演的 `overlay_criticsTop_ratingCount` 和 `criticsScore_ratingCount` 總和
director_rating_counts <- movies_clean %>%
  group_by(Director.Name) %>%
  summarise(
    total_overlay_criticsTop_ratingCount = sum(overlay_criticsTop_ratingCount, na.rm = TRUE),
    total_criticsScore_ratingCount = sum(criticsScore_ratingCount, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("total"), names_to = "rating_type", values_to = "rating_count")

# 2. 繪製導演的評分數量長條圖
ggplot(director_rating_counts, aes(x = reorder(Director.Name, rating_count), y = rating_count, fill = rating_type)) +
  geom_bar(stat = "identity", position = "dodge") +  # 長條圖
  coord_flip() +  # 水平顯示
  labs(title = "Critic Rating Counts by Director",
       x = "Director",
       y = "Rating Count",
       fill = "Rating Type") +
  theme_minimal()

# 3. 計算每個作品類別的 `overlay_criticsTop_ratingCount` 和 `criticsScore_ratingCount` 總和
genre_rating_counts <- movies_clean %>%
  separate_rows(content_metadataGenres, sep = ",") %>%  # 分割每個作品的類型
  mutate(content_metadataGenres = str_trim(content_metadataGenres)) %>%  # 去除空格
  group_by(content_metadataGenres) %>%
  summarise(
    total_overlay_criticsTop_ratingCount = sum(overlay_criticsTop_ratingCount, na.rm = TRUE),
    total_criticsScore_ratingCount = sum(criticsScore_ratingCount, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("total"), names_to = "rating_type", values_to = "rating_count")

# 4. 繪製作品類別的評分數量長條圖
ggplot(genre_rating_counts, aes(x = reorder(content_metadataGenres, rating_count), y = rating_count, fill = rating_type)) +
  geom_bar(stat = "identity", position = "dodge") +  # 長條圖
  coord_flip() +  # 水平顯示
  labs(title = "Critic Rating Counts by Movie Genre",
       x = "Genre",
       y = "Rating Count",
       fill = "Rating Type") +
  theme_minimal()
########################################
###############導演作品數量#############
########################################
library(ggplot2)
library(dplyr)

# 計算每位導演的作品數量
director_counts <- movies_clean %>%
  group_by(Director.Name) %>%
  summarise(total_movies = n()) %>%
  arrange(desc(total_movies)) # 按作品數降序排列

# 繪製長條圖
ggplot(director_counts, aes(x = reorder(Director.Name, -total_movies), y = total_movies)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Movies per Director",
       x = "Director Name",
       y = "Total Movies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 旋轉導演名稱以便顯示

library(dplyr)
########################################
###############導演合作作品數量#########
########################################
library(dplyr)
library(ggplot2)

# 計算合作情況：相同 Title 但不同導演
director_collaboration <- movies_clean %>%
  group_by(title) %>%
  filter(n_distinct(Director.Name) > 1) %>% # 篩選出多導演的電影
  ungroup() %>%
  separate_rows(Director.Name, sep = ",\\s*") %>% # 分割多導演
  group_by(Director.Name) %>%
  summarise(co_directed_movies = n(), .groups = "drop") %>% # 計算每位導演的合作次數
  filter(co_directed_movies > 0) %>% # 排除 0 的情況
  arrange(desc(co_directed_movies)) # 按合作次數降序排列

# 繪製長條圖
ggplot(director_collaboration, aes(x = reorder(Director.Name, -co_directed_movies), y = co_directed_movies)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(title = "Number of Co-Directed Movies per Director",
       x = "Director Name",
       y = "Co-Directed Movies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # 旋轉 x 軸文字
  scale_x_discrete() + # 使用離散刻度
  scale_y_continuous(breaks = seq(0, max(director_collaboration$co_directed_movies), by = 3)) # 遞增3刻度

################################################
######################合作作品數量/作品數量柱方圖##################
################################################
library(dplyr)
library(ggplot2)

########################################
###############導演作品數量#############
########################################

# 計算每位導演的作品數量
director_counts <- movies_clean %>%
  group_by(Director.Name) %>%
  summarise(total_movies = n()) %>%
  arrange(desc(total_movies)) # 按作品數降序排列

########################################
###############導演合作作品數量#########
########################################

# 計算合作情況：相同 Title 但不同導演
director_collaboration <- movies_clean %>%
  group_by(title) %>%
  filter(n_distinct(Director.Name) > 1) %>% # 篩選出多導演的電影
  ungroup() %>%
  separate_rows(Director.Name, sep = ",\\s*") %>% # 分割多導演
  group_by(Director.Name) %>%
  summarise(co_directed_movies = n(), .groups = "drop") %>% # 計算每位導演的合作次數
  filter(co_directed_movies > 0) %>% # 排除 0 的情況
  arrange(desc(co_directed_movies)) # 按合作次數降序排列

########################################
###############合併資料集##############
########################################

# 合併導演的作品數量和合作作品數量
director_summary <- director_counts %>%
  left_join(director_collaboration, by = "Director.Name") %>%
  mutate(co_directed_movies = ifelse(is.na(co_directed_movies), 0, co_directed_movies), # 處理沒有合作的導演
         collaboration_ratio = co_directed_movies / total_movies) # 計算合作比例

########################################
###############繪製柱狀圖##############
########################################

# 繪製合作作品數量 / 作品數量的比率柱狀圖
ggplot(director_summary, aes(x = reorder(Director.Name, -collaboration_ratio), y = collaboration_ratio)) +
  geom_bar(stat = "identity", fill = "lightcoral", color = "black") +
  labs(title = "Collaboration Ratio per Director",
       x = "Director Name",
       y = "Collaboration Ratio (Co-directed Movies / Total Movies)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 旋轉導演名稱以便顯示

############################
############################
#
############################
############################
summary(movies_clean)