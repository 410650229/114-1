setwd("C:\\Users\\User\\Desktop\\大學\\四上\\DM")
RLCt13C2_merged_data <- read.csv("RLCt13C2_merged_data.csv")
# 假設資料集名為 RLCt13C2_merged_data
# 確認第六個和第五個變數存在於資料集中
# 建立列聯表 (第六個變數與第五個變數)

data =RLCt13C2_merged_data
install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)
library(reshape2)

# 建立列聯表 (第六個變數與第五個變數)
contingency_table <- table(data[[6]], data[[7]])

# 將列聯表轉換為長格式資料框
contingency_df <- as.data.frame(as.table(contingency_table))
colnames(contingency_df) <- c("Var6", "Var5", "Frequency")
# 繪製矩陣圖
ggplot(contingency_df, aes(x = Var5, y = Var6, fill = Frequency)) +
  geom_tile(color = "white") + # 繪製矩形
  scale_fill_gradient(low = "lightblue", high = "blue") + # 設定漸層配色
  labs(
    
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # 調整 X 軸標籤
  )