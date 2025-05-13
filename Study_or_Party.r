allData<-read.csv("student-mat.csv", sep=";", header=TRUE)

purgeData<-subset(allData, absences<=20 & higher=="yes" & G3>0 & school=="GP")
purgeData<-purgeData[,-1]
df.train<-purgeData[1:ceiling((nrow(purgeData)*(3/4))),]
df.test<-purgeData[((ceiling(nrow(purgeData)*(3/4)))+1):nrow(purgeData),]

# 載入必要套件
library(tidyverse)
library(caret)

# 1. 轉換類別變數為 factor
cat_vars <- c("sex","address","famsize","Pstatus",
              "Mjob","Fjob","reason","guardian",
              "schoolsup","famsup","paid","activities",
              "nursery","higher","internet","romantic")
df.train[cat_vars] <- lapply(df.train[cat_vars], as.factor)
df.test[cat_vars] <- lapply(df.test[cat_vars], as.factor)

# 移除只含單一層級的因子變數
factor_vars <- names(df.train)[sapply(df.train, is.factor)]
single_level_vars <- factor_vars[sapply(df.train[factor_vars], nlevels) < 2]
if (length(single_level_vars) > 0) {
  df.train <- df.train[, !(names(df.train) %in% single_level_vars)]
  df.test <- df.test[, !(names(df.test) %in% single_level_vars)]
}
# 移除測試集中的未使用層級
df.test <- droplevels(df.test)

# 2. 建立多元線性回歸模型 (以所有變數預測 G3)
model <- lm(G3 ~ ., data = df.train)

# 檢視模型摘要，確認係數、R²、p-value 等
summary(model)

# 3. 在測試集上做預測
pred <- predict(model, newdata = df.test)

# 計算 RMSE（評估模型在測試集的誤差）
rmse <- sqrt(mean((df.test$G3 - pred)^2))
cat("Test RMSE:", round(rmse, 3), "\n")

# 計算測試集 R²
r2 <- cor(df.test$G3, pred)^2
cat("Test R-squared:", round(r2, 3), "\n")

# 計算訓練集RMSE
trainRMSE <- sqrt(mean((df.train$G3 - predict(model, newdata = df.train))^2))
cat("Train RMSE:", round(trainRMSE, 3), "\n")

# 計算訓練集 R²
trainR2 <- cor(df.train$G3, predict(model, newdata = df.train))^2
cat("Train R-squared:", round(trainR2, 3), "\n")

# （可選）畫出預測值 vs 真實值的散佈圖，檢查模型效果
p<-ggplot(df.test, aes(x = pred, y = G3)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Predicted vs Actual G3",
       x = "Predicted G3",
       y = "Actual G3") +
  theme_minimal()

print(p)