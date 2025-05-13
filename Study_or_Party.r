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

# 4. 讓使用者輸入自己的變數值，留空以中位數(數值)或眾數(類別)補上，並預測 G3
predict_vars <- setdiff(names(df.train), "G3")
cat("請依序輸入以下變數的值，一行一個，留空則以中位數(數值)或眾數(類別)補上：\n")
user_input <- list()
for (var in predict_vars) {
  if (is.numeric(df.train[[var]])) {
    med <- median(df.train[[var]], na.rm = TRUE)
    val <- readline(sprintf("%s (numeric) [默認 %s]: ", var, med))
    if (val == "" || is.na(val)) {
      user_input[[var]] <- med
    } else {
      user_input[[var]] <- as.numeric(val)
    }
  } else if (is.factor(df.train[[var]])) {
    levs <- levels(df.train[[var]])
    mode_val <- names(sort(table(df.train[[var]]), decreasing = TRUE))[1]
    cat(sprintf("%s (categorical)，可選值: %s [默認 %s]\n", var, paste(levs, collapse = "/"), mode_val))
    val <- readline(sprintf("%s: ", var))
    if (!(val %in% levs)) {
      user_input[[var]] <- mode_val
    } else {
      user_input[[var]] <- val
    }
  }
}
new_df <- as.data.frame(user_input, stringsAsFactors = FALSE)
# 將類別變數轉回 factor 並設定 levels
for (var in predict_vars) {
  if (is.factor(df.train[[var]])) {
    new_df[[var]] <- factor(new_df[[var]], levels = levels(df.train[[var]]))
  }
}
pred_user <- predict(model, newdata = new_df)
cat("預測的 G3 分數為:", round(pred_user, 1), "\n")