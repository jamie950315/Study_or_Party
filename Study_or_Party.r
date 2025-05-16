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
  scale_x_continuous(breaks = seq(5, 20, 1)) +
  scale_y_continuous(breaks = seq(5, 20, 1)) +
  labs(title = "Predicted vs Actual G3",
       x = "Predicted G3",
       y = "Actual G3") +
  theme_minimal()
  

print(p)

# 4. 先詢問詳細版或精簡版
cat("請選擇分析版本：1. 詳細版分析；2. 精簡版分析\n")
choice <- readline("輸入 1 或 2: ")
# 定義所有預測變數
predict_vars <- setdiff(names(df.train), "G3")
# 若選擇精簡版，只提示以下變數，其餘自動填中位數/眾數
if (choice == "2") {
  input_vars <- c("G2", "G1", "failures", "age", "goout", "Medu", "Fedu", "higher", "reason", "traveltime", "address", "studytime")
} else {
  input_vars <- predict_vars
}
# 開始收集輸入
cat("請依序輸入以下變數的值，一行一個，留空則以中位數(數值)或眾數(類別)補上：\n")
user_input <- list()
for (var in predict_vars) {
  # 計算中位數或眾數
  if (is.numeric(df.train[[var]])) {
    med <- median(df.train[[var]], na.rm = TRUE)
    if (var %in% input_vars) {
      # 計算該變數的最小和最大值作為可接受範圍
      min_val <- min(df.train[[var]], na.rm = TRUE)
      max_val <- max(df.train[[var]], na.rm = TRUE)
      while(TRUE) {
        val <- readline(sprintf("%s (numeric, 範圍 %s - %s) [默認 %s]: ", var, min_val, max_val, med))
        if (val == "" || is.na(val)) {
          val_num <- med
          break
        }
        val_num <- suppressWarnings(as.numeric(val))
        if (!is.na(val_num) && val_num >= min_val && val_num <= max_val) {
          break
        }
        cat(sprintf("輸入不合法，請輸入 %s 到 %s 之間的數字。\n", min_val, max_val))
      }
      user_input[[var]] <- val_num
    } else {
      user_input[[var]] <- med
    }
  } else if (is.factor(df.train[[var]])) {
    levs <- levels(df.train[[var]])
    mode_val <- names(sort(table(df.train[[var]]), decreasing = TRUE))[1]
    if (var %in% input_vars) {
      cat(sprintf("%s (categorical)，可選值: %s [默認 %s]\n", var, paste(levs, collapse = "/"), mode_val))
      while(TRUE) {
        val <- readline(sprintf("%s: ", var))
        if (val == "") {
          val_chr <- mode_val
          break
        }
        if (val %in% levs) {
          val_chr <- val
          break
        }
        cat(sprintf("輸入不合法，可選值：%s\n", paste(levs, collapse = "/")))
      }
      user_input[[var]] <- val_chr
    } else {
      user_input[[var]] <- mode_val
    }
  }
}
# 建立新資料框並轉回 factor
new_df <- as.data.frame(user_input, stringsAsFactors = FALSE)
for (var in predict_vars) {
  if (is.factor(df.train[[var]])) {
    new_df[[var]] <- factor(new_df[[var]], levels = levels(df.train[[var]]))
  }
}
# 預測並輸出結果
pred_user <- predict(model, newdata = new_df)
cat("預測的 G3 分數為:", round(pred_user, 1), "\n")