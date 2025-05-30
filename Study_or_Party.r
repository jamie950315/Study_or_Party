library(ggplot2)

allData<-read.csv("student-mat.csv", sep=";", header=TRUE)

purgeData<-subset(allData, higher=="yes" & absences<=30 & G3>0 & school=="GP")


purgeData<-purgeData[, -c(1,21,ncol(purgeData)-1)]
# purgeData<-purgeData[, -c(1,21)]


# 1. 轉換類別變數為 factor
category<-c("sex", "address", "famsize", "Pstatus", 
  "Mjob", "Fjob", "reason", "guardian", 
  "schoolsup", "famsup", "paid", "activities", 
  "nursery", "internet", "romantic")
purgeData[category]<-lapply(purgeData[category], as.factor)

set.seed(123)  # 設定隨機種子以重現結果
df.train<-purgeData[sample(1:nrow(purgeData),ceiling(nrow(purgeData)*0.7)), ]
df.test<-purgeData[-as.numeric(rownames(df.train)), ]

# 2. 建立多元線性回歸模型 (以所有變數預測 G3)
model<-lm(G3~., data=df.train)

# 檢視模型摘要，確認係數、R²、p-value 等
print(summary(model))

# 3. 在測試集上做預測
prediction<-predict(model, newdata=df.test)

# 計算 RMSE（評估模型在測試集的誤差）
rmse.test<-sqrt(mean((df.test$G3-prediction)^2))
print(paste("Test RMSE:", round(rmse.test, 3)))

# 計算測試集 R-squared
r2.test<-cor(df.test$G3, prediction)^2
print(paste("Test R-squared:", round(r2.test, 3)))

# 計算訓練集RMSE
rmse.train<-sqrt(mean((df.train$G3-predict(model, newdata=df.train))^2))
print(paste("Train RMSE:", round(rmse.train, 3)))

# 計算訓練集 R-squared
r2.train<-cor(df.train$G3, predict(model, newdata=df.train))^2
print(paste("Train R-squared:", round(r2.train, 3)))

# （可選）畫出預測值 vs 真實值的散佈圖，檢查模型效果
plot<-ggplot(df.test, aes(x=prediction, y=G3))+
  geom_point(alpha=0.6)+
  geom_abline(intercept=0, slope=1, linetype="dashed")+
  scale_x_continuous(breaks=seq(5, 20, 1))+
  scale_y_continuous(breaks=seq(5, 20, 1))+
  labs(title="Predicted vs Actual G3", 
    x="Predicted G3", 
    y="Actual G3")+
  theme_minimal()
  

print(plot)

# 自動挑選模型中絕對 t 值前10大的變數（排除截距）
coef_sum <- summary(model)$coefficients
tvals_raw <- abs(coef_sum[, "t value"])
tvals_raw <- tvals_raw[names(tvals_raw) != "(Intercept)"]

# 建立「係數名稱 → 原始欄位」對應（handle factor dummies）
orig_vars <- names(df.train)
map_term_to_var <- function(term) {
  if (term %in% orig_vars) return(term)
  # 找到第一個原始欄位名稱是 term 的前綴
  hit <- orig_vars[startsWith(term, orig_vars)]
  if (length(hit) > 0) return(hit[1])
  term
}
mapped_vars <- vapply(names(tvals_raw), map_term_to_var, character(1))
tvals <- tvals_raw  # 方便閱讀，仍保留原始 t 值
names(tvals) <- mapped_vars          # 重設名稱為原始變數
# 取得前10個（若重複取唯一）
top10_vars <- unique(names(sort(tvals, decreasing = TRUE)))[1:10]

cat("\nTop 10 t-value variables:", paste(top10_vars, collapse = ", "), "\n")


# 4. 先詢問詳細版或精簡版
cat("\n請選擇分析版本：1. 詳細版分析；2. 精簡版分析\n")
while(TRUE){
  analyzeMode<-readline("輸入 1 或 2: ")
  if(analyzeMode==""){
    analyzeMode<-"2"
  }
  if(analyzeMode=="1"||analyzeMode=="2"){
    break
  }
  print("輸入不合法，請輸入 1 或 2。")
}
# 定義所有預測變數
predictVar<-setdiff(names(df.train), "G3")

# 若選擇精簡版，只提示以下變數，其餘自動填中位數/眾數
if(analyzeMode=="2"){
    considerInput<-top10_vars
}else{
  considerInput<-predictVar
}
# 開始收集輸入
cat("\n請依序輸入以下變數的值，一行一個，留空則以中位數(數值)或眾數(類別)補上：\n")
userInput<-list()
for(inputVar in predictVar){
  # 計算中位數或眾數
  if(is.numeric(df.train[[inputVar]])){
    numeric.common<-median(df.train[[inputVar]], na.rm=TRUE)
    if(inputVar%in%considerInput){
      cat("\n============================================\n")
      # 顯示此變數對 G3 的影響方向（在輸入前）
      est <- coef_sum[inputVar, "Estimate"]
      dir <- if(est > 0) "正向" else if(est < 0) "負向" else "無方向"
      cat("\n\n",inputVar,"係數=",est,"影響",dir,"G3\n")
      # 計算該變數的最小和最大值作為可接受範圍
      numeric.min<-min(df.train[[inputVar]], na.rm=TRUE)
      numeric.max<-max(df.train[[inputVar]], na.rm=TRUE)
      while(TRUE){
        rawInput<-readline(sprintf(" =>%s 範圍 %s - %s [默認 %s]: ", inputVar, numeric.min, numeric.max, numeric.common))
        if(rawInput==""||is.na(rawInput)){
          numeric.input<-numeric.common
          break
        }
        numeric.input<-suppressWarnings(as.numeric(rawInput))
        if(!is.na(numeric.input)&&numeric.input>=numeric.min&&numeric.input<=numeric.max){
          break
        }
        cat("輸入不合法，請輸入",numeric.min,"到",numeric.max,"之間的數字。\n")
      }
      userInput[[inputVar]]<-numeric.input
    }else{
      userInput[[inputVar]]<-numeric.common
    }
  }else if(is.factor(df.train[[inputVar]])){
    factor.valid<-levels(df.train[[inputVar]])
    factor.common<-names(sort(table(df.train[[inputVar]]), decreasing=TRUE))[1]
    if(inputVar%in%considerInput){
      cat("\n============================================\n")
      # 列出此類別變數各水平對 G3 的影響方向（在輸入前）
      cat("\n",inputVar,"各水平影響：\n")
      for(lv in factor.valid){
        dummyName <- paste0(inputVar, lv)
        if(dummyName %in% rownames(coef_sum)){
          est_lv <- coef_sum[dummyName, "Estimate"]
          dir_lv <- if(est_lv > 0) "正向" else if(est_lv < 0) "負向" else "無方向"
          cat(" ",lv,"(係數=",est_lv,dir_lv,"\n")
        } else {
          cat(" ",lv,"(基準)","\n")
        }
      }
      cat("\n")
      while(TRUE){
        rawInput<-readline(sprintf(" =>%s ，可選值: %s [默認 %s]: ", inputVar, paste(factor.valid, collapse="/"), factor.common))
        if(rawInput==""){
          factor.input<-factor.common
          break
        }
        if(rawInput%in%factor.valid){
          factor.input<-rawInput
          break
        }
        print(sprintf("輸入不合法，可選值：%s", paste(factor.valid, collapse="/")))
      }
      userInput[[inputVar]]<-factor.input
    }else{
      userInput[[inputVar]]<-factor.common
    }
  }
}
# 建立新資料框並轉回 factor
df.input<-as.data.frame(userInput, stringsAsFactors=FALSE)
for(inputVar in predictVar){
  if(is.factor(df.train[[inputVar]])){
    df.input[[inputVar]]<-factor(df.input[[inputVar]], levels=levels(df.train[[inputVar]]))
  }
}
# 預測並輸出結果
predUser<-predict(model, newdata=df.input)
cat("預測的 G3 分數為:", round(predUser, 1),"\n")