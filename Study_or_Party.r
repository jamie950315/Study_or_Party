# 載入必要套件
library(tidyverse)
library(caret)

allData<-read.csv("student-mat.csv", sep=";", header=TRUE)

purgeData<-subset(allData, absences<=20&higher=="yes"&G3>0&school=="GP")
purgeData<-purgeData[, -c(1,21)]

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
p<-ggplot(df.test, aes(x=prediction, y=G3))+
  geom_point(alpha=0.6)+
  geom_abline(intercept=0, slope=1, linetype="dashed")+
  scale_x_continuous(breaks=seq(5, 20, 1))+
  scale_y_continuous(breaks=seq(5, 20, 1))+
  labs(title="Predicted vs Actual G3", 
    x="Predicted G3", 
    y="Actual G3")+
  theme_minimal()
  

print(p)

# 4. 先詢問詳細版或精簡版
print("請選擇分析版本：1. 詳細版分析；2. 精簡版分析")
while(TRUE){
  analyzeMode<-readline("輸入 1 或 2: ")
  if(analyzeMode=="1"||analyzeMode=="2"){
    break
  }
  print("輸入不合法，請輸入 1 或 2。")
}
# 定義所有預測變數
predictVar<-setdiff(names(df.train), "G3")
# 若選擇精簡版，只提示以下變數，其餘自動填中位數/眾數
if(analyzeMode=="2"){
  considerInput<-c("G2", "G1", "failures", "age", "goout", "Medu", "Fedu", "reason", "traveltime", "address", "studytime")
}else{
  considerInput<-predictVar
}
# 開始收集輸入
print("請依序輸入以下變數的值，一行一個，留空則以中位數(數值)或眾數(類別)補上：")
userInput<-list()
for(inputVar in predictVar){
  # 計算中位數或眾數
  if(is.numeric(df.train[[inputVar]])){
    numeric.common<-median(df.train[[inputVar]], na.rm=TRUE)
    if(inputVar%in%considerInput){
      # 計算該變數的最小和最大值作為可接受範圍
      numeric.min<-min(df.train[[inputVar]], na.rm=TRUE)
      numeric.max<-max(df.train[[inputVar]], na.rm=TRUE)
      while(TRUE){
        rawInput<-readline(sprintf("%s (numeric,  範圍 %s - %s) [默認 %s]: ", inputVar, numeric.min, numeric.max, numeric.common))
        if(rawInput==""||is.na(rawInput)){
          numeric.input<-numeric.common
          break
        }
        numeric.input<-suppressWarnings(as.numeric(rawInput))
        if(!is.na(numeric.input)&&numeric.input>=numeric.min&&numeric.input<=numeric.max){
          break
        }
        print(sprintf("輸入不合法，請輸入 %s 到 %s 之間的數字。", numeric.min, numeric.max))
      }
      userInput[[inputVar]]<-numeric.input
    }else{
      userInput[[inputVar]]<-numeric.common
    }
  }else if(is.factor(df.train[[inputVar]])){
    factor.valid<-levels(df.train[[inputVar]])
    factor.common<-names(sort(table(df.train[[inputVar]]), decreasing=TRUE))[1]
    if(inputVar%in%considerInput){
      while(TRUE){
        rawInput<-readline(sprintf("%s (categorical)，可選值: %s [默認 %s]: ", inputVar, paste(factor.valid, collapse="/"), factor.common))
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
print(paste("預測的 G3 分數為:", round(predUser, 1)))