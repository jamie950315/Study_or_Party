allData<-read.csv("student-mat.csv", sep=";", header=TRUE)

purgeData<-subset(allData, absences<=20&higher=="yes"&G3>0&school=="GP")
purgeData<-purgeData[, -1]
dfTrain<-purgeData[1:ceiling((nrow(purgeData)*(3/4))), ]
dfTest<-purgeData[((ceiling(nrow(purgeData)*(3/4)))+1):nrow(purgeData), ]

# 載入必要套件
library(tidyverse)
library(caret)

# 1. 轉換類別變數為 factor
catVars<-c("sex", "address", "famsize", "Pstatus", 
  "Mjob", "Fjob", "reason", "guardian", 
  "schoolsup", "famsup", "paid", "activities", 
  "nursery", "higher", "internet", "romantic")
dfTrain[catVars]<-lapply(dfTrain[catVars], as.factor)
dfTest[catVars]<-lapply(dfTest[catVars], as.factor)

# 移除只含單一層級的因子變數
factorVars<-names(dfTrain)[sapply(dfTrain, is.factor)]
singleLevelVars<-factorVars[sapply(dfTrain[factorVars], nlevels)<2]
if(length(singleLevelVars)>0){
  dfTrain<-dfTrain[, !(names(dfTrain)%in%singleLevelVars)]
  dfTest<-dfTest[, !(names(dfTest)%in%singleLevelVars)]
}
# 移除測試集中的未使用層級
dfTest<-droplevels(dfTest)

# 2. 建立多元線性回歸模型 (以所有變數預測 G3)
model<-lm(G3~., data=dfTrain)

# 檢視模型摘要，確認係數、R²、p-value 等
summary(model)

# 3. 在測試集上做預測
pred<-predict(model, newdata=dfTest)

# 計算 RMSE（評估模型在測試集的誤差）
rmse<-sqrt(mean((dfTest$G3-pred)^2))
print(paste("Test RMSE:", round(rmse, 3)))

# 計算測試集 R²
r2<-cor(dfTest$G3, pred)^2
print(paste("Test R-squared:", round(r2, 3)))

# 計算訓練集RMSE
trainRmse<-sqrt(mean((dfTrain$G3-predict(model, newdata=dfTrain))^2))
print(paste("Train RMSE:", round(trainRmse, 3)))

# 計算訓練集 R²
trainR2<-cor(dfTrain$G3, predict(model, newdata=dfTrain))^2
print(paste("Train R-squared:", round(trainR2, 3)))

# （可選）畫出預測值 vs 真實值的散佈圖，檢查模型效果
p<-ggplot(dfTest, aes(x=pred, y=G3))+
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
  choice<-readline("輸入 1 或 2: ")
  if(choice=="1"||choice=="2"){
    break
  }
  print("輸入不合法，請輸入 1 或 2。")
}
# 定義所有預測變數
predictVars<-setdiff(names(dfTrain), "G3")
# 若選擇精簡版，只提示以下變數，其餘自動填中位數/眾數
if(choice=="2"){
  inputVars<-c("G2", "G1", "failures", "age", "goout", "Medu", "Fedu", "higher", "reason", "traveltime", "address", "studytime")
}else{
  inputVars<-predictVars
}
# 開始收集輸入
print("請依序輸入以下變數的值，一行一個，留空則以中位數(數值)或眾數(類別)補上：")
userInput<-list()
for(var in predictVars){
  # 計算中位數或眾數
  if(is.numeric(dfTrain[[var]])){
    med<-median(dfTrain[[var]], na.rm=TRUE)
    if(var%in%inputVars){
      # 計算該變數的最小和最大值作為可接受範圍
      minVal<-min(dfTrain[[var]], na.rm=TRUE)
      maxVal<-max(dfTrain[[var]], na.rm=TRUE)
      while(TRUE){
        val<-readline(sprintf("%s (numeric,  範圍 %s - %s) [默認 %s]: ", var, minVal, maxVal, med))
        if(val==""||is.na(val)){
          valNum<-med
          break
        }
        valNum<-suppressWarnings(as.numeric(val))
        if(!is.na(valNum)&&valNum>=minVal&&valNum<=maxVal){
          break
        }
        print(sprintf("輸入不合法，請輸入 %s 到 %s 之間的數字。", minVal, maxVal))
      }
      userInput[[var]]<-valNum
    }else{
      userInput[[var]]<-med
    }
  }else if(is.factor(dfTrain[[var]])){
    levs<-levels(dfTrain[[var]])
    modeVal<-names(sort(table(dfTrain[[var]]), decreasing=TRUE))[1]
    if(var%in%inputVars){
      while(TRUE){
        val<-readline(sprintf("%s (categorical)，可選值: %s [默認 %s]: ", var, paste(levs, collapse="/"), modeVal))
        if(val==""){
          valChr<-modeVal
          break
        }
        if(val%in%levs){
          valChr<-val
          break
        }
        print(sprintf("輸入不合法，可選值：%s", paste(levs, collapse="/")))
      }
      userInput[[var]]<-valChr
    }else{
      userInput[[var]]<-modeVal
    }
  }
}
# 建立新資料框並轉回 factor
newDf<-as.data.frame(userInput, stringsAsFactors=FALSE)
for(var in predictVars){
  if(is.factor(dfTrain[[var]])){
    newDf[[var]]<-factor(newDf[[var]], levels=levels(dfTrain[[var]]))
  }
}
# 預測並輸出結果
predUser<-predict(model, newdata=newDf)
print(paste("預測的 G3 分數為:", round(predUser, 1)))