library(ggplot2)

allData<-read.csv("student-mat.csv", sep=";", header=TRUE)

purgeData<-subset(allData, higher=="yes" & absences<=30 & G3>0 & school=="GP")

purgeData<-purgeData[, -c(1, 21, ncol(purgeData)-1)]

attributeExplain<-list(
  school="學生的學校 (GP: Gabriel Pereira, MS: Mousinho da Silveira)",
  sex="學生性別 (F: 女, M: 男)",
  age="學生年齡 (15~20 歲)",
  address="家庭地址型態 (U: 城市, R: 鄉村)",
  famsize="家庭人數 (LE3: 小於等於3人, GT3: 大於3人)",
  Pstatus="父母同住狀態 (T: 同住, A: 分居)",
  Medu="母親教育程度 (0: 無, 1: 小學, 2: 國中, 3: 高中, 4: 大學)",
  Fedu="父親教育程度 (0: 無, 1: 小學, 2: 國中, 3: 高中, 4: 大學)",
  Mjob="母親職業 (teacher/health/services/at_home/other)",
  Fjob="父親職業 (teacher/health/services/at_home/other)",
  reason="選擇此校原因 (home/reputation/course/other)",
  guardian="監護人 (mother/father/other)",
  traveltime="通勤時間 (1: <15分, 2: 15~30分, 3: 30~60分, 4: >1小時)",
  studytime="每週讀書時數 (1: <2小時, 2: 2~5小時, 3: 5~10小時, 4: >10小時)",
  failures="不及格次數 (1~3, 4: 超過3次)",
  schoolsup="是否有額外學業輔導 (yes/no)",
  famsup="家庭是否支持學業 (yes/no)",
  paid="是否有額外付費課程 (yes/no)",
  activities="是否參加課外活動 (yes/no)",
  nursery="是否上過幼兒園 (yes/no)",
  higher="是否想升學 (yes/no)",
  internet="家中是否有網路 (yes/no)",
  romantic="是否有戀愛關係 (yes/no)",
  famrel="家庭關係品質 (1: 很差 ~ 5: 很好)",
  freetime="自由時間 (1: 很少 ~ 5: 很多)",
  goout="外出頻率 (1: 很少 ~ 5: 很多)",
  Dalc="平日飲酒量 (1: 很少 ~ 5: 很多)",
  Walc="週末飲酒量 (1: 很少 ~ 5: 很多)",
  health="健康狀況 (1: 很差 ~ 5: 很好)",
  absences="曠課次數 (0~30)",
  G1="第一學期成績 (0~20)",
  G2="第二學期成績 (0~20)"
)


category<-c("sex", "address", "famsize", "Pstatus", 
  "Mjob", "Fjob", "reason", "guardian", 
  "schoolsup", "famsup", "paid", "activities", 
  "nursery", "internet", "romantic")
purgeData[category]<-lapply(purgeData[category], as.factor)

set.seed(123)
df.train<-purgeData[sample(1:nrow(purgeData),ceiling(nrow(purgeData)*0.7)), ]
df.test<-purgeData[-as.numeric(rownames(df.train)), ]

model<-lm(G3~., data=df.train)
print(summary(model))

prediction<-predict(model, newdata=df.test)

rmse.test<-sqrt(mean((df.test$G3-prediction)^2))
print(paste("Test RMSE:", round(rmse.test, 3)))

r2.test<-cor(df.test$G3, prediction)^2
print(paste("Test R-squared:", round(r2.test, 3)))

rmse.train<-sqrt(mean((df.train$G3-predict(model, newdata=df.train))^2))
print(paste("Train RMSE:", round(rmse.train, 3)))

r2.train<-cor(df.train$G3, predict(model, newdata=df.train))^2
print(paste("Train R-squared:", round(r2.train, 3)))

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

coef_sum<-summary(model)$coefficients
tvals_raw<-abs(coef_sum[, "t value"])
tvals_raw<-tvals_raw[names(tvals_raw)!="(Intercept)"]

orig_vars<-names(df.train)
map_term_to_var<-function(term) {
  if(term %in% orig_vars) return(term)
  hit<-orig_vars[startsWith(term, orig_vars)]
  if(length(hit)>0) return(hit[1])
  term
}
mapped_vars<-vapply(names(tvals_raw), map_term_to_var, character(1))
tvals<-tvals_raw
names(tvals)<-mapped_vars
top10_vars<-unique(names(sort(tvals, decreasing = TRUE)))[1:10]

cat("\nTop 10 t-value variables:", paste(top10_vars, collapse = ", "), "\n")


cat("\n請選擇分析模式：1. 詳細版分析；2. 精簡版分析\n")
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
predictVar<-setdiff(names(df.train), "G3")

if(analyzeMode=="2"){
    considerInput<-top10_vars
}else{
  considerInput<-predictVar
}
cat("\n請依序輸入以下變數的值，一行一個，留空則以中位數(數值)或眾數(類別)補上：\n")
userInput<-list()
for(inputVar in predictVar){

  if(is.numeric(df.train[[inputVar]])){
    numeric.common<-median(df.train[[inputVar]], na.rm=TRUE)
    if(inputVar%in%considerInput){
      cat("\n============================================\n")
      cat(sprintf("\n【%s】說明：\n%s", inputVar, attributeExplain[[inputVar]]))
      est<-coef_sum[inputVar, "Estimate"]
      dir<-if(est > 0) "正向" else if(est < 0) "負向" else "無方向"
      cat("\n\n", inputVar, "係數=", est, "影響", dir, "G3\n")
      numeric.min<-min(df.train[[inputVar]], na.rm=TRUE)
      numeric.max<-max(df.train[[inputVar]], na.rm=TRUE)
      while(TRUE){
        rawInput<-readline(sprintf(" => %s [默認 %s]: ", inputVar, numeric.common))
        if(rawInput==""||is.na(rawInput)){
          numeric.input<-numeric.common
          break
        }
        numeric.input<-suppressWarnings(as.numeric(rawInput))
        if(!is.na(numeric.input) && numeric.input>=numeric.min && numeric.input<=numeric.max){
          break
        }
        cat("輸入不合法，請輸入", numeric.min, "到", numeric.max, "之間的數字。\n")
      }
      userInput[[inputVar]]<-numeric.input
    }else{
      userInput[[inputVar]]<-numeric.common
    }
  }else if(is.factor(df.train[[inputVar]])){
    factor.valid<-levels(df.train[[inputVar]])
    factor.common<-names(sort(table(df.train[[inputVar]]), decreasing=TRUE))[1]
    if(inputVar %in% considerInput){
      cat("\n============================================\n")
      cat(sprintf("\n【%s】說明：\n%s", inputVar, attributeExplain[[inputVar]]))
      cat("\n\n", inputVar,"各水平影響：\n")
      for(lv in factor.valid){
        dummyName<-paste0(inputVar, lv)
        if(dummyName %in% rownames(coef_sum)){
          est_lv<-coef_sum[dummyName, "Estimate"]
          dir_lv<-if(est_lv>0) "正向" else if(est_lv<0) "負向" else "無方向"
          cat(" ", lv, "(係數=", est_lv, dir_lv, "\n")
        } else {
          cat(" ", lv, "(基準)", "\n")
        }
      }
      cat("\n")
      while(TRUE){
        rawInput<-readline(sprintf(" => %s [默認 %s]: ", inputVar, factor.common))
        if(rawInput==""){
          factor.input<-factor.common
          break
        }
        if(rawInput %in% factor.valid){
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
df.input<-as.data.frame(userInput, stringsAsFactors=FALSE)
for(inputVar in predictVar){
  if(is.factor(df.train[[inputVar]])){
    df.input[[inputVar]]<-factor(df.input[[inputVar]], levels=levels(df.train[[inputVar]]))
  }
}
predUser<-predict(model, newdata=df.input)
cat("預測的 G3 分數為:", round(predUser, 1),"\n")