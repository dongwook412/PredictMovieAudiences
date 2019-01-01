  
  setwd("C:/Users/Yang DongWook/Desktop/data")
  grade <- read.csv("result/grade.csv", head=T, fileEncoding="euc-kr")
  grade <- grade$grade
  week_one_Data <- read.csv("result/week_one_Data.csv",head=T, fileEncoding="euc-kr")
  week_one_Data <- week_one_Data[-c(1,2,3,4)]
  week_one_Data2 <- cbind(week_one_Data,grade) # 평점을 추가한 1주차데이터를 다른 변수에 저자

  week_two_Data <- read.csv("result/week_two_Data.csv",head=T, fileEncoding="euc-kr")
  week_two_Data <- week_two_Data[-c(1:5)]
  
  lmData <- lm(formula = CumulativeAudience~., data = week_one_Data) # 1주차 데이터에 대해 다중회귀분석 진행
  Blm <- step(lmData, direction = 'both') # 변수선택방법중에 단계적선택법 이용
  summary(Blm) # Blm에 대한 정보 
  test <- read.csv("result/test.csv",head=T,fileEncoding="euc-kr") # 예측하기 위한 csv
  pd <- predict(Blm, test) # 예측 진행
  pd
  
  lmData2 <- lm(formula = CumulativeAudience~., data = week_one_Data2) # 1주차 데이터에 대해 다중회귀분석 진행, grade포함
  Blm2 <- step(lmData2, direction = 'both') # 변수선택방법중에 단계적선택법 이용
  summary(Blm2) # Blm2에 대한 정보 
  test2 <- read.csv("result/test2.csv",head=T,fileEncoding="euc-kr")
  pd2 <- predict(Blm2, test2) # 예측 진행
  pd2
  
  
  #2주차 데이터에 대해 다중회귀분석 진행, 낮아서 못씀
  #lmData2 <- lm(formula = CumulativeAudience~., data = week_two_Data)
  #Blm2 <- step(lmData2, direction = 'both')
  #summary(Blm2)
  
