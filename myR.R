  #install.packages("stringr")
  library(stringr)
  
  setwd("C:/Users/Yang DongWook/Desktop/data")
  filename = dir(pattern="csv")
  datas = lapply(filename,read.csv,header = TRUE)
  finalData = do.call(rbind,datas)
  #csv파일 취합
  
  finalData <- finalData[-1]
  #1열인 '순서' 제외
  Data <- finalData[complete.cases(finalData[,c(4)]),]
  #각 csv파일마다 가장 밑에 합계부분이 있는데 이것을 제거시킴
  
  #결측값 갯수 확인
  #write.csv(Data, file="result/Data.csv", row.names=TRUE) # 단순히 취합한 데이터를 csv파일로 만듬
  #Data <- read.csv("result/Data.csv",head=T, na.strings=c("","NA")) # csv파일에 결측값처리가 안되있어 빈칸도 결측값으로 읽음
  #Data <- Data[-1]
  #naData <- is.na(Data[,1:ncol(Data)])
  #naData <- colSums(naData) # 결측값 갯수 확인
  
  #1주차, 2주차로 나누는 코드
  duplicated <- duplicated(Data$영화명) # 중복된 element가 있으면 TRUE을 반환
  newData <- cbind(Data,duplicated)
  TRUEData <- subset(newData,duplicated == "TRUE") # 2주차부터 마지막주차까지
  week_one_Data <- subset(newData,duplicated == "FALSE") # 첫주차
  
  nDuplicated <- ncol(TRUEData) # 2주차부터 duplicated의 값이 모두 TRUE이므로 없애줌
  TRUEData <- TRUEData[-nDuplicated]
  
  duplicated <- duplicated(TRUEData$영화명) 
  TRUEData <- cbind(TRUEData,duplicated)
  week_two_Data <- subset(TRUEData,duplicated == "FALSE") # 두번째주차
  
  realData <- rbind(week_one_Data, week_two_Data) # 1주차와 2주차를 취합
  
  nDuplicated <- ncol(realData) # 이제 duplicated의 값이 쓸모가 없으므로 지움
  realData <- realData[-nDuplicated]
  
  realData <- realData[order(realData[1]),] # 이름순으로 배열, 1,2주차에 대한 정보 저장
  
  # 각 데이터들에 대한 상관분석
  #매출액 <- realData[3]  
  #누적매출액 <- realData[5]
  #관객수 <- realData[6]
  #누적관객수 <- realData[7]
  #스크린수 <- realData[8]
  #상영횟수 <- realData[9]
  #numeric_data <- data.frame(매출액,누적매출액,관객수,누적관객수,스크린수,상영횟수)
  #수치화된 데이터를 뽑아서 dataframe에 저장
  #cor(numeric_data[,1:6])
  
  
  
  realData2 <- realData[-c(2,4,9,11,12,16,17)] # 필요없는 변수들 제거(상관분석과 결측값 갯수 이용)
  colnames(realData2) <- c('MovieName','Sales','CumulativeSales','Audience','CumulativeAudience','Screen','National','Distributor','Rating','Genre')
  #변수명을 영어로 바꿔줌
  realData3 <- subset(realData2, Audience >=5000) # 관객수 5천명 이상인 자료만 확인
  
  #텍스트로 되어있는 변수들을 사용하기 위해 원핫인코딩을 이용
  # 먼저 등급과 장르를 ","로 구분해서 최대 두가지로 나눔
  Rating1 <- str_split_fixed(realData3$Rating,",",3)[,1] 
  Rating2 <- str_split_fixed(realData3$Rating,",",3)[,2]
  Genre1 <- str_split_fixed(realData3$Genre,",",3)[,1]
  Genre2 <- str_split_fixed(realData3$Genre,",",3)[,2]
  Distributor1 <- str_split_fixed(realData3$Distributor,",",3)[,1]
  Distributor2 <- str_split_fixed(realData3$Distributor,",",3)[,2]
  
  r1 <- model.matrix(~Rating1,realData3)[,-1] #등급에 대해 원핫인코딩 진행(,로 나눠진 것들 중 맨 처음것)
  r2 <- model.matrix(~Rating2,realData3)[,-1] #등급에 대해 원핫인코딩 진행(,로 나눠진 것들 중 두번째것)
  g1 <- model.matrix(~Genre1,realData3)[,-1] #장르에 대해 원핫인코딩 진행(,로 나눠진 것들 중 맨 처음것)
  g2 <- model.matrix(~Genre2,realData3)[,-1] #장르에 대해 원핫인코딩 진행(,로 나눠진 것들 중 두번째것)
  n1 <- model.matrix(~National,realData3)[,-1] #대표국적에 대해 원핫인코딩 진행
  d1 <- model.matrix(~Distributor1,realData3)[,-1] # 배급사에 대해 원핫인코딩 진행(,로 나눠진 것들 중 맨 처음것)
  d2 <- model.matrix(~Distributor2,realData3)[,-1] # 배급사에 대해 원핫인코딩 진행((,로 나눠진 것들 중 두번째것))
  
  #원핫인코딩한것들에서 제목에 기존의 변수명이 추가되므로 쓸데없는 것들을 제거
  colnames(r1) <- str_split_fixed(colnames(r1),"1",2)[,2] 
  colnames(r2) <- str_split_fixed(colnames(r2),"2",2)[,2] 
  colnames(g1) <- str_split_fixed(colnames(g1),"1",2)[,2]
  colnames(g2) <- str_split_fixed(colnames(g2),"2",2)[,2]
  colnames(n1) <- str_split_fixed(colnames(n1),"l",2)[,2]
  colnames(d1) <- str_split_fixed(colnames(d1),"1",2)[,2]
  colnames(d2) <- str_split_fixed(colnames(d2),"2",2)[,2]
  
  realData3 <- realData3[-c(7,8,9,10)] # 등급과 장르변수, 대표국적, 배급사는 원핫인코딩을 진행하였으므로 기존것들을 제거
  
  #각각 원핫인코딩이 진행된 변수들이 동일할 경우 동일한 열끼리 더해준다. 
  for(i in 1:ncol(r1)){ # r1과 r2의 변수들명이 같은 column끼리 더해준다.
    for(j in 1:ncol(r2)){
      if(colnames(r1)[i] == colnames(r2)[j]){
        r1[,i] <- r1[,i]+r2[,j] # 
        }
    }
  }
  for(i in 1:ncol(g1)){ # g1과 g2의 변수들명이 같은 column끼리 더해준다.
    for(j in 1:ncol(g2)){
      if(colnames(g1)[i] == colnames(g2)[j]){
        g1[,i] <- g1[,i]+g2[,j] # 
      }
    }
  }
  for(i in 1:ncol(d1)){ # d1과 d2의 변수들명이 같은 column끼리 더해준다.
    for(j in 1:ncol(d2)){
      if(colnames(d1)[i] == colnames(d2)[j]){
        d1[,i] <- d1[,i]+d2[,j] # 
      }
    }
  }
  
  realData3 <- cbind(realData3,r1,g1,n1,d1) # 원핫인코딩이 완료된 변수들을 취합해준다.
  #write.csv(realData3, file="result/realData.csv", row.names=TRUE)
  
  #1,2주차로 나누는 작업. 위와 매우 비슷
  duplicated <- duplicated(realData3$MovieName) # 중복된 element가 있으면 TRUE을 반환
  newData <- cbind(realData3,duplicated)
  week_one_Data <- subset(newData,duplicated == "FALSE") # 첫주차
  week_two_Data <- subset(newData,duplicated == "TRUE") # 2주차부터 마지막주차까지
  Audience <- week_one_Data$Audience
  CAudience <- week_one_Data$CumulativeAudience
  
  # 1주차 자료에서 관객수와 누적관객수 차이가 크면 이전에 개봉했을 가능성이 높으므로 제거해준다.
  for(i in 1:length(CAudience)){ 
  if(CAudience[i]-Audience[i] > 3000){
      week_one_Data <- week_one_Data[-i,]
  }
  }
  
  #1주차 자료에서 관객수에 7을 나눠서 첫날의 관객수로 가정하고, 누적관객수에 8/7을 곱해서 8일차의 관객수로 가정
  Audience <- week_one_Data$Audience
  Audience2 <- Audience/7
  week_one_Data$Audience <- Audience2 # 일주일간의 관객수이기 때문에 7로 나눠서 첫날의 관객수로 만들어줌
  CumulativeAudience <- week_one_Data$CumulativeAudience
  CumulativeAudience2 <- CumulativeAudience*8/7 
  week_one_Data$CumulativeAudience <- CumulativeAudience2 # 누적관객수는 8일차를 구하는 것이기 때문에 첫주차에 8/7을 곱해줌
  
  #1,2주차를 나눴을 때 duplicated를 넣어줬는데 필요 없으므로 제거
  week_one_Data <- week_one_Data[-ncol(week_one_Data)]
  week_two_Data <- week_two_Data[-ncol(week_two_Data)]
  
  #csv파일로 1,2주차를 따로 저장
  write.csv(week_one_Data, file="result/week_one_Data.csv", row.names=TRUE)
  write.csv(week_two_Data, file="result/week_two_Data.csv", row.names=TRUE)
  
  
  rm(list=ls())
