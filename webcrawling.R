install.packages('rvest')
library(rvest)
library(stringr)

setwd("C:/Users/Yang DongWook/Desktop/data")
week_one_Data <- read.csv("result/week_one_Data.csv",head=T, fileEncoding="euc-kr")

grade <- data.frame() # 평점 저장할 변수
Mn <- week_one_Data$MovieName # 1주차 자료에서 영화이름을 변수에 저자
urlbase <- "https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=1&ie=utf8&query="
for(i in 1:10){ # 인코딩, 스페이스바가 있을 경우 +로 대체
  Mn2 <- str_replace(Mn2," ","+")
}
Mn3 <- str_replace(Mn2,":","%3A") # 인코딩, :가 있을 경우 %3A로 대체
Mn4 <- str_c("영화+",Mn3) # 검색을 더 잘 되게 하기 위해 앞에 영화를 붙여줌
url <- str_c(urlbase,Mn4) # url로 만들어줌
for(i in 1:length(Mn)){ # 각각 영화명에 해당하는 평점을 grade에 저장
  url_one <- url[i]
  used <- read_html(url_one)
used <- used %>% html_nodes(".r_grade em") %>% html_text() %>% data.frame()
used <- used[1,]
used <- as.character(used)
used <- as.numeric(used)
grade <- rbind(grade,used)
  
}
colnames(grade) <- "grade" 
result <- cbind(grade,Mn) 
result
write.csv(result, file="result/grade.csv", row.names=TRUE) # 평점을 따로 csv파일로 만들어줌




