install.packages('dplyr') # EDA 를 위한 dplyr 패키지 설치
install.packages('readxl') #엑셀 파일 불러오기 위한 패키지 설치

require(dplyr) #패키지 블러오기, 설치된 패키지라도 사용할 때 마다 불러와야 한다. 
require(readxl)#패키지 블러오기


setwd('C:/Users/tkpeo/Documents/r_statistics') #working directory 세팅, 파일을 저장해 놓은 경로를 설정한다. 


#0. 데이터 불러오기 
covid <- read_excel('covid19.xlsx') #파일 불러오기
is.data.frame(covid) #데이터 형태 확인

#1. 데이터 구조 확인
dim(covid) #데이터 프레임의 행과 열의 개수 확인 360행, 5개 열
nrow(covid) #행의 개숫 확인 360행 
ncol(covid) #열의 개수 확인 5컬럼
head(covid) #상위 6개의 값 확인
tail(covid) #하위 6개 값 확인
names(covid) #컬럼명 확인
names(covid) <- c('time', 'location', 'state1','state2','count')#컬럼명 바꾸기


#2. 결측치 확인
is.na(covid) #전체 확인
sum(is.na(covid))# 전체 na 값합계 확인
colSums(is.na(covid)) #컬럼별 na 값 확인

#3. 날짜 데이터 다루기
install.packages('lubridate') #날짜 다루기 위한 패키지 설치
require(lubridate)#패키지 불러오기
covid$newdate <- date(covid$time) #일자별 발표 시간은 매일 동일하여 시간 제외한 날짜 데이터 생성


#4. 2월 29일 지역별 검사현황 및 확진자 현황
covid %>% filter(newdate =="2020-02-29") %>% 
  group_by(location, state1) %>% 
  summarise(total = sum(count),  #검사 현황 및 확진자 현황
            mean = mean(count), # 검사 현황 및 확진자 평균
            var = var(count), #검사 현황 및 확진자 분산
            std = sd(count), #검사 현황 및 확진자 표준편차
            '25%' = quantile(count, probs = 0.25), #검사 현황 및 확진자 4분위수
            '50%' = quantile(count, probs = 0.5),
            '75%' = quantile(count, probs = 0.75),
            iqr = IQR(count)) #검사 현황 및 확진자 IQR Q3-Q1

descriptive <- covid %>% filter(newdate == '2020-02-29') %>%  #descriptive 변수로 저장하기
  group_by(location, state1) %>% 
  summarise(total = sum(count), 
            mean = mean(count), 
            var = var(count), 
            std = sd(count), 
            '25%' = quantile(count, probs = 0.25),
            '50%' = quantile(count, probs = 0.5),
            '75%' = quantile(count, probs = 0.75),
            iqr = IQR(count))


#5. 지역별 확진 및 검사 현황 그래프 그리기

require(ggplot2)
ggplot(data = descriptive, aes(x = location, y = total, fill = state1))+
  geom_bar(stat = 'identity')+
  ggtitle('코로나19 현황')+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = descriptive %>% filter(state1 =='확진'), aes(x = location, y = total))+
  geom_bar(stat = 'identity')+
  ggtitle('코로나19 확진현황')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = descriptive %>% filter(state1 =='검사현황'), aes(x = location, y = total))+
  geom_bar(stat = 'identity')+
  ggtitle('코로나19 검진현황')+
  theme(plot.title = element_text(hjust = 0.5))


