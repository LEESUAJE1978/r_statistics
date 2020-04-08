install.packages('prob')
library(prob)

tosscoin(1)
rolldie(1)
urnsamples(1:3, size = 2)
urnsamples(1:3, size = 2, replace = T)
urnsamples(c(rep("R",3), rep("B",2)), size = 2)
tosscoin(3, makespace = T)


#기댓값 구하기
x <- c(0, 1, 2)
px <- c(1/4, 2/4, 1/4)
EX <- sum(x*px)
EX

#분산 구하기
VX <- sum(x^2 *px) -EX^2
VX


#이항 분포의 확률 계산
#dbinorr, density, the density function(p.f. or p.d.f) 확률 변수 x의 확률
n <- 6   #시행 횟수 6번
p <- 1/3 #성공 확률 1/3
x <- 0:n #확률 변수 X 가 가질 수 있는 값

(dbinom(2, size = n, prob = p)) #확률 변수 X=2일 확률 P(X = 2)
(dbinom(4, size = n, prob = p)) #확률 변수 X=4일 확률 P(X = 4)

px <- dbinom(x, size = n, prob = p) #확률 변수(성공 횟수)별 확률
plot(x, px, type = 'h', xlab = '성공횟수(x)', ylab = "확률(P[X=x])",
     main = "B(6, 1/3)") #성공 횟수와 각각의 확률 그래프로 보기

help("dbinom") #dbinom 상세 내용 보기


#pbinorm, probability, the cumulative distribution function 확률 변수 X가 특정 값 이하일 확률 P(X<=2)을 구함
pbinom(2, size = n, prob = p) #성공횟수가 2 이하일 확률
pbinom(4, size = n, prob = p) #성공횟수가 4 이하인 확률
pbinom(4, size = n, prob = p) - pbinom(2, size = n, prob = p) #성공횟수가 2초과(3이상) 4 이하인 확률


#qbinom, quantile, the inverse c.d.f 사분위 수의 값 구하기. C.D.F 의 반대, 분위수에 해당하는 확률 변수 X의 값 x를 구함
qbinom(0.75, size = n, prob = p ) #확률 변수 X의 값이 0일 경우 CDF가 0.09, 1일 경우 CDF가 0.35임, 0~0.09 미만은 0, 0.09부터 0.35미만까지는 1이 선택됨
qbinom(0.2, size = n, prob = p) #확률 변수 x의 값이 2일 경우 CDF가 0.68임. 0.35~0.68미만은 확률 변수의 값은 2가 선택됨


#rbinom, 이항 분포를 따르는 확률 표본 추출, n번 시도 했을 때 성공 확률 1/3인 경우 확률 변수 X(성공 횟수)의 값을 임의로 추출, 이 과정을 15번 진행
rbinom(15, size = n, prob = p)


#기댓값과 분산
(ex <- sum(px * x))
ex2 <- sum(x^2 * px) #확률 변수 각각의 값에 대한 제곱 값
(varx <- ex2 - ex^2) #이항 분포의 분산 



#ggplot을 활용한 이항분포 그리기
require(ggplot2)
nn = 10000 #실험 반복 횟수 또는 실험 관찰 횟수
pp = 1/10 #성공 확률
ss = 10 #시행횟수

binomdata_ten <-data.frame(Successes = rbinom(n = nn, size = ss, prob = p), size ='ten')
binomdata_thousand <-data.frame(Successes = rbinom(n = nn, size = ss*100, prob = p), size ='thousand')
binomdata_million <-data.frame(Successes = rbinom(n = nn, size = ss*100000, prob = p), size = 'million')
binomdata_billion <-data.frame(Successes = rbinom(n = nn, size = ss*100000000, prob = p), size = 'billion')
binomall<- rbind(binomdata_ten,binomdata_thousand, binomdata_million, binomdata_billion)

ggplot2::ggplot(binomall, aes(x=Successes))+geom_histogram(bins=30)+
  facet_wrap(~size, scales = 'free')


#본 소스는 제대로 알고쓰는 R 통계 분석, 이윤환, 한빛 아카데이, 2018 내용을 기반으로 합니다.