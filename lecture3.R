#정규분포 

#0. 평균이 170이 분산 6인 자료, 다른 표현으로는 확률변수 X가 N(170, 6)을 따르는 자료 작성

options(digits = 3)
mu <- 170 #평균
sigma <-6 #분산
ll <- mu - 3*sigma #3배의 표준편차 3시그마 범위
ul <- mu + 3*sigma
print(ul)

#1. 평균이 170이 분산 6인 확률분포 그래프 작성 :dnrom()
x <- seq(ll, ul, by = 1) #확률변수 X 생성
nd <- dnorm(x, mean = mu, sd = sigma) ##평균 mu 분산이 sigma를 따르는 확률 변수 x가 발생할 확률
plot(x, nd,type = 'l', xlab = 'x', ylab = "P(X=x)", lwd = 2, col = 'red') # 확률변수 x가 평균mu와 분산 sigma의 조건에서 발생할 확률을 표시한 그래프
sum(nd)


#2. 평균이 170이 분산 6인 확률분포 함수 값 계산 :pnorm()
pnorm(mu, mean = mu, sd = sigma) #확률 변수 X가 평균 mu, 분산 sigma를 따를 때 X가 평균보다 작을 확률
pnorm(ll, mean = mu, sd = sigma)#확률 변수 X가 평균 mu, 분산 sigma를 따를 때 X가 152보다 작을 확률
pnorm(ul, mean = mu, sd = sigma) #확률 변수 X가 평균 mu, 분산 sigma를 따를 때 X가 188보다 작을 확률
pnorm(180, mean = mu, sd = sigma) - pnorm(175, mean = mu, sd = sigma) #확률 변수 X가 평균 mu, 분산 sigma를 따를 때 X가 152보다 크고 188보다 작을 확률


#3. 평균이 170이 분산 6인 확률분포 분위수 확인: qnorm()
qnorm(0.25, mean = mu, sd = sigma)
qnorm(0.5, mean = mu, sd = sigma)
qnorm(0.75, mean = mu, sd = sigma)
qnorm(c(0.25, 0.5, 0.75), mean = mu, sd = sigma)


#4. 평균이 170 분산이 6인 모집단에서 표본 추출: rnorm
par(family = "AppleGothic")
options(digits = 5)
set.seed(5)
smp <- rnorm(400, mean = mu, sd = sigma)
hist(smp, probability = T,
     main = "N(170, 6^2)으로 부터 추출한 표본의 분포(n=400)",
     xlab = "", ylab = "", col = "white", border = "black")
lines(x, nd, lty = 2, col = 'red')


#표준 정규 분포
option(digit = 4)
mu <-0
sigma <-1
ll <- mu - 3 * sigma
ul <- mu + 3 * sigma
d
x <- seq(ll, ul, by = 0.01)
nd <- dnorm(x, mean = mu, sd = sigma)
plot(x, nd, type = 'l', xlab = "x", ylab = "P(X=x)", lwd = 2, col = 'blue' )
x

p0.05 <-qnorm(0.05, mean = mu, sd = sigma)
p0.025 <- qnorm(0.025, mean = mu, sd =sigma)

pnorm(1.645) - pnorm(-1.645)
pnorm(1.96) - pnorm(-1.96)

x <- seq()
dnorm()