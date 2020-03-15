#1. 정규분포 

#1.1. 평균이 170이 분산 6인 자료, 다른 표현으로는 확률변수 X가 N(170, 6)을 따르는 자료 작성

options(digits = 3)
mu <- 170 #평균
sigma <-6 #분산
ll <- mu - 3*sigma #3배의 표준편차 3시그마 범위
ul <- mu + 3*sigma
print(ul)

#1.2. 평균이 170이 분산 6인 확률분포 그래프 작성 :dnrom()
x <- seq(ll, ul, by = 0.01) #확률변수 X 생성
nd <- dnorm(x, mean = mu, sd = sigma) ##평균 mu 분산이 sigma를 따르는 확률 변수 x가 발생할 확률
plot(x, nd,type = 'l', xlab = 'x', ylab = "P(X=x)", lwd = 2, col = 'red') # 확률변수 x가 평균mu와 분산 sigma의 조건에서 발생할 확률을 표시한 그래프
sum(nd)


#1.3.평균이 170이 분산 6인 확률분포 함수 값 계산 :pnorm()
pnorm(mu, mean = mu, sd = sigma) #확률 변수 X가 평균 mu, 분산 sigma를 따를 때 X가 평균보다 작을 확률
pnorm(ll, mean = mu, sd = sigma)#확률 변수 X가 평균 mu, 분산 sigma를 따를 때 X가 152보다 작을 확률
pnorm(ul, mean = mu, sd = sigma) #확률 변수 X가 평균 mu, 분산 sigma를 따를 때 X가 188보다 작을 확률
pnorm(180, mean = mu, sd = sigma) - pnorm(175, mean = mu, sd = sigma) #확률 변수 X가 평균 mu, 분산 sigma를 따를 때 X가 152보다 크고 188보다 작을 확률


#1.4. 평균이 170이 분산 6인 확률분포 분위수 확인: qnorm()
qnorm(0.25, mean = mu, sd = sigma)
qnorm(0.5, mean = mu, sd = sigma)
qnorm(0.75, mean = mu, sd = sigma)
qnorm(c(0.25, 0.5, 0.75), mean = mu, sd = sigma)


#1.5. 평균이 170 분산이 6인 모집단에서 표본 추출: rnorm
par(family = "AppleGothic") #맥북의 경우 한글 깨짐 방지를 위해 필요
options(digits = 5)
set.seed(5)
smp <- rnorm(400, mean = mu, sd = sigma)
hist(smp, probability = T,
     main = "N(170, 6^2)으로 부터 추출한 표본의 분포(n=400)",
     xlab = "", ylab = "", col = "white", border = "black")
lines(x, nd, lty = 2, col = 'red')


#1.5 표준 정규 분포
 
option(digit = 4)
mu <-0
sigma <-1
ll <- mu - 3 * sigma
ul <- mu + 3 * sigma

x <- seq(ll, ul, by = 0.01)
nd <- dnorm(x, mean = mu, sd = sigma)
plot(x, nd, type = 'l', xlab = "x", ylab = "P(X=x)", lwd = 2, col = 'blue' )

pnorm(180, mean = mu, sd = sigma) - pnorm(175, mean = mu, sd = sigma)

p0.05 <-qnorm(0.05, mean = mu, sd = sigma)
p0.025 <- qnorm(0.025, mean = mu, sd =sigma)

pnorm(1.645) - pnorm(-1.645)
pnorm(1.96) - pnorm(-1.96)

x <- seq()
dnorm()

#2. 표본 평균 x의 분포

#2.1 각 표본의 표본 평균 값을 저장할 벡터 생성
m10 <- rep(NA, 1000) #표본 크기가 10인 표본을 1000번 추출
m40 <- rep(NA, 1000) #표본 크기가 40인 표본을 1000번 추출

set.seed(9) #동일한 난수 생성을 위해 난수 생성 값 고정

#2.2. 각 표본의 표본 평균 생성 모집단 학생들의 평균 키는 170, 표준 편차는 6
for(i in 1:1000){
  m10[i] <- mean(rnorm(10, mean = 170, sd = 6)) #학생 키의 평균이 170, 표준편차가 6인 학생 집단에서 10명의 학생을 한 표본으로  하여 이 표본의 평균을 1000번 생성
  m40[i] <- mean(rnorm(40, mean = 170, sd = 6)) #학생 키의 평균이 170, 표준편차가 6인 학생 집단에서 40명의 학생을 한 표본으로  하여 이 표본의 평균을 1000번 생성
} 

#2.3. 각 표본평균의 평균과 표준 편차 구하기
options(digits = 4) #자리수 설정, 출력 숫자의 자리수를 4자리로
c(mean(m10), sd(m10)) #10명의 학생들을 한 표본으로 한 표본 평균 분포의 평균과 편차
c(mean(m40), sd(m40)) #40명의 학생들을 한 표본으로 한 표본 평균 분포의 평균과 편차

#각 표본 평균의 분포 그래프 그리기
hist(m10, xlim = c(160, 180), main = "", xlab = "x", ylab = "", col = 'cyan', border = "blue") #10명 학생들을 한 표본으로 한 표본 평균들의 분포 그래프
hist(m40, xlim = c(160, 180), main = "", xlab = "x", ylab = "", col = 'cyan', border = "blue") #40명 학생들을 한 표본으로 한 표본 평균들의 분포 그래프



#3. 중심극한 정리

#3.1. 모집단이 정규 분포일 때 추출된 표본 평균 x의 분포
#서로 다른 정규분포 1.N(3, 1의 제곱) 2.N(170, 6의 제곱)로 부터 표본 크기가 4인 표본을 1000번 추출

set.seed(123) #난수 고정
n <- 1000 #추출 횟수

r.1.mean = rep(NA, n) #N(3, 1의 제곱)의 표본 평균을 저장할 벡터
r.2.mean = rep(NA, n) #N(170, 6의 제곱)의 표본 평균을 저장할 벡터

for (i in 1:n){
  r.1.mean[i] = mean(rnorm(4, mean = 3, sd=1))
  r.2.mean[i] = mean(rnorm(4, mean = 170, sd = 6))
} #각 표본의 표본 평균을 1000번 구하여 각각에 저장

options(digits = 4)
c(mean(r.1.mean), sd(r.1.mean)) #1번 모집단의 표본 평균의 평균과 표준 편차 값
c(mean(r.2.mean), sd(r.2.mean)) #2번 모집단의 표본 평균의 평균과 표준 편차 값

hist(r.1.mean, prob = T, xlab = "표본평균", ylab = "밀도",
     main = "", col = "orange", border = "red")
x1 <- seq(min(r.1.mean), max(r.1.mean), length = 1000)
y1 <- dnorm(x=x1, mean = 3, sd =(1/sqrt(4)))
lines(x1, y1, lty =2, lwd=2, col ="blue")


hist(r.2.mean, prob = T, xlab = "표본평균", ylab = "밀도",
     main = "", col = "orange", border = "red")
x2 <- seq(min(r.2.mean), max(r.2.mean), length = 1000)
y2 <- dnorm(x=x2, mean = 170, sd =(6/sqrt(4)))
lines(x2, y2, lty =2, lwd=2, col ="blue")


#3.2. 모집단이 정규 분포가 아닌 임의의 분포일 때 표본평균의 분포

set.seed(12)
t<-10
p<- 0.1
#x<-0:10
n<-1000
b.2.mean <- rep(NA, n)
b.4.mean <- rep(NA, n)
b.32.mean <- rep(NA, n)

for(i in 1:n){
  b.2.mean[i] <-mean(rbinom(2, size = t, prob = p))
  b.4.mean[i] <-mean(rbinom(4, size = t, prob = p))
  b.32.mean[i] <-mean(rbinom(32, size = t, prob = p))
}

c(mean(b.2.mean), sd(b.2.mean))
c(mean(b.4.mean), sd(b.4.mean))
c(mean(b.32.mean), sd(b.32.mean))

hist(b.2.mean, probability = T, xlim = c(0,4), main = "표본크기:2", col ="orange", border = 'red')
x1 <- seq(min(b.2.mean), max(b.2.mean), length = 1000)
y1 <- dnorm(x=x1, mean = 1, sd = sqrt(0.9)/sqrt(2))
lines(x1, y1, lty = 2, lwd = 2, col = 'blue')


hist(b.4.mean, probability = T, xlim = c(0,4), main = "표본크기:4", col ="orange", border = 'red')
x2 <- seq(min(b.4.mean), max(b.4.mean), length = 1000)
y2 <- dnorm(x = x2, mean = 1, sd = sqrt(0.9)/sqrt(4))
lines(x2, y2, lty = 2, lwd = 2, col = 'blue')

hist(b.32.mean, probability = T, xlim = c(0,4), main = "표본크기:32", col ="orange", border = 'red')
x3 <- seq(min(b.32.mean), max(b.32.mean), length = 1000)
y3 <- dnorm(x = x3, mean = 1, sd = sqrt(0.9)/sqrt(32))
lines(x3, y3, lty = 2, lwd = 2, col = 'blue')

#4. 유효성
x <- seq(-3, 3, by = 0.01)
y <- dnorm(x)
y.1 <- dnorm(x, sd = sqrt(1/3))
y.2 <- dnorm(x, sd = sqrt(7/18))

pnorm(0.1, sd = sqrt(1/3)) - pnorm(-0.1, sd = sqrt(1/3))
pnorm(0.1, sd = sqrt(7/18)) - pnorm(-0.1, sd = sqrt(7/18))

plot(x, y, type ='l', ylim = c(0,0.8), axes = F, ylab = "",lwd = 3, col = "yellow")
lines(x, y.1, col ='red', lwd =3)
lines(x, y.2, col = 'green', lty=2, lwd=3)
axis(1)

#5. 구간 추정

#5.1. 신뢰구간 의미 파악
set.seed(123)
n <- 10
x <- 1:100
y <- seq(-3,3, by = 0.01)

smps <- matrix(rnorm(n*length(x)), ncol = n)

xbar <- apply(smps, 1, mean)
se <- 1 /sqrt(10)
alpha <- 0.05
z <- qnorm(1 - alpha/2)
ll <- xbar -z *se
ul <- xbar +z *se

plot(y, type = "n", xlab = "trial", ylab = "Z",
     main = "95% Confidence Interval for Populatoin mean",
     xlim = c(1, 100), ylim = c(-1.5, 1.5), cex.lab = 1.8)
abline(h = 0, col = "red", lty = 2)
l.c <- rep(NA, length(x))
l.c <- ifelse(ll * ul > 0, "red", "black")
arrows(1:length(x), ll, 1:length (x), ul, code = 3,
       angle= 90, length = 0.02, col = l.c, lwd = 1.5)


#모분산을 모를 때 95% 신뢰구간
ci.t <- function(x, alpha=0.05){
  n <- length(smp)
  m <- mean(x)
  s <- sd(x)
  t <- qt(1 - (alpha /2), df = n-1)
  ll <- m - t * (s / sqrt(n))
  ul <- m + t * (s /sqrt(n))
  ci <- c(1- alpha, ll, m, ul)
  names(ci) <- c("Confidence Level", "Lower limit", "Mean", "Upper limit")
  return(ci)
}

smp <- c(520, 498, 481, 512, 515, 542, 520, 518, 527, 526)
ci.t(smp)
ci.t(smp, 0.1)
