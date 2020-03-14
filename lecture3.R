#정규분포 
#0. 평균이 170이 분산 6인 자료, 다른 표현으로는 확률변수 X가 N(170, 6)을 따름

options(digits = 3)
mu <- 170 #평균
sigma <-6 #분산
ll <- mu - 3*sigma #3배의 표준편차 3시그마 범위
ul <- mu + 3*sigma

x <- seq(ll, ul, by = 1) #확률변수 X 생성
nd <- dnorm(x, mean = mu, sd = sigma) #확률ㅂ평균이 mu이고  분산이 sigma 정규분포
plot(x, nd,type = 'l', xlab = 'x', ylab = "P(X=x)", lwd = 2, col = 'red')

sum(nd)
pnorm(mu, mean = mu, sd = sigma)
pnorm(ll, mean = mu, sd = sigma)
pnorm(ul, mean = mu, sd = sigma)
pnorm(180, mean = mu, sd = sigma) - pnorm(175, mean = mu, sd = sigma)

qnorm(0.25, mean = mu, sd = sigma)
qnorm(0.5, mean = mu, sd = sigma)
qnorm(0.75, mean = mu, sd = sigma)
qnorm(c(0.25, 0.5, 0.75), mean = mu, sd = sigma)

par(family = "AppleGothic")
options(digits = 5)
set.seed(5)
smp <- rnorm(400, mean = mu, sd = sigma)
hist(smp, probability = T,
     main = "N(170, 6^2)으로 부터 추출한 표본의 분포(n=400)",
     xlab = "", ylab = "", col = "white", border = "black")
lines(x, nd, lty = 2, col = 'red')
