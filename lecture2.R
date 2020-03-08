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

n <- 6
p <- 1/3
x <- 0:n

(dbinom(2, size = n, prob = p))
(dbinom(4, size = n, prob = p))

px <- dbinom(x, size = n, prob = p)
plot(x, px, type = 's', xlab = '성공횟수(x)', ylab = "확률(P[X=x])",
     main = "B(6, 1/3)")

help("dbinom")
