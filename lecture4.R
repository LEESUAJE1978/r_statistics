qt(0.025, df = 14)


hf <- read.table("https://www.randomservices.org/random/data/Galton.txt", header = T, stringsAsFactors = F)
str(hf)
str(hf$Gender)
hf$Gender <- factor(hf$Gender, levels = c("M","F"))
str(hf$Gender)
str(hf)
hf.son <- subset(hf, Gender =="M")
str(hf.son)

par(mar = c(4,4,1,1))
plot(hf.son$Father, hf.son$Height, 
     xlab = "아버지의 키", ylab = "아들의 키", main = "아버지와 아들의 키", col = 3)
abline(v = mean(hf.son$Father), col = 2, lty = 2)
abline(h = mean(hf.son$Height), col = 2, lty = 2)
head(hf)


f.mean <- mean(hf.son$Father) #아버지들의 키 평균
s.mean <- mean(hf.son$Height) #아들들의 키 평균
cov.num <- sum((hf.son$Father - f.mean) * (hf.son$Height - s.mean)) #두 변수의 편차 곱을 합한 값 저장
cov.xy <- cov.num / (nrow(hf.son)-1) # 편차 곱의 합을(자료의 개수 1로)나누고 변수에 저장
cov.xy

cov(hf.son$Father, hf.son$Height) #cov 함수를 이용한 공분산 구하기


(r.xy <- cov.xy / (sd(hf.son$Father) * sd(hf.son$Height))) #식을 활용한 상관 계수 구하기
cor(hf.son$Father, hf.son$Height) #함수를 활용한 상관 계수 구하기


# 그림 9-2
par(mfrow=c(1, 1), mar=c(4, 4, 1, 1))
plot(Height~Father,col = 3, pch=16, data=hf.son, xlab="아버지의 키(인치)", ylab="아들의 키(인치)")
abline(lm(Height~Father, data=hf.son), col="red", lwd=2)


#회귀분석(Regression Analysis)
mean.x <- mean(hf.son$Father) #아버지들의 키 평균
mean.y <- mean(hf.son$Height) #아들들의 키 평균

sxy <- sum((hf.son$Father - mean.x) * (hf.son$Height - mean.y)) #아버지 키의 편차와 아들 키의 편차의 곱의 합
sxx <- sum((hf.son$Father - mean.x)^2) #아버지의 키의 편차 제곱 합
b1 <- sxy / sxx #회귀 계수(기울기) 추정치
b0 <- mean.y - b1 * mean.x #회귀 계수 (절편) 추정치
print(paste('기울기:', b1,'and', '절편:', b0, sep = " "))

out <- lm(Height ~Father, data = hf.son) #회귀 함수를 활용하여 회귀식 구하기

par(mar=c(2,2,2,1))
plot(Height~Father, data=hf.son, main="", xlab="아버지의 키(인치)", ylab="아들의 키(인치)", ylim=c(65, 75))
abline(out, lwd=1.5)


