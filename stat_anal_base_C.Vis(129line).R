# C. 데이터 시각화 단계 가이드

## C.1. 기본 도표 및 그래프

set.seed(1)
Blood.Type <- sample(x=1:4, size=30, replace=TRUE)
Blood.Type <- c("A", "B", "AB", "O")[Blood.Type]
Blood.Type <- factor(Blood.Type)
Blood.Type
table(Blood.Type)                          # frequency table
table(Blood.Type)/length(Blood.Type)       # relative frequency table

library(MASS)
attach(quine)
table(Age)
par(mfrow=c(2,2))
barplot(table(Blood.Type), col=3, xlab="Blood types", ylab="Frequency")
barplot(table(Blood.Type)/length(Blood.Type), col=3, xlab="Blood types", ylab="Relative frequency")
barplot(table(Age), col=7, xlab="Age", ylab="Frequency")
barplot(table(Age)/length(Age), col=7, xlab="Age", ylab="Relative frequency")
par(mfrow=c(1,2))
pie(table(Blood.Type))
title("Blood types")
pie(table(Age))
title("Age")
par(mfrow=c(1,1))

data(iris)
head(iris)
x <- iris$Sepal.Length
hist(x, prob=TRUE, col="skyblue", border="white")
lines(density(x))
boxplot(x)          # boxplot




## C.2. 자료 분포의 요약

x <- c(1, 2, 4, 5, 7)
mean(x)

x <- c(1, 2, 4, 5, 70)   # 70: 오타에 의한 이상점 
mean(x)

x <- c(1, 2, 4, 5, 70)   # 70: 오타에 의한 이상점 
median(x)

x <- iris$Sepal.Length
mean(x)             # 평균
median(x)           # 중앙값
mean(x, trim=0.1)   # 절사평균: 크기순으로 배열하여 양쪽 극단의 10%(= 0.1) 값들을 버린 후 평균

quantile(x, probs=c(0.1, 0.25, 0.5, 0.75, 0.9))

sd(x)             # 표준편차
var(x)            # 분산
diff(range(x))    # 범위
IQR(x)            # 사분위수 범위 = quantile(x, 0.75) - quantile(x, 0.25)
IQR(rnorm(10000)) # 1.35 정도

pairs(iris[iris$Species=="virginica",-5], panel = panel.smooth)
cor(iris[iris$Species=="virginica",-5]) # Pearson 상관계수 (default)
cor(iris[iris$Species=="virginica",-5], method="kendall")
cor(iris[iris$Species=="virginica",-5], method="spearman")

boxplot(Petal.Length ~ Species, data = iris, col = "lightgray")




## C.3. 모집단, 표본, 표본분포

mu <- 1/2; sig.sq <- 1/12    # Unif(0,1)의 평균, 분산
# https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)

par(mfrow=c(1,2))

n <- 100
X.bar <- NULL
for ( k in 1:10000) {
  X <- runif(n)
  X.bar <- c(X.bar, mean(X))
}
hist(X.bar, prob=T, col="gray", border="white",
     xlim=c(0.35, 0.65), ylim=c(0, 30),
     xlab="Sample Means", main="Histogram\n of sample means", sub=paste("n =",n))
rug(X.bar, col="orange")
z <- seq(from=0, to=1, by=0.001)
lines(z, dnorm(z, mean=mu, sd=sqrt(sig.sq/n)), col=2)

n <- 400
X.bar <- NULL 
for ( k in 1:10000) { 
  X <- runif(n) 
  X.bar <- c(X.bar, mean(X)) 
} 
hist(X.bar, prob=T, col="gray", border="white",          
     xlim=c(0.35, 0.65), ylim=c(0, 30),
     xlab="Sample Means", main="Histogram\n of sample means", sub=paste("n =",n)) 
rug(X.bar, col="orange")
z <- seq(from=0, to=1, by=0.001)
lines(z, dnorm(z, mean=mu, sd=sqrt(sig.sq/n)), col=2)




## C.4. ggplot2를 이용한 고급 시각화

### C.4.1 Example을 통한 기초 이해

str(iris)
head(iris)

library(ggplot2)
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()

myplot <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width))
myplot + geom_point()

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(size = 3)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + 
  geom_point(size = 3)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + 
  geom_point(aes(shape = Species), size = 3)

