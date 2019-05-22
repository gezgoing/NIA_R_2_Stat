# D. 프로세스별 데이터셋 구성 및 분석단계 가이드
# Part 1

## D.1. 예측 모형에 기반한 분석

### D.1.1 회귀분석

# 1. 산점도 작성
data(cars)
summary(cars)
plot(cars$speed, cars$dist)

# 2. 모형 적합(model fitting)
res.lm <- lm(dist ~ speed, data=cars)
summary(res.lm)

# 축 제목을 수정하고 적합된 회귀함수 그래프를 겹쳐 그림 
plot(cars$speed, cars$dist, xlab="Speed", ylab="Distance to stop", 
     xlim=c(0, 30), ylim=c(0, 125))
abline(coef(res.lm), col=2)

# 3. 회귀 진단 - 잔차 분석
par(mfrow=c(2,2))
plot(res.lm)      # Diagnostic plots
par(mfrow=c(1,1))

# 4. 모형 수정 및 확정
res.lm2 <- lm(dist ~ speed + I(speed^2) - 1, data=cars)   
summary(res.lm2)

plot(cars$speed, cars$dist, xlab="Speed", ylab="Distance to stop", 
     xlim=c(0, 30), ylim=c(0, 125))
abline(coef(res.lm), col=4)
x <- seq(from=0, to=30, by=0.01)
lines(x, res.lm2$coefficients[1]*x+res.lm2$coefficients[2]*x*x, col=2)
points(cars$speed, predict(res.lm2), col=2, pch=19)

par(mfrow=c(2,2))
plot(res.lm2)      # New diagnostic plots
par(mfrow=c(1,1))

# 5. 예측에 활용
newdata <- data.frame(speed=c(15, 20))
predict(res.lm2, newdata)   # 최종 회귀식인 이차식에 의한 예측

predict(res.lm, newdata)    # 일차식에 의한 예측


### D.1.2 다중회귀분석

# MASS 패키지의 Cars93 데이터셋을 불러와 자료 구조 파악
library(MASS)
data(Cars93)
str(Cars93)

# lm를 이용해 다중회귀모형 적합
res.lm <- lm(Price ~ Type + AirBags + Cylinders + Man.trans.avail, data=Cars93)
summary(res.lm)

# 회귀분석 결과에 따른 가격 예측치 산출
newdata <- data.frame(Type='Large', AirBags='Driver only', Cylinders='6', Man.trans.avail='Yes')
predict(res.lm, newdata)

# 변수선택(step)
step <- stepAIC(res.lm, direction="both")
anova(step)

# 다중 공선성(multiple collinearity)
#   - $x$ 변수 간 선형종속관계가 심한 경우 분석이 어려움  
#   - 탐지 방법: 추정된 회귀계수 중 얼토당토 않은 값이 포함된 경우, 또는 분산팽창계수(variance inflation factor, VIF)가 10보다 큰 변수가 있으면 다중공선성이 있다고 봄  
#   - 해결 방안: 문제가 되는 변수를 제거하거나, 주성분회귀(principal component regression), 능형회귀(ridge regression) 등의 방법 사용   
#   - 아래 코드는 `car` 패키지의 내장 데이터셋인 `mtcars`의 mpg(연비)를 반응변수로 하는 회귀모형의 다중공선성에 대한 예임  

library(car)
str(mtcars)
fit <- lm(mpg~., data=mtcars)
summary(fit)

vif(fit)


### D.1.3 비모수적 회귀분석

#### 커널 평활법: 국소선형회귀 (kernel smoothing: local linear regression)

# 국소선형회귀 설명
set.seed(1)
n <- 100
X <- runif(n)*4
Y <- sin(X) + rnorm(n, sd=.3)
x <- seq(from=0, to=4, by=0.005)
y <- sin(x)
plot(x, y, type='l', ylim=c(min(Y), max(Y)), lty=2)
points(X, Y)

x0 <- 2
x0nb <- seq(from=x0-1/2, to=x0+1/2, by=0.0025)
ll <- sin(x0)+cos(x0)*(x0nb-x0)
lines(x0nb, ll, col=4)
points(x0, sin(x0), pch=19, col=4)
lines(x0nb, 0.3*dnorm(x0nb, mean=x0, sd=0.2)+min(Y), lty=2, col="lightgray", lwd=2)
lines(x0nb, rep(min(Y), length(x0nb)), col="lightgray")
abline(v=x0-1/2, lty=2, col="lightgray")
abline(v=x0+1/2, lty=2, col="lightgray")
abline(v=x0, col="lightgray")
points(X[abs(X-x0)<=.5], Y[abs(X-x0)<=.5], col=2)
#    res <- lm(Y[abs(X-x0)<=.5]~I(X[abs(X-x0)<=.5]-x0))
#    lines(x0nb, coef(res)[1]+coef(res)[2]*(x0nb-x0), col=2)

# KernSmooth 패키지의 locpoly()함수 이용 예제
set.seed(1)
n <- 100
X <- runif(n)*4
Y <- sin(X) + rnorm(n, sd=.3)

x <- seq(from=0, to=4, by=0.005)
y <- sin(x)

windows()
RStudioGD()

plot(x, y, type='l', ylim=c(min(Y), max(Y)), lty=2)
points(X, Y)

library(KernSmooth)

h <- c(0.1, 0.3, 1.5)

for ( k in 1:length(h) ) {
  res.lp <- locpoly(X, Y, bandwidth=h[k])
  lines(res.lp, col=k+1)
}
legend(3, 1.5, c(paste("bandwidth = ",h[1]), paste("bandwidth = ",h[2]), 
                 paste("bandwidth = ",h[3]), "True curve"),
       col=c(2:4, 1), lty=c(rep(1, 3), 2))

res.lm <- lm(Y~X)
points(X, predict(res.lm), col=8, pch="*")

# 실제 자료 분석: MASS 패키지의 built-in 데이터셋인 mcycle 분석
data(mcycle)
str(mcycle)
plot(mcycle)

res.lp <- with(mcycle, locpoly(times, accel, bandwidth=1.5))
lines(res.lp, col=2)


### D.1.4 로지스틱회귀분석
library(faraway) # install.packages("faraway")

data(orings)
str(orings)
? glm
res.glm <- glm(cbind(damage, 6-damage) ~ temp, family=binomial, data=orings)
summary(res.glm)

plot(damage/6 ~ temp, xlim=c(25, 85), ylim=c(0, 1), xlab="Temperature", ylab="Pr(damage)", data=orings)
x <- seq(from=25, to=85, by=0.01)
beta <- res.glm$coefficients
lines(x, ilogit(beta[1]+beta[2]*x), col=2)
?ilogit

predict(res.glm, newdata=data.frame(temp=31), type="response")

