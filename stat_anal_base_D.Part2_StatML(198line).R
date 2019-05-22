# D. 프로세스별 데이터셋 구성 및 분석단계 가이드
# Part 2

## D.2. 통계적 기계 학습

# 통계적 학습(statistical learning)이란 데이터 내 구조를 이해하기 위한 다양한 도구를 통칭한다. 크게 지도학습과 자율학습으로 분류

### D.2.1 지도학습에 의한 분류

#### 로지스틱 회귀 이용법  

# 데이터 불러오기
credit <- read.csv("./Data/germancredit.csv", fileEncoding="UTF-8")
dim(credit)

# 불러온 자료에 포함된 몇몇 변수에 대해 알기 쉬운 표현으로 수준(level) 부여
credit$history <- factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) <- c("good","good","poor","poor","terrible")
credit$foreign <- factor(credit$foreign, 
                         levels=c("A201","A202"),
                         labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, 
                         levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

# 분석에 사용할 변수만 잘라내어 새로운 데이터셋 생성
credit <- credit[,c("Default","duration","amount","installment","age","history", "purpose","foreign","rent")]
summary(credit) # check out the data
head(credit,2)

# 디자인 행렬 생성
# dummy variables를 이용해 범주형 변수를 0과 1의 숫자형으로 변환
Xcred <- model.matrix(Default~.,data=credit)[,-1]
head(Xcred,2)
unique(credit$history)
unique(credit$purpose)
unique(credit$foreign)
unique(credit$rent)

# 분류모형의 예측력 평가를 위해 데이터셋 분리
set.seed(1)
train <- sample(1:1000,900)
xtrain <- Xcred[train,]
xnew <- Xcred[-train,]
ytrain <- credit$Default[train]
ynew <- credit$Default[-train]

train[1:10]

# training set에 대해 로지스틱 회귀를 적용해 예측 모형 구축
cred.glm <- glm(Default~.,family=binomial, data=data.frame(Default=ytrain, xtrain))
summary(cred.glm)

# test set으로 예측력 평가
ptest <- predict(cred.glm, newdata=data.frame(xnew), type="response")
predict(cred.glm, newdata=data.frame(xnew)[1:6,], type="terms")
dat <- data.frame(labels=ynew, preds=ptest)
head(dat)

cut <- 1/6
gg1 <- as.numeric(ptest >= cut)
ttt <- table(ynew, gg1)
print(ttt)

# 예측 성능을 sensitivity, specificity 측면에서 확인
truepos <- ynew==1 & ptest>=cut 
trueneg <- ynew==0 & ptest<cut

# Sensitivity (predict default when it does happen)
sensitivity <- sum(truepos)/sum(ynew==1) 

# Specificity (predict no default when it does not happen)
specificity <- sum(trueneg)/sum(ynew==0) 

print(c(sensitivity, specificity))

# ROC curve
library(ROCR)

pred <- prediction(dat$preds, dat$labels)
perf <- performance(pred, "sens", "fpr")
plot(perf)
points(1-specificity, sensitivity, col=2, pch=15)

library(pROC)
plot(roc(dat$labels,dat$preds))
plot.roc(dat$labels,dat$preds)

par(mfrow=c(1,3))
plot(perf)
plot(roc(dat$labels,dat$preds))
plot.roc(dat$labels,dat$preds)
par(mfrow=c(1,1))


#### 선형판별분석 (linear discriminant analysis, LDA)
data(iris)
head(iris)
plot(iris[,1:4], col=as.integer(iris$Species), 
     pch=substring((iris$Species), 1, 1))

n <- 30
tr <- c(sample(1:50, n), sample(51:100, n), sample(101:150, n))

library(MASS)  # for lda()
res <- lda(Species~ ., data=iris[tr,], prior = c(1,1,1)/3, subset = tr)

plot(res)
plot(res, dimen=1, type="both")

pred <- predict(res, iris[-tr,-5])$class
actual <- iris[-tr,]$Species
table(pred, actual)


#### 신경망 모형을 이용한 분류

# 신경망 모형을 위한 R패키지: nnet
library(nnet)

# training set에 대해 신경망 모형을 적합해 예측 모형 구축
ir1 <- nnet(Species~., data=iris[tr,], size=5, decay=.1)
summary(ir1)
?nnet

# test set에 대해 예측 실시 및 예측력 확인
pred <- predict(ir1, iris[-tr,-5], type="class")
table(pred, actual)


#### 의사결정나무를 이용한 분류
library(tree)
iris.tr <- tree(Species~., iris)

summary(iris.tr)
plot(iris.tr)
text(iris.tr, all=T)

iris.tr2 <- prune.misclass(iris.tr, best=4)
summary(iris.tr2)
plot(iris.tr2)
text(iris.tr2, all=T)


### D.2.2 자율학습에 의한 군집분석

# 계층적 군집 분석

# 데이터 불러오기
protein <- read.table("./Data/protein.txt", sep="\t", header=T, fileEncoding="UTF-8")
summary(protein)

# 분석에 사용할 변수 지정 및 scaling
vars.to.use <- colnames(protein)[-1]
pmat <- scale(protein[,vars.to.use])
pcenter <- attr(pmat, "scaled:center")
pscale <- attr(pmat, "scaled:scale")
head(pmat,2)
summary(pmat)

d <- dist(pmat, method="euclidean") # Euclidean distances
pfit <- hclust(d, method="ward.D")  # hierarchical clustering
pfit_1 <- hclust(dist(pmat), method="ward.D")  # hierarchical clustering

# 분석 결과 시각화를 위한 dendrogram을 작성
plot(pfit, labels=protein$Country)  # draw dendrogram
plot(pfit_1, labels=protein$Country)  # draw dendrogram

# 5개 군집을 형성함을 확인. dendrogram 위에 그룹별로 직사각형 표시
plot(pfit, labels=protein$Country)  # draw dendrogram
rect.hclust(pfit, k=5) # draw rentangles

# 25개 국가를 5개 그룹으로 할당한 그룹 인덱스
groups <- cutree(pfit, k=5)
groups

# 각 그룹별 특성을 알아보기 위해 그룹별로 Country, RedMeat, Fish, Fr.Veg 등 4개 변수 값 출력해보기
print.clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(protein[labels==i,c("Country","RedMeat","Fish","Fr.Veg")])
  }
}

print.clusters(groups, 5)


# K-평균 군집법 적용
pKmeans <- kmeans(pmat, 5) # K means clustering
summary(pKmeans)

# K-평균 군집법으로 생성된 그룹별 특징 알아보기
pKmeans$centers
pKmeans$size
print.clusters(pKmeans$cluster, 5)

