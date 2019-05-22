# B. 데이터 전처리 및 저장 단계 가이드

## B.1. Data load

library(data.table)
titanic <- read.csv("Data/titanic.csv", fileEncoding="UTF-8")
class(titanic)

titanic.dt <- data.table(titanic) 
class(titanic.dt)
str(titanic.dt)
head(titanic.dt)




## B.2. data.table과 data.frame

### B.2.1 처리 속도 차이

DF <- data.frame(x = runif(2.6e+07), y = rep(LETTERS, each = 10000))
system.time(x <- DF[DF$y == "C", ])
DT <- as.data.table(DF)
setkey(DT,y)
system.time(x <- DT[J("C"), ])

# rm(DF, DT)


### B.2.2 데이터 선택

head(titanic[,1])
titanic.dt[,1]
titanic.dt[,1, with=F]


## B.3. 조건을 이용한 데이터 선택

### B.3.1 특정 등급 승객들만 선택하는 방법

titanic.dt[pclass=="1st",] 
titanic.dt[pclass=="1st"]


### B.3.2 key를 이용한 선택

#1
setkeyv(titanic.dt, c("sex","pclass"))
tables()

#2
setkey(titanic.dt,pclass)
tables()

#3
v <-  "pclass"
setkeyv(titanic.dt,v)
setkey(titanic.dt, pclass) 
setkey(titanic.dt, "pclass")
tables()


### B.3.3 key 지정 후

#1
titanic.dt["1st"]
#2
titanic.dt[J("1st")]

titanic.dt[J("1st"),mean(survived)]
titanic.dt["1st",mean(survived)]




## B.4. Grouping 연산

# 1. 좌석등급별 생존률
titanic.dt[, mean(survived), by="pclass"]

# 2. 남녀 생존률
titanic.dt[,lapply(.SD,mean), by=sex, .SDcols=c("survived")]
titanic.dt[,mean(survived),by=sex]

# 3. 1등급 승객 중 성별 생존률
titanic.dt[J("1st"), mean(survived), by="sex"]

# 4. 다수 group key 지정(sex, boat)
titanic.dt[J("2nd"), mean(survived), by="sex,boat"]

# 5. 1등급 승객의 counting
titanic.dt[,length(which(pclass=="1st"))]
titanic.dt[pclass=="1st",.N]

# 6. 등급별, 성별 counting  
#    1. 1등급 중 성별 counting  
#    2. 등급별 counting  
#1
titanic.dt[pclass=="1st", .N, by="sex"] 
#2
titanic.dt[, .N ,by="pclass"]

# 7. 1등급 승객 중 성별 20세 이상 성인 비율
titanic.dt[J("1st"), length(which(age>20))/.N, by="sex"]
titanic.dt[J("1st"), length(which(age>20))/nrow(.SD), by="sex"]

# 8. 성별, 등급, 나이별 생존률
titanic.dt[,":="(isminor,"adult")]
titanic.dt[age<15,":="(isminor,"child")]

survived_pclass_sex_isminor <- titanic.dt[,list(cntsurv=length(which(survived == 1)), cntdie=length(which(survived == 0))), by=list(pclass, sex, isminor)][,list(psurvived=cntsurv/(cntsurv + cntdie)),by=list(pclass, sex, isminor)]

survived_pclass_sex_isminor

survived_pclass_sex_isminor$sex_age <- apply(survived_pclass_sex_isminor[,list(sex,isminor)], 1, paste, collapse="_")

survived_pclass_sex_isminor

