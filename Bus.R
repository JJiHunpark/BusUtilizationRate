rm(list=ls())
# Sys.setlocale("LC_CTYPE") 
# getwd()
setwd("C:/Users/qkrwl/Desktop/빅데이터 논문/DataSet/")

############################################################################
# 서울시 통학,통근시 이용교통편 현황 데이터
Transportation<-read.csv("transportation.csv", sep=",", header=T)
head(Transportation)
Transportation_seoul <- Transportation[Transportation$분류=="서울시",] # 서울시에 대한 정보만 추출
Transportation_seoul <- Transportation_seoul[,-c(2,3,6,13,14)] # 필요한 컬럼만 추출
Transportation_seoul <- Transportation_seoul[c(order(Transportation_seoul$기간)),]
# dim(Transportation_seoul) # 행, 열 개수 확인
# Transportation_seoul <- Transportation_seoul[complete.cases(Transportation_seoul),]  # 결측치 제거
Transportation_seoul

# 이용교통편 비교 그래프
# install.packages("ggplot2")
library("ggplot2")

# 연간 버스 이용률
ggplot(Transportation_seoul, aes(x=기간, y=버스)) + 
  xlab('YEAR') +
  ylab('Rate') +
  geom_line(colour="red") + 
  geom_point(size=2, shape=19, colour="red") + 
  theme_bw() +
  scale_x_continuous(breaks = c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016))+ ggtitle("Annual bus utilization rate") +
  theme(plot.title = element_text(hjust = 0.5))
  
# 연간 교통수단 이용률
ggplot(Transportation_seoul, aes(x=기간, y=버스)) + 
  xlab('YEAR') +
  ylab('Rate') +

  geom_line(colour="red") + 
  geom_line(aes(x=기간,y=도보),colour="Orange") +
  geom_line(aes(x=기간,y=자전거),colour="Yellow") +
  geom_line(aes(x=기간,y=오토바이),colour="Green") +
  geom_line(aes(x=기간,y=지하철),colour="Blue") +
  geom_line(aes(x=기간,y=택시),colour="Navy") +
  geom_line(aes(x=기간,y=승용차),colour="Purple") +
  geom_line(aes(x=기간,y=버스.지하철),colour="Black") +
  
  geom_point(size=2, shape=19, colour="red") + 
  geom_point(aes(x=기간,y=도보),colour="Orange") +
  geom_point(aes(x=기간,y=자전거),colour="Yellow") +
  geom_point(aes(x=기간,y=오토바이),colour="Green") +
  geom_point(aes(x=기간,y=지하철),colour="Blue") +
  geom_point(aes(x=기간,y=택시),colour="Navy") +
  geom_point(aes(x=기간,y=승용차),colour="Purple") +
  geom_point(aes(x=기간,y=버스.지하철),colour="Black") +
  
  theme_bw() +
  scale_x_continuous(breaks = c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016))+ ggtitle("Annual traffic usage rate") +
  theme(plot.title = element_text(hjust = 0.5))


# 연간 버스, 지하철, 버스+지하철 이용률
ggplot(Transportation_seoul, aes(x=기간, y=버스)) + 
  xlab('YEAR') +
  ylab('Rate') +
  
  geom_line(colour="red") + 
  geom_line(aes(x=기간,y=지하철),colour="Blue") +
  geom_line(aes(x=기간,y=버스.지하철),colour="Black") +
  
  geom_point(size=2, shape=19, colour="red") + 
  geom_point(aes(x=기간,y=지하철),colour="Blue") +
  geom_point(aes(x=기간,y=버스.지하철),colour="Black") +
  
  theme_bw() +
  scale_x_continuous(breaks = c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)) + ggtitle("Annual public transportation usage rate") +
  theme(plot.title = element_text(hjust = 0.5))

############################################################################
# 서울시 버스 만족도
Satisfaction<-read.csv("satisfaction.csv", sep=",", header=T)
head(Satisfaction)
Satisfaction_seoul <- Satisfaction[Satisfaction$분류=="서울시",] # 서울시에 대한 정보만 추출
Satisfaction_seoul <- Satisfaction_seoul[,-c(2,3)] # 필요한 컬럼만 추출
Satisfaction_seoul <- Satisfaction_seoul[c(order(Satisfaction_seoul$기간)),]
Satisfaction_seoul

# 연간 버스 만족도
ggplot(Satisfaction_seoul, aes(x=기간, y=환승이용)) + 
  xlab('YEAR') +
  ylab('satisfaction') +
  
  geom_line(colour="Orange") + 
  geom_line(aes(x=기간,y=정류장위치),colour="Yellow") +
  geom_line(aes(x=기간,y=운행시간),colour="Green") +
  geom_line(aes(x=기간,y=도착시간),colour="Blue") +
  geom_line(aes(x=기간,y=요금),colour="Navy") +
  
  geom_point(size=2, shape=19, colour="Orange") + 
  geom_point(aes(x=기간,y=정류장위치),colour="Yellow") +
  geom_point(aes(x=기간,y=운행시간),colour="Green") +
  geom_point(aes(x=기간,y=도착시간),colour="Blue") +
  geom_point(aes(x=기간,y=요금),colour="Navy") +
  
  theme_bw() +
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+ ggtitle("Annual bus satisfaction") +
  theme(plot.title = element_text(hjust = 0.5))

# 연간 버스 만족도에 따른 이용률
Transportation_seoul_comparison <- Transportation_seoul[7:12,c(1,5)]
Satisfaction_seoul_comparison <- Satisfaction_seoul[,c(1,2)]

ggplot(Transportation_seoul_comparison, aes(x=기간, y=버스)) + 
  xlab('YEAR') +
  ylab('Rate') +
  
  geom_line(colour="red") +   # 버스 이용률  
  geom_line(aes(x=Satisfaction_seoul_comparison$기간,y=Satisfaction_seoul_comparison$종합*3),colour="Orange") + # 버스 만족도 
  
  geom_point(size=2, shape=19, colour="red") + 
  geom_point(aes(x=Satisfaction_seoul_comparison$기간,y=Satisfaction_seoul_comparison$종합*3),colour="Orange") +
  
  theme_bw() +
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+ ggtitle("Utilization according to annual bus satisfaction") +
  theme(plot.title = element_text(hjust = 0.5))

############################################################################
# 지하철 혼잡도
Congested_train <- read.csv("congested.csv", sep=",", header=T, stringsAsFactors = F)
Congested_train <- Congested_train[-c(1,2),c(1,3)]  # 필요 low와 colum 추출
Congested_train <- Congested_train[c(order(Congested_train$기간)),]   # 기간 기준 정렬
Congested_train <- Congested_train[-c(1:5),] # 05년~16년 데이터 사용
Congested_train$기간 <- as.numeric(as.character(Congested_train$기간))
Congested_train$지하철혼잡도 <- as.numeric(as.character(Congested_train$지하철혼잡도))
Congested_train

# 지하철 이용도
use_train <- Transportation_seoul[,c(1,6)]
use_train

# 지하철 만족도
Satisfaction_train <- read.csv("satisfaction_train.csv", sep=",", header=T, stringsAsFactors = F)
head(Satisfaction_train)
Satisfaction_train <- Satisfaction_train[Satisfaction_train$분류=="서울시",] # 서울시에 대한 정보만 추출
Satisfaction_train <- Satisfaction_train[,c(1,6)] # 필요한 컬럼만 추출
Satisfaction_train <- Satisfaction_train[c(order(Satisfaction_train$기간)),]
Satisfaction_train$지하철 <- as.numeric(as.character(Satisfaction_train$지하철))
Satisfaction_train

# 연간 지하철 만족도에 따른 이용률
ggplot(Congested_train, aes(x=기간, y=지하철혼잡도/10)) + 
  xlab('YEAR') +
  ylab('RATE') +
  
  # geom_line(colour="Orange") + # 지하철 혼잡도
  geom_line(aes(x=use_train$기간,y=use_train$지하철),colour="blue") +   # 지하철 이용률
  geom_line(aes(x=Satisfaction_train$기간,y=Satisfaction_train$지하철*2),colour="Green") +  # 지하철 만족도
  
  # geom_point(size=2, shape=19, colour="Orange") + 
  geom_point(aes(x=use_train$기간,y=use_train$지하철),colour="blue") +  
  geom_point(aes(x=Satisfaction_train$기간,y=Satisfaction_train$지하철*2),colour="Green") + 
  
  theme_bw() +
  scale_x_continuous(breaks = c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016))+ ggtitle("Utilization according to annual subway satisfaction") +
  theme(plot.title = element_text(hjust = 0.5))

############################################################################
# 지하철 이용률 요인 분석
# 지하철 요금, 편의시설, 공기질, 혼잡도가 지하철 이용에 영향을 미치는지 확인
Subway <- read.csv("Subway_Utilization_Factor.csv", sep=",", header=T)
Subway <- Subway[,-c(1)] # 필요한 컬럼만 추출
Subway
str(Subway)

# 변수 간 산포도 매트릭스 출력
cor(Subway[c("Subway", "Fare", "Congested", "PM10", "CO2", "Elevator", "Escalator")])
pairs(Subway[c("Subway", "Fare", "Congested", "PM10", "CO2", "Elevator", "Escalator")])

# install.packages("psych")
library(psych)
pairs.panels(Subway[c("Subway", "Fare", "Congested", "PM10", "CO2", "Elevator", "Escalator")])

################################################################################
# 변수 선택 모형
fit <- lm(Subway ~ ., data=Subway)
summary(fit)
# forward 모형
fit.con <- lm(Subway~1,data = Subway)
fit.forward <- step(fit.con,scope=list(lower=fit.con, upper=fit),direction = "forward")
summary(fit.forward)
# backward 모형
fit.backward <- step(fit, scope = list(lower = fit.con, upper = fit), direction = "backward")
summary(fit.backward)
# stepwise 모형
fit.both <- step(fit.con, scope = list(lower = fit.con, upper = fit), direction = "both")
summary(fit.backward)

# forward 모형 선택 
fit <- lm(formula = Subway ~ PM10, data = Subway)
summary(fit)
