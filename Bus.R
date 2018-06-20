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

Transportation_seoul_comparison <- transform(Transportation_seoul_comparison,
                                             s.버스 = scale(버스)
                                             )
Satisfaction_seoul_comparison <- transform(Satisfaction_seoul_comparison,
                                           s.종합 = scale(종합)
)

ggplot(Transportation_seoul_comparison, aes(x=기간, y=s.버스)) + 
  xlab('YEAR') +
  ylab('Rate') +
  
  geom_line(colour="red") +   # 버스 이용률  
  geom_line(aes(x=Satisfaction_seoul_comparison$기간,y=Satisfaction_seoul_comparison$s.종합),colour="Orange") + # 버스 만족도 
  
  geom_point(size=2, shape=19, colour="red") + 
  geom_point(aes(x=Satisfaction_seoul_comparison$기간,y=Satisfaction_seoul_comparison$s.종합),colour="Orange") +
  
  theme_bw() +
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+ ggtitle("Utilization according to annual bus satisfaction") +
  theme(plot.title = element_text(hjust = 0.5))

############################################################################
# 지하철 혼잡도
Congested_train <- read.csv("congested.csv", sep=",", header=T, stringsAsFactors = F)
Congested_train <- Congested_train[-c(1,2),c(1,4)]  # 필요 low와 colum 추출 (서울 1호선~4호선)
Congested_train <- Congested_train[c(order(Congested_train$기간)),]   # 기간 기준 정렬
Congested_train <- Congested_train[-c(1:5),] # 05년~16년 데이터 사용
Congested_train$기간 <- as.numeric(as.character(Congested_train$기간))
Congested_train$지하철혼잡도.1 <- as.numeric(as.character(Congested_train$지하철혼잡도.1))
Congested_train

# 지하철 사용 인원 평균
Transportation_personnel <- read.csv("Transportation_personnel.csv", sep=",", header=T)
dim(Transportation_personnel) # 행, 열 개수 확인
Transportation_personnel <- Transportation_personnel[complete.cases(Transportation_personnel),]  # 결측치 제거
Transportation_personnel <- Transportation_personnel[Transportation_personnel$구분=="수송인원(명)",] # 서울시에 대한 정보만 추출
Transportation_personnel <- Transportation_personnel[c(order(Transportation_personnel$연도)),]   # 기간 기준 정렬
Transportation_personnel <- Transportation_personnel[c(21:32),c(1,3,4,5,6)]   # 사용할 2005년부터 2016년까지의 데이터를 추출
Transportation_personnel
# rowMeans(Transport_Average)  # 행 평균
# install.packages("dplyr")
library(magrittr) # %>% 함수 사용을 위한 패키지 적용
library(dplyr)  # mutate 함수 사용을 위한 패키지 적용
# 연도별 서울역 지하철 사용 인원 평균을 계산하여 Average라는 열을 추가
Transport_Average <- Transportation_personnel %>%
  mutate(Average = (X1호선 + X2호선 + X3호선 + X4호선)/4)
  
Transport_Average

# 지하철 이용도
# use_train <- Transportation_seoul[,c(1,6)]
# use_train

# 지하철 만족도
Satisfaction_train <- read.csv("satisfaction_train.csv", sep=",", header=T, stringsAsFactors = F)
head(Satisfaction_train)
Satisfaction_train <- Satisfaction_train[Satisfaction_train$분류=="서울시",] # 서울시에 대한 정보만 추출
Satisfaction_train <- Satisfaction_train[,c(1,6)] # 필요한 컬럼만 추출
Satisfaction_train <- Satisfaction_train[c(order(Satisfaction_train$기간)),]
Satisfaction_train$지하철 <- as.numeric(as.character(Satisfaction_train$지하철))
Satisfaction_train

# 표준화
Transport_Average_scale <- transform(Transport_Average,
                               Average = scale(Average)
                    ) 
Satisfaction_train_scale <- transform(Satisfaction_train,
                              s.지하철 = scale(지하철)
                  ) 

# 연간 지하철 만족도에 따른 이용률
ggplot(Transport_Average_scale, aes(x=연도, y=Average)) + 
  xlab('YEAR') +
  ylab('RATE') +
  
  geom_line(colour="blue") + # 지하철 이용률
  geom_line(aes(x=Satisfaction_train_scale$기간,y=Satisfaction_train_scale$s.지하철),colour="Green") +  # 지하철 만족도
  
  geom_point(size=2, shape=19, colour="blue") + 
  geom_point(aes(x=Satisfaction_train_scale$기간,y=Satisfaction_train_scale$s.지하철),colour="Green") + 
  
  theme_bw() +
  scale_x_continuous(breaks = c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016))+ ggtitle("Utilization according to annual subway satisfaction") +
  theme(plot.title = element_text(hjust = 0.5))

############################################################################
# 지하철 이용률 요인 분석
# 지하철 요금, 편의시설, 공기질, 혼잡도가 지하철 이용에 영향을 미치는지 확인
Subway_Factor <- Transport_Average[,c(1,6)] # 지하철 요소에 지하철 이용 인원 추가

Fare <- read.csv("Fare.csv", sep=",", header=T)
Fare <- Fare[,c(1,7)] # 필요한 열 추출
Fare <- Fare[-c(1:4),] # 필요한 행 추출
names(Fare)[names(Fare) == "대중교통요금.4"] <- c("요금")
Subway_Factor <- cbind(Subway_Factor, Fare$요금) # 지하철 요소에 지하철 요금 추가

congested <- read.csv("congested.csv", sep=",", header=T)
congested <- congested[-c(1,2),c(1,4)] # 필요한 행, 열 추출
congested <- congested[c(order(congested$기간)),]
congested <- congested[-c(1:5),]
Subway_Factor <- cbind(Subway_Factor, congested$지하철혼잡도.1) # 지하철 요소에 지하철 혼잡도 추가

Airquality <- read.csv("Airquality_year.csv", sep=",", header=T)
Airquality <- Airquality[,c(1,3,4)]
Airquality <- Airquality[-c(1,2,3),]
Airquality <- Airquality[c(order(Airquality$년도)),]
names(Airquality)[names(Airquality) == "유지기준"] <- c("PM10")
names(Airquality)[names(Airquality) == "유지기준.1"] <- c("CO2")
Subway_Factor <- cbind(Subway_Factor, Airquality$PM10, Airquality$CO2) # 지하철 요소에 지하철 공기질 추가

Elevator <- read.csv("Elevator.csv", sep=",", header=T)
Elevator <- Elevator[Elevator$구분=="서울메트로" & Elevator$구분.1=="소계",] # 서울시 지하철에 관련된 데이터만 추출
Elevator <- Elevator[,c(1,6,8)]
Elevator <- Elevator[c(order(Elevator$기간)),]
Elevator <- Elevator[-c(1),]
Elevator
Subway_Factor <- cbind(Subway_Factor, Elevator$엘레베이터.1, Elevator$에스컬레이터.1) # 지하철 요소에 지하철 편의시설 추가

colnames(Subway_Factor) = c("Year", "Subway", "Fare", "Congested", "PM10", "CO2", "Elevator", "Escalator") # 테이블 열 이름 변경
Subway_Factor
str(Subway_Factor)

# 요소들을 숫자로 변환
Subway_Factor[,3] <- as.integer(gsub(",", "", Subway_Factor[,3]))  # 천 단위 요금에 , 가 들어가므로 , 를 공백으로 변환
Subway_Factor$Congested <- as.numeric(as.character(Subway_Factor$Congested))
Subway_Factor$PM10 <- as.numeric(as.character(Subway_Factor$PM10))
Subway_Factor$CO2 <- as.numeric(as.character(Subway_Factor$CO2))
Subway_Factor$Elevator <- as.numeric(as.character(Subway_Factor$Elevator))
Subway_Factor$Escalator <- as.numeric(as.character(Subway_Factor$Escalator))

# 변수의 단위 표준화 작업
Subway_Factor_scale <- transform(Subway_Factor,
                            Subway = scale(Subway),
                            Fare = scale(Fare),
                            Congested = scale(Congested),
                            PM10 = scale(PM10),
                            CO2 = scale(CO2),
                            Elevator = scale(Elevator),
                            Escalator = scale(Escalator)
                          )
Subway_Factor_scale <- Subway_Factor_scale[,-c(1)] # 회기분석에 불필요한 기간 column은 제거
Subway_Factor_scale

# 변수 간 산포도 매트릭스 출력
#cor(Subway_scale[c("Subway", "Fare", "Congested", "PM10", "CO2", "Elevator", "Escalator")])
#pairs(Subway_scale[c("Subway", "Fare", "Congested", "PM10", "CO2", "Elevator", "Escalator")])

# install.packages("psych")
library(psych)
pairs.panels(Subway_Factor_scale[c("Subway", "Fare", "Congested", "PM10", "CO2", "Elevator", "Escalator")])

################################################################################
# 변수 선택 모형
fit <- lm(Subway ~ ., data=Subway_Factor_scale)
summary(fit)
# forward 모형
# fit.con <- lm(Subway~1,data = Subway_Factor_scale)
# fit.forward <- step(fit.con,scope=list(lower=fit.con, upper=fit),direction = "forward")
# summary(fit.forward)
# backward 모형
# fit.backward <- step(fit, scope = list(lower = fit.con, upper = fit), direction = "backward")
# summary(fit.backward)
# stepwise 모형
# fit.both <- step(fit.con, scope = list(lower = fit.con, upper = fit), direction = "both")
# summary(fit.backward)

# forward 모형 선택 
# fit <- lm(formula = Subway ~ PM10, data = Subway_Factor_scale)
# summary(fit)

###############################################################################
# 지하철 이용률과 요금, 엘리베이터, 에스컬레이터 수와의 관계 (양의 관계)
Subway_Factor
Subway_plus <- Subway_Factor
Subway_plus <- Subway_plus[,c(1,2,3,7,8)]
Subway_plus

Subway_plus_scale <- transform(Subway_plus,
                          Subway = scale(Subway),
                          Fare = scale(Fare),
                          Elevator = scale(Elevator),
                          Escalator = scale(Escalator)
)
Subway_plus_scale

ggplot(Subway_plus_scale, aes(x=Year, y=Subway)) + 
  xlab('YEAR') +
  ylab('RATE') +
  
  geom_line(colour="red") + # 지하철 이용률
  geom_line(aes(x=Year,y=Fare),colour="Orange") +  # 지하철 요금
  geom_line(aes(x=Year,y=Elevator),colour="Green") +  # 지하철 엘리베이터 수
  geom_line(aes(x=Year,y=Escalator),colour="blue") +  # 지하철 에스컬레이터 수
  
  geom_point(size=2, shape=19, colour="red") + 
  geom_point(aes(x=Year,y=Fare),colour="Orange") +  
  geom_point(aes(x=Year,y=Elevator),colour="Green") +  
  geom_point(aes(x=Year,y=Escalator),colour="blue") +  # 지하철 에스컬레이터 수
  
  theme_bw() +
  scale_x_continuous(breaks = c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016))+ ggtitle("Factors related to the amount of subway utilization") +
  theme(plot.title = element_text(hjust = 0.5))

# 지하철 이용률과 혼잡도, 미세먼지농도와의 관계 (음의 관계)
Subway_minus <- Subway_Factor
Subway_minus <- Subway_minus[,c(1,2,4,5)]
Subway_minus

Subway_minus_scale <- transform(Subway_minus,
                               Subway = scale(Subway),
                               Congested = scale(Congested),
                               PM10 = scale(PM10)
)

ggplot(Subway_minus_scale, aes(x=Year, y=Subway)) + 
  xlab('YEAR') +
  ylab('RATE') +
  
  geom_line(colour="red") + # 지하철 이용률
  geom_line(aes(x=Year,y=Congested),colour="Orange") +  # 혼잡도
  geom_line(aes(x=Year,y=PM10),colour="Green") +  # 미세먼지
  
  geom_point(size=2, shape=19, colour="red") + 
  geom_point(aes(x=Year,y=Congested),colour="Orange") +  
  geom_point(aes(x=Year,y=PM10),colour="Green") +  
  
  theme_bw() +
  scale_x_continuous(breaks = c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016))+ ggtitle("Factors unrelated to the amount of subway utilization") +
  theme(plot.title = element_text(hjust = 0.5))
