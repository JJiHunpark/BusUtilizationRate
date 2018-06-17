rm(list=ls())
Sys.setlocale("LC_CTYPE") 
# getwd()
setwd("C:/Users/qkrwl/Desktop/빅데이터 논문/DataSet/")

##########################################
# 서울시 통학,통근시 이용교통편 현황 데이터
Transportation<-read.csv("transportation.csv", sep=",", header=T)
head(Transportation)
Transportation_seoul <- Transportation[Transportation$분류=="서울시",] # 서울시에 대한 정보만 추출
Transportation_seoul <- Transportation_seoul[,-c(2,3,6,10,13,14)] # 필요한 컬럼만 추출
Transportation_seoul <- Transportation_seoul[c(order(Transportation_seoul$기간)),]
Transportation_seoul
# 이용교통편 비교 그래프
# install.packages("ggplot2")
library("ggplot2")

# 년도 별 버스 이용률
ggplot(Transportation_seoul, aes(x=기간, y=버스)) + 
  xlab('YEAR') +
  ylab('Rate') +
  geom_line(colour="red") + 
  geom_point(size=2, shape=19, colour="red") + 
  theme_bw() +
  scale_x_continuous(breaks = c(2005,2007,2009,2011,2013,2015))+ ggtitle("Bus utilization rate") +
  theme(plot.title = element_text(hjust = 0.5))
  
# 년도 별 이용교통편 이용률 비교
Tg <- ggplot(Transportation_seoul, aes(x=기간, y=버스)) + 
  xlab('YEAR') +
  ylab('Rate') +

  geom_line(colour="red") + 
  geom_line(aes(x=기간,y=도보),colour="Orange") +
  geom_line(aes(x=기간,y=자전거),colour="Yellow") +
  geom_line(aes(x=기간,y=오토바이),colour="Green") +
  geom_line(aes(x=기간,y=지하철),colour="Blue") +
  geom_line(aes(x=기간,y=택시),colour="Navy") +
  geom_line(aes(x=기간,y=승용차),colour="Purple") +
  
  geom_point(size=2, shape=19, colour="red") + 
  geom_point(aes(x=기간,y=도보),colour="Orange") +
  geom_point(aes(x=기간,y=자전거),colour="Yellow") +
  geom_point(aes(x=기간,y=오토바이),colour="Green") +
  geom_point(aes(x=기간,y=지하철),colour="Blue") +
  geom_point(aes(x=기간,y=택시),colour="Navy") +
  geom_point(aes(x=기간,y=승용차),colour="Purple") +
  
  theme_bw() +
  scale_x_continuous(breaks = c(2005,2007,2009,2011,2013,2015))+ ggtitle("Status of use transportation") +
  theme(plot.title = element_text(hjust = 0.5))
Tg
######################################
# 서울시 버스 만족도
Satisfaction<-read.csv("satisfaction.csv", sep=",", header=T)
head(Satisfaction)
Satisfaction_seoul <- Satisfaction[Satisfaction$분류=="서울시",] # 서울시에 대한 정보만 추출
Satisfaction_seoul <- Satisfaction_seoul[,-c(2,3)] # 필요한 컬럼만 추출
Satisfaction_seoul <- Satisfaction_seoul[c(order(Satisfaction_seoul$기간)),]
Satisfaction_seoul

# 년도 별 버스 만족도
sg<- ggplot(Satisfaction_seoul, aes(x=기간, y=환승이용)) + 
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
  scale_x_continuous(breaks = c(2012, 2014))+ ggtitle("Bus satisfaction rate") +
  theme(plot.title = element_text(hjust = 0.5))
sg

# 버스 만족도에 따른 버스 이용률
Transportation_seoul_comparison <- Transportation_seoul[7:12,c(1,5)]
Satisfaction_seoul_comparison <- Satisfaction_seoul[,c(1,2)]

ggplot(Transportation_seoul_comparison, aes(x=기간, y=버스/3)) + 
  xlab('YEAR') +
  ylab('Rate') +
  
  geom_line(colour="red") + 
  geom_line(aes(x=Satisfaction_seoul_comparison$기간,y=Satisfaction_seoul_comparison$종합),colour="Orange") +
  
  geom_point(size=2, shape=19, colour="red") + 
  geom_point(aes(x=Satisfaction_seoul_comparison$기간,y=Satisfaction_seoul_comparison$종합),colour="Orange") +
  
  theme_bw() +
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+ ggtitle("Utilization rate relative to bus satisfaction") +
  theme(plot.title = element_text(hjust = 0.5))
