library(dplyr)
# 파일로드
train<-read.csv('data/train.csv', fileEncoding = 'UTF-8')

# 전처리
#===============================================================================
# 칼럼이름변경
colnames(train)[12]<-'지하철'
colnames(train)[13]<-'버스'
colnames(train)[7]<-'세대수'
colnames(train)[14]<-'주차면수'
colnames(train)[3]<-'건물구분'
# 이상치 제거
train1<-train
train1<-subset(
  train1, !train1$단지코드 %in%
    c('C1490','C2497','C2620','C1344','C1024','C2470','C1206','C1740','C2405',
      'C1804','C2085','C1397','C2431','C1649','C1036','C1095','C2051','C1218',
      'C1894','C2483','C1502','C1988'))
rownames(train1)<-c(1:nrow(train1))
# 단지별로 그룹화
train1$단지코드<-as.factor(train1$단지코드)
# 연속형변수만 추출
train1<-train1[,c(1,2,6,8,10,11,12,13,14,15)]
#train_2 단지별로 묶은 데이터셋
train2<-data.frame(levels(train1$단지코드),
                    tapply(train1$총세대수, train1$단지코드, mean),
                    tapply(train1$공가수, train1$단지코드, mean),
                    tapply(train1$주차면수, train1$단지코드, mean),
                    tapply(train1$등록차량수, train1$단지코드, mean),
                    tapply(train1$버스, train1$단지코드, mean),
                    tapply(train1$지하철, train1$단지코드, mean))
colnames(train2)<-c('단지코드', '총세대수', '공가수', '주차면수',
                     '등록차량수', '버스', '지하철')
rownames(train2)<-c(1:nrow(train2))
#===============================================================================
#면적세대 계산 & 시각화
train1$단지코드<-as.factor(train1$단지코드)
train1$건물구분<-as.factor(train1$건물구분)
train1$공급유형<-as.factor(train1$공급유형)
train1['면적세대']<-train1['전용면적']*train1['세대수']/train1['총세대수']
boxplot(tapply(train1$면적세대, train1$단지코드, sum))
b<-tapply(train1$면적세대, train1$단지코드, sum)
단지코드<-rownames(b)
b<-data.frame(단지코드,b)
rownames(b)<-c(1:nrow(b))