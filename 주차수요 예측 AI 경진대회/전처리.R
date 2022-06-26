library(dplyr)

#파일로드
train<-read.csv('data/train.csv', fileEncoding = 'UTF-8')
test<-read.csv('data/test.csv', fileEncoding = 'UTF-8')
submission<-read.csv('result/sample_submission.csv', fileEncoding = 'UTF-8')
submission<-submission[1]
age<-read.csv('data/age_gender_info.csv', fileEncoding = 'UTF-8')

#==============================================================================

# 칼럼이름변경
colnames(train)[12]<-'지하철'
colnames(train)[13]<-'버스'
colnames(train)[7]<-'세대수'
colnames(train)[14]<-'주차면수'
colnames(train)[3]<-'건물구분'

#==============================================================================

#train1 공지된 이상치만 제거한 데이터 2952>2818
train1<-train
rownames(train)<-c(1:2952)
train1<-subset(
  train1, !train1$단지코드 %in%
    c('C1490','C2497','C2620','C1344','C1024','C2470','C1206','C1740','C2405',
      'C1804','C2085','C1397','C2431','C1649','C1036','C1095','C2051','C1218',
      'C1894','C2483','C1502','C1988'))

#test셋에 없는 서울 지역 제거
train1<-subset(train1, train1$지역!='서울특별시')

#==============================================================================

#단지별 평균면적 계산 & 시각화
train1$단지코드<-factor(train1$단지코드)
train1$건물구분<-as.factor(train1$건물구분)
train1$공급유형<-as.factor(train1$공급유형)
train1['면적세대']<-train1['전용면적']*train1['세대수']/train1['총세대수']
평균면적<-tapply(train1$면적세대, train1$단지코드, sum)
단지코드<-rownames(평균면적)
areamean<-data.frame(단지코드,평균면적)
rownames(areamean)<-c(1:nrow(areamean))
boxplot(areamean$평균면적)

#==============================================================================

#train2 단지별로 묶은 데이터셋
train2<-data.frame(levels(train1$단지코드),
                   tapply(train1$총세대수, train1$단지코드, mean),
                   tapply(train1$세대수, train1$단지코드, sum),
                   tapply(train1$공가수, train1$단지코드, mean),
                   tapply(train1$주차면수, train1$단지코드, mean),
                   tapply(train1$등록차량수, train1$단지코드, mean),
                   tapply(train1$면적세대, train1$단지코드, sum),
                   tapply(train1$버스, train1$단지코드, mean),
                   tapply(train1$지하철, train1$단지코드, mean))
colnames(train2)<-c('단지코드', '총세대수', '세대수합', '공가수', '주차면수',
                    '등록차량수', '평균면적', '버스', '지하철')
rownames(train2)<-c(1:nrow(train2))

#실세대수 계산. (실세대수=총세대수-공가수)
train2['실세대수']<-train2$총세대수-train2$공가수

#결측치 0으로 치환
train2[is.na(train2)]<-0

#지하철&버스 라벨인코딩
train2['지하철여부']<-ifelse(train2$지하철==0,0,1)
#==============================================================================

#지역변수 추가
location<-data.frame(train1$단지코드, train1$지역)
location<-location[-which(duplicated(location$train1.단지코드)),]
colnames(location)<-c('단지코드', '지역')
train2<-merge(train2, location, by = '단지코드')
train2$단지코드<-as.factor(train2$단지코드)
train2$지역<-as.factor(train2$지역)
summary(train2)

#==============================================================================

#건물구분&공급유형
house<-train1[c(1,3,5,7)]
library(reshape2)
house<-dcast(data=house,
             formula=단지코드~건물구분+공급유형,
             fun.aggregate=sum,
             value.var='세대수')
train2<-merge(train2, house, by='단지코드')
# 단지코드, 총세대수, 세대수합, 공가수, 지역 제외
train3<-train2[c(5,6,7,8,9,10,11,13,14,15,16,17,18,19,20)]
# train3<-train3[-10]
# train3<-train3[-9]


#==============================================================================

#test셋 변환
colnames(test)[13]<-'버스'
colnames(test)[12]<-'지하철'
colnames(test)[7]<-'세대수'
colnames(test)[14]<-'주차면수'
colnames(test)[3]<-'건물구분'
test$단지코드<-factor(test$단지코드)
test['면적세대']<-test['전용면적']*test['세대수']/test['총세대수']

test_2<-data.frame(levels(test$단지코드),
                    tapply(test$총세대수, test$단지코드, mean),
                    tapply(test$세대수, test$단지코드, sum),
                    tapply(test$공가수, test$단지코드, mean),
                    tapply(test$주차면수, test$단지코드, mean),
                    tapply(test$면적세대, test$단지코드, sum),
                    tapply(test$버스, test$단지코드, mean),
                    tapply(test$지하철, test$단지코드, mean))
colnames(test_2)<-c('단지코드', '총세대수', '세대수합', '공가수', '주차면수',
                     '평균면적', '버스', '지하철')
rownames(test_2)<-c(1:nrow(test_2))
test_2['실세대수']<-test_2$총세대수-test_2$공가수

location_test=data.frame(test$단지코드, test$지역)
location_test<-location_test[-which(duplicated(location_test$test.단지코드)),]
colnames(location_test)<-c('단지코드', '지역')

test_2<-merge(test_2, location_test, by = '단지코드')
test_2$단지코드<-as.factor(test_2$단지코드)
test_2$지역<-as.factor(test_2$지역)
summary(test_2)
test_2[is.na(test_2)]<-0

#건물구분&공급유형
colnames(test)[1]<-'code'
test_2<-test_2[-11]
test2<-test[c(1,3,5,7)]
test3<-dcast(data=test2, code~건물구분+공급유형, sum)
test_2<-merge(test_2, test3, by='code')
test_3<-test_2[c(5,6,7,8,9,10,11,13,14,15,16,17)]

#==============================================================================

#임대보증금&임대료
summary(train_1)
train_1$임대보증금<-as.numeric(train_1$임대보증금)
train_1$임대료<-as.numeric(train_1$임대료)
dfcor<-data.frame(train_1$임대보증금, train_1$임대료)
colnames(dfcor)<-c('임대보증금', '임대료')
cor(dfcor$임대보증금, dfcor$료, use='completeobs')
dfcor2<-subset(dfcor, !is.na(dfcor$임대보증금)&!is.na(dfcor$임대료)) #571
options(scipen = 6)
plot(dfcor2)
plot(dfcor2$임대보증금,dfcor2$임대료)

train1<-train_1
train1$건물구분<-as.factor(train1$건물구분)
train1$공급유형<-as.factor(train1$공급유형)
train1$자격유형<-as.factor(train1$자격유형)

out<-subset(train1, train1$임대료/train1$임대보증금<=0.002)
plot(out$임대보증금, out$임대료)

df<-subset(train1, train1$건물구분=='아파트')
df<-subset(train1, train1$공급유형=='행복주택') #66 37 33
df<-subset(train1, train1$자격유형 %in% c( 'D'))
plot(df$임대보증금, df$임대료)
lmcor<-lm(df$임대료~df$임대보증금)
abline(lmcor)
summary(lmcor)
par(mfrow=c(1,1))
plot(lmcor)
abline(a=0, b=1)
boxplot(df$등록차량수)
levels(train1$공급유형)

cor(train_1$임대보증금, train_1$전용면적, use='complete.obs')
plot(train_1$임대보증금, train_1$전용면적, ylim=c(0,150))


#==============================================================================
age$지역<-as.factor(age$지역)
boxplot(train_1$임대보증금, na.rm=T)
plot(age[c(2,3)], age$지역)
x=seq(-10,10,0.01)
y=x*log(x)-x
plot(x,y)
#==============================================================================

answer['num']<-predict.lm(lm4, newdata = test_3)^2
write.csv(answer, 'answer6.csv', row.names = F, quote=F)
