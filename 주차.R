library(dplyr)
train<-read.csv('train.csv', fileEncoding = 'UTF-8')
test<-read.csv('test.csv', fileEncoding = 'UTF-8')
submission<-read.csv('sample_submission.csv', fileEncoding = 'UTF-8')
colnames(train)[12]<-'지하철'
colnames(train)[13]<-'버스'
colnames(train)[7]<-'세대수'
colnames(train)[14]<-'주차면수'
colnames(train)[3]<-'건물구분'

# 면적세대계산
train['면적세대']<-train['전용면적']*train['세대수']/train['총세대수']
train$단지코드<-as.factor(train$단지코드)
train$단지코드

plot(tapply(train$면적세대, train$단지코드, sum))
boxplot(tapply(train$면적세대, train$단지코드, sum))

a<-tapply(train$면적세대, train$단지코드, sum)
단지코드<-rownames(a)
a<-data.frame(단지코드,a)
rownames(a)<-c(1:nrow(a))

# train$자격유형<-as.factor(train$자격유형)
# levels(train$자격유형)
# 
# train$공급유형<-as.factor(train$공급유형)
# levels(train$공급유형)
# 
# plot(tapply(train$면적세대, train$단지코드, sum))
# boxplot(a$a)
# boxplot(a$a)$stats
# shapiro.test(a$a)
# 
# out<-a[(a$a<boxplot(a$a)$stats[1])|(a$a>=boxplot(a$a)$stats[5]),]
# nrow(out)
# out$단지코드
# out2<-train[train$단지코드 %in%
#               c("C1206", "C1286", "C1350", "C1357", "C1402", "C1490",
#               "C1544", "C1636", "C1649", "C1697", "C1743", "C1756",
#               "C1804", "C1933", "C1941", "C2040", "C2051", "C2085",
#               "C2086", "C2143", "C2259", "C2496", "C2595", "C2620"),]
# 
# out2$단지코드 <- out2$단지코드 %>%
#   as.character() %>% 
#   as.factor()
# 
# 
# tapply(out2$세대수, out2$단지코드, sum)
# tapply(out2$총세대수, out2$단지코드, mean)
# 
# 
# 
# plot(train$총세대수, train$등록차량수)
# 
# train$지역<-as.factor(train$지역)
# train2<-data.frame(levels(train$단지코드),
#                    tapply(train$총세대수, train$단지코드, mean),
#                    tapply(train$세대수, train$단지코드, sum),
#                    tapply(train$공가수, train$단지코드, mean),
#                    tapply(train$주차면수, train$단지코드, mean),
#                    tapply(train$등록차량수, train$단지코드, mean),
#                    tapply(train$면적세대, train$단지코드, sum))
# colnames(train2)<-c('단지코드', '총세대수', '세대수합', '공가수', '주차면수',
#                     '등록차량수', '평균면적')
# 
# cor(train2[, c(2:7)])
# train2['오차']=train2$총세대수-train2$세대수합
# 
# lm1<-lm(data=train2, 등록차량수~평균면적)
# par(mfrow=c(2,2))
# plot(lm1)
# par(mfrow=c(1,1))
# lm1
# train2[c('C1095', 'C2051', 'C1218', 'C1894', 'C2483', 'C1502', 'C1988'),]
# 
# tapply(train$면적세대, train$단지코드, sum)
# tapply(train$등록차량수, train$단지코드, mean)
# train['면적세대2']<-train2
# plot(train2, train$등록차량수)

#train_1 이상치 제거한 데이터
train_1<-train
rownames(train)<-c(1:2952)
train_1<-subset(
  train_1, !train_1$단지코드 %in%
    c('C1490','C2497','C2620','C1344','C1024','C2470','C1206','C1740','C2405',
      'C1804','C2085','C1397','C2431','C1649','C1036','C1095','C2051','C1218',
      'C1894','C2483','C1502','C1988'))

train_1$단지코드<-factor(train_1$단지코드)
train_1['면적세대']<-train_1['전용면적']*train_1['세대수']/train_1['총세대수']
boxplot(tapply(train_1$면적세대, train_1$단지코드, sum))
b<-tapply(train_1$면적세대, train_1$단지코드, sum)
단지코드<-rownames(b)
b<-data.frame(단지코드,b)
rownames(b)<-c(1:nrow(b))

train_2<-data.frame(levels(train_1$단지코드),
                tapply(train_1$총세대수, train_1$단지코드, mean),
                tapply(train_1$세대수, train_1$단지코드, sum),
                tapply(train_1$공가수, train_1$단지코드, mean),
                tapply(train_1$주차면수, train_1$단지코드, mean),
                tapply(train_1$등록차량수, train_1$단지코드, mean),
                tapply(train_1$면적세대, train_1$단지코드, sum),
                tapply(train_1$버스, train_1$단지코드, mean),
                tapply(train_1$지하철, train_1$단지코드, mean))
colnames(train_2)<-c('단지코드', '총세대수', '세대수합', '공가수', '주차면수',
                    '등록차량수', '평균면적', '버스', '지하철')
rownames(train_2)<-c(1:nrow(train_2))
train_2['실세대수']<-train_2$총세대수-train_2$공가수
lm1<-lm(data=train_2, 등록차량수~log(평균면적)+실세대수)
summary(lm1)
sum(abs(lm1$residuals))/402
par(mfrow=c(2,2))
plot(lm1)
par(mfrow=c(1,1))
plot(train_2$실세대수)
plot(train_2$평균면적)

#df
df=data.frame(train_1$단지코드, train_1$지역)
df2<-df[-which(duplicated(df$train_1.단지코드)),]
colnames(df2)<-c('단지코드', '지역')
train_3<-merge(train_2, df2, by = '단지코드')
train_3$단지코드<-as.factor(train_3$단지코드)
train_3$지역<-as.factor(train_3$지역)
summary(train_3)
train_3[is.na(train_3)]<-0

lm2<-lm(data = train_3, 등록차량수~log(평균면적)+실세대수+지역+지하철+버스+주차면수)
summary(lm2)
sum(abs(lm2$residuals))/402
par(mfrow=c(2,2))
plot(lm2)
par(mfrow=c(1,1))

#test
colnames(test)[13]<-'버스'
colnames(test)[12]<-'지하철'
colnames(test)[7]<-'세대수'
colnames(test)[14]<-'주차면수'
colnames(test)[3]<-'건물구분'
test$단지코드<-factor(test$단지코드)
test['면적세대']<-test['전용면적']*test['세대수']/test['총세대수']

test2<-data.frame(levels(test$단지코드),
                    tapply(test$총세대수, test$단지코드, mean),
                    tapply(test$세대수, test$단지코드, sum),
                    tapply(test$공가수, test$단지코드, mean),
                    tapply(test$주차면수, test$단지코드, mean),
                    tapply(test$면적세대, test$단지코드, sum),
                    tapply(test$버스, test$단지코드, mean),
                    tapply(test$지하철, test$단지코드, mean))
colnames(test2)<-c('단지코드', '총세대수', '세대수합', '공가수', '주차면수',
                     '평균면적', '버스', '지하철')
rownames(test2)<-c(1:nrow(test2))
test2['실세대수']<-test2$총세대수-test2$공가수

df_test=data.frame(test$단지코드, test$지역)
df_test2<-df_test[-which(duplicated(df_test$test.단지코드)),]
colnames(df_test2)<-c('단지코드', '지역')

test3<-merge(test2, df_test2, by = '단지코드')
test3$단지코드<-as.factor(test3$단지코드)
test3$지역<-as.factor(test3$지역)
summary(test3)
test3[is.na(test3)]<-0

test3['predict']<-predict.lm(lm2, newdata = test3)^2

plot(sqrt(train_3$등록차량수))

lm2<-lm(data = train_3, sqrt(등록차량수)~log(평균면적)+실세대수+지역+지하철+버스+주차면수)
summary(lm2)
mean(abs(lm2$fitted.values^2-train_3$등록차량수))
lm2$residuals
sum(abs(lm2$residuals))/402
par(mfrow=c(2,2))
plot(lm2)
par(mfrow=c(1,1))
colnames(test3)[1]<-'code'

answer<-merge(submission, test3[c(1,11)], by='code')
answer<-answer[-2]
write.csv(answer, 'answer2.csv', row.names = F, quote=F)
