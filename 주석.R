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

# test3['predict']<-predict.lm(lm2, newdata = test3)^2
# 
# plot(sqrt(train_3$등록차량수))
# 
# lm2<-lm(data = train_3, sqrt(등록차량수)~log(평균면적)+sqrt(주차면수)+지역+지하철+버스)
# summary(lm2)
# mean(abs(lm2$fitted.values^2-train_3$등록차량수))
# lm2$residuals
# sum(abs(lm2$residuals))/402
# par(mfrow=c(2,2))
# plot(lm2)
# par(mfrow=c(1,1))
# colnames(test3)[1]<-'code'
# 
# answer<-merge(submission, test3[c(1,11)], by='code')
# answer<-answer[-2]
# write.csv(answer, 'answer2.csv', row.names = F, quote=F)
# 
# #train11 : train_1에서 서울지역을 제거한 데이터
# train11<-train_1
# train11$단지코드<-as.character(train11$단지코드)
# train11<-subset(train11, !train11$지역=='서울특별시')
# train11$단지코드<-as.factor(train11$단지코드)
# train12<-data.frame(levels(train11$단지코드),
#                     tapply(train11$총세대수, train11$단지코드, mean),
#                     tapply(train11$세대수, train11$단지코드, sum),
#                     tapply(train11$공가수, train11$단지코드, mean),
#                     tapply(train11$주차면수, train11$단지코드, mean),
#                     tapply(train11$등록차량수, train11$단지코드, mean),
#                     tapply(train11$면적세대, train11$단지코드, sum),
#                     tapply(train11$버스, train11$단지코드, mean),
#                     tapply(train11$지하철, train11$단지코드, mean))
# colnames(train12)<-c('단지코드', '총세대수', '세대수합', '공가수', '주차면수',
#                      '등록차량수', '평균면적', '버스', '지하철')
# train12[is.na(train12)]<-0
# rownames(train12)<-c(1:nrow(train12))
# train12['실세대수']<-train12$총세대수-train12$공가수
# train13<-merge(train12, df2, by = '단지코드')
# train13$단지코드<-as.factor(train13$단지코드)
# train13$지역<-as.factor(train13$지역)
# lm3<-lm(data = train13, sqrt(등록차량수)~log(평균면적)+sqrt(실세대수)+지역+지하철+버스+sqrt(주차면수))
# summary(lm3)
# mean(abs(lm3$fitted.values^2-train13$등록차량수))
# par(mfrow=c(2,2))
# plot(lm2)
# par(mfrow=c(1,1))
