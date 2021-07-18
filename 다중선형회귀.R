#다중회귀분석 (등록차량수~지하철+버스+주차면수+평균면적+실세대수)
lm1<-lm(data=train_2, 등록차량수~지하철+버스+주차면수+평균면적+실세대수)
summary(lm1)
sum(abs(lm1$residuals))/402 #MAE 124.3502
par(mfrow=c(2,2))
plot(lm1)
par(mfrow=c(1,1))
plot(train_2$실세대수)
plot(train_2$평균면적)

#지역변수 추가
lm2<-lm(data = train_2, 등록차량수~평균면적+실세대수+지역+버스+주차면수+지하철2)
summary(lm2)
sum(abs(lm2$residuals))/402 #MAE 116.7176
par(mfrow=c(2,2))
plot(lm2)
par(mfrow=c(1,1))

#변수변환
lm3<-lm(data = train_2, sqrt(등록차량수)~log(평균면적)+sqrt(실세대수)+지역+
          sqrt(주차면수)+지역)
summary(lm3)
sum(abs(lm3$residuals))/402
mean(abs(lm3$fitted.values^2-train_2$등록차량수)) # 109.2535
