#다중회귀분석 (등록차량수~지하철+버스+주차면수+평균면적+실세대수)
lm1<-lm(data=train2, 등록차량수~지하철+버스+주차면수+평균면적+실세대수)
summary(lm1)
sum(abs(lm1$residuals))/398 #MAE 125.6
par(mfrow=c(2,2))
plot(lm1)
par(mfrow=c(1,1))
plot(train2$실세대수)
plot(train2$평균면적)

#지역변수 추가
lm2<-lm(data = train2, 등록차량수~평균면적+실세대수+지역+버스+주차면수+지하철여부)
summary(lm2)
sum(abs(lm2$residuals))/398 #MAE 117.9816
par(mfrow=c(2,2))
plot(lm2)
par(mfrow=c(1,1))

#변수변환
lm3<-lm(data = train2, sqrt(등록차량수)~log(평균면적)+실세대수+지역+주차면수) #answer2
summary(lm3)
sum(abs(lm3$residuals))/398
mean(abs(lm3$fitted.values^2-train2$등록차량수)) # 114.9663

#건물구분&공급유형 추가
lm4<-lm(data=train3, sqrt(등록차량수)~.-평균면적+log(평균면적)) #answer5
summary(lm4)
sum(abs(lm4$residuals))/402
mean(abs(lm4$fitted.values^2-train2$등록차량수)) # 114.3057
par(mfrow=c(2,2))
plot(lm4)
par(mfrow=c(1,1))