# 자료 불러오기
shop <- scan("C:/Users/sum/OneDrive/바탕 화면/shop.txt")
options(scipen=100)

# ts 객체 생성
shop.ts <- ts(shop,start=c(2012),freq=12)
shop.ts
p1 <- autoplot(shop.ts)
p1

# 자료분리
train_shop <- window(shop.ts, end = c(2019,12))
test_shop <- window(shop.ts, start = c(2020, 1))

#시계열 그래프
autoplot(shop.ts, size=1) +
  autolayer(test_shop, color="red", size=1) +
  labs(y = NULL, x = NULL)

# ETS 모형

# 모형적합
fit_s <- ets(train_shop)
summary(fit_s)
autoplot(fit_s)

# 오차 가정 만족 여부 확인
checkresiduals(fit_s)

# 예측 및 평가
fc_ets <- forecast(fit_s, h = length(test_shop))
accuracy(fc_ets, test_shop)

# 예측 그래프
autoplot(fc_ets) + 
  autolayer(test_shop, color = "red", size = 1) +
  labs(y = NULL, x = NULL)

# 로그변환(승법->가법)
fit_lns <- ets(train_shop, lambda = 0)
summary(fit_lns)
autoplot(fit_lns)

#가정만족여부확인
checkresiduals(fit_lns)

#예측및결과비교
fc_ets_1 <- forecast(fit_lns, h = length(test_shop))
accuracy(fc_ets_1, test_shop)

# 그래프 비교
autoplot(train_shop) +
  autolayer(test_shop, series = "Test data", size = 1) +
  autolayer(fc_ets, PI = FALSE, series = "ETS(M,Ad,M)", size = 1) +
  autolayer(fc_ets_1, PI = FALSE, series = "ETS(A,Ad,A)", size = 1) +
  labs(y = NULL, x = NULL, color = NULL)+
  theme(legend.position = "top")

# test 비교
autoplot(test_shop, series = "Test data", size = 1) +
  autolayer(fc_ets, PI = FALSE, series = "ETS(M,Ad,M)", size = 1) +
  autolayer(fc_ets_1, PI = FALSE, series = "ETS(A,Ad,A)", size = 1) +
  labs(y = NULL, x = NULL, color = NULL) +
  theme(legend.position = "top")

# test 예측구간 비교
library(patchwork)
plot1 <- autoplot(fc_ets) +
  autolayer(test_shop, color = "red", size = 1) +
  labs(x = NULL, y = NULL) 
plot2 <- autoplot(fc_ets, include = 0) +
  autolayer(test_shop, color = "red", size = 1) +
  labs(y = NULL, x = NULL) 
plot1 + plot2
plot1

# ARIMA

# 시계열 정상화
ggtsdisplay(train_shop)

# BoxCox transformation
BoxCox.lambda(shop.ts)
lnshop <- log(shop.ts)

# 변환 후 그래프 비교
p2 <- autoplot(lnshop)
p1+p2

# 로그변환한 자료 시계열 정상화
ggtsdisplay(lnshop)

ndiffs(lnshop)
nsdiffs(lnshop)

lnshop_12 <- diff(lnshop, lag=12)
ggtsdisplay(lnshop_12)
ndiffs(lnshop_12)

lnshop_12_1 <- diff(lnshop_12)

ggtsdisplay(lnshop_12_1)

ndiffs(lnshop_12_1)
nsdiffs(lnshop_12_1)

train_shop %>%
  diff(lag = 12) %>%
  ggtsdisplay()

train_shop %>%
  diff(lag = 12) %>%
  diff() %>%
  ggtsdisplay()

# 모형 적합
fit_arima_1 <- auto.arima(train_shop, d = 0, lambda=0,
                   stepwise = FALSE, approximation = FALSE)
fit_arima_1

fit_arima_2 <- auto.arima(train_shop, lambda=0,
                    stepwise = FALSE, approximation = FALSE)
fit_arima_2

fit_arima_3 <- auto.arima(train_shop, d = 0,
                          stepwise = FALSE, approximation = FALSE)
fit_arima_3
fit_arima_4 <- auto.arima(train_shop,
                          stepwise = FALSE, approximation = FALSE)
fit_arima_4

# 모형 진단
checkresiduals(fit_arima_1)
checkresiduals(fit_arima_2)

fc_arima_1 <- forecast(fit_arima_1)

fc_arima_2 <- forecast(fit_arima_2)

# 모형 평가 측도 비교
accuracy(fc_arima_1, test_shop)
accuracy(fc_arima_2, test_shop)

# 그래프 비교
autoplot(fc_arima_2) +
  autolayer(test_shop, color="red", size=1)
pp <- autoplot(fc_arima_2, include = 20) +
  autolayer(test_shop, color="red", size=1)

autoplot(test_shop, series = "Test data", size = 1) +
  autolayer(fc_arima_2, PI = FALSE, series = "ARIMA(0,1,1)(0,1,1)",size = 1) +
  labs(y = NULL, x = NULL, color = NULL) +
  theme(legend.position = "top")

pp2 <- autoplot(fc_arima_2, include = 0) +
  autolayer(test_shop, color = "red", size = 1) +
  labs(y = NULL, x = NULL) 

pp + pp2

# ets 비교
p1_1 <- autoplot(fc_arima_2, include = 10) +
  autolayer(test_shop, color = " red", size=1)

p2_1 <- autoplot(fc_ets, include=10) +
  autolayer(test_shop, color = "red", size=1)
p2_1/p1_1

autoplot(fc_ets, include = 70) +
  autolayer(test_shop, color = "red", size = 1) +
  labs(x = NULL, y = NULL) 

# 최종 결과 예측 그래프
autoplot(fc_ets) +
  autolayer(test_shop, color = "red", size = 1) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim=c(2020,2022))

a <- autoplot(fc_ets) +
  autolayer(test_shop, color = "red", size = 1) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim=c(2020,2021))
b <- a <- autoplot(fc_ets) +
  autolayer(test_shop, color = "red", size = 1) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim=c(2021,2022))
a/b
