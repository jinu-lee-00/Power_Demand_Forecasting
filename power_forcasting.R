# 데이터 불러오기
data <- read.csv("./전력수요량_기상관측.csv")

# 결측치 처리: 강수량, 적설, 3시간신적설
data$강수량.mm.[is.na(data$강수량.mm.)] <- 0
data$적설.cm.[is.na(data$적설.cm.)] <- 0
data$X3시간신적설.cm.[is.na(data$X3시간신적설.cm.)] <- 0

# 풍향 데이터를 cos, sin으로 변환
data$풍향_cos <- cos(data$풍향.16방위. * pi / 180)
data$풍향_sin <- sin(data$풍향.16방위. * pi / 180)
data$풍향.16방위. <- NULL

# 나머지 결측치를 평균값으로 처리
for (col in colnames(data)) {
  if (is.numeric(data[[col]])) {
    data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
  }
}

# 일시를 DateTime 형식으로 변환
data$일시 <- as.POSIXct(data$일시, format = "%Y-%m-%d %H:%M")

# 휴일 변수 추가
data$휴일 <- ifelse(weekdays(data$일시) %in% c("토요일", "일요일"), 1, 0)

# 낮/밤 변수 수정
data$낮_밤 <- ifelse(as.numeric(format(data$일시, "%H")) >= 6 & as.numeric(format(data$일시, "%H")) < 18, "낮", "밤")

# 계절 변수 수정
data$계절 <- cut(as.numeric(format(data$일시, "%m")),
               breaks = c(0, 2, 5, 8, 11, 12),
               labels = c("겨울", "봄", "여름", "가을", "겨울"),
               include.lowest = TRUE)

# 팩터 변환
data$낮_밤 <- as.factor(data$낮_밤)
data$계절 <- as.factor(data$계절)

# 낮/밤 값 분포 확인
print(table(data$낮_밤))

# 계절 값 분포 확인
print(table(data$계절))

# 다중 회귀 분석 수행
model <- lm(전력수요량 ~ . - 일시, data = data)

# 결과 요약
coefficients = summary(model)$coefficients


# 데이터 불러오기
data_2023 <- read.csv("./2023년_데이터셋.csv")

# 결측치 처리: 강수량, 적설, 3시간신적설
data_2023$강수량.mm.[is.na(data_2023$강수량.mm.)] <- 0
data_2023$적설.cm.[is.na(data_2023$적설.cm.)] <- 0
data_2023$X3시간신적설.cm.[is.na(data_2023$X3시간신적설.cm.)] <- 0

# 풍향 데이터를 cos, sin으로 변환
data_2023$풍향_cos <- cos(data_2023$풍향.16방위. * pi / 180)
data_2023$풍향_sin <- sin(data_2023$풍향.16방위. * pi / 180)
data_2023$풍향.16방위. <- NULL

# 나머지 결측치를 평균값으로 처리
for (col in colnames(data_2023)) {
  if (is.numeric(data_2023[[col]])) {
    data_2023[[col]][is.na(data_2023[[col]])] <- mean(data_2023[[col]], na.rm = TRUE)
  }
}

# 일시를 DateTime 형식으로 변환
data_2023$일시 <- as.POSIXct(data_2023$일시, format = "%Y-%m-%d %H:%M")

# 휴일 변수 추가
data_2023$휴일 <- ifelse(weekdays(data_2023$일시) %in% c("토요일", "일요일"), 1, 0)

# 낮/밤 변수 수정
data_2023$낮_밤 <- ifelse(as.numeric(format(data_2023$일시, "%H")) >= 6 & as.numeric(format(data_2023$일시, "%H")) < 18, "낮", "밤")

# 계절 변수 수정
data_2023$계절 <- cut(as.numeric(format(data_2023$일시, "%m")),
               breaks = c(0, 2, 5, 8, 11, 12),
               labels = c("겨울", "봄", "여름", "가을", "겨울"),
               include.lowest = TRUE)

# 팩터 변환
data_2023$낮_밤 <- as.factor(data_2023$낮_밤)
data_2023$계절 <- as.factor(data_2023$계절)


data_2023$예측_전력수요량 <- 130900 +
  (-851.3) * data_2023$기온 +
  (-88.98) * data_2023$강수량 +
  (560.0) * data_2023$풍속 +
  (-473.7) * data_2023$습도 +
  (934.9) * data_2023$증기압 +
  (864.4) * data_2023$이슬점온도 +
  (-17130) * data_2023$현지기압 +
  (16960) * data_2023$해면기압 +
  (-24.26) * data_2023$일조 +
  (-3045) * data_2023$일사 +
  (386.3) * data_2023$적설 +
  (890.7) * data_2023$X3시간신적설 +
  (87.29) * data_2023$전운량 +
  (-70.15) * data_2023$중하층운량 +
  (-12.03) * data_2023$최저운고 +
  (-1.767) * data_2023$시정 +
  (63.38) * data_2023$지면온도 +
  (-169.3) * data_2023$풍향_cos +
  (-741.2) * data_2023$풍향_sin +
  (-9216) * data_2023$휴일 +
  (-691.8) * ifelse(data_2023$낮_밤 == "낮", 1, 0) +
  (-7644) * ifelse(data_2023$계절 == "봄", 1, 0) +
  (-6413) * ifelse(data_2023$계절 == "여름", 1, 0) +
  (-7994) * ifelse(data_2023$계절 == "가을", 1, 0)

# 실제 값과 예측 값 비교
data_2023$오차 <- abs(data_2023$전력수요량 - data_2023$예측_전력수요량)

err <- mean(data_2023$오차 / data_2023$전력수요량) * 100

print(paste("실제 데이터에 대한 오차:", err, "%"))
