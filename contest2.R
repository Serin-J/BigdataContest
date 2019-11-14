library(dplyr)

# 데이터 불러오기

ddarung_road = vector(mode = "list")
?vector
ddarung_road[[1]] = read.csv("./data/contest/서울특별시 공공자전거 대여정보_201810_01.csv",
                             stringsAsFactors = F)
ddarung_road[[2]] = read.csv("./data/contest/서울특별시 공공자전거 대여정보_201810_02.csv",
                             stringsAsFactors = F)
ddarung_road[[3]] = read.csv("./data/contest/서울특별시 공공자전거 대여정보_201811.csv",
                             stringsAsFactors = F)
ddarung_road[[4]] = read.csv("./data/contest/서울특별시 공공자전거 대여정보_201812.csv",
                             stringsAsFactors = F)

ddarung_road[[3]][is.na(ddarung_road[[3]]$대여소번호),]
for(i in 1:3){
  link = paste0("./data/contest/서울특별시 공공자전거 대여정보_20190", i ,".csv")
  ddarung_road[[i + 4]] = read.csv(link, stringsAsFactors = F)
}

for(i in 4:5){
  link = paste0("./data/contest/서울특별시 공공자전거 대여정보_20190", i ,".csv")
  ddarung_road[[i + 4]] = read.csv(link, stringsAsFactors = F, header = F)
}

# 분기별로 통합


for(i in 2:9){
  colnames(ddarung_road[[i]]) = colnames(ddarung_road[[1]])
}

ddarung_road[[1]] %>% group_by(대여소번호, 반납대여소번호) %>%
  tally()


ddarung2018_4 = rbind(ddarung_road[[1]], ddarung_road[[2]], ddarung_road[[3]], ddarung_road[[4]])
ddarung2019_1 = rbind(ddarung_road[[5]], ddarung_road[[6]], ddarung_road[[7]])
ddarung2019_2 = rbind(ddarung_road[[8]], ddarung_road[[9]])

ddarung_road = list("ddarung2018_4" = ddarung2018_4, 
                    "ddarung2019_1" = ddarung2019_1, 
                    "ddarung2019_2" = ddarung2019_2)
sum(is.na(ddarung_road[[3]]$대여소번호))
str(ddarung_road)

# 대여 대여소-반납 대여소 빈도 
ddarung_freq = vector(mode = "list")
for(i in 1:3){
  a = ddarung_road[[i]] %>% group_by(대여소번호, 반납대여소번호) %>%
    tally()
  ddarung_freq[[i]] = as.data.frame(a)
}
ddarung_freq[[2]]


rm(list = ls())
gc()
load("./data/contest/ddarung_gcd.RData")
load("./data/contest/ddarung15.9-17.12.RData")
load("./data/contest/ddarung18.1-18.9.RData")
load("./data/contest/ddarung18.10-19.5.RData")



sum(is.na(ddarung_road[[1]]$대여소번호))
str(ddarung_road567)

for(i in 1:9){
  for(j in c(3, 7)){
    ddarung_road567[[i]][,j] = as.numeric(ddarung_road567[[i]][,j])
  }
}
warnings()

library(stringr)
for(i in 1:9){
  for(j in c(4,8)){
    ddarung_road567[[i]][,j] = str_trim(ddarung_road567[[i]][,j], "both")
  }
}

null_num = vector(mode = "list")
for(i in 1:9){
  null_num[[i]] = is.na(ddarung_road567[[i]][,3])
  print(all(!null_num[[i]]))
}

library(dplyr)
for(i in 1:9){
  ddarung_road567[[i]][null_num[[i]],] %>% select(대여소명) %>% unique() %>% print()
}

final_ddarung %>% filter(대여소명 == "상암센터 정비실")
final_ddarung$대여소명
null_closed_ddarung %>% select(대여소명)
final_ddarung$대여소명[str_detect(final_ddarung$대여소명, "위트")]


# 중랑센터, 상암센터 정비실, 위트콤, 위트콤공장 데이터 읎음
# 이용시간(분), 이용거리(m)
str(ddarung_road567)

a = colnames(ddarung_road[[1]])
for(i in 1:9){
  colnames(ddarung_road567[[i]]) = a
}
save(ddarung_road567, file = "./data/contest/ddarung15.9-17.12.RData")

##############################################################################################
ddarung_road_8[[1]]
null_n = vector(mode = "list")
for(i in 1:3){
  null_n[[i]] = is.na(ddarung_road_8[[i]][,3])
  print(all(!null_n[[i]]))
}

sum(is.na(ddarung_road_8[[3]]$X.대여대여소번호.))
str(ddarung_road[[1]])
which(is.na(ddarung_road[[1]]))

str(final_ddarung)
final_ddarung[is.na(final_ddarung$대여소번호),3] = str_trim(final_ddarung[is.na(final_ddarung$대여소번호),3], "both")
final_ddarung$대여소번호
tail(final_ddarung, 15)

final_ddarung[1541, 2] = 1693
final_ddarung[is.na(final_ddarung$대여소번호),3]
ddarung_road[[2]] %>% select(대여소번호, 대여대여소명) %>% 
  filter(대여대여소명 == "신목동역 3번출구") 
str(ddarung_road)


# JW타워 2510
# 이수역 6번출구 앞 2511
# 대륭테크노타운 18차 1859
# 대림아파트 후문 상가 옆 2071
# 신도림 4차 e편한세상 아파트 1109동 앞 2000
# 수궁동 성당 주변 1999
# 원자력 병원 1693
# 
date_final_null
save(final_ddarung, date_final_null, null_closed_ddarung, file = "./data/contest/ddarung_gcd.RData")
load("./data/contest/ddarung_gcd.RData")
ddarung_road89 = ddarung_road
save(ddarung_road89, file = "./data/contest/ddarung18.10-19.5.RData")
ddarung_road89[[1]][(is.na(ddarung_road89[[1]][3])),]
library(dplyr)



summary(ddarung_road89[[1]])
ddarung_road89[[1]] %>% filter(대여소번호 == 3)

library(dplyr)
a = ddarung_road89[[1]][,3] %in% final_ddarung[,2]
null_data1 = ddarung_road89[[1]][!a, ] %>% select(대여소번호, 대여대여소명) %>% unique()
d = ddarung_road89[[3]][,3] %in% final_ddarung[,2]
null_data3 = ddarung_road89[[3]][!d, ] %>% select(대여소번호, 대여대여소명) %>% unique()



tail(final_ddarung, 15)
final_ddarung[1547, 2] = 766
final_ddarung[1545:1546, 2:3] = null_data3[5:6,]
f = cbind(NA, null_data3[1:4,], matrix(NA, nrow = 4, ncol = 4), T)
colnames(f) = colnames(final_ddarung)
final_ddarung = rbind(final_ddarung, f)
tail(final_ddarung, 18)


# 아마 깃허브에는 ddarung_gcd_new라는 이름으로 올릴거임
save(final_ddarung, date_final_null, null_closed_ddarung, file = "./data/contest/ddarung_gcd.RData")

null_num1 = vector(mode = "list")
null_num2 = vector(mode = "list")
for(i in 1:9){
  null_num1[[i]] = is.na(ddarung_road567[[i]][,3])
  null_num2[[i]] = is.na(ddarung_road567[[i]][,7])
}

str(ddarung_road567[[1]])
matching_num1 = vector(mode = "list")
matching_num2 = vector(mode = "list")
for(i in 1:9){
  matching_num1[[i]] = match(ddarung_road567[[i]][null_num1[[i]],4], final_ddarung_new[,3])
  matching_num2[[i]] = match(ddarung_road567[[i]][null_num2[[i]],8], final_ddarung_new[,3])
}


for(i in 1:9){
  ddarung_road567[[i]][null_num1[[i]], 3] = final_ddarung_new[matching_num1[[i]], 2]
  ddarung_road567[[i]][null_num2[[i]], 7] = final_ddarung_new[matching_num2[[i]], 2]
}

save(ddarung_road567, file = "./data/contest/ddarung15.9-17.12.RData")

str(ddarung_road567[[1]])
d
str(final_ddarung)
as.integer()
