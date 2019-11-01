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
str(ddarung_road)

# 대여 대여소-반납 대여소 빈도 
ddarung_freq = vector(mode = "list")
for(i in 1:3){
  a = ddarung_road[[i]] %>% group_by(대여소번호, 반납대여소번호) %>%
    tally()
  ddarung_freq[[i]] = as.data.frame(a)
}
ddarung_freq[[2]]


save(ddarung_road, ddarung_freq, file = "./data/contest/ddarung18.10-19.5.RData")

load("./data/contest/ddarung18.10-19.5.RData")


