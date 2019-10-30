rm(list = ls())
gc()

# 한국 mapdata에서 서울만 추출
{
  
library(sf)
library(maptools)
list.files("./sigungu_mapdata")

si_gun_gu = st_read("./sigungu_mapdata/TL_SCCO_SIG.shp",
                    stringsAsFactors= F)
str(si_gun_gu)
?st_read
#iconv
Encoding(si_gun_gu$SIG_KOR_NM) = "CP949"
head(si_gun_gu)
plot(si_gun_gu)


library(dplyr)

si_gun_gu %>% select(SIG_CD, SIG_KOR_NM)
tail(si_gun_gu[1:27,])
seoulmap = si_gun_gu[grep("^[11]", si_gun_gu$SIG_CD),]
seoul_kor = seoulmap %>% select(SIG_KOR_NM)
plot(seoul_kor)
st_write(seoulmap, "./data/contest/seoul.shp",
         delete_dsn = T, layer_options = "ENCODING=UTF-8",
         delete_layer = T)

}
getwd()
##############################################################################
# 여기서부터 돌리면 됨
library(sf)
library(maptools)
library(ggplot2)
library(sp)
library(dplyr)
library(stringr)

seoulmap = st_read("./data/contest/seoul.shp", stringsAsFactors = F)
Encoding(seoulmap$SIG_KOR_NM) = "CP949"

seoul = seoulmap %>% select(SIG_KOR_NM)

seoulmap_sp = as(seoul, "Spatial")

seoulmap_longlat = spTransform(seoulmap_sp,  
                               CRS("+proj=longlat"))
seoullonglat = fortify(seoulmap_longlat)

levels(seoullonglat$group) = seoul$SIG_KOR_NM


# 서울시 구 이름 표시(돌릴 때는 생략)
{
  
levels(seoullonglat$group) = seoul$SIG_KOR_NM

mean(seoullonglat$long)
guname = seoul$SIG_KOR_NM

mean_long = aggregate(formula = long~group, data = seoullonglat,
          FUN = mean, trim = 0.15)
mean_lat = aggregate(formula = lat~group, data = seoullonglat,
          FUN = mean, trim = 0.15)

mean_longlat = merge(mean_long, mean_lat, by_x = group)


mean_longlat[1, 2] = mean_longlat[1, 2] - 0.01
mean_longlat[2, 2] = mean_longlat[2, 2] - 0.015
mean_longlat[4, 2] = mean_longlat[4, 2] + 0.015
mean_longlat[6, 2] = mean_longlat[6, 2] - 0.005 
mean_longlat[7, 3] = mean_longlat[7, 3] + 0.006
mean_longlat[8, 2] = mean_longlat[8, 2] + 0.002
mean_longlat[9, 2] = mean_longlat[9, 2] + 0.007
mean_longlat[9, 3] = mean_longlat[9, 3] + 0.01
mean_longlat[10, 3] = mean_longlat[10, 3] + 0.01
mean_longlat[11, 3] = mean_longlat[11, 3] - 0.005
mean_longlat[12, 3] = mean_longlat[12, 3] + 0.008
mean_longlat[13, 2] = mean_longlat[13, 2] - 0.012
mean_longlat[14, 2] = mean_longlat[14, 2] - 0.011
mean_longlat[15, 3] = mean_longlat[15, 3] + 0.008
mean_longlat[16, 2] = mean_longlat[16, 2] + 0.02
mean_longlat[17, 3] = mean_longlat[17, 3] - 0.003
mean_longlat[19, 2] = mean_longlat[19, 2] - 0.01
mean_longlat[19, 3] = mean_longlat[19, 3] - 0.007
mean_longlat[20, 2] = mean_longlat[20, 2] + 0.02
mean_longlat[21, 3] = mean_longlat[21, 3] - 0.01
mean_longlat[23, 3] = mean_longlat[23, 3] + 0.005
mean_longlat[24, 3] = mean_longlat[24, 3] - 0.002
mean_longlat[25, 3] = mean_longlat[25, 3] - 0.011

seoul_gu_center = mean_longlat
seoul_gu_center$group = as.character(seoul_gu_center$group)
?Encoding
?write.csv
write.csv(seoul_gu_center, "./data/contest/seoul_gu_center.csv", fileEncoding = "CP949")


}

#서울시 지도(구이름 표시)

seoul_gu_center = read.csv("./data/contest/seoul_gu_center.csv", encoding = "CP949")



empty_theme = theme(legend.position = "right",
                    legend.title = element_blank(),
                    legend.text = element_text(size = 8),
                    axis.title = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    panel.background = element_blank()) 


# 컬러 버전

ggplot(data = seoullonglat) + 
  geom_polygon(aes(x = long, y= lat, group = group, fill = group),
               col = "white") +
  empty_theme


# 흑백 버전
seoulmap = ggplot(data = seoullonglat) + 
  geom_polygon(aes(x = long, y= lat, group = group),
               fill = "grey", col = "white") + empty_theme

seoulmap +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), 
            size = 2.8) 
  

#######################################################
# 따릉이 위치정보
ddarung = read.csv("./data/contest/공공자전거 대여소 정보_201905.csv",
                   sep = ",", header = T,fileEncoding = "UTF-8",
                   stringsAsFactors = F)
head(ddarung)
str(ddarung)

seoulmap + 
  geom_point(data = ddarung, aes(x = 경도, y = 위도), col = "red", alpha = 0.7) +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), 
            size = 2.8) +
  empty_theme 


# 거치대 수 고려(안쓸듯)
{
library(RColorBrewer)
display.brewer.all()
pal = brewer.pal(7, "OrRd")

a = seq(min(ddarung$거치대수), max(ddarung$거치대수), length = 8)
a[1] = a[1] - 1
size = cut(ddarung$거치대, breaks = a)

ggplot(data = seoullonglat) + 
  geom_polygon(aes(x = long, y= lat, group = group),
               fill = "lightgrey", col = "white") + 
  geom_point(data = ddarung, aes(x = 경도, y = 위도, col = size), size = 2) +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), 
            size = 2.8) +
  theme(legend.text = element_text(size = 8), 
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) + 
      scale_color_manual(values = pal) + labs(colour = "거치대수")
}


###############################################################################
# 따릉이 데이터 가공(위치정보, 설치일자, 잔존여부 통합)
ddarung_date = read.table("./data/contest/date.csv",
         stringsAsFactors = F, header = T,
         sep = ",")
str(ddarung_date)
head(ddarung_date)

# 1
b = strsplit(ddarung_date$대여소명, "\\.")

date_num = c()
for(i in 1:nrow(ddarung_date)){
  date_num[i] = head(b[[i]], 1)
}
date_num = as.numeric(date_num)


ddarung_total = cbind(ddarung, NA)
ddarung_total = rbind(ddarung, NA, NA)
matching_num = match(date_num, ddarung$대여소번호)
for(i in 1:nrow(ddarung_date)){
  if(!is.na(date_num[i])){
    a = matching_num[i]
    ddarung_total[a, 7] = ddarung_date[i, 3]
  }
} 

# 2
date_null = ddarung_date[is.na(matching_num), ]
ddarung_total[998,]
b = strsplit(ddarung_total[,3], "\\.")
ddarung_name = c()
for(i in 1:length(b)){
  if(length(b[[i]]) > 2){
    if(str_detect(b[[i]][1], "[0-9]+")){
      ddarung_name[i] = paste(b[[i]][-1], collapse = ".")
      print(ddarung_name[i])
    }else{
      ddarung_name[i] = paste(b[[i]], collapse = ".")
      print(ddarung_name[i])
    }
  }else{
    ddarung_name[i] = tail(b[[i]], 1)
  }
}

matching_num2 = match(date_null[,2], ddarung_name)
ddarung_total[matching_num2, 3]
date_null[,2] == ddarung_total[matching_num2, 3]
ddarung_date[,2] == ddarung_total[matching_num, 3]

for(i in 1:length(matching_num2)){
  if(!is.na(matching_num2[i])){
    a = matching_num2[i]
    ddarung_total[a, 7] = date_null[i, 3]
  }
} 

# 3 
date_null[is.na(matching_num2),]
ddarung_total[is.na(ddarung_total[,7]),]

date_final_null = date_null[is.na(matching_num2),]


# 설치 일자 데이터에는 있지만 위치정보 데이터에는 빠진 대여소도 추가 
# 위치정보에는 있지만 설치 일자는 없는경우는 전부 폐쇄된 대여소였음

ddarung_real_name = str_trim(ddarung_name,"both")
ddarung_total[1:length(ddarung_real_name), 3] = ddarung_real_name

names(ddarung_total)[7] = "설치일자"


final_ddarung = rbind(ddarung_total, NA, NA, NA, NA, NA, NA, NA, NA, NA)
final_ddarung = cbind(final_ddarung, NA)
names(final_ddarung)[8] = "잔존여부"

final_ddarung[(nrow(ddarung) + 1):nrow(final_ddarung), c(1, 3, 7)] = date_final_null
final_ddarung[(nrow(ddarung) + 1):nrow(final_ddarung),]
tail(final_ddarung)
# 폐쇄 대여소 정보 추가(잔존 여부 추가)

closed_ddarung = read.csv("./data/contest/서울특별시 공공자전거 폐쇄 대여소(2019.06.24).csv", stringsAsFactors = F,
         fileEncoding = "UTF-8")
b = strsplit(closed_ddarung[,2], "\\.") 
num = rep(NA, length(b))
name = rep(NA, length(b))
length(b)
for(i in 1:length(b)){
  if(length(b[[i]]) == 2){
    num[i] = b[[i]][1]
    name[i] = str_trim(b[[i]][2], "both")
  }else if(length(b[[i]] == 1)){
    name[i] = str_trim(b[[i]], "both")
  }else{
    print("error")
  }
}
closed_ddarung$대여소명 = name
num = as.numeric(num)
length(num)

closed_ddarung = cbind(closed_ddarung, num)
names(closed_ddarung)[5] = "대여소번호"

closed_matching = match(name[!is.na(name)], final_ddarung$대여소명)
final_ddarung[,8] = T
final_ddarung[closed_matching[!is.na(closed_matching)], 8] = F
closed_ddarung
null_closed_ddarung = closed_ddarung[is.na(closed_matching),]

# 설치 일자 format 통합
format1 = str_detect(final_ddarung$설치일자, "^[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}")
format2 = str_detect(final_ddarung$설치일자, "^[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}") & !is.na(final_ddarung$설치일자)
format3 = !(format1|format2) & !is.na(final_ddarung$설치일자)
final_ddarung$설치일자[format2] = str_replace_all(final_ddarung$설치일자[format2], "\\.", "\\-")
final_ddarung$설치일자[format3] = c("2018-07-04", "2018-07-04", "2018-05-30", "2018-10-02",
                                "2018-09-20", "2018-07-04")
v = str_detect(final_ddarung$설치일자, "^[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}")
all(v[!is.na(v)])

final_ddarung$설치일자 = as.Date(final_ddarung$설치일자, "%F")

str(final_ddarung)
# 해야할 거 : 위치정보에 누락된 대여소들 정보(번호, 위치 정보, 거치대수) 추가
# 대여소 번호 매기는 기준은? 빠진 번호는 왜빠졌을까ㅏㅏㅏㅏㅏ 설마 데이터 없나
# 
library(ggplot2)
final_ddarung


library(plotly)

seoulmap + plot_ly()