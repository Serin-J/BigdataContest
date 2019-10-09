# 한국 mapdata에서 서울만 추출
library(sf)
library(maptools)
list.files("./sigungu_mapdata")

Sys.getlocale()
Sys.setlocale(category = "LC_ALL", "CP949")
?Sys.setlocale
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

seoulmap = st_read("./data/contest/seoul.shp", stringsAsFactors = F)
Encoding(seoulmap$SIG_KOR_NM) = "CP949"

seoul = seoulmap %>% select(SIG_KOR_NM)
############################################
# 서울 지도
seoulmap_sp = as(seoul, "Spatial")
class(seoulmap_sp)
seoulmap_df = fortify(seoulmap_sp)
head(seoulmap_df)
str(seoulmap_df)
levels(seoulmap_df$group) = seoulmap$SIG_KOR_NM
ggplot(data = seoulmap_df) + 
  geom_polygon(aes(x = long, y = lat, group = group,
                   fill = group), col = "white")


#좌표계 변경(위경도 좌표계로)
library(sp)
library
str(seoulmap_sp)
seoulmap_longlat = spTransform(seoulmap_sp,  
                               CRS("+proj=longlat"))
str(seoulmap_longlat)
seoullonglat = fortify(seoulmap_longlat)

# 서울시 구 이름 표시
levels(seoullonglat$group) = seoul$SIG_KOR_NM

mean(seoullonglat$long)
guname = seoul$SIG_KOR_NM

mean_long = aggregate(formula = long~group, data = seoullonglat,
          FUN = mean, trim = 0.15)
mean_lat = aggregate(formula = lat~group, data = seoullonglat,
          FUN = mean, trim = 0.15)

mean_longlat = merge(mean_long, mean_lat, by_x = group)


empty_theme = theme(legend.position = "right",
                    legend.title = element_blank(),
                    legend.text = element_text(size = 8),
                    axis.title = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    panel.background = element_blank()) 


seoul = ggplot(data = seoullonglat) + 
  geom_polygon(aes(x = long, y= lat, group = group),
               fill = "grey", col = "white") +
  geom_text(data = mean_longlat, aes(label = group, x = long, y = lat), 
            size = 2.8) +
  empty_theme

ggplot(data = seoullonglat) + 
  geom_polygon(aes(x = long, y= lat, group = group, fill = group),
               col = "white") +
  empty_theme





#######################################################
# 따릉이 위치정보
list.files("./data/contest")
ddarung = read.csv("./data/contest/공공자전거 대여소 정보_201905.csv",
                   sep = ",", header = T,fileEncoding = "UTF-8",
                   stringsAsFactors = F)
?read.csv
head(ddarung)
str(ddarung)

ggplot() + geom_point(data = ddarung, aes(x = 경도, y = 위도), size = 5)
ggplot(data = seoullonglat) + 
  geom_polygon(aes(x = long, y= lat, group = group),
               fill = "grey", col = "white") + 
  geom_point(data = ddarung, aes(x = 경도, y = 위도), col = "red", alpha = 0.7) +
  geom_text(data = mean_longlat, aes(label = group, x = long, y = lat), 
            size = 2.8) +
  empty_theme 
summary(ddarung)


# 거치대 수 고려
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
  geom_text(data = mean_longlat, aes(label = group, x = long, y = lat), 
            size = 2.8) +
  theme(legend.text = element_text(size = 8), 
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) + 
      scale_color_manual(values = pal) + labs(colour = "거치대수")

  