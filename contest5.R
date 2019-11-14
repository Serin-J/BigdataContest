library(sf)
library(maptools)
list.files("./data/contest")

ji_sang = st_read("./data/contest/서울시 지역생활권 경계.shp",
                    stringsAsFactors= F)

plot(ji_sang)
str(ji_sang)
ji_sang_sp = as(ji_sang, "Spatial")
str(ji_sang_sp)
sp::proj4string(ji_sang_sp) = "+proj=tmerc +lat_0=38 +lon_0=127.0028902777778 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +towgs84=-146.414,507.337,680.507,0,0,0,0 +units=m +no_defs"


ji_sang_longlat = spTransform(ji_sang_sp,  
                               CRS("+proj=longlat"))
ji_sang_longlat = fortify(ji_sang_longlat)
ji_sang_fix = ji_sang_longlat
ji_sang_fix$long = ji_sang_longlat$long -0.002
ji_sang_fix$lat = ji_sang_longlat$lat + 0.0025
str(ji_sang_longlat)
ji_sang_longlat = ji_sang_fix

save(ji_sang_longlat, file = "./data/contest/seouljisang.RData")

f + geom_polygon(data = ji_sang_fix, aes(x = long, y = lat, group = group), alpha = 0, col = "black")

seoulmap + geom_polygon(data = ji_sang_fix, aes(x = long, y = lat, group = group), alpha = 0, col = "black")
ggplot() + geom_polygon(data = ji_sang_fix, aes(x = long, y = lat, group = group), alpha = 0, col = "black")

ju_geo = st_read("./data/contest/주거지역/UPIS_SHP_UQA100.shp",
                 stringsAsFactors = F)
library(patchwork)
library(dplyr)
ju_geo$ENT_NAME
plot(ju_geo)
str(ju_geo)
plot(t)
ju_geo_sp = as(ju_geo, "Spatial")
ju_geo_longlat = spTransform(ju_geo_sp,  
                              CRS("+proj=longlat"))
ju_geo_longlat = fortify(ju_geo_longlat)
str(ju_geo_longlat)
a = floor(as.numeric(levels(ju_geo_longlat$group)))
t = ju_geo_longlat$group
levels(t) = ju_geo$ENT_NAME[a]
ju_geo_label = cbind(ju_geo_longlat, label = t)


ggplot() + geom_polygon(data = ju_geo_label, aes(x = long, y = lat, group = group, fill = label), size = 0)

seoulmap +
  geom_polygon(data = ju_geo_label, aes(x = long, y = lat, group = group), fill = "yellow", alpha = 0.4,size = 0) +
  geom_polygon(data = ji_sang_fix, aes(x = long, y = lat, group = group), alpha = 0, col = "white") +
  geom_point(data = a, 
             aes(x = 경도, y = 위도, size = 전체이용량, color = a1,
                 text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                               도착빈도)), alpha = 0.8, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt", name = "전체 이용량(대여 횟수 + 도착 횟수)", 
             breaks = c(100, 1000, 10000)) +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10) + theme(legend.position = "none") +
  ggtitle("2019년도 4 ~ 5월 오전 7시 ~ 10시 거치대 이용량") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


sang_up = st_read("./data/contest/상업지역/UPIS_SHP_UQA200.shp",
                  stringsAsFactors = F)
plot(sang_up %>% select(ENT_NAME))

sang_up_sp = as(sang_up, "Spatial")
sang_up_longlat = spTransform(sang_up_sp,  
                             CRS("+proj=longlat"))
sang_up_longlat = fortify(sang_up_longlat)
str(sang_up_longlat)
a = floor(as.numeric(levels(sang_up_longlat$group)))
t = sang_up_longlat$group
levels(t) = sang_up$ENT_NAME[a]
sang_up_label = cbind(sang_up_longlat, label = t)

gong_up = st_read("./data/contest/상업지역/UPIS_SHP_UQA300.shp",
                  stringsAsFactors = F)
gong_up_sp = as(gong_up, "Spatial")
gong_up_longlat = spTransform(gong_up_sp,  
                              CRS("+proj=longlat"))
gong_up_longlat = fortify(gong_up_longlat)
str(gong_up_longlat)
a = floor(as.numeric(levels(gong_up_longlat$group)))
t = gong_up_longlat$group
levels(t) = gong_up$ENT_NAME[a]
gong_up_label = cbind(gong_up_longlat, label = t)

save(sang_up_label, gong_up_label,file = "./data/contest/sang_gong.RData")

mor = seoulmap +
  geom_polygon(data = sang_up_label, aes(x = long, y = lat, group = group), fill = "yellow", alpha = 0.8,size = 0) +
  geom_polygon(data = gong_up_label, aes(x = long, y = lat, group = group), fill = "green", alpha = 0.8,size = 0) +
  geom_polygon(data = ji_sang_fix, aes(x = long, y = lat, group = group), alpha = 0, col = "white") +
  geom_point(data = morning, aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                 text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                               도착빈도)), alpha = 0.5, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt") +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10) + theme(legend.position = "none") +
  ggtitle("2019년도 4 ~ 5월 오전 7시 ~ 10시 거치대 이용량") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

eve = seoulmap +
  geom_polygon(data = sang_up_label, aes(x = long, y = lat, group = group), fill = "yellow", alpha = 0.8,size = 0) +
  geom_polygon(data = gong_up_label, aes(x = long, y = lat, group = group), fill = "green", alpha = 0.8,size = 0) +
  geom_polygon(data = ji_sang_fix, aes(x = long, y = lat, group = group), alpha = 0, col = "white") +
  geom_point(data = evening, aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                                 text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                                               도착빈도)), alpha = 0.5, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt", name = "전체 이용량", 
             breaks = c(10, 100, 1000)) +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10, name = NULL,
                        breaks = c("대여 >> 도착" = 1, "대여 << 도착" = 19)) + 
  ggtitle("2019년도 4 ~ 5월 오후 7시 ~ 10시 거치대 이용량") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

mor + eve
str(transp)
seoulmap +
  geom_point(data = morning, aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                                 text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                                               도착빈도)), alpha = 0.5, shape = "O") + 
  geom_point(data = transp[[1]], aes(x = X좌표, y = Y좌표), shape = "+", color = "white") +

  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt") +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10) + theme(legend.position = "none") +
  ggtitle("2019년도 4 ~ 5월 오전 7시 ~ 10시 거치대 이용량") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
