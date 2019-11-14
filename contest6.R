library(ggplot2)
library(dplyr)
load("./data/contest/pertime2.RData")
names(pertime3)
nrow(pertime3[[8]])
freq = c()
times = c()
for(i in 1:24){
  freq[i] = nrow(pertime3[[i]])
  times[i] = paste0(i, ":", "30")
}
times = c("00:30", times[-24])
time_freq = data.frame(time = times, freq = freq, group = 1)

time_freq[,1] = as.POSIXct(time_freq[,1], format = "%H:%M")
library(scales)
time_line = ggplot() + 
  geom_bar(data = time_freq, aes(x = time, y = freq), fill = pal[2], stat = "identity", alpha = 0.7) +
  scale_x_datetime( date_labels = "%H:%M") +
  xlab("시간별 공공자전거 이용량") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) 
######################################################################################

total_morn = rbind(pertime3[[8]], pertime3[[9]], pertime3[[10]])
total_eve = rbind(pertime3[[18]], pertime3[[19]], pertime3[[20]])
total = list(morning = total_morn, evening = total_eve)

time_start_freq = vector(mode = "list")
n = length(total)
for(i in 1:n){
  time_start_freq[[i]] = total[[i]] %>% group_by(대여소번호) %>% 
    tally() %>% as.data.frame()
}

time_final_freq = vector(mode = "list")
for(i in 1:n){
  time_final_freq[[i]] = total[[i]] %>% 
    group_by(반납대여소번호) %>% 
    tally() %>% as.data.frame()
  colnames(time_final_freq[[i]])[1] = "대여소번호"
}

time_gcd_freq = vector(mode = "list")
for(i in 1:n){
  a = merge(time_start_freq[[i]], 
            time_final_freq[[i]], by = "대여소번호", all = T)  
  colnames(a)[c(2,3)] = c("대여빈도", "도착빈도")
  time_gcd_freq[[i]] = a
  print(all(time_start_freq[[i]][,1] %in% time_gcd_freq[[i]][,1]))
  print(all(time_final_freq[[i]][,1] %in% time_gcd_freq[[i]][,1]))
}

for(i in 1:n){
  b = match(time_gcd_freq[[i]]$대여소번호, 
            final_ddarung$대여소번호)
  print(all(!is.na(b)))
  time_gcd_freq[[i]] = cbind(time_gcd_freq[[i]], final_ddarung[b, c(4, 5)])
}
str(time_gcd_freq[[1]])

for(i in 1:n){
  time_gcd_freq[[i]] = time_gcd_freq[[i]][,1:5]
  time_gcd_freq[[i]]$대여빈도[is.na(time_gcd_freq[[i]]$대여빈도)] = 0
  time_gcd_freq[[i]]$도착빈도[is.na(time_gcd_freq[[i]]$도착빈도)] = 0
  time_gcd_freq[[i]] = time_gcd_freq[[i]] %>% mutate(전체이용량 = 도착빈도 + 대여빈도,
                                                          유입량 = 도착빈도 - 대여빈도) %>%
    mutate(pr = 유입량/전체이용량)
}

total_mor_eve = time_gcd_freq
names(total_mor_eve) = names(total)
str(total_mor_eve[[1]])

for(i in 1:n){
  a1 = cut(total_mor_eve[[i]][,8], breaks = c(-1, seq(-0.5, 0.5, length = 18), 1))
  total_mor_eve[[i]] = cbind(total_mor_eve[[i]][,1:8], per_label = as.numeric(a1))
}

#################################################################

total_mor = seoulmap +
  geom_polygon(data = ji_sang_fix, aes(x = long, y = lat, group = group), alpha = 0, col = "white") +
  geom_point(data = total_mor_eve[[1]], aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                                 text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                                               도착빈도)), alpha = 0.5, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt") +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10) + theme(legend.position = "none") +
  ggtitle("오전 7시 ~ 10시 거치대 이용량") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

total_eve = seoulmap +
  geom_polygon(data = ji_sang_fix, aes(x = long, y = lat, group = group), alpha = 0, col = "white") +
  geom_point(data = total_mor_eve[[2]], aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                                 text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                                               도착빈도)), alpha = 0.5, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt", name = "전체 이용량", 
             breaks = c(100, 1000, 10000)) +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10, name = NULL,
                        breaks = c("대여 >> 도착" = 1, "대여 << 도착" = 19)) + 
  ggtitle("오후 5시 ~ 8시 거치대 이용량") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

library(patchwork)
mor + eve


################################################################

ddarung_19_2_mor = total[[1]] %>% 
  filter(as.Date("2019-03-31", format = "%F") < 
           as.Date(대여일시, format = "%F"))

ddarung_19_2_eve = total[[2]] %>% 
  filter(as.Date("2019-03-31", format = "%F") < 
           as.Date(대여일시, format = "%F"))

ddarung_19_2 = list(morning = ddarung_19_2_mor,
                    evening = ddarung_19_2_eve)

start_19_2 = vector(mode = "list")
n = 2
for(i in 1:n){
  start_19_2[[i]] = ddarung_19_2[[i]] %>% group_by(대여소번호) %>% 
    tally() %>% as.data.frame()
}

final_19_2 = vector(mode = "list")
for(i in 1:n){
  final_19_2[[i]] = ddarung_19_2[[i]] %>% 
    group_by(반납대여소번호) %>% 
    tally() %>% as.data.frame()
  colnames(final_19_2[[i]])[1] = "대여소번호"
}

time_gcd_19_2 = vector(mode = "list")
for(i in 1:n){
  a = merge(start_19_2[[i]], 
            final_19_2[[i]], by = "대여소번호", all = T)  
  colnames(a)[c(2,3)] = c("대여빈도", "도착빈도")
  time_gcd_19_2[[i]] = a
  print(all(start_19_2[[i]][,1] %in% time_gcd_19_2[[i]][,1]))
  print(all(final_19_2[[i]][,1] %in% time_gcd_19_2[[i]][,1]))
}

for(i in 1:n){
  b = match(time_gcd_19_2[[i]]$대여소번호, 
            final_ddarung$대여소번호)
  print(all(!is.na(b)))
  time_gcd_19_2[[i]] = cbind(time_gcd_19_2[[i]], final_ddarung[b, c(4, 5)])
}


for(i in 1:n){
  time_gcd_19_2[[i]] = time_gcd_19_2[[i]][,1:5]
  time_gcd_19_2[[i]]$대여빈도[is.na(time_gcd_19_2[[i]]$대여빈도)] = 0
  time_gcd_19_2[[i]]$도착빈도[is.na(time_gcd_19_2[[i]]$도착빈도)] = 0
  time_gcd_19_2[[i]] = time_gcd_19_2[[i]] %>% mutate(전체이용량 = 도착빈도 + 대여빈도,
                                                          유입량 = 도착빈도 - 대여빈도) %>%
    mutate(pr = 유입량/전체이용량)
}

ddar_19_2_mor_eve = time_gcd_19_2

names(ddar_19_2_mor_eve) = names(ddarung_19_2)
str(ddar_19_2_mor_eve[[1]])
for(i in 1:n){
  a1 = cut(ddar_19_2_mor_eve[[i]][,8], breaks = c(-1, seq(-0.5, 0.5, length = 18), 1))
  ddar_19_2_mor_eve[[i]] = cbind(ddar_19_2_mor_eve[[i]][,1:8], per_label = as.numeric(a1))
}


str(ddar_19_2_mor_eve[[1]])

mor_19_2 = seoulmap +
  geom_polygon(data = ji_sang_fix, aes(x = long, y = lat, group = group), alpha = 0, col = "white") +
  geom_point(data = ddar_19_2_mor_eve[[1]], aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                                            text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                                                          도착빈도)), alpha = 0.5, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt") +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10) + theme(legend.position = "none") +
  ggtitle("2019년 4월 ~ 5월 오전 7시 ~ 10시 거치대 이용량") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

eve_19_2 = seoulmap +
  geom_polygon(data = ji_sang_fix, aes(x = long, y = lat, group = group), alpha = 0, col = "white") +
  geom_point(data = ddar_19_2_mor_eve[[2]], aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                                            text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                                                          도착빈도)), alpha = 0.5, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt", name = "전체 이용량", 
             breaks = c(10, 100, 1000)) +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10, name = NULL,
                        breaks = c("대여 >> 도착" = 1, "대여 << 도착" = 19)) + 
  ggtitle("2019년 4월 ~ 5월 오후 5시 ~ 8시 거치대 이용량") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

a = (total_mor + total_eve)/(mor_19_2 + eve_19_2)/time_line + plot_layout(heights = c(10, 10, 1))

ggsave("./big.png", height = 22, width = 26, units = "cm")
?ggsave


total_mor_dosi = seoulmap +
  geom_polygon(data = sang_up_label, aes(x = long, y = lat, group = group), fill = "yellow", alpha = 0.8,size = 0) +
  geom_polygon(data = gong_up_label, aes(x = long, y = lat, group = group), fill = "green", alpha = 0.8,size = 0) +
  geom_polygon(data = ji_sang_fix, aes(x = long, y = lat, group = group), alpha = 0, col = "white") +
  geom_point(data = total_mor_eve[[1]], aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                                            text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                                                          도착빈도)), alpha = 0.5, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt") +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10) + theme(legend.position = "none") +
  ggtitle("오전 7시 ~ 10시 거치대 이용량") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

total_eve_dosi = seoulmap +
  geom_polygon(data = sang_up_label, aes(x = long, y = lat, group = group), fill = "yellow", alpha = 0.8,size = 0) +
  geom_polygon(data = gong_up_label, aes(x = long, y = lat, group = group), fill = "green", alpha = 0.8,size = 0) +
  geom_polygon(data = ji_sang_fix, aes(x = long, y = lat, group = group), alpha = 0, col = "white") +
  geom_point(data = total_mor_eve[[2]], aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                                            text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                                                          도착빈도)), alpha = 0.5, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt", name = "전체 이용량", 
             breaks = c(100, 1000, 10000)) +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10, name = NULL,
                        breaks = c("대여 >> 도착" = 1, "대여 << 도착" = 19)) + 
  ggtitle("오후 5시 ~ 8시 거치대 이용량") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

total_mor_dosi + total_eve_dosi 

load("./data/contest/total_7.RData")
load("./data/contest/ondo.RData")
head(ondo)
head(total_7)
str(total_7)
total_7$대여일시 = as.Date(total_7$대여일시, "%F")
new_ondo = ondo[ondo$date >= min(total_7$대여일시),]

library(scales)
summary(ondo)

library(ggthemes)
library(colorspace)
pal = choose_palette()
a = pal(3)
ggplot() + geom_point(data = total_7, aes(x = 대여일시, y = n, color = n), size = 2) +
  geom_line(data = new_ondo, aes(x = date, y = (ondo + 80) * 1000), lwd = 0.6, color = "orange") +
  xlim(c(as.Date("2017-06-25", "%F"), as.Date("2019-06-10", "%F"))) + 
  scale_x_date(breaks = seq(as.Date("2017-07-01", "%F"), as.Date("2019-05-31", "%F"), length = 8), 
               labels = date_format("%Y-%m")) +
  ylab("대여횟수") + xlab("날짜") +
  scale_y_continuous(sec.axis = sec_axis(~./1000 - 80, name = "평균 온도 ("~degree~"C)", 
                                         breaks = c(-15, 0, 15, 30))) +
  scale_color_gradient2(low = a[1], mid = a[2], high = a[3]) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white",
                                        color = "lightgrey"),
        panel.grid.major.x = element_line(color = "lightgrey",
                                          linetype = "dashed"),
        panel.grid.major.y = element_line(color = "lightgrey",
                                          size = 1.2)) 

