rm(list = ls())
gc()
str(pertime)
names(pertime)
tail(pertime[[8]])

load("./data/contest/ddarung_gcd.RData")
load("./data/contest/ddarung_road.RData")
library(dplyr)
ddarung_road

gcd_start_freq = vector(mode = "list")
n = length(ddarung_road)
for(i in 1:n){
  gcd_start_freq[[i]] = ddarung_road[[i]] %>% group_by(대여소번호) %>% 
    tally() %>% as.data.frame()
}

gcd_final_freq = vector(mode = "list")
for(i in 1:n){
  gcd_final_freq[[i]] = ddarung_road[[i]] %>% 
    group_by(반납대여소번호) %>% 
    tally() %>% as.data.frame()
  colnames(gcd_final_freq[[i]])[1] = "대여소번호"
}

gcd_freq = vector(mode = "list")
for(i in 1:n){
  a = merge(gcd_start_freq[[i]], 
            gcd_final_freq[[i]], by = "대여소번호", all = T)  
  colnames(a)[c(2,3)] = c("대여빈도", "도착빈도")
  gcd_freq[[i]] = a
  print(all(gcd_start_freq[[i]][,1] %in% gcd_freq[[i]][,1]))
  print(all(gcd_final_freq[[i]][,1] %in% gcd_freq[[i]][,1]))
}

for(i in 1:n){
  b = match(gcd_freq[[i]]$대여소번호, 
            final_ddarung$대여소번호)
  print(all(!is.na(b)))
  gcd_freq[[i]] = cbind(gcd_freq[[i]], final_ddarung[b, c(4, 5)])
}
for(i in 1:n){
  gcd_freq[[i]]$대여빈도[is.na(gcd_freq[[i]]$대여빈도)] = 0
  gcd_freq[[i]]$도착빈도[is.na(gcd_freq[[i]]$도착빈도)] = 0
  gcd_freq[[i]] = gcd_freq[[i]] %>% mutate(전체이용량 = 도착빈도 + 대여빈도,
                                            유입량 = 도착빈도 - 대여빈도)
  
}
head(gcd_freq[[1]])
summary(c(gcd_freq[[1]][,7], gcd_freq[[2]][,7], gcd_freq[[3]][,7], gcd_freq[[4]][,7],
        gcd_freq[[5]][,7], gcd_freq[[6]][,7], gcd_freq[[7]][,7], gcd_freq[[8]][,7]))
?plot


library(ggplot2)
load("./data/contest/seoulmap.RData")


empty_theme = theme(legend.position = "right",
                    legend.text = element_text(size = 8),
                    axis.title = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    panel.background = element_blank()) 

seoulmap = ggplot(data = seoullonglat) +  
  geom_polygon(aes(x = long, y= lat, group = group),
               fill = "lightgrey", col = "white", size = 1) + empty_theme

seoulmap +
  geom_point(data = final_ddarung, aes(x = 경도, y = 위도), col = "darkblue") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), 
            size = 2.8)


head(gcd_freq[[1]])
gcd_freq_level = vector(mode = "list")


head(gcd_freq_level)
library(colorspace)
pal = choose_color()
head(gcd_freq[[8]])
a1 = cut(gcd_freq[[8]][,7], breaks = c(-5000, -500, seq(-200, 200, length = 16), 500, 5000))
a = cbind(gcd_freq[[8]], a1 = as.numeric(a1))
str(a)

b = seoulmap +
  geom_point(data = a, 
             aes(x = 경도, y = 위도, size = 전체이용량, color = a1,
                 text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                               도착빈도)), alpha = 0.5, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt", name = "전체 이용량(대여 횟수 + 도착 횟수)", 
            breaks = c(100, 1000, 10000)) +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10)
library(plotly)
ggplotly(b, tooltip = "text", height = 1400, width = 2000)
?ggplotly
  
?scale_fill_gradient2

#######################################################################3333
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(patchwork)
library(colorspace)

load("./data/contest/pertime.RData")
load("./data/contest/seoulmap.RData")
load("./data/contest/ddarung_gcd.RData")
pertime
str(pertime)

time_start_freq = vector(mode = "list")
n = length(pertime)
for(i in 1:n){
  time_start_freq[[i]] = pertime[[i]] %>% group_by(대여소번호) %>% 
    tally() %>% as.data.frame()
}

time_final_freq = vector(mode = "list")
for(i in 1:n){
  time_final_freq[[i]] = pertime[[i]] %>% 
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
str(time_gcd_freq[[1]])


names(time_gcd_freq) = names(pertime)


total_per = rbind(time_gcd_freq[[1]][,6:8], time_gcd_freq[[2]][,6:8], time_gcd_freq[[3]][,6:8], time_gcd_freq[[4]][,6:8],
               time_gcd_freq[[5]][,6:8], time_gcd_freq[[6]][,6:8], time_gcd_freq[[7]][,6:8], time_gcd_freq[[8]][,6:8])
summary(total_per)
ggplot(data = total_per) + geom_histogram(aes(x = pr, y = ..density..), 
                                          bins = round(sqrt(nrow(total_per))) + 1, fill = "white", color = "black") +
  geom_density(aes(x = pr), col = "red")

str(time_gcd_freq[[3]])

a1 = cut(time_gcd_freq[[3]][,8], breaks = c(-1, seq(-0.5, 0.5, length = 18), 1))
morning = cbind(time_gcd_freq[[3]], per_label = as.numeric(a1))
str(a)
library(colorspace)
pal = choose_color()
pal = c("#FB5F50", "#BF49DC", "#47B1F7")


b = seoulmap +
  geom_point(data = morning, 
             aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                 text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                               도착빈도)), alpha = 0.5, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt", name = "전체 이용량", 
             breaks = c(100, 1000, 10000)) +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10) + theme(legend.position = "none") +
  ggtitle("오전 7시 ~ 10시") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

d1 = cut(time_gcd_freq[[7]][,8], breaks = c(-1, seq(-0.5, 0.5, length = 18), 1))
evening = cbind(time_gcd_freq[[7]], per_label = as.numeric(d1))
str(evening)

f = seoulmap +
  geom_point(data = evening, 
             aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                 text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                               도착빈도)), alpha = 0.5, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt", name = "전체 이용량", 
             breaks = c(100, 1000, 10000)) +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10, breaks = c("대여 >> 도착" = 1, "대여 << 도착" = 19),
                        name = NULL) + 
  ggtitle("오후 7시 ~ 10시") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
?ggtitle
  
head(time_gcd_freq[7])
library(devtools)
install_github("thomasp85/patchwork")
library("patchwork")

hist(d$유입량, breaks = 100, freq = F)
lines(density(d$유입량), col = "red")
cor(a$pr[a$대여소번호 %in% d$대여소번호],
    d$pr[d$대여소번호 %in% a$대여소번호])

(b + f )



############################################
str(pertime[3])
pertime[[3]][,11]


library(dplyr)
load("./data/contest/pertime.RData")
ddarung_19_2_mor = pertime[[3]] %>% 
  filter(as.Date("2019-03-31", format = "%F") < 
           as.Date(대여일시, format = "%F"))

ddarung_19_2_eve = pertime[[7]] %>% 
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
str(start_19_2)

final_19_2 = vector(mode = "list")
for(i in 1:n){
  final_19_2[[i]] = ddarung_19_2[[i]] %>% 
    group_by(반납대여소번호) %>% 
    tally() %>% as.data.frame()
  colnames(final_19_2[[i]])[1] = "대여소번호"
}
str(final_19_2)

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
str(time_gcd_19_2[[1]])


for(i in 1:n){
  time_gcd_19_2[[i]] = time_gcd_19_2[[i]][,1:5]
  time_gcd_19_2[[i]]$대여빈도[is.na(time_gcd_19_2[[i]]$대여빈도)] = 0
  time_gcd_19_2[[i]]$도착빈도[is.na(time_gcd_19_2[[i]]$도착빈도)] = 0
  time_gcd_19_2[[i]] = time_gcd_19_2[[i]] %>% mutate(전체이용량 = 도착빈도 + 대여빈도,
                                                          유입량 = 도착빈도 - 대여빈도) %>%
    mutate(pr = 유입량/전체이용량)
}



a1 = cut(time_gcd_19_2[[1]][,8], breaks = c(-1, seq(-0.5, 0.5, length = 18), 1))
morning = cbind(time_gcd_19_2[[1]], per_label = as.numeric(a1))
str(morning)
pal = choose_color()
pal = c("#FB5F50", "#BF49DC", "#47B1F7")


b = seoulmap +
  geom_point(data = a, 
             aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                 text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                               도착빈도)), alpha = 0.5, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt", name = "전체 이용량(대여 횟수 + 도착 횟수)", 
             breaks = c(100, 1000, 10000)) +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10) + theme(legend.position = "none") +
  ggtitle("2019년도 4 ~ 5월 오전 7시 ~ 10시 거치대 이용량") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


d1 = cut(time_gcd_19_2[[2]][,8], breaks = c(-1, seq(-0.5, 0.5, length = 18), 1))
evening = cbind(time_gcd_19_2[[2]], per_label = as.numeric(d1))
str(d)

f = seoulmap +
  geom_point(data = evening, 
             aes(x = 경도, y = 위도, size = 전체이용량, color = per_label,
                 text = paste0("대여소번호 : ", 대여소번호, "\n", "대여빈도 : ", 대여빈도, "\n", "도착빈도 : ", 
                               도착빈도)), alpha = 0.7, shape = "O") +
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), size = 2.8) + 
  scale_size(range = c(0.01, 12), trans = "sqrt", name = "전체 이용량", 
             breaks = c(10, 100, 1000)) +
  scale_color_gradient2(low = pal[1], high = pal[3], mid = pal[2],
                        midpoint = 10, breaks = c("대여 >> 도착" = 1, "대여 << 도착" = 19),
                        name = NULL) + 
  ggtitle("2019년도 4 ~ 5월 오후 7시 ~ 10시 거치대 이용량") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

b + f


load("./data/contest/transport.RData")
transp
str(transp)
