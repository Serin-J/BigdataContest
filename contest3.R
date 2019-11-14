rm(list = ls())
gc()
library(dplyr)
load("./data/contest/ddarung_road.RData")
load("./data/contest/seoulmap.RData")
load("./data/contest/ddarung_gcd.RData")
load("./data/contest/networkline.RData")

ddarung = NULL
for(i in 1:8){
  ddarung = rbind(ddarung, ddarung_road[[i]])
}


dist = ddarung %>% group_by(대여소번호, 반납대여소번호) %>% 
  summarize(mean = mean(이용거리, trim = 0.15, na.rm = T), min = min(이용거리), max = max(이용거리)) %>% as.data.frame()
dist = dist[!is.na(dist$mean), ]
summary(dist)
dist[dist$mean == 0, ]

freq = ddarung %>% group_by(대여소번호, 반납대여소번호) %>%
  tally() %>% as.data.frame()
summary(freq)

dist_level[[8]]
networkline = n
rep(c(1, 2), each = 10)

?rep
networkline[[i]]
dist[[8]]$mean[is.na(dist[[2]]$mean)]

networkline
{
  for(i in 1:7){
    freq[[i]] = ddarung_road[[i]] %>% group_by(대여소번호, 반납대여소번호) %>% tally() %>% as.data.frame()
  }
  
  load("./data/contest/ddarung_gcd.RData")
  
  str(ddarung_road[[1]])
  networkline = vector(mode = "list")
  col_name = c(colnames(final_ddarung)[c(2, 4, 5)], "group")
  for(j in 1:7){
    a = data.frame(matrix(NA, nrow = 2 * nrow(freq[[j]]), ncol = 4))
    colnames(a) = col_name
    num1 = match(freq[[j]][,1], final_ddarung$대여소번호) 
    num2 = match(freq[[j]][,2], final_ddarung$대여소번호) 
    for(i in 1:nrow(freq[[j]])){
      a[c(2*i - 1, 2*i), ] = cbind(final_ddarung[c(num1[i], num2[i]),c(2, 4, 5)], i)
    }
    networkline[[j]] = a
    print(j)
  }
}
save(networkline, file = "./data/contest/networkline.RData")

#############################################################################################

dist = vector(mode = "list")

for(i in 1:8){
  dist[[i]] = ddarung_road[[i]] %>% group_by(대여소번호, 반납대여소번호) %>% 
    summarize(mean = mean(이용거리, trim = 0.15, na.rm = T)) %>% as.data.frame()
  
}
freq = vector(mode = "list")
for(i in 1:8){
  freq[[i]] = ddarung_road[[i]] %>% group_by(대여소번호, 반납대여소번호) %>% 
    tally() %>% as.data.frame()
  
}
str(dist)
str(freq)
dist_freq = vector(mode = "list")
for(i in 1:8){
  dist_freq[[i]] = cbind(dist[[i]], freq[[i]]$n)
}
str(dist_freq)
str(n)
nrow(dist_freq[[1]])
nrow(n[[i]])
n = networkline

for(i in 1:8){
  distance = rep(dist_freq[[i]][,3], each = 2)
  number = rep(dist_freq[[i]][,4], each = 2)
  n[[i]] = cbind(n[[i]], number, distance)
}
for(i in 1:8){
  print(all(!is.na(n[[i]]$distance)))
  n[[i]] = n[[i]][!is.na(n[[i]]$distance),]
  print(all(!is.na(n[[i]]$number)))
}

level_name = c("~ 4km", "4km ~ 10km", "10km ~" )
for(i in 1:8){
  networkline_dist_freq[[i]] = networkline_dist_freq[[i]][,1:6]
  dist_level = cut(networkline_dist_freq[[i]]$distance, c(-0.1, 4000, 10000, 10000000))
  levels(dist_level) = level_name
  networkline_dist_freq[[i]] = cbind(networkline_dist_freq[[i]], dist_level)
}

freq_level_name = c("~ 3회", "4회 ~ 10회", "11회 ~ 30회", "31회 ~ 100회", "100회 ~")
for(i in 1:8){
  freq_level = cut(networkline_dist_freq[[i]]$number, c(-0.1, 3, 10, 30, 100, 100000))
  levels(freq_level) = freq_level_name
  networkline_dist_freq[[i]] = cbind(networkline_dist_freq[[i]], freq_level)
}

for(i in 1:8){
  colnames(networkline_dist_freq[[i]])[8] = "이용횟수"
  levels(networkline_dist_freq[[i]][,8]) = c("~ 3회", "4회 ~ 10회", "11회 ~ 30회", "31회 ~ 100회", "100회 ~")
}
names(networkline_dist_freq) = names(ddarung_road)

save(networkline_dist_freq, file = "./data/contest/networkline_level.RData")
str(networkline_dist_freq[[1]])


###########################################################################################

library(ggplot2)
library(dplyr)

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

load("./data/contest/networkline_level.RData")

level_name = c("~ 4km", "4km ~ 10km", "10km ~" )
short = networkline_dist_freq[[8]] %>% filter(dist_level == level_name[1])
middle = networkline_dist_freq[[8]] %>% filter(dist_level == level_name[2])
long = networkline_dist_freq[[8]] %>% filter(dist_level == level_name[3])
library(colorspace)

pal = choose_palette()
col1 = pal(5)
str(short)
colnames(short)[8] = "이용횟수"
colnames(middle)[8] = "이용횟수"
colnames(long)[8] = "이용횟수"

str(networkline_dist_freq)
long_new = long %>% filter(freq_level != "~ 3")
seoulmap +
  geom_line(data = short, aes(x = 경도, y = 위도, group = group, col = 이용횟수, size = 이용횟수, alpha = 이용횟수)) + 
  geom_polygon(data = seoullonglat, aes(x = long, y = lat, group = group), alpha = 0, col = "white", size = 1) + 
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), 
            size = 3.2, col = "white")  + scale_color_manual(values = col1) + 
  ggtitle("2019년도 4 ~ 5월 서울시 공공자전거 이용 기록", subtitle = "주행거리 4km 이하") +
  scale_size_manual(values = c(0.5, 1, 1.5, 2, 2.5)) + scale_alpha_manual(values = c(0.08, 0.1, 0.2, 0.6, 1)) +
  guides(color = guide_legend(loverrides.aes = list(alpha = 1)))

seoulmap +
  geom_line(data = middle, aes(x = 경도, y = 위도, group = group, col = 이용횟수, size = 이용횟수, alpha = 이용횟수)) + 
  geom_polygon(data = seoullonglat, aes(x = long, y = lat, group = group), alpha = 0, col = "white", size = 1) + 
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), 
            size = 3.2, col = "white")  + scale_color_manual(values = col1) + 
  ggtitle("2019년도 4 ~ 5월 서울시 공공자전거 이용 기록", subtitle = "주행거리 4km 초과, 10km 이하") +
  scale_size_manual(values = c(0.5, 1, 1.5, 2, 2.5)) + scale_alpha_manual(values = c(0.08, 0.1, 0.2, 0.6, 1)) +
  guides(color = guide_legend(loverrides.aes = list(alpha = 1)))

seoulmap +
  geom_line(data = long, aes(x = 경도, y = 위도, group = group, col = 이용횟수, size = 이용횟수, alpha = 이용횟수)) + 
  geom_polygon(data = seoullonglat, aes(x = long, y = lat, group = group), alpha = 0, col = "white", size = 1) + 
  geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), 
            size = 3.2, col = "white")  + scale_color_manual(values = col1) + 
  ggtitle("2019년도 4 ~ 5월 서울시 공공자전거 이용 기록", subtitle = "주행거리 10km 이상") +
  scale_size_manual(values = c(0.5, 1, 1.5, 2, 2.5)) + scale_alpha_manual(values = c(0.08, 0.1, 0.2, 0.6, 1)) +
  guides(color = guide_legend(loverrides.aes = list(alpha = 1)))

#############################################################################

networkline_year = rbind(cbind(기간 = "2018년_3분기", networkline_dist_freq[[5]]), 
                         cbind(기간 = "2018년_4분기", networkline_dist_freq[[6]]), 
                         cbind(기간 = "2019년_1분기", networkline_dist_freq[[7]]), 
                         cbind(기간 = "2019년_2분기", networkline_dist_freq[[8]]))
a = cbind(기간 = "2018년_2분기", networkline_dist_freq[[4]])
colnames(a)[8:9] = c("주행거리", "이용횟수")
networkline_year_plus = rbind(a,
                         networkline_year)
colnames(networkline_year)[8:9] = c("주행거리", "이용횟수")
seoulmap +
  geom_line(data = networkline_year_plus, aes(x = 경도, y = 위도, group = group, 
                                         col = 이용횟수, size = 이용횟수, alpha = 이용횟수)) + 
  facet_grid(주행거리~기간)+ 
  geom_polygon(data = seoullonglat, aes(x = long, y = lat, group = group), alpha = 0, col = "white", size = 0.5) + 
  # geom_text(data = seoul_gu_center, aes(label = group, x = long, y = lat), 
  #           size = 1, col = "white")  + 
  scale_color_manual(values = col1) +
  ggtitle("2018년도 2분기 ~ 2019년도 2분기 서울시 공공자전거 이용 기록") +
  scale_size_manual(values = c(0.5, 1, 1.5, 2, 2.5)) + scale_alpha_manual(values = c(0.08, 0.1, 0.2, 0.6, 1)) +
  guides(color = guide_legend(loverrides.aes = list(alpha = 1)))
str(networkline)
load("./data/contest/ddarung_road.RData")
load("./data/contest/ddarung_gcd.RData")
str(networkline)
