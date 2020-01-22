
library(dplyr)
a = read.csv("./data/contest/서울특별시 공공자전거 신규가입자 정보(월별)_2018.7_2019.5.csv")
library(reshape2)
org = a[,1:8]
a = melt(org, id.vars = "구분", variable.name = "연령" )
str(a)
levels(a$varia)

b = org %>% mutate(전체 = X10대 + X20대 + X30대 + X40대 + X50대 + X60대 + X70대.) 
b = b[,c(1, 9)]

d = read.csv("./data/contest/서울특별시 공공자전거 신규가입자 정보(월별)_2018년.csv")
str(d)

f = d %>% group_by(X.대여일자.) %>% summarize(전체 = sum(X.신규가입자수.)) %>% as.data.frame()
colnames(f) = colnames(b)
f
library(stringr)
f$구분 = str_replace_all(f$구분, "\\'", "")
new_p =rbind(f, b)
library(ggplot2)

new_p

p_201706 = 1126584 - sum(new_p[1:20, 2])
cum_p = new_p
cum_p[1,2] = cum_p[1,2] + p_201706
cum_p[,2] = cumsum(cum_p[,2])
total_p = cbind(new_p, cum_p[,2])
colnames(total_p) = c("date", "신규", "누적")
str(total_p)
total_p
total_p = total_p %>% mutate(이전달누적 = 누적 - 신규)
total_p = total_p[,-3]
total_p_l = melt(total_p, id.vars = "date", variable.name = "구분", value.name = "회원수")
total_p_l$구분 = factor(total_p_l$구분, levels = rev(levels(total_p_l$구분)))

total_p_l$date = paste0(total_p_l$date, "01")
total_p_l$date = as.Date(total_p_l$date, format("%Y%m%d"))
library(scales)
ggplot() + geom_bar(data = total_p_l, aes(x = date, y = 회원수, fill = 구분), stat = "identity") +
  scale_x_date(labels = date_format("%Y-%m"), breaks = as.Date(c("2017-07-01", "2018-01-01", "2018-07-01", "2019-01-01", "2019-05-01"))) +
  scale_y_continuous(labels = comma, breaks = seq(200000, 1400000, length = 7),
                     expand = c(0,0)) + scale_fill_manual(values = pal[1:2]) +
  theme(panel.background = element_rect(fill = "white", color = NULL),
        axis.line.x = element_line(color = "gray"),
        axis.ticks.y = element_line(color = "lightgray", size = 1.2),
        panel.grid.major.y = element_line(color = "lightgray",
                                          size = 1.2),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank())
ggsave("C:/Users/Serin/Desktop/누적회원수.png")
ggplot2::theme
library(colorspace)
a = choose_palette()
pal = a(3)
library(dplyr)
?scale_y_continuous
load("./data/contest/perday.RData")
perday
str(perday)
str(per_d)
per_d =  rbind(perday[[1]], perday[[2]], perday[[3]], perday[[4]],
               perday[[5]], perday[[6]], perday[[7]])

same = per_d %>% filter(대여소번호 == 반납대여소번호)
count_per = per_d %>% 
  group_by(as.Date(대여일시), 요일) %>% 
  tally() %>% as.data.frame()
count_per_same = same %>% 
  group_by(as.Date(대여일시), 요일) %>% 
  tally() %>% as.data.frame()
merge(count)
nrow(count_per_same)
levels(count_per$요일)
as.numeric(count_per$요일) <= 5
all(count_per[,1] == count_per_same[,1])
b = c()
for(i in 1:nrow(count_per)){
  if(as.numeric(count_per$요일[i]) <= 5){
    b[i] = "평일"
  }else{
    b[i] = "주말"
  }
}
total_per = cbind(count_per, count_per_same[,3], day = b)
colnames(total_per) = c("date", "요일", "전체횟수", "회귀횟수", "day")
total_per = total_per %>% mutate(비율 = 회귀횟수/전체횟수)

save(total_per, file = "C:/Users/Serin/Desktop/평일주말비율.RData")

colnames(total_per)[5:6] = c("구분", "귀환비율")
weekday_end_rate = total_per
bartlett.test(귀환비율 ~ 구분, data = weekday_end_rate)
library(dplyr)
summary(aov(귀환비율 ~ 구분, data = weekday_end_rate))
hist(weekday_end_rate[weekday_end_rate$구분=="주말", 6])
shapiro.test(weekday_end_rate$귀환비율)
qqnorm(weekday_end_rate$귀환비율)
qqline(weekday_end_rate$귀환비율, col = "red")
wilcox.test(귀환비율 ~ 구분, data = weekday_end_rate)
ks.test(weekday_end_rate$귀환비율, "pnorm")
weekday_rate = weekday_end_rate[weekday_end_rate$구분=="평일", c(1, 6)]
weekend_rate = weekday_end_rate[weekday_end_rate$구분=="주말", c(1, 6)]
weekday_end_rate$구분 = factor(weekday_end_rate$구분, levels = rev(levels(weekday_end_rate$구분)))
library(ggplot2)


library(ggthemes)
ggplot() + 
  geom_histogram(data = weekday_end_rate, aes(x = 귀환비율, y = ..density.., fill = 구분), bins = sqrt(nrow(weekday_rate)),
                 color = "white") +
  facet_wrap(~요일, ncol = 5) + 
  scale_fill_manual(values = c("skyblue", "orange")) + xlim(c(0, 0.3)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() + theme(legend.position = NULL)
ggplot2::facet