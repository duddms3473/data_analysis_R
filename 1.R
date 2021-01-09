###1번

install.packages("ggplot2")
metro<-read.csv("metro_win.csv")
library(ggplot2)
library(tidyr)
metro_m<-gather(metro,key="time",value="people",X04...05:X03...04)
#metro파일 중 열에 해당하는 시간을 key(time)으로 나타내고 유동인구수는 people로 나타낸다.

metro_m$날짜<-as.Date(as.character(metro_m$날짜),format="%Y-%m-%d")
#vector인 날짜를 date로 바꾼다.
str(metro_m)
metro_m$time<-factor(metro_m$time)
#시간변수도 factor로 바꿔준다.
str(metro_m)

prob1<-subset(metro_m,날짜=="2019-11-01"&역명=="숭실대입구(살피재)")
#prob1에 11월1일이고, 숭실대입구역에 해당하는 데이터를 가져온다.

ggplot(prob1,aes(x=time,y=people))+geom_line(aes(group=구분,color=구분))+
  labs(x="시간",y="인원 수",title="숭실대입구(살피재)역 11/1 승하차 인원 수")+scale_x_discrete(labels=0:23)
#line있는 그래프로 나타내준다.x축은 시간이고, y는 유동인구수이다.

###2번

prob2<-subset(metro_m,역명=="숭실대입구(살피재)"&날짜>"2019-11-03"&날짜<"2019-11-11")
#역명이 숭실대입구인 데이터중 11/4~11/10 데이터를 불러왔다.
prob2_up<-subset(prob2,구분=="승차")
prob2_down<-subset(prob2,구분=="하차")
#그 중 승차,하차 데이터를 subset으로 불러왔다.
ggplot(prob2_up,aes(x=time,y=날짜,fill=people))+geom_tile()+labs(x="시간", title="숭실대입구역 요일별 시간대별 승차 인원 수",fill="인원 수")+scale_x_discrete(labels=0:23)+scale_fill_continuous(type="viridis")
ggplot(prob2_down,aes(x=time,y=날짜,fill=people))+geom_tile()+labs(x="시간", title="숭실대입구역 요일별 시간대별 하차 인원 수",fill="인원 수")+scale_x_discrete(labels=0:23)+scale_fill_continuous(type="viridis")
#승하차 인원수를 heat map으로 나타냈다.


###3번

prob3<-subset(metro_m,호선=="7호선")
prob3<-prob3[c(5:8)]
#7호선 data중 5~8열만 불러왔다.
prob3<-aggregate(x=prob3$people,by=list(prob3$역명),sum)
#유동인구수를 sum으로 합치고,group기준은 역명으로 했다.
library(dplyr)
prob3<-arrange(prob3,-x)
#데이터를 내림차순으로 정렬했다.
prob3<-prob3[1:20,]
#상위20위만 뽑아냈다.
colnames(prob3)<-c("역명","인원수")
#칼럼 이름을 Group.1 x를 역명, 인원수로 변경했다.
ggplot(prob3,aes(x=reorder(역명,인원수),y=인원수))+geom_bar(stat="identity")+coord_flip()+geom_text(aes(label=인원수),hjust=-0.1,size=3)+scale_y_continuous(limits=c(0,2700000),breaks=seq(0,2500000,500000),label=scales::comma)+labs(x="",y="",title="7호선 월간 유동인구 수")+theme_minimal()
#데이터를 수직으로 바꿔서 막대그래프를 나타냈다.



###4번

prob3<-subset(metro_m,호선=="7호선")
prob3<-prob3[c(5:8)]
prob3<-aggregate(x=prob3$people,by=list(prob3$역명),sum)
prob3<-arrange(prob3,-x)
colnames(prob3)<-c("역명","인원수")
#3번문제를 풀며 상위20위로 변경된 데이터를 다시 복구해줌..

coord<-read.csv("metro_coord_win.csv")
prob4<-left_join(prob3,coord,by=c("역명"))
#prob3와 coord를 역명을 기준으로 데이터를 합쳤다. 
bbox<-c(left=min(prob4$lon)-0.05,right=max(prob4$lon)+0.05,bottom=min(prob4$lat)-0.05,top=max(prob4$lat)+0.05)
#지도로 나타낼 위도,경도를 설정했다. 위도 경도의 최소,최대 +-0.05로 했다.

install.packages("ggmap")
library(ggmap)
map<-get_stamenmap(bbox,zoom=12)
ggmap(map,base_layer=ggplot(prob4,aes(x=lon,y=lat,alpha=인원수,size=인원수)))+geom_point(color="blue")+labs(title="7호선 유동인구 분포",alpha="유동인구",size="유동인구")
#지도를 나타내고, 지도위에 prob4데이터를 point로 나타내었다.