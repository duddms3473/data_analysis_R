###1��

install.packages("ggplot2")
metro<-read.csv("metro_win.csv")
library(ggplot2)
library(tidyr)
metro_m<-gather(metro,key="time",value="people",X04...05:X03...04)
#metro���� �� ���� �ش��ϴ� �ð��� key(time)���� ��Ÿ���� �����α����� people�� ��Ÿ����.

metro_m$��¥<-as.Date(as.character(metro_m$��¥),format="%Y-%m-%d")
#vector�� ��¥�� date�� �ٲ۴�.
str(metro_m)
metro_m$time<-factor(metro_m$time)
#�ð������� factor�� �ٲ��ش�.
str(metro_m)

prob1<-subset(metro_m,��¥=="2019-11-01"&����=="���Ǵ��Ա�(������)")
#prob1�� 11��1���̰�, ���Ǵ��Ա����� �ش��ϴ� �����͸� �����´�.

ggplot(prob1,aes(x=time,y=people))+geom_line(aes(group=����,color=����))+
  labs(x="�ð�",y="�ο� ��",title="���Ǵ��Ա�(������)�� 11/1 ������ �ο� ��")+scale_x_discrete(labels=0:23)
#line�ִ� �׷����� ��Ÿ���ش�.x���� �ð��̰�, y�� �����α����̴�.

###2��

prob2<-subset(metro_m,����=="���Ǵ��Ա�(������)"&��¥>"2019-11-03"&��¥<"2019-11-11")
#������ ���Ǵ��Ա��� �������� 11/4~11/10 �����͸� �ҷ��Դ�.
prob2_up<-subset(prob2,����=="����")
prob2_down<-subset(prob2,����=="����")
#�� �� ����,���� �����͸� subset���� �ҷ��Դ�.
ggplot(prob2_up,aes(x=time,y=��¥,fill=people))+geom_tile()+labs(x="�ð�", title="���Ǵ��Ա��� ���Ϻ� �ð��뺰 ���� �ο� ��",fill="�ο� ��")+scale_x_discrete(labels=0:23)+scale_fill_continuous(type="viridis")
ggplot(prob2_down,aes(x=time,y=��¥,fill=people))+geom_tile()+labs(x="�ð�", title="���Ǵ��Ա��� ���Ϻ� �ð��뺰 ���� �ο� ��",fill="�ο� ��")+scale_x_discrete(labels=0:23)+scale_fill_continuous(type="viridis")
#������ �ο����� heat map���� ��Ÿ�´�.


###3��

prob3<-subset(metro_m,ȣ��=="7ȣ��")
prob3<-prob3[c(5:8)]
#7ȣ�� data�� 5~8���� �ҷ��Դ�.
prob3<-aggregate(x=prob3$people,by=list(prob3$����),sum)
#�����α����� sum���� ��ġ��,group������ �������� �ߴ�.
library(dplyr)
prob3<-arrange(prob3,-x)
#�����͸� ������������ �����ߴ�.
prob3<-prob3[1:20,]
#����20���� �̾Ƴ´�.
colnames(prob3)<-c("����","�ο���")
#Į�� �̸��� Group.1 x�� ����, �ο����� �����ߴ�.
ggplot(prob3,aes(x=reorder(����,�ο���),y=�ο���))+geom_bar(stat="identity")+coord_flip()+geom_text(aes(label=�ο���),hjust=-0.1,size=3)+scale_y_continuous(limits=c(0,2700000),breaks=seq(0,2500000,500000),label=scales::comma)+labs(x="",y="",title="7ȣ�� ���� �����α� ��")+theme_minimal()
#�����͸� �������� �ٲ㼭 ����׷����� ��Ÿ�´�.



###4��

prob3<-subset(metro_m,ȣ��=="7ȣ��")
prob3<-prob3[c(5:8)]
prob3<-aggregate(x=prob3$people,by=list(prob3$����),sum)
prob3<-arrange(prob3,-x)
colnames(prob3)<-c("����","�ο���")
#3�������� Ǯ�� ����20���� ����� �����͸� �ٽ� ��������..

coord<-read.csv("metro_coord_win.csv")
prob4<-left_join(prob3,coord,by=c("����"))
#prob3�� coord�� ������ �������� �����͸� ���ƴ�. 
bbox<-c(left=min(prob4$lon)-0.05,right=max(prob4$lon)+0.05,bottom=min(prob4$lat)-0.05,top=max(prob4$lat)+0.05)
#������ ��Ÿ�� ����,�浵�� �����ߴ�. ���� �浵�� �ּ�,�ִ� +-0.05�� �ߴ�.

install.packages("ggmap")
library(ggmap)
map<-get_stamenmap(bbox,zoom=12)
ggmap(map,base_layer=ggplot(prob4,aes(x=lon,y=lat,alpha=�ο���,size=�ο���)))+geom_point(color="blue")+labs(title="7ȣ�� �����α� ����",alpha="�����α�",size="�����α�")
#������ ��Ÿ����, �������� prob4�����͸� point�� ��Ÿ������.