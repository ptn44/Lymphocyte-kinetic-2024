

TCD3.pred=read.csv(file = 'TCD3.pred.csv')%>%select(-X)%>%mutate(dff=c(diff(conc)/.84*100,0))
B.pred=read.csv(file = 'B.pred.csv')%>%select(-X)%>%mutate(dff=c(diff(conc)/.84*100,0))
B.pred=B.pred%>%summarise(time=seq(0, 180, by = 0.5),
                          conc=ifelse(time<=60, conc, 0.9908224),
                          type='B cells')%>%mutate(dff=c(diff(conc)/.96*100,0))
pred=rbind(B.pred, TCD3.pred)%>%mutate(dff.2=ifelse(abs(dff)<0.01, 0, dff))
rec.time=pred%>%group_by(type)%>%summarise(t=which.min(abs(dff.2)),
                                           t2=round(time[t]))

ggplot()+
  geom_line(data=pred, aes(x=time, y=conc, color=type), linewidth=1.5)+
  scale_color_jco()+
  theme_bw()+
  labs(y='Relative concentration to baseline', x='Day after last fractionated dose', color=NULL)+
  theme(legend.position = 'top')+ylim(0.4, 1.3)+
  geom_hline(yintercept = 1, linetype=2, color='#619CFF')+
  geom_hline(yintercept = 0.57, linetype=2, color='#619CFF')+
  geom_hline(yintercept = 0.48, linetype=2, color='#619CFF')+
  geom_text(aes(150,0.57,label = 'Grade 1 lymphopenia', vjust = -1), color='#619CFF')+
  geom_text(aes(150,0.48,label = 'Grade 2 lymphopenia', vjust = -1), color='#619CFF')+
  geom_text(aes(160,1,label = 'Baseline', vjust = -1), color='#619CFF')


#T-cells----
dat.out.CD3.3=read.csv('mod.CD3.3comp.csv')
#Meancriteria
dat.G1= dat.out.CD3.3%>%mutate(G1=A_circ-0.57)%>%
  filter(G1>=0)%>%
  group_by(ID)%>%filter(G1==min(G1))%>%distinct()%>%
  summarise(ID=ID, type='G1', time=time)

dat.G2= dat.out.CD3.3%>%mutate(G1=A_circ-0.48)%>%
  filter(G1>=0)%>%
  group_by(ID)%>%filter(G1==min(G1))%>%distinct()%>%
  summarise(ID=ID, type='G2', time=time)

dat.G3= dat.out.CD3.3%>%mutate(G1=A_circ-0.21)%>%
  filter(G1>=0)%>%
  group_by(ID)%>%filter(G1==min(G1))%>%distinct()%>%
  summarise(ID=ID, type='G3', time=time)

dat.G4= dat.out.CD3.3%>%mutate(G1=A_circ-0.14)%>%
  filter(G1>=0)%>%
  group_by(ID)%>%filter(G1==min(G1))%>%distinct()%>%
  summarise(ID=ID, type='G4', time=time)

dat.G=rbind(dat.G1, dat.G2, dat.G3)%>%distinct()%>%
  mutate(log=log(time+0.001))


ggplot(dat.G, aes(x=type, y=time))+
  geom_boxplot()+
  scale_y_log10()+
  labs(y='Days after irradiation', x='Lymphopenia grade')+
  theme_bw()
dat.G=dat.G%>%group_by(type)%>%
  summarise(mean=mean(time),
            mode=mode(time))


#B-cells----
dat.out.B.3=read.csv('mod.B.cells.2comp125.csv')

#Meancriteria
dat.G1= dat.out.B.3%>%mutate(G1=A_circ-0.57)%>%
  filter(G1>=0)%>%
  group_by(ID)%>%filter(G1==min(G1))%>%distinct()%>%
  summarise(ID=ID, type='G1', time=time)

dat.G2= dat.out.B.3%>%mutate(G1=A_circ-0.48)%>%
  filter(G1>=0)%>%
  group_by(ID)%>%filter(G1==min(G1))%>%distinct()%>%
  summarise(ID=ID, type='G2', time=time)

dat.G3= dat.out.B.3%>%mutate(G1=A_circ-0.21)%>%
  filter(G1>=0)%>%
  group_by(ID)%>%filter(G1==min(G1))%>%distinct()%>%
  summarise(ID=ID, type='G3', time=time)

dat.G4= dat.out.B.3%>%mutate(G1=A_circ-0.14)%>%
  filter(G1>=0)%>%
  group_by(ID)%>%filter(G1==min(G1))%>%distinct()%>%
  summarise(ID=ID, type='G4', time=time)

dat.G=rbind(dat.G1, dat.G2, dat.G3)%>%distinct()%>%
  mutate(log=log(time+0.001))

ggplot(dat.G, aes(x=type, y=time))+
  geom_violin()+
  scale_y_log10()+
  labs(y='Days after irradiation', x='Lymphopenia grade')+
  theme_bw()
dat.G=dat.G%>%group_by(type)%>%
  summarise(mean=mean(time),
            mode=mode(time))


