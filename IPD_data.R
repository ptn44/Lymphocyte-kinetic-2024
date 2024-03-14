#Simulation of individual patient data from litterature aggregate data----
data=read.csv(file = 'GBM_baseline_IPD.csv')
data.rate=melt(data = data[,c(1, 4:7)],
               id='ID')
ggplot(data, aes(x=round(ALC,0))) + geom_density(alpha=.2, fill="#FF6666")+
  scale_y_continuous(name = "Density (%)", labels=scales::percent)+
  labs(x='Absolute lymphocyte count (cells/??L)')+
  theme_bw()+
  labs(x='Absolute lymphocyte count (cells/??L)', color='Lymphopenia grade')+
  theme_bw()+
  geom_vline(aes(color="Grade 1", xintercept = 1000),   
             linetype="dashed", size=1)+
  geom_vline(aes(color="Grade 2",xintercept = 800),   
             linetype="dashed", size=1)+
  geom_vline(aes(color="Grade 3",xintercept = 500),   
             linetype="dashed", size=1)+
  geom_vline(aes(color="Grade 4",xintercept = 200),   
             linetype="dashed", size=1)+
  scale_color_jco()+
  scale_fill_viridis_d()
ggplot(data, aes(x=round(ALC,0), fill=Study)) + 
  geom_density(alpha=.3) +
  scale_y_continuous(name = "Density (%)", labels=scales::percent)+
  labs(x='Absolute lymphocyte count (cells/??L)', color='Lymphopenia grade')+
  theme_bw()+
  geom_vline(aes(color="Grade 1", xintercept = 1000),   
             linetype="dashed", size=1)+
  geom_vline(aes(color="Grade 2",xintercept = 800),   
             linetype="dashed", size=1)+
  geom_vline(aes(color="Grade 3",xintercept = 500),   
             linetype="dashed", size=1)+
  geom_vline(aes(color="Grade 4",xintercept = 200),   
             linetype="dashed", size=1)+
  scale_color_jco()+
  scale_fill_viridis_d()


ggplot(data=data.rate[data.rate$value<1,], aes(x=variable, y=value))+
  # geom_boxplot()+
  scale_y_log10() +
  labs(y='Relative concentration to baseline', x='Lymphopenia grade')+
  theme_bw()+
  geom_violin(width=1)+
  stat_summary(fun=mean, colour="red", geom="point",
               size=2, show.legend=FALSE)+
  stat_summary(fun=mean, colour="red", geom="point",
               size=2, show.legend=FALSE)
