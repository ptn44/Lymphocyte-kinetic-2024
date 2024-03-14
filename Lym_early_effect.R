library(dplyr)
library(ggplot2)
library(ggpubr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggsci)
library(ggsignif)
library(caret)

data=read.csv(file = 'D3.csv')
scale_y_ln <- function(...) scale_y_continuous(..., trans = scales::log_trans())
scale_x_ln <- function(...) scale_x_continuous(..., trans = scales::log_trans())


data$Group=factor(data$Group, levels = c('Control', 'Tumor_D9','Tumor_D15', 'PTRT', 'XRT_5Gy', 'XRT_10Gy', 'Tumor_XRT_10Gy'))
#Visualization----
ggplot(data=data, aes(y=p.T.cells,x=Group, fill=Group))+
  geom_boxplot()+
  theme_bw()+
  labs(y='Relative concentration to control group', x=NULL, fill=NULL)+
  scale_fill_jco()+
  stat_compare_means(ref.group = 'Control', label = "p.signif",
                     label.y = 2)+
  ggsignif::geom_signif(comparisons = list(c('XRT_10Gy', 'Tumor_XRT_10Gy'),
                                           c('XRT_10Gy','XRT_5Gy'),
                                           c('Tumor_D9','Tumor_D15')), 
                        annotations = c("ns", "ns", "ns"),
                        y_position = c(1.4,1.2, 1.82))+
  ggtitle('T cells')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(data=data, aes(y=p.B.cells,x=Group, fill=Group))+
  geom_boxplot()+
  theme_bw()+
  labs(y='Relative concentration to control group', x=NULL, fill=NULL)+
  scale_fill_jco()+
  stat_compare_means(ref.group = 'Control', label = "p.signif",
                     label.y = 2)+
  ggsignif::geom_signif(comparisons = list(c('XRT_10Gy', 'Tumor_XRT_10Gy'),
                                           c('XRT_10Gy','XRT_5Gy'),
                                           c('Tumor_D9','Tumor_D15')), 
                        annotations = c("**", "**", 'ns'),
                        y_position = c(1.2,1, 1),
                        method='t.test')+
  ggtitle('B cells')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Decision tree----
mod.T=rpart(p.T.cells~., data=data[,c(3:9,11)])
rpart.plot(mod.T, type=5)

mod.B=rpart(p.B.cells~., data=data[,c(3:10)])
rpart.plot(mod.B, type=5)

#Random forest----
set.seed(4497)
mydata=data[,c(3:9,11)]
mod.T <-randomForest(p.T.cells~.,data=mydata, mtry=best.m, importance=TRUE,ntree=500)
imp=data.frame(importance(mod.T))
imp$param=row.names(imp)
ggplot(data=imp, aes(y=X.IncMSE, x=param))+
  geom_bar(stat="identity")+coord_flip()+
  labs(y='Increase of MSE (%)', x=NULL)

set.seed(4497)
mydata=data[,c(3:9,10)]
mod.B <-randomForest(p.B.cells~.,data=mydata, mtry=best.m, importance=TRUE,ntree=500)
imp=data.frame(importance(mod.B))
imp$param=row.names(imp)
ggplot(data=imp, aes(y=X.IncMSE, x=param))+
  geom_bar(stat="identity")+coord_flip()+
  labs(y='Increase of MSE (%)', x=NULL)
