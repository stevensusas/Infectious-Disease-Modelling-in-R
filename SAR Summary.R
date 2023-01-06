  library(ggplot2)
  SAR1<- read.csv("SAR.csv", TRUE, ",")
  
  SAR1
  
  SARGraph<-ggplot(SAR1, aes(x=Contact.Type, y=SAR,fill = Contact.Type,alpha=0.000001)) + 
    geom_violin()+ geom_boxplot(width=0.1)+ ggtitle("Statistical Summary of Collected Data Points on Secondary Attack Rates (SAR) By Contact Scenarios")+stat_summary(fun=mean, geom="point",shape = 23, size=3, color="black")+geom_jitter(shape=10, position=position_jitter(0.2))+ theme(legend.position="none")+ scale_x_discrete(limits=c("Household Unprotected","Household Protected",  "Nonhousehold Unprotected","Nonhousehold Protected","Close Contact"))
  SARGraph