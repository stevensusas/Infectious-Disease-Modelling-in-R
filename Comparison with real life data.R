comp<- read.csv("Comparison with real life data.csv")

library(ggplot2)
graph<- ggplot(comp, aes(x=Weeks.after.introduction.of.index.case,y=Cases,color= Quantity))+geom_point()+geom_line()
graph+xlab("Weeks after introduction of index case")+ggtitle("Comparison of Projected Incidence and Real Life Incidence")+ylim(0,800)