ICE<- read.csv("Intervention Coefficient Summary.csv")
ICE
positions <- c("Enforcement of Dorm&Barracks Mandate", "Tactical Pause", "Contact Tracing")
ggplot(ICE, aes(fill=Quantity, y=Intervention.Coefficient, x=Intervention)) + 
  geom_bar(position="stack", stat="identity")+ylab("Intervention Coefficient")+ggtitle("Intervention Coefficient by Interventions and Effect and Quantities")+scale_x_discrete(limits = positions)
