options(max.print=1000000)

Asymptomatic_SAR_H <- 0.04
Asymptomatic_SAR_NN <- 0.008
Asymptomatic_SAR_NP<- 0.002 
Presymptomatic_SAR_H<- 0.061
Presymptomatic_SAR_NN <- 0.046
Presymptomatic_SAR_NP<- 0.0115
Symptomatic_SAR_H <- 0.1745
Symptomatic_SAR_NN <- 0.0520
Symptomatic_SAR_NP<- 0.0177

#Input the SARs
Student_ID <- 1
dw = Student_ID

repeat{
  gender <- rbinom(1,1,480/838)
  Asymptomatic_or_presymptomatic <- if(gender == 1){rbinom(1,1,3/11)}else{
    rbinom(1,1,2/9)
    
  }
  #Determine Student Gender and Initial Symptom Status
  
  Date_of_Test <- sample(0:4,1)
  False_Negativity_Rate <- if (Date_of_Test == 0){
    1
  }else{if(Date_of_Test == 1){
    1
  }else{if(Date_of_Test == 2){
    runif(1,min = 0.96,max = 1)
  }else{if(Date_of_Test ==3){
    runif (1, min = 0.588,max = 0.999)
  }else{if(Date_of_Test == 4){
    runif (1, min = 0.296, max = 0.941)
  }else{if(Date_of_Test == 5){
    runif (1, min = 0.184,max = 0.646)
  }else{if(Date_of_Test == 6){
    runif(1, min = 0.14,max = 0.399)
  }else{if(Date_of_Test == 7){
    runif(1,min = 0.125,max = 0.310)
    
  }}}}}}}}
  
  Test_False_Negative <- rbinom (1,1,False_Negativity_Rate)
  Symptom_onset <- sample(3:6,1)
  
  #If()else{}loop and runif() to determine if student test false-negative
  
  if(Test_False_Negative == 0 & Asymptomatic_or_presymptomatic == 1){
    Removal_Date_of_Asymptomatic<- Date_of_Test;Removal_Date_of_Presymptomatic <- 0;Removal_Date_of_Symptomatic <- 0
  }else{if(Test_False_Negative == 1 & Asymptomatic_or_presymptomatic == 1){
    Removal_Date_of_Asymptomatic <- Date_of_Test+4;Removal_Date_of_Presymptomatic <- 0; Removal_Date_of_Symptomatic <- 0
  }else{if(Symptom_onset <= Date_of_Test & Asymptomatic_or_presymptomatic ==0 ){
    Removal_Date_of_Asymptomatic <- 0;Removal_Date_of_Presymptomatic <- Symptom_onset;Removal_Date_of_Symptomatic <- sample(0:(Date_of_Test-Symptom_onset),1)
  }else{if(Symptom_onset > Date_of_Test & Asymptomatic_or_presymptomatic == 0 & Test_False_Negative == 0){
    Removal_Date_of_Asymptomatic<- 0;Removal_Date_of_Presymptomatic <- Date_of_Test;Removal_Date_of_Symptomatic <- 0
  }else{if(Symptom_onset > Date_of_Test & Asymptomatic_or_presymptomatic == 0 & Test_False_Negative == 1){
    Removal_Date_of_Asymptomatic <- 0;Removal_Date_of_Presymptomatic <- Symptom_onset;Removal_Date_of_Symptomatic <- sample (0:4,1)
  }}
  }}
  }
  
  DaysA <- 0
  InfectionsA <- 0 
  #If()else{}loop to determine the Removal Date of Asymptomatic and Presymptomatic status and Removal Date of Symptomatic Status
  
  if(Removal_Date_of_Asymptomatic == 0){InfectionsA<- 0}else{repeat {
    
    #Define initial infection and days to be 0 for repeat()loop break condition
    
    AContact_Number_M_H <- sample(0:2,1)
    AContact_Number_M_NN <- sample(5:40,1)
    AContact_Number_M_NP <- sample(8:118,1)
    AContact_Number_G_H <- sample(1:3,1)
    AContact_Number_G_NN<- sample (3:14,1)
    AContact_Number_G_NP <- sample(8:34,1)
    #Input Contact Numbers as vectors inside the repeat()loop since they are to be randomized by day
    
    if (gender == 1){
      AContactNumberH <- AContact_Number_M_H;AContactNumberNN<- AContact_Number_M_NN;AContactNumberNP <- AContact_Number_M_NP
    }else{if(gender == 0){
      AContactNumberH <- AContact_Number_G_H;AContactNumberNN<- AContact_Number_G_NN;AContactNumberNP <- AContact_Number_G_NP }
    }
    #If()else{} loop to determine Student's Contact Numbers in vectors based on gender
    
    Asymptomatic_Infections<- sum(rbinom(AContactNumberH,1,Asymptomatic_SAR_H))+sum(rbinom(AContactNumberNN,1,Asymptomatic_SAR_NN))+sum(rbinom(AContactNumberNP,1,Asymptomatic_SAR_NP))
    
    InfectionsA <- InfectionsA+Asymptomatic_Infections
    
    DaysA <- DaysA+1
    if(DaysA == Removal_Date_of_Asymptomatic){break}
  }}
  
  DaysP<- 0
  InfectionsP<- 0
  
  if(Removal_Date_of_Presymptomatic == 0){InfectionsP <- 0}else{repeat{
    
    PContact_Number_M_H <- sample(0:2,1)
    PContact_Number_M_NN <- sample(5:40,1)
    PContact_Number_M_NP <- sample(8:118,1)
    PContact_Number_G_H <- sample(1:3,1)
    PContact_Number_G_NN<- sample (3:14,1)
    PContact_Number_G_NP <- sample(8:34,1)
    #Input Contact Numbers as vectors inside the repeat()loop since they are to be randomized by day
    
    if (gender == 1){
      PContactNumberH <- PContact_Number_M_H;PContactNumberNN<- PContact_Number_M_NN;PContactNumberNP <- PContact_Number_M_NP
    }else{if(gender == 0){
      PContactNumberH <- PContact_Number_G_H;PContactNumberNN<- PContact_Number_G_NN;PContactNumberNP <- PContact_Number_G_NP }
    }
    Presymptomatic_Infections<- sum(rbinom(PContactNumberH,1,Presymptomatic_SAR_H))+sum(rbinom(PContactNumberNN, 1, Presymptomatic_SAR_NN))+sum(rbinom(PContactNumberNP,1,Presymptomatic_SAR_NP))
    
    InfectionsP<- InfectionsP+Presymptomatic_Infections
    
    DaysP<- DaysP+1
    if(DaysP == Removal_Date_of_Presymptomatic){break}
  }}
  
  DaysS<- 0
  InfectionsS<- 0
  
  if(Removal_Date_of_Symptomatic == 0){InfectionsS <- 0}else{repeat{
    
    SContact_Number_M_H <- sample(0:2,1)
    SContact_Number_M_NN <- sample(5:40,1)
    SContact_Number_M_NP <- sample(8:118,1)
    SContact_Number_G_H <- sample(1:3,1)
    SContact_Number_G_NN<- sample (3:14,1)
    SContact_Number_G_NP <- sample(8:34,1)
    #Input Contact Numbers as vectors inside the repeat()loop since they are to be randomized by day
    
    if (gender == 1){
      SContactNumberH <- SContact_Number_M_H;SContactNumberNN<- SContact_Number_M_NN;SContactNumberNP <- SContact_Number_M_NP
    }else{if(gender == 0){
      SContactNumberH <- SContact_Number_G_H;SContactNumberNN<- SContact_Number_G_NN;SContactNumberNP <- SContact_Number_G_NP }
    }
    Symptomatic_Infections<- sum(rbinom(SContactNumberH,1,Symptomatic_SAR_H))+sum(rbinom(SContactNumberNN, 1, Symptomatic_SAR_NN))+sum(rbinom(SContactNumberNP,1,Symptomatic_SAR_NP))
    
    InfectionsS<- InfectionsS+Symptomatic_Infections
    
    DaysS<- DaysS+1
    if(DaysS == Removal_Date_of_Symptomatic){break}
  }}
  
  
  Infections <- InfectionsA+InfectionsP+InfectionsS
  
  Student_ID <- Student_ID+1
  
  dw = rbind(dw,data.frame(Student_ID, Infections)) #Input each individual trial's outcome into data frame
  
  
  if(Student_ID == 10000){break}
}

print(dw)

options(max.print=1000000)

Asymptomatic_SAR_H <- 0.04
Asymptomatic_SAR_NN <- 0.008
Asymptomatic_SAR_NP<- 0.002 
Presymptomatic_SAR_H<- 0.061
Presymptomatic_SAR_NN <- 0.046
Presymptomatic_SAR_NP<- 0.0115
Symptomatic_SAR_H <- 0.1745
Symptomatic_SAR_NN <- 0.0520
Symptomatic_SAR_NP<- 0.0177

#Input the SARs
Student_ID <- 1
d = Student_ID

repeat{
  gender <- rbinom(1,1,480/838)
  Asymptomatic_or_presymptomatic <- if(gender == 1){rbinom(1,1,3/11)}else{
    rbinom(1,1,2/9)
    
  }
  #Determine Student Gender and Initial Symptom Status
  
  Date_of_Test <- sample(0:7,1)
  False_Negativity_Rate <- if (Date_of_Test == 0){
    1
  }else{if(Date_of_Test == 1){
    1
  }else{if(Date_of_Test == 2){
    runif(1,min = 0.96,max = 1)
  }else{if(Date_of_Test ==3){
    runif (1, min = 0.588,max = 0.999)
  }else{if(Date_of_Test == 4){
    runif (1, min = 0.296, max = 0.941)
  }else{if(Date_of_Test == 5){
    runif (1, min = 0.184,max = 0.646)
  }else{if(Date_of_Test == 6){
    runif(1, min = 0.14,max = 0.399)
  }else{if(Date_of_Test == 7){
    runif(1,min = 0.125,max = 0.310)
    
  }}}}}}}}
  
  Test_False_Negative <- rbinom (1,1,False_Negativity_Rate)
  Symptom_onset <- sample(3:6,1)
  
  #If()else{}loop and runif() to determine if student test false-negative
  
  if(Test_False_Negative == 0 & Asymptomatic_or_presymptomatic == 1){
    Removal_Date_of_Asymptomatic<- Date_of_Test;Removal_Date_of_Presymptomatic <- 0;Removal_Date_of_Symptomatic <- 0
  }else{if(Test_False_Negative == 1 & Asymptomatic_or_presymptomatic == 1){
    Removal_Date_of_Asymptomatic <- Date_of_Test+7;Removal_Date_of_Presymptomatic <- 0; Removal_Date_of_Symptomatic <- 0
  }else{if(Symptom_onset <= Date_of_Test & Asymptomatic_or_presymptomatic ==0 ){
    Removal_Date_of_Asymptomatic <- 0;Removal_Date_of_Presymptomatic <- Symptom_onset;Removal_Date_of_Symptomatic <- sample (0:4,1)
  }else{if(Symptom_onset > Date_of_Test & Asymptomatic_or_presymptomatic == 0 & Test_False_Negative == 0){
    Removal_Date_of_Asymptomatic<- 0;Removal_Date_of_Presymptomatic <- Date_of_Test;Removal_Date_of_Symptomatic <- 0
  }else{if(Symptom_onset > Date_of_Test & Asymptomatic_or_presymptomatic == 0 & Test_False_Negative == 1){
    Removal_Date_of_Asymptomatic <- 0;Removal_Date_of_Presymptomatic <- Symptom_onset;Removal_Date_of_Symptomatic <- sample (0:4,1)
  }}
  }}
  }
  
  DaysA <- 0
  InfectionsA <- 0 
  #If()else{}loop to determine the Removal Date of Asymptomatic and Presymptomatic status and Removal Date of Symptomatic Status
  
  if(Removal_Date_of_Asymptomatic == 0){InfectionsA<- 0}else{repeat {
    
    #Define initial infection and days to be 0 for repeat()loop break condition
    
    AContact_Number_M_H <- sample(0:2,1)
    AContact_Number_M_NN <- sample(5:40,1)
    AContact_Number_M_NP <- sample(8:118,1)
    AContact_Number_G_H <- sample(1:3,1)
    AContact_Number_G_NN<- sample (3:14,1)
    AContact_Number_G_NP <- sample(8:34,1)
    #Input Contact Numbers as vectors inside the repeat()loop since they are to be randomized by day
    
    if (gender == 1){
      AContactNumberH <- AContact_Number_M_H;AContactNumberNN<- AContact_Number_M_NN;AContactNumberNP <- AContact_Number_M_NP
    }else{if(gender == 0){
      AContactNumberH <- AContact_Number_G_H;AContactNumberNN<- AContact_Number_G_NN;AContactNumberNP <- AContact_Number_G_NP }
    }
    #If()else{} loop to determine Student's Contact Numbers in vectors based on gender
    
    Asymptomatic_Infections<- sum(rbinom(AContactNumberH,1,Asymptomatic_SAR_H))+sum(rbinom(AContactNumberNN,1,Asymptomatic_SAR_NN))+sum(rbinom(AContactNumberNP,1,Asymptomatic_SAR_NP))
    
    InfectionsA <- InfectionsA+Asymptomatic_Infections
    
    DaysA <- DaysA+1
    if(DaysA == Removal_Date_of_Asymptomatic){break}
  }}
  
  DaysP<- 0
  InfectionsP<- 0
  
  if(Removal_Date_of_Presymptomatic == 0){InfectionsP <- 0}else{repeat{
    
    PContact_Number_M_H <- sample(0:2,1)
    PContact_Number_M_NN <- sample(5:40,1)
    PContact_Number_M_NP <- sample(8:118,1)
    PContact_Number_G_H <- sample(1:3,1)
    PContact_Number_G_NN<- sample (3:14,1)
    PContact_Number_G_NP <- sample(8:34,1)
    #Input Contact Numbers as vectors inside the repeat()loop since they are to be randomized by day
    
    if (gender == 1){
      PContactNumberH <- PContact_Number_M_H;PContactNumberNN<- PContact_Number_M_NN;PContactNumberNP <- PContact_Number_M_NP
    }else{if(gender == 0){
      PContactNumberH <- PContact_Number_G_H;PContactNumberNN<- PContact_Number_G_NN;PContactNumberNP <- PContact_Number_G_NP }
    }
    Presymptomatic_Infections<- sum(rbinom(PContactNumberH,1,Presymptomatic_SAR_H))+sum(rbinom(PContactNumberNN, 1, Presymptomatic_SAR_NN))+sum(rbinom(PContactNumberNP,1,Presymptomatic_SAR_NP))
    
    InfectionsP<- InfectionsP+Presymptomatic_Infections
    
    DaysP<- DaysP+1
    if(DaysP == Removal_Date_of_Presymptomatic){break}
  }}
  
  DaysS<- 0
  InfectionsS<- 0
  
  if(Removal_Date_of_Symptomatic == 0){InfectionsS <- 0}else{repeat{
    
    SContact_Number_M_H <- sample(0:2,1)
    SContact_Number_M_NN <- sample(5:40,1)
    SContact_Number_M_NP <- sample(8:118,1)
    SContact_Number_G_H <- sample(1:3,1)
    SContact_Number_G_NN<- sample (3:14,1)
    SContact_Number_G_NP <- sample(8:34,1)
    #Input Contact Numbers as vectors inside the repeat()loop since they are to be randomized by day
    
    if (gender == 1){
      SContactNumberH <- SContact_Number_M_H;SContactNumberNN<- SContact_Number_M_NN;SContactNumberNP <- SContact_Number_M_NP
    }else{if(gender == 0){
      SContactNumberH <- SContact_Number_G_H;SContactNumberNN<- SContact_Number_G_NN;SContactNumberNP <- SContact_Number_G_NP }
    }
    Symptomatic_Infections<- sum(rbinom(SContactNumberH,1,Symptomatic_SAR_H))+sum(rbinom(SContactNumberNN, 1, Symptomatic_SAR_NN))+sum(rbinom(SContactNumberNP,1,Symptomatic_SAR_NP))
    
    InfectionsS<- InfectionsS+Symptomatic_Infections
    
    DaysS<- DaysS+1
    if(DaysS == Removal_Date_of_Symptomatic){break}
  }}
  
  
  Infections <- InfectionsA+InfectionsP+InfectionsS
  
  Student_ID <- Student_ID+1
  
  d = rbind(d,data.frame(Student_ID, Infections)) #Input each individual trial's outcome into data frame
  
  
  if(Student_ID == 10000){break}
}

print(d)

options(max.print=1000000)

Asymptomatic_SAR_H <- 0.04
Asymptomatic_SAR_NN <- 0.008
Asymptomatic_SAR_NP<- 0.002 
Presymptomatic_SAR_H<- 0.061
Presymptomatic_SAR_NN <- 0.046
Presymptomatic_SAR_NP<- 0.0115
Symptomatic_SAR_H <- 0.1745
Symptomatic_SAR_NN <- 0.0520
Symptomatic_SAR_NP<- 0.0177

#Input the SARs
Student_ID <- 1
dwo = Student_ID

repeat{
  gender <- rbinom(1,1,480/838)
  Asymptomatic_or_presymptomatic <- if(gender == 1){rbinom(1,1,3/11)}else{
    rbinom(1,1,2/9)
    
  }
  #Determine Student Gender and Initial Symptom Status
  
  Date_of_Test <- sample(0:7,1)
  
  False_Negativity_Rate_Test1 <- if (Date_of_Test == 0){
    1
  }else{if(Date_of_Test == 1){
    1
  }else{if(Date_of_Test == 2){
    runif(1,min = 0.96,max = 1)
  }else{if(Date_of_Test ==3){
    runif (1, min = 0.588,max = 0.999)
  }else{if(Date_of_Test == 4){
    runif (1, min = 0.296, max = 0.941)
  }else{if(Date_of_Test == 5){
    runif (1, min = 0.184,max = 0.646)
  }else{if(Date_of_Test == 6){
    runif(1, min = 0.14,max = 0.399)
  }else{if(Date_of_Test == 7){
    runif(1,min = 0.125,max = 0.310)
    
  }}}}}}}}
  
  Test1<- rbinom(1,1,False_Negativity_Rate_Test1)
  
  False_Negativity_Rate_Test2 <- if (Date_of_Test == 0){
    1
  }else{if(Date_of_Test == 1){
    1
  }else{if(Date_of_Test == 2){
    runif(1,min = 0.96,max = 1)
  }else{if(Date_of_Test ==3){
    runif (1, min = 0.588,max = 0.999)
  }else{if(Date_of_Test == 4){
    runif (1, min = 0.296, max = 0.941)
  }else{if(Date_of_Test == 5){
    runif (1, min = 0.184,max = 0.646)
  }else{if(Date_of_Test == 6){
    runif(1, min = 0.14,max = 0.399)
  }else{if(Date_of_Test == 7){
    runif(1,min = 0.125,max = 0.310)
    
  }}}}}}}}
  
  Test2<- rbinom(1,1,False_Negativity_Rate_Test2)
  
  if(Test1 == 0  | Test2 == 0){Test_False_Negative = 0}
  Symptom_onset <- sample(3:6,1)
  
  #If()else{}loop and runif() to determine if student test false-negative
  
  if(Test_False_Negative == 0 & Asymptomatic_or_presymptomatic == 1){
    Removal_Date_of_Asymptomatic<- Date_of_Test;Removal_Date_of_Presymptomatic <- 0;Removal_Date_of_Symptomatic <- 0
  }else{if(Test_False_Negative == 1 & Asymptomatic_or_presymptomatic == 1){
    Removal_Date_of_Asymptomatic <- Date_of_Test+7;Removal_Date_of_Presymptomatic <- 0; Removal_Date_of_Symptomatic <- 0
  }else{if(Symptom_onset <= Date_of_Test & Asymptomatic_or_presymptomatic ==0 ){
    Removal_Date_of_Asymptomatic <- 0;Removal_Date_of_Presymptomatic <- Symptom_onset;Removal_Date_of_Symptomatic <- sample (0:4,1)
  }else{if(Symptom_onset > Date_of_Test & Asymptomatic_or_presymptomatic == 0 & Test_False_Negative == 0){
    Removal_Date_of_Asymptomatic<- 0;Removal_Date_of_Presymptomatic <- Date_of_Test;Removal_Date_of_Symptomatic <- 0
  }else{if(Symptom_onset > Date_of_Test & Asymptomatic_or_presymptomatic == 0 & Test_False_Negative == 1){
    Removal_Date_of_Asymptomatic <- 0;Removal_Date_of_Presymptomatic <- Symptom_onset;Removal_Date_of_Symptomatic <- sample (0:4,1)
  }}
  }}
  }
  
  DaysA <- 0
  InfectionsA <- 0 
  #If()else{}loop to determine the Removal Date of Asymptomatic and Presymptomatic status and Removal Date of Symptomatic Status
  
  if(Removal_Date_of_Asymptomatic == 0){InfectionsA<- 0}else{repeat {
    
    #Define initial infection and days to be 0 for repeat()loop break condition
    
    AContact_Number_M_H <- sample(0:2,1)
    AContact_Number_M_NN <- sample(5:40,1)
    AContact_Number_M_NP <- sample(8:118,1)
    AContact_Number_G_H <- sample(1:3,1)
    AContact_Number_G_NN<- sample (3:14,1)
    AContact_Number_G_NP <- sample(8:34,1)
    #Input Contact Numbers as vectors inside the repeat()loop since they are to be randomized by day
    
    if (gender == 1){
      AContactNumberH <- AContact_Number_M_H;AContactNumberNN<- AContact_Number_M_NN;AContactNumberNP <- AContact_Number_M_NP
    }else{if(gender == 0){
      AContactNumberH <- AContact_Number_G_H;AContactNumberNN<- AContact_Number_G_NN;AContactNumberNP <- AContact_Number_G_NP }
    }
    #If()else{} loop to determine Student's Contact Numbers in vectors based on gender
    
    Asymptomatic_Infections<- sum(rbinom(AContactNumberH,1,Asymptomatic_SAR_H))+sum(rbinom(AContactNumberNN,1,Asymptomatic_SAR_NN))+sum(rbinom(AContactNumberNP,1,Asymptomatic_SAR_NP))
    
    InfectionsA <- InfectionsA+Asymptomatic_Infections
    
    DaysA <- DaysA+1
    if(DaysA == Removal_Date_of_Asymptomatic){break}
  }}
  
  DaysP<- 0
  InfectionsP<- 0
  
  if(Removal_Date_of_Presymptomatic == 0){InfectionsP <- 0}else{repeat{
    
    PContact_Number_M_H <- sample(0:2,1)
    PContact_Number_M_NN <- sample(5:40,1)
    PContact_Number_M_NP <- sample(8:118,1)
    PContact_Number_G_H <- sample(1:3,1)
    PContact_Number_G_NN<- sample (3:14,1)
    PContact_Number_G_NP <- sample(8:34,1)
    #Input Contact Numbers as vectors inside the repeat()loop since they are to be randomized by day
    
    if (gender == 1){
      PContactNumberH <- PContact_Number_M_H;PContactNumberNN<- PContact_Number_M_NN;PContactNumberNP <- PContact_Number_M_NP
    }else{if(gender == 0){
      PContactNumberH <- PContact_Number_G_H;PContactNumberNN<- PContact_Number_G_NN;PContactNumberNP <- PContact_Number_G_NP }
    }
    Presymptomatic_Infections<- sum(rbinom(PContactNumberH,1,Presymptomatic_SAR_H))+sum(rbinom(PContactNumberNN, 1, Presymptomatic_SAR_NN))+sum(rbinom(PContactNumberNP,1,Presymptomatic_SAR_NP))
    
    InfectionsP<- InfectionsP+Presymptomatic_Infections
    
    DaysP<- DaysP+1
    if(DaysP == Removal_Date_of_Presymptomatic){break}
  }}
  
  DaysS<- 0
  InfectionsS<- 0
  
  if(Removal_Date_of_Symptomatic == 0){InfectionsS <- 0}else{repeat{
    
    SContact_Number_M_H <- sample(0:2,1)
    SContact_Number_M_NN <- sample(5:40,1)
    SContact_Number_M_NP <- sample(8:118,1)
    SContact_Number_G_H <- sample(1:3,1)
    SContact_Number_G_NN<- sample (3:14,1)
    SContact_Number_G_NP <- sample(8:34,1)
    #Input Contact Numbers as vectors inside the repeat()loop since they are to be randomized by day
    
    if (gender == 1){
      SContactNumberH <- SContact_Number_M_H;SContactNumberNN<- SContact_Number_M_NN;SContactNumberNP <- SContact_Number_M_NP
    }else{if(gender == 0){
      SContactNumberH <- SContact_Number_G_H;SContactNumberNN<- SContact_Number_G_NN;SContactNumberNP <- SContact_Number_G_NP }
    }
    Symptomatic_Infections<- sum(rbinom(SContactNumberH,1,Symptomatic_SAR_H))+sum(rbinom(SContactNumberNN, 1, Symptomatic_SAR_NN))+sum(rbinom(SContactNumberNP,1,Symptomatic_SAR_NP))
    
    InfectionsS<- InfectionsS+Symptomatic_Infections
    
    DaysS<- DaysS+1
    if(DaysS == Removal_Date_of_Symptomatic){break}
  }}
  
  
  Infections <- InfectionsA+InfectionsP+InfectionsS
  
  Student_ID <- Student_ID+1
  
  dwo = rbind(dwo,data.frame(Student_ID, Infections)) #Input each individual trial's outcome into data frame
  
  
  if(Student_ID == 10000){break}
}

data<- data.frame("None"=d$Infections,"With Two Test Sessions Per Week" = dw$Infections,"With Two Tests Per Test Session" = dwo$Infections)



frequency_of_None<- table(factor(d$Infections,levels=0:max(data)))
frequency_of_Two_Test_Sessions_Per_Week<- table(factor(dw$Infections,levels=0:max(data)))
frequency_of_Two_Tests_Per_Test_Session<- table(factor(dwo$Infections,levels=0:max(data)))

frequency_of_None <- as.data.frame(frequency_of_None)
frequency_of_Two_Test_Sessions_Per_Week<- as.data.frame(frequency_of_Two_Test_Sessions_Per_Week)
frequency_of_Two_Tests_Per_Test_Session<- as.data.frame(frequency_of_Two_Tests_Per_Test_Session)

frequency_of_None
chi<- cbind(data.frame("Reproductive Number"=c(0:max(data)),None_Frequency=frequency_of_None$Freq,Two_Tests_Sessions_Per_Week_Frequency= frequency_of_Two_Test_Sessions_Per_Week$Freq,Two_Tests_Per_Test_Session_Frequency = frequency_of_Two_Tests_Per_Test_Session$Freq))


chisquare1<- (chi$Two_Tests_Sessions_Per_Week_Frequency-chi$None_Frequency)^2/chi$None_Frequency
dat1<-data.frame(chisquare1)

dat1 <- do.call(data.frame, lapply(dat1, function(x) {
  replace(x, is.infinite(x) | is.na(x), 0)
})
)
calculatedchisquare1<- sum(dat1$chisquare1)

chisquare2<- (chi$Two_Tests_Per_Test_Session_Frequency-chi$None_Frequency)^2/chi$None_Frequency
dat2<-data.frame(chisquare2)

dat2 <- do.call(data.frame, lapply(dat2, function(x) {
  replace(x, is.infinite(x) | is.na(x), 0)
})
)
calculatedchisquare2<- sum(dat2$chisquare2)

calculatedchisquare1
calculatedchisquare2
