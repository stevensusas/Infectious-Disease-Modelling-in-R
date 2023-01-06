  library (deSolve)
  library (ggplot2)
  library (reshape2)
  
  initial_state_values <- c(S = 837, 
                            I = 1,
                            R = 0,
                            cum_incid = 0)
  
  
  times <- seq(from = 0, to = 36, by = 1)
  
  sir_model <- function(time, state, parameters) {  
    with(as.list(c(state, parameters)), {
      
      N <- S+I+R
      new_infections <- beta*S*I/N
      dS <- -new_infections
      dI <- new_infections-gamma*I
      dR <- gamma*I
     
      
      
      
      return(list(c(dS,dI,dR, new_infections))) 
    })
    
  }
  
  SIR_SSQ <- function(parameters, dat){
    
    result <- as.data.frame(ode(y = initial_state_values, 
                                times = times, 
                                func = sir_model,
                                parms = parameters))
    
    dat <- na.omit(dat)  
    
    deltas2 <- (result$cum_incid[result$time %in% dat$time]  
                - dat$cum_incid)^2                             
    SSQ   <- sum(deltas2)
    
    return(SSQ)
    
  }
  
  
  flu_dat <- data.frame(time = c(1:36), 
                        cum_incid = c(1,NA,NA,NA,NA,NA,NA,13,NA,NA,NA,NA,NA,NA,54,NA,NA,NA,NA,NA,NA,82,NA,NA,NA,NA,NA,NA,97,NA,NA,NA,NA,NA,NA,106))
  
  
  beta_start <- 1
  gamma_start <- 0.5
  
  
  
  optimised <- optim(par = c(beta = beta_start,
                             gamma = gamma_start
                             
  )      # these are the starting beta 
  # and gamma that will be fed 
  # first, into SSQ_fn
  , fn = SIR_SSQ
  , dat = flu_dat)
  
  
  optimised 
  
  optimised$par
  
  opt_mod <- as.data.frame(ode(y = initial_state_values  # named vector of initial
                               # state values
                               , times = times            # vector of times
                               ,  func = sir_model           # your predefined SIR function
                               , parms = optimised$par))
  
  require(ggplot2)
  
  
  opt_plot <- ggplot()+xlab("Time(Days)")+ylab("Cumulative Incidence")+ggtitle("Real Life Cumulative Incidence and Modeled Cumulative Incidence Over Time
  R0=1.066 SSQ=93.62")+
  
  geom_point(aes(x = time, y = cum_incid,colour = "Real Life") 
                                    , data = flu_dat)+
  geom_line(aes(x = time, y = cum_incid,colour = "Model Output")
                                   , data = opt_mod)+labs(colour="")
  
  opt_plot