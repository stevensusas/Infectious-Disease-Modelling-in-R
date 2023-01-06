library (deSolve)
library (ggplot2)
library (reshape2)

initial_state_values <- c(S = 837, 
                          Eam = 0,
                          Epm = 1,
                          Eag = 0,
                          Epg = 0,
                          Im = 0,
                          Ig = 0,
                          Q = 0,
                          R = 0,
                          cum_incid = 0)

parameters <- c(A = 0.1562,  
                B = 0.4166,
                C = 0.0949,
                D = 0.3323,
                alpha = 0.933,
                beta = 0.33,
                xone = 0.32,
                xtwo = 1.69,
                xthree = 0.21,
                xfour = 0.99,
                xi = 2.17,
                xii = 1.3,
                gone = 0.25,
                gtwo = 0.0909,
                gthree = 0.2,
                gfour = 0.25,
                gi = 0.57,
                gii = 0.45,
                gr = 0.1,
                N = 838)

times <- seq(from = 0, to = 60, by = 1)

sir_model <- function(time, state, parameters) {  
  with(as.list(c(state, parameters)), {
    
    
    
    dS <- (-xone*S*Eam - xtwo * S * Epm - xthree * S * Eag - xfour * S * Epg - xi*S * Im - xii * S *Ig)/N 
    dEam <- -dS * A - alpha * gone * Eam - (1-alpha) * gtwo * Eam
    dEpm <- -dS * B - (1-beta) * gthree * Epm - beta * gfour * Epm
    dEag <- -dS * C - alpha * gone * Eag - (1-alpha) * gtwo * Eag
    dEpg <- -dS * D - (1-beta)* gthree * Epg - beta*gfour*Epg
    dIm <- (1-beta)*gthree*Epm-gi*Im
    dIg <- (1-beta)*gthree*Epg - gii*Ig
    dQ <- alpha *gone*Eam+(1-alpha)*gtwo*Eam+gi*Im+gii*Ig+beta*gfour*Epm+alpha*gone*Eag+(1-alpha)*gtwo*Eag+beta*gfour*Epg-gr*Q
    dR <- gr*Q
    new_infections <- -dS
    
    
    return(list(c(dS, dEam, dEpm, dEag, dEpg, dIm, dIg, dQ, dR, new_infections))) 
  })
  
}

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))

output


ggplot() +
  geom_line(data = output, aes(x = time, y = cum_incid, col = "Cumulative incidence")) +
  xlab("Time (days)")+                                              
  ylab("Number of Students") +                                 
  labs(title = "Population of Compartments Over Time",
       colour = "")

