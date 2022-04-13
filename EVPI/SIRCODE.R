
SIR_evpi <- function(t, x, parms) {
  with(as.list(c(parms, x)), {
    
    S <- x[1:3]   ### The Susceptible Unvaccinated Class
    SV <- x[4:6] ### The Susceptible Vaccinated Class
    I <- x[7:9] # The Infected
    IV <- x[10:12] ###The Infected Vaccinated
    R <- x[13:15] ###The Recovered Class
    K<- x[16:18] ###THe case incidence 
    
    v= rep(0,3)
    
    DT1 = -log(1-0.75)/(r/((1/3)/1))
    
    #DT1 represents the end timepoint where before it
    #we're only vaccinating the
    #priority age group
    
    r1 <- ifelse (t <= DT1, r/(1/3), r)
    
    #r-sub is the adjusted vaccination rate as only the
    #18 to 80 years are being vaccinated
    
    r2 <- ifelse (t > DT1, r, 0)
    
    ###Vaccinated
    if (t <= DT1){
      v[inc] <-r1
    }
    else if (t > DT1){
      v[1:3] <- r2
    }
    
    
    WI <-W %*% ((tran * (IV + I)) / 1) # The contact matrix times the prevalence
    
    phi_S <-  R0 * (gamma + mu) * (WI) # FOI for unvaccinated/failed
    phi_V <-  (sus * (R0 * (gamma + mu)) * (WI)) # FOI for vaccinated
    
    dS <-  c((1/80 + 1/26) * (S[3] + SV[3] +  I[3] + IV[3]
                              + R[3]),0,0) - c(0,0,mu)*S - phi_S*S - v*S + c(0, a[1:2] * S[1:2]) - (a * S)
    
    dSV <- v*S - c(0,0,mu)*SV - phi_V*SV + c(0, a[1:2] * SV[1:2]) - (a * SV)
    dI <-  phi_S*S - gamma*I - c(0,0,mu)*I +  c(0, a[1:2] * I[1:2]) - (a * I)
    dIV <- phi_V*SV - gamma*IV - c(0,0,mu)*IV + c(0, a[1:2] * IV[1:2]) - (a * IV)
    
    dR <- gamma*(I+IV) -c(0,0,mu)*R + c(0, a[1:2] * R[1:2]) - (a * R)
    
    dK <- (phi_S*S) + (phi_V*SV)
    
    result <- c(dS,dSV,dI,dIV,dR,dK)
    
    list(result)
  })
}

