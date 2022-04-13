
output_calulator <- function(dat,time){
  the_incidence_class <- dat[time,17:19] * 1e5
  cfr<- c(0.05, 0.10, 0.50)
    mort <- the_incidence_class * cfr
  
  return(c(cases=sum(the_incidence_class), 
                      mort =sum(mort)))
  
  
}
