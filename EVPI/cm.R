

Contact1 <-matrix(c(40,50,10,
                    50,40,10,
                    10,10,80),nrow=3,ncol=3)

Contact2 =  matrix(c(10,80,10,
                     80,10,10,
                     10,10,80),nrow=3,ncol=3)

Contact3= matrix(c(10,10,80,
                   10,80,10,
                   80,10,10),nrow=3,ncol=3)

times = seq(0, 2,by=1/12 )

Rec = c(1/3,1/3,1/3)

y_initial <- c(
  S = 0.999 * Rec,
  SV = 0 * Rec,
  I =  0.001 * Rec,
  IV = 0 * Rec,
  R = 0 * Rec,
  K = 0 *Rec
)
