
#Comparing independent correlations.
zdifference<-function(r1, r2, n1, n2)
{zd<-(atanh(r1)-atanh(r2))/sqrt(1/(n1-3)+1/(n2-3))
p <-1 - pnorm(abs(zd))
print(paste("Z Difference: ", zd))
print(paste("One-Tailed P-Value: ", p))
}

#Comparing dependent correlations.
tdifference<-function(rxy, rxz, rzy, n) 
{df<-n-3
td<-(rxy-rzy)*sqrt((df*(1+ rxz))/(2*(1-rxy^2-rxz^2-rzy^2+(2*rxy*rxz*rzy))))
p<-pt(td, df)
print(paste("tDifference: ", td))
print(paste("One-Tailed P-Value: ", p))
}

zdifference(-0.506, -0.381, 52, 51)
tdifference(-0.441, -0.709, 0.397, 103)