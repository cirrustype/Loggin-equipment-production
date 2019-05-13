#Austin Schenk
#timber equipment production data
#note: locations have been changed to "site" for confidentiality reasons. The sites
#are in the same order as the excel workbook. 
# this is no longer used 
#####data######
mpnd <- read.csv(file.choose()) #for mean production without delay
#attach(mpnd)




#####bootstrapping######

#####site1#####
bootslow <- numeric(100)
bootsv <-numeric(100)
bootsup <- numeric(100)
width <- numeric(100)
samples <- numeric(100)
bootsvar <- numeric(100000)

for (i in 2:100){
  s = i
  for (j in 1:100000){
    y <- sample(na.omit(mpnd$site1.mpnd), size = s, replace = T)
    vary <- var(y)
    bootsvar[j] = mean(vary)
    
  }
  VAR = mean(bootsvar)
  SD = sd(bootsvar)
  SE = SD/10
  test = qt(.975, 99)
  MOE = test*SE
  bootslow[i] = VAR - MOE
  bootsv[i] = VAR
  bootsup[i] = VAR + MOE
  width[i] = bootsup[i] - bootslow[i]
  samples[i] = s
  ints <- data.frame(samples, width, bootslow, bootsv, bootsup)
}

ints.site1 <- ints[-1,]
ints.site1
var(mpnd$site1.mpnd)

write.csv(ints.site1, file = file.choose(new = T))

####test plot for site1 #####
plot(ints.site1$bootsup~ints.site1$samples, type="l",
     ylim=c(-1, 1.4), ylab="95% CI", xlab="Sample Size", las=1, 
     main= "95% CI vs Sample Size from site1 \n (using 100,000 bootstrap samples)")
lines(ints.site1$bootslow~ints.site1$samples)
abline(.2090166, 0, col="red")



#####site2#####

bootslow <- numeric(100)
bootsv <-numeric(100)
bootsup <- numeric(100)
width <- numeric(100)
samples <- numeric(100)
bootsvar <- numeric(100)

for (i in 2:100){
  s = i
  
  for (j in 1:100){
    y <- sample(na.omit(mpnd$site2.mpnd), size = s, replace = T)
    vary <- var(y)
    bootsvar[j] = mean(vary)
    
  }
  
  VAR = mean(bootsvar)
  SD = sd(bootsvar)
  SE = SD/10
  test = qt(.975, 99)
  MOE = test*SE
  bootslow[i] = VAR - MOE
  bootsv[i] = VAR
  bootsup[i] = VAR + MOE
  width[i] = bootsup[i] - bootslow[i]
  samples[i] = s
  ints <- data.frame(samples, width, bootslow, bootsv, bootsup)
}


ints.site2 <- ints[-1,]
ints.site2



#####site3#####
bootslow <- numeric(100)
bootsv <-numeric(100)
bootsup <- numeric(100)
width <- numeric(100)
samples <- numeric(100)
bootsvar <- numeric(100)

for (i in 2:100){
  s = i
  
  for (j in 1:100){
    y <- sample(na.omit(mpnd$site3.wkhr), size = s, replace = T)
    vary <- var(y)
    bootsvar[j] = mean(vary)
    
  }
  
  VAR = mean(bootsvar)
  SD = sd(bootsvar)
  SE = SD/10
  test = qt(.975, 99)
  MOE = test*SE
  bootslow[i] = VAR - MOE
  bootsv[i] = VAR
  bootsup[i] = VAR + MOE
  width[i] = bootsup[i] - bootslow[i]
  samples[i] = s
  ints <- data.frame(samples, width, bootslow, bootsv, bootsup)
}


ints.site3 <- ints[-1,]
ints.site3

#####site3#####
bootslow <- numeric(100)
bootsv <-numeric(100)
bootsup <- numeric(100)
width <- numeric(100)
samples <- numeric(100)
bootsvar <- numeric(100)

for (i in 2:100){
  s = i
  
  for (j in 1:100){
    y <- sample(na.omit(mpnd$site3.wkhr), size = s, replace = T)
    vary <- var(y)
    bootsvar[j] = mean(vary)
    
  }
  
  VAR = mean(bootsvar)
  SD = sd(bootsvar)
  SE = SD/10
  test = qt(.975, 99)
  MOE = test*SE
  bootslow[i] = VAR - MOE
  bootsv[i] = VAR
  bootsup[i] = VAR + MOE
  width[i] = bootsup[i] - bootslow[i]
  samples[i] = s
  ints <- data.frame(samples, width, bootslow, bootsv, bootsup)
}


ints.site3 <- ints[-1,]
ints.site3

#####site4#####
bootslow <- numeric(100)
bootsv <-numeric(100)
bootsup <- numeric(100)
width <- numeric(100)
samples <- numeric(100)
bootsvar <- numeric(100)

for (i in 2:100){
  s = i
  
  for (j in 1:100){
    y <- sample(na.omit(mpnd$site4.wkhr), size = s, replace = T)
    vary <- var(y)
    bootsvar[j] = mean(vary)
    
  }
  
  VAR = mean(bootsvar)
  SD = sd(bootsvar)
  SE = SD/10
  test = qt(.975, 99)
  MOE = test*SE
  bootslow[i] = VAR - MOE
  bootsv[i] = VAR
  bootsup[i] = VAR + MOE
  width[i] = bootsup[i] - bootslow[i]
  samples[i] = s
  ints <- data.frame(samples, width, bootslow, bootsv, bootsup)
}


ints.site4 <- ints[-1,]
ints.site4


#####site5#####

bootslow <- numeric(100)
bootsv <-numeric(100)
bootsup <- numeric(100)
width <- numeric(100)
samples <- numeric(100)
bootsvar <- numeric(100)

for (i in 2:100){
  s = i
  
  for (j in 1:100){
    y <- sample(na.omit(mpnd$site5.wkhr), size = s, replace = T)
    vary <- var(y)
    bootsvar[j] = mean(vary)
    
  }
  
  VAR = mean(bootsvar)
  SD = sd(bootsvar)
  SE = SD/10
  test = qt(.975, 99)
  MOE = test*SE
  bootslow[i] = VAR - MOE
  bootsv[i] = VAR
  bootsup[i] = VAR + MOE
  width[i] = bootsup[i] - bootslow[i]
  samples[i] = s
  ints <- data.frame(samples, width, bootslow, bootsv, bootsup)
}


ints.site5 <- ints[-1,]
ints.site5


#####site6#####

bootslow <- numeric(100)
bootsv <-numeric(100)
bootsup <- numeric(100)
width <- numeric(100)
samples <- numeric(100)
bootsvar <- numeric(100)

for (i in 2:100){
  s = i
  
  for (j in 1:100){
    y <- sample(na.omit(mpnd$site6.wkhr), size = s, replace = T)
    vary <- var(y)
    bootsvar[j] = mean(vary)
    
  }
  
  VAR = mean(bootsvar)
  SD = sd(bootsvar)
  SE = SD/10
  test = qt(.975, 99)
  MOE = test*SE
  bootslow[i] = VAR - MOE
  bootsv[i] = VAR
  bootsup[i] = VAR + MOE
  width[i] = bootsup[i] - bootslow[i]
  samples[i] = s
  ints <- data.frame(samples, width, bootslow, bootsv, bootsup)
}


ints.site6 <- ints[-1,]
ints.site6

#####site7#####

bootslow <- numeric(100)
bootsv <-numeric(100)
bootsup <- numeric(100)
width <- numeric(100)
samples <- numeric(100)
bootsvar <- numeric(100000)

for (i in 2:100){
  s = i
  
  for (j in 1:100000){
    y <- sample(na.omit(mpnd$site7.mpnd), size = s, replace = T)
    vary <- var(y)
    bootsvar[j] = mean(vary)
    
  }
  
  VAR = mean(bootsvar)
  SD = sd(bootsvar)
  SE = SD/10
  test = qt(.975, 99)
  MOE = test*SE
  bootslow[i] = VAR - MOE
  bootsv[i] = VAR
  bootsup[i] = VAR + MOE
  width[i] = bootsup[i] - bootslow[i]
  samples[i] = s
  ints <- data.frame(samples, width, bootslow, bootsv, bootsup)
}


ints.site7 <- ints[-1,]
ints.site7

write.csv(ints.site7, file = file.choose(new=T))

#####site8#####

bootslow <- numeric(100)
bootsv <-numeric(100)
bootsup <- numeric(100)
width <- numeric(100)
samples <- numeric(100)
bootsvar <- numeric(100)

for (i in 2:100){
  s = i
  
  for (j in 1:100){
    y <- sample(na.omit(mpnd$site8.wkhr), size = s, replace = T)
    vary <- var(y)
    bootsvar[j] = mean(vary)
    
  }
  
  VAR = mean(bootsvar)
  SD = sd(bootsvar)
  SE = SD/10
  test = qt(.975, 99)
  MOE = test*SE
  bootslow[i] = VAR - MOE
  bootsv[i] = VAR
  bootsup[i] = VAR + MOE
  width[i] = bootsup[i] - bootslow[i]
  samples[i] = s
  ints <- data.frame(samples, width, bootslow, bootsv, bootsup)
}


ints.site8 <- ints[-1,]
ints.site8


#####site9#####
bootslow <- numeric(100)
bootsv <-numeric(100)
bootsup <- numeric(100)
width <- numeric(100)
samples <- numeric(100)
bootsvar <- numeric(100)

for (i in 2:100){
  s = i
  
  for (j in 1:100){
    y <- sample(na.omit(mpnd$site9.wkhr), size = s, replace = T)
    vary <- var(y)
    bootsvar[j] = mean(vary)
    
  }
  
  VAR = mean(bootsvar)
  SD = sd(bootsvar)
  SE = SD/10
  test = qt(.975, 99)
  MOE = test*SE
  bootslow[i] = VAR - MOE
  bootsv[i] = VAR
  bootsup[i] = VAR + MOE
  width[i] = bootsup[i] - bootslow[i]
  samples[i] = s
  ints <- data.frame(samples, width, bootslow, bootsv, bootsup)
}


ints.site9 <- ints[-1,]
ints.site9



######plots#####
#par(mfrow=c(2,5))
par(mfrow=c(2,2))

plot(width~samples, type= "l", data= ints.site1, main="site1")#1
plot(width~samples, type= "l", data= ints.site2, main="site2")#2
plot(width~samples, type= "l", data= ints.site3, main= "site3")#3
#plot(width~samples, type= "l", data= ints.site3, main = "site3")#4
#plot(width~samples, type= "l", data= ints.site4, main="site4")#5
#plot(width~samples, type= "l", data= ints.site5, main = "site5")#6
#plot(width~samples, type= "l", data= ints.site6, main ="site6")#7
plot(width~samples, type= "l", data= ints.site7, main = "site7")#8
#plot(width~samples, type= "l", data= ints.site8, main = "site8")#9
#plot(width~samples, type= "l", data= ints.site9, main ="site9")#10

#####plots for the ones with output data#####

site2 <- read.csv(file.choose())
site3 <- read.csv(file.choose())
site7 <- read.csv(file.choose())


par(mfrow=c(2,2))

#site1
plot(ints.site1$bootsup~ints.site1$samples, type="l",
     ylim=c(-1, 1.4), ylab="95% CI", xlab="Sample Size", las=1, 
     main= "site1")
lines(ints.site1$bootslow~ints.site1$samples)
abline(.2090166, 0, col="red")
legend("topright", legend=c("actual variance"),
       col=c("red"), lty=1:2, cex=0.8)

#site2
plot(site2$bootsup~site2$samples, type="l", ylim=c(0,750000),ylab="95% CI", xlab="Sample Size", las=1, 
     main= "site2", cex.axis=.7)
lines(site2$bootslow~site2$samples)
abline(var(na.omit(mpnd$site2.mpnd)), 0, col="red")
legend("topright", legend=c("actual variance"),
       col=c("red"), lty=1:2, cex=0.8)

#site3
plot(site3$bootsup~site3$samples, type="l", ylim=c(0,10000000),ylab="95% CI", xlab="Sample Size", las=1, 
     main= "site3", cex.axis=.7)
lines(site3$bootslow~site3$samples)
abline(var(na.omit(mpnd$site3.mpnd)), 0, col="red")
legend("topright", legend=c("actual variance"),
       col=c("red"), lty=1:2, cex=0.8)

#site7
plot(ints.site7$bootsup~ints.site7$samples, type="l", ylim=c(0,35),ylab="95% CI", xlab="Sample Size", las=1, 
     main= "site7")
lines(ints.site7$bootslow~ints.site7$samples)
abline(var(na.omit(mpnd$site7.mpnd)), 0, col="red")
legend("topright", legend=c("actual variance"),
       col=c("red"), lty=1:2, cex=0.8)







######test#####


# bootslow.2 <- numeric(100)
# bootsv.2 <-numeric(100)
# bootsup.2 <- numeric(100)
# width.2 <- numeric(100)
# samples.2 <- numeric(100)
bootsvar.2 <- numeric(100)

for (i in 2:100){
  n = i
  for (j in 1:100){
    y <- sample(mpnd$site2.mpnd, size = n, replace = F)
    vary <- var(y)
    bootsvar.2[j] = mean(vary)
    
  }
}

vary <- numeric(99)
for (i in 2:100){
  n <- i
  y <- sample(na.omit(mpnd$site2.mpnd), size = n, replace = T)
  vary[i] = var(y)
}
 vary
#NA's because there are NA's in site2 after the data ends. so use na.omit(variable)!
