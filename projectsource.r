mpnd <- read.csv(file.choose())
mpnd <- mpnd[,-11]

site1 <- read.csv(file.choose())



#####main boot loop#####
samples <- numeric(99)
VAR <- numeric(99)
SD <- numeric(99)
SE <- numeric(99)
MOE <- numeric(99)
bootsvar <- numeric(100000)

for (i in 1:99){
  s = i+1
  for (j in 1:100000){
    y <- sample(na.omit(data$site), size = s, replace = T)
    vary <- var(y)
    bootsvar[j] = mean(vary)
  }
  VAR[i] = mean(bootsvar)
  SD[i] = sd(bootsvar)
  SE[i] = SD[i]/10
  samples[i] = s
  summints <- data.frame(samples, VAR, SD, SE)
}

site1 <- summints

#####setting up data for plots#####
#upper bounds
site1$up68 <- site1$VAR + qt(.84, site1$samples-1)*site1$SE
site1$up95 <- site1$VAR + qt(.975, site1$samples-1)*site1$SE
site1$up99 <- site1$VAR + qt(.995, site1$samples-1)*site1$SE

#lower bounds
site1$low68 <- site1$VAR - qt(.84, site1$samples-1)*site1$SE
site1$low95 <- site1$VAR - qt(.975, site1$samples-1)*site1$SE
site1$low99 <- site1$VAR - qt(.995, site1$samples-1)*site1$SE

#1% change in width of 95% CI
site1$width95 <- abs(site1$up95 - site1$low95)

plot(site1$width95 ~site1$samples, type="l")

#delta%

delta <- numeric(99)
 for (i in 1:98){
   delta[i] =  (abs(site1$width95[i+1]-site1$width95[i]))/(site1$width95[i])
 }
 
plot(delta~site1$samples, type="l")

delta <.01


#####export .csv#####
write.csv(site1, file= file.choose(new = T))


#####finding 1% change in width #####

par(mfrow=c(1,1))
#####ploting#####
#upper:
plot(site1$up99~site1$samples, type="l", ylim= c(-4,4), las=1, 
     main ="Site 1 \n CI width vs Sample Size", ylab="CI", 
     xlab="Sample Size", col="red")

lines(site1$up95~site1$samples, type="l", col ="peru")
lines(site1$up68~site1$samples, type = "l", col= "orange")
#lower:
lines(site1$low68~site1$samples, type = "l", col= "orange")
lines(site1$low95~site1$samples, type = "l", col = "peru")
lines(site1$low99~site1$samples, type = "l", col = "red")
#actual var of site 1:
abline(var(mpnd$abrantes.mpnd), 0, col="green")
#legend:
legend("topright", legend=c("actual variance", "68% CI", "95% CI", "99% CI"),
       col=c("green", "orange", "peru", "red"), lty=1:2, cex=0.8)

#####no longer used#####
