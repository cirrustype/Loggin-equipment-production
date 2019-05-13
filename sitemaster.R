#Austin Schenk
#timber harvesting equipment

######siteinfo Site#####
#site info
#location....
#data.....
#machine...
#total sample size....
#output measured in... 



#####deataset for production without delay#####
mpnd <- read.csv(file.choose()) #mpnd = Mean Production No delay
dim(mpnd)
head(mpnd)






#####Loop for variance#####
samples <- numeric(99) #empty numeric vector for samples 
VAR <- numeric(99) #empty numeric vector for bootstrap variance
SD <- numeric(99) #empty numeric vector for estimated standard deviation 
SE <- numeric(99) #empty numeric Vector for estimated standard error 
bootsvar <- numeric(10000) #empty vector for varience of each boot

for (i in 1:99){  #open loop to run through sample sizes 
  s = i+1 #samples 2 through 100 
  for (j in 1:10000){ #open loop for bootstarpping
    y <- sample(na.omit(mpnd$siteinfo), size = s, replace = T) # boots: i + 1 sized samples from site data
    bootsvar[j] = var(y)  #taking the average varaince for all the samples of size i+1 
  } #end of boot loop 
  VAR[i] = mean(bootsvar) #taking the average variance for all boots for each sample size 
  SD[i] = sd(bootsvar) #taking the standard deviation for all boots for each sample size 
  SE[i] = SD[i]/sqrt(s) #taking the standard error for all boots for each sample size 
  samples[i] = s # to add the vector of sample sizes to the data frame
  summints <- data.frame(samples, VAR, SD, SE) # creating a data frame with the data collected by loop
} #end of sample size loop

siteinfo <- summints #assigning the site name to the data frame output of the loop 
head(siteinfo) 




#####augmenting the site data frame#####
#upper bounds
siteinfo$up68 <- siteinfo$VAR + qt(.84, siteinfo$samples-1)*siteinfo$SE # adding upper bound for 68% CI
siteinfo$up95 <- siteinfo$VAR + qt(.975, siteinfo$samples-1)*siteinfo$SE # adding upper bound for 95% CI
siteinfo$up99 <- siteinfo$VAR + qt(.995, siteinfo$samples-1)*siteinfo$SE # adding upper bound for 99% CI

#lower bounds
siteinfo$low68 <- siteinfo$VAR - qt(.84, siteinfo$samples-1)*siteinfo$SE # adding lower bound for 68% CI
siteinfo$low95 <- siteinfo$VAR - qt(.975, siteinfo$samples-1)*siteinfo$SE # adding lower bound for 95% CI
siteinfo$low99 <- siteinfo$VAR - qt(.995, siteinfo$samples-1)*siteinfo$SE # adding lower bound for 99% CI

siteinfo$width95 <- siteinfo$up95-siteinfo$low95 #adding the width of the 95% CI
siteinfo$dif <- siteinfo$VAR-var(mpnd$siteinfo) # adding the difference between bootstarpped and actual varaince





######change in 95% CI width for additional samples using raw 95% CI#####
#this loop is to find the change in the 95% CI width for each additional sample 
#we will consider a change of less than 2% to be too little to justify the cost of the additional sample
delta <- numeric(99) #sets up empty numeric vector change 

for (i in 1:98){ #open loop
  delta[i] =  (abs(siteinfo$width95[i+1]-siteinfo$width95[i]))/(siteinfo$width95[i]) #difference formula
} #end loop 

siteinfo$delta <- delta #adding delta to df

fit <- nls(siteinfo$delta ~ a + b*(siteinfo$samples)^(-c), start = list(a=1, b=2, c=.1)) # fitting trend line to delta v sample size
summary(fit)#for coefficients


plot(siteinfo$delta~siteinfo$samples, type ="l")#taking a look at trend line. 
lines(siteinfo$samples, predict(fit)) #adding trend line to the above plot

siteinfo$deltatrend <- predict(fit) #adding predicted delta fit to df




#dont use but add the data to the site data frame
#######################################################################################################################################
#####change in 95% CI width using nls trend lines for 95% CI#####
#this method seems too artificial 
#upper
upper95 <-nls(siteinfo$up95 ~ a + b*(siteinfo$samples)^(-c), data= siteinfo, start = list(a=60, b=20, c=.9)) # fitting trend line to delta v sample size
summary(upper95)#for coefficients
siteinfo$trend95up <- predict(upper95) #adding upper trend to the site data frame 

#lower
lower95 <-nls(siteinfo$low95 ~ a + b*(siteinfo$samples)^(-c), data= siteinfo, start = list(a=-60, b=20, c=.9)) # fitting trend line to delta v sample size
summary(lower95)#for coefficients
siteinfo$trend95low <- predict(lower95) #adding lower trend to the site data frame 

#trendwidth
siteinfo$trendwidth <- siteinfo$trend95up-siteinfo$trend95low #creating and adding width between upper and lower trend 


#loop for change in width for additional samples 
trendif <- numeric(99) #sets up empty numeric vector change 
for (i in 1:98){ #open loop
  trendif[i] =  (abs(siteinfo$trendwidth[i+1]-siteinfo$trendwidth[i]))/(siteinfo$trendwidth[i]) #difference formula
} #end loop 

siteinfo$trendif <- trendif #adding trend difference to site data frame 

#plotting with the trend lines, looks plastic 
plot(siteinfo$trend95up~siteinfo$samples, type="l", ylim= c(-3,3), las=1, 
     main ="siteinfo \n CI width vs Sample Size", ylab="CI", 
     xlab="Sample Size", col="black", xlim=c(4,100), lty=1, lwd=2) # main plot setup
lines(siteinfo$trend95low~siteinfo$samples, type = "l", col = "black", lwd = 2, lty=1) # lower CI
abline(var(mpnd$siteinfo), 0, col="darkred", lty=1, lwd=1) #variance for full dataset 
abline(v=78, col="forestgreen", lwd=1, lty=2) # this is the 2% change in CI width mark 
legend("topright", legend=c( "95% CI", "Actual Variance", "Efficient Sample"), col=c("black", "darkred", 
                                                                                     "forestgreen"), lty = c(1,1,2))
###########################################################################################################################################








#####site CSV export #####
write.csv(siteinfo, file= file.choose(new = T)) #export the boots as a csv


#####ploting 95% CI and sample efficiency####

siteinfo$deltatrend > .05 #at n=18, this is the point past which additional samples reduce CI width < 5%
siteinfo$deltatrend > .04 #at n=27, this is the point past which additional samples reduce CI width < 4%
siteinfo$deltatrend > .03 #at n=79, this is the point past which additional samples reduce CI width < 3%
siteinfo$deltatrend > .02 #at n= never 

plot(siteinfo$up95~siteinfo$samples, type="l", ylim= c(-3,3), las=1, 
     main ="siteinfo \n CI width vs Sample Size", ylab="CI", 
     xlab="Sample Size", col="black", xlim=c(4,100), lty=1, lwd=2) # main plot setup with 95% CI v sample size 

lines(siteinfo$low95~siteinfo$samples, type = "l", col = "black", lwd = 2, lty=1) # adding lower bound
abline(var(mpnd$siteinfo), 0, col="darkred", lty=1, lwd=1) #variance for full dataset 

abline(v=18, col="forestgreen", lwd=1, lty=2) # this is the 5% change in CI width mark
text(locator(1),col="forestgreen",c("5% reduction \n at n=18"), cex=1) # adds text to plot 
abline(v=27, col="forestgreen", lwd=1, lty=2) # this is the 4% change in CI width mark
text(locator(1),col="forestgreen",c("4% reduction \n at n=27"), cex=1) #adds text to plot 
abline(v=79, col="forestgreen", lwd=1, lty=2) # this is the 3% change in CI width mark 
text(locator(1),col="forestgreen",c("3% reduction \n at n=79"), cex=1) #adds text to plot 

legend("topright", legend=c( "95% CI", "Actual Variance", "Efficient Sampling"), 
       col=c("black", "darkred","forestgreen"), lty = c(1,1,2), cex=.85) #adds a legend to the plot