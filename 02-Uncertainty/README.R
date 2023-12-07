#To load the dataset with browser data, which is stored as a csv file
browser <- read.csv("~/Desktop/Data_science/Data/web-browsers.csv")

#Shows the number of rows and number of columns of matrix browser
dim(browser)

#head command shows the first 6 rows of datset browser
head(browser)

#Mean amount of money spent
mean(browser$spend)

#Calculates the varience of the money spend, divided by 10000, because N=10000
var(browser$spend)/1e4

#This gives the standard deviation, of online spending, 
#for large population of 10000 household
sqrt(var(browser$spend)/1e4)

#creating the bootstrap
B <- 1000 #creates 1000 bootstrap samples
mub <- c() #creates a vector to contain the means of the samples
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE) #performs sampling with replacement, otherwise there would be no variation
  mub <- c(mub, mean(browser$spend[samp_b]))
} 
sd(mub) #This bootstrap standard deviation is almost the same as the original samples SD, shows reliability of this technique.

#test with more samples
B <- 2000 #creates 2000 bootstrap samples
mub <- c() 
for (b in 1:2000){
  samp_b <- sample.int(nrow(browser), replace=TRUE) 
  mub <- c(mub, mean(browser$spend[samp_b]))
} 
sd(mub) #Not really a difference, indicates that 1000 is enough samples

#test with less samples
B <- 200 #creates 200 bootstrap samples
mub <- c() 
for (b in 1:200){
  samp_b <- sample.int(nrow(browser), replace=TRUE) 
  mub <- c(mub, mean(browser$spend[samp_b]))
} 
sd(mub) #After running is command multiple times, it look like this is less trustworthy than more samples. SD ranges from 70-88

#Test without replacement, so no variation, results in SD of 0, because sample means are identical
B <- 1000
mub <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=FALSE) 
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

#Creating histogram of bootstraps sample means stored in mub
h <- hist(mub)

#Generates a sequence of x-values, in the range of the minimum and maximum of vector mub, with a length of 40
xfit <- seq(min(mub), max(mub), length = 40) 

#Generates the mean and SD for y values (spending variable) for a normal distribution, corresponding to the xfit values. 
yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
#Scales the yfit values to match the scale of the histogram
yfit <- yfit * diff(h$mids[1:2]) * length(mub)

#To create a line overlaying the normal distribution curve on the hisogram.
#xfit and yfit are needed for the the corresponding x- and y-coordinates, to connect the line
#the colour is specified to be black and lwd= 2, gives the line a width of 2
lines(xfit, yfit, col = "black", lwd = 2)

#to save the histogram as a png, with different color and thicker line width 
png(file = "~/Desktop/Data_science/OUTPUT_FIGURES/histogram_plot.png", width = 800, height = 400)
h <- hist(mub)
xfit <- seq(min(mub), max(mub), length = 40) 
yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
yfit <- yfit * diff(h$mids[1:2]) * length(mub)
lines(xfit, yfit, col = "red", lwd = 4)
dev.off()

#Bootstrapping regressions
B <- 1000 #creates 1000 bootstrap samples
betas <- c() #empty vector to store the estimated coefficients
for (b in 1:1000){ #creates a loop, that goes B times
  samp_b <- sample.int(nrow(browser), replace=TRUE) #randomly samples with replacements from browser data
  reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,]) #fits logistic regression with the bootstrap sample
  betas <- rbind(betas, coef(reg_b))  #stores the estimates coefficients of the regression
}; head(betas, n=3) #shows the first 3 rows from vector betas
head(betas, n=10) #shows 10 rows

#calculates covariance between having broadband and having children
cov(betas[,"broadband"], betas[,"anychildren"])
#lows cov, important that original sample is a good approximation of the population
#also important that the population does not have heavy tails


#logistic regression
spendy <- glm(log(spend) ~ . -id, data=browser) #fits logistic regression model "spendy" using all controls, exept id variable
round(summary(spendy)$coef,2) #shows the estimated coefficients and rounds them to 2 decimals

#Creating, plotting and saving BH procedure
pval <- summary(spendy)$coef[-1, "Pr(>|t|)"] #extracts the p values from the summary statiscics of the regression
pvalrank <- rank(pval) #ranks the p values
reject <- ifelse(pval< (0.1/9)*pvalrank, 2, 1) #creates the treshold for rejection, a line with slope 0.1/9, Because N=9 and q=0,1
png(file="~/Desktop/Data_science/OUTPUT_FIGURES/BHAlgoExample.png",
    width=600, height=350) ##creates PNG file to store the plot
plot(pvalrank, pval, ylab="p-value", xlab="p-value rank", pch=16, col=reject) #plots the p values based on rank
lines(pvalrank, (0.1/9)*pvalrank) # plots  the rejection line, everything below the line is significant, above the line are not
dev.off() #closes the png to save to plot

#BH example 2
#opens the csv data from the semiconductor
SC <- read.csv("~/Desktop/Data_science/Data/semiconductor.csv")
dim(SC) #shows rows and columns

#BH example 2 regression, cutoff and plot. Saved in PNG file
full <- glm(FAIL ~ ., data=SC, family=binomial) # logistic regression on Fail, using all controls
pvals <- summary(full)$coef[-1,4] #extracting the p-values for all controls, -1 to drop the intercept

#creates a histogram of p-values and how many times they occured
hist(pvals, xlab="p-value", main="", col="lightblue")

png(file="~/Desktop/Data_science/OUTPUT_FIGURES/BHAlgoExample2.png",
    width=600, height=350) #creates png file

#difines the false discovery rate control function
fdr_cut <- function(pvals, q=0.1){ 
  pvals <- sort(pvals[!is.na(pvals)]) #removes NA values and sorts an the p values
  N <- length(pvals) #number of p-values
  k <- rank(pvals, ties.method="min") #ranks the p values from low to high
  alpha <- max(pvals[ pvals<= (q*k/(N+1)) ]) #calculates treshhold alpha, an adjusted treshhold for each p-values. Alpha becomes the highest allowed p-value 
  
  plot(pvals, log="xy", xlab="order", main=sprintf("FDR of %g",q),
       ylab="p-value", bty="n", col=c(8,2)[(pvals<=alpha) + 1], pch=20)
  lines(1:N, q*(1:N)/(N+1))
  
  return(alpha) #returns value alpha, the treshhold that is used to identify statistically significant p-values while controlling for the False Discovery Rate
}

fdr_cut(pvals)
dev.off() #closes and saves png file


#Extra test 1, to see what happens with different assumptions (lower q=0,05)
full <- glm(FAIL ~ ., data=SC, family=binomial) # logistic regression on Fail, using all controls
pvals <- summary(full)$coef[-1,4] #extracting the p-values for all controls, -1 to drop the intercept

#creates a histogram of p-values and how many times they occured
hist(pvals, xlab="p-value", main="", col="lightblue")

png(file="~/Desktop/Data_science/OUTPUT_FIGURES/BHAlgoExample3.png",
    width=600, height=350) #creates png file

#difines the false discovery rate control function
fdr_cut <- function(pvals, q=0.05){ 
  pvals <- sort(pvals[!is.na(pvals)]) #removes NA values and sorts an the p values
  N <- length(pvals) #number of p-values
  k <- rank(pvals, ties.method="min") #ranks the p values from low to high
  alpha <- max(pvals[ pvals<= (q*k/(N+1)) ]) #calculates treshhold alpha, an adjusted treshhold for each p-values. Alpha becomes the highest allowed p-value 
  
  plot(pvals, log="xy", xlab="order", main=sprintf("FDR of %g",q),
       ylab="p-value", bty="n", col=c(8,2)[(pvals<=alpha) + 1], pch=20)
  lines(1:N, q*(1:N)/(N+1))
  
  return(alpha) #returns value alpha, the treshhold that is used to identify statistically significant p-values while controlling for the False Discovery Rate
}

fdr_cut(pvals)
dev.off() #closes and saves png file
##result, less significant p values


#Extra test 2, different plot (bty="o" and pch=10)
full <- glm(FAIL ~ ., data=SC, family=binomial) # logistic regression on Fail, using all controls
pvals <- summary(full)$coef[-1,4] #extracting the p-values for all controls, -1 to drop the intercept

#creates a histogram of p-values and how many times they occured
hist(pvals, xlab="p-value", main="", col="lightblue")

png(file="~/Desktop/Data_science/OUTPUT_FIGURES/BHAlgoExample4.png",
    width=600, height=350) #creates png file

#difines the false discovery rate control function
fdr_cut <- function(pvals, q=0.1){ 
  pvals <- sort(pvals[!is.na(pvals)]) #removes NA values and sorts an the p values
  N <- length(pvals) #number of p-values
  k <- rank(pvals, ties.method="min") #ranks the p values from low to high
  alpha <- max(pvals[ pvals<= (q*k/(N+1)) ]) #calculates treshhold alpha, an adjusted treshhold for each p-values. Alpha becomes the highest allowed p-value 
  
  plot(pvals, log="xy", xlab="order", main=sprintf("FDR of %g",q),
       ylab="p-value", bty="o", col=c(8,2)[(pvals<=alpha) + 1], pch=10)
  lines(1:N, q*(1:N)/(N+1))
  
  return(alpha) #returns value alpha, the treshhold that is used to identify statistically significant p-values while controlling for the False Discovery Rate
}

fdr_cut(pvals)
dev.off() #closes and saves png file

##shows box around plot and thicker data points.

