#open semiconducter csv dataset
SC <- read.csv("~/Desktop/Data_science/Data/semiconductor.csv")

#code stores all data of the GML regression model in a dataset.
#The regression model predicts binary outcome variable FAIL, using all variables in dataset SC
full <- glm(FAIL ~ ., data=SC, family=binomial)

#this calculates the R^2 by using the deviance and the null deviance
1 - full$deviance/full$null.deviance
#to summarize the fitted model
summary(full)
#to check the deviance shows by the summary statistics
1-320.33/731.59
##to extract all coefficients in the full fitted model
coefficients(full)

## Out of sample prediction experiment
## first, define the deviance and R2 functions
## pred must be probabilities (0<pred<1) for binomial
#the custom deviance function is stored, the function calculates the deviance for both gaussian and binomial predicted variables
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family) #makes sure that only guassian and binomial valaues are considered
  if(family=="gaussian"){ #this part is for the gaussian variables, returns the deviance
    return( sum( (y-pred)^2 ) ) #returns the sum of squared differences between observed and predicted
  }else{ #if it's not gaussian then it returns the log-likelyhood calculation for the binary variables
    if(is.factor(y)) y <- as.numeric(y)>1 #checks whether the response variable is a factor, if so it converts the variable to a numeric vector, 1 indicating True and 0 indicating FALSE
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )## log likelihood calculation
  }
}

# Step 1 - K-fold, this part stores the function
# Now the function to calculate the R^2 is stored, by calculating the deviance and null deviane
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)#same as above, to check that the family is gaussian or binomial
  if(fam=="binomial"){ #if binomial then check if it is a factor, if it is, it is converted to 1 and 0, same as before
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam) #if not binomial, or it's not a factor then it calculated thedeviance and null deviance without modifying the data
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0) #returns the calculated R squared
}


# Step 2 - K-fold Partition/Experiment
# setup the experiment
n <- nrow(SC) # the number of observations
K <- 10 # the number of `folds'
# create a vector of fold memberships (random order)
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
# create an empty dataframe of results
Out <- data.frame(full=rep(NA,K)) 
# use a for loop to run the experiment
for(k in 1:K){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit regression on full sample
  rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
  
  ## get prediction: type=response so we have probabilities
  predfull <- predict(rfull, newdata=SC[-train,], type="response")
  
  ## calculate and log R2
  Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
  
  ## print progress
  cat(k, " ")
}

# Step 3 - K-fold plots
boxplot(Out, col="plum", ylab="R2") #does not show plot
print(Out) #values seem fine
print(boxplot(Out, col="plum", ylab="R2")) #boxplot values seem fine

#saving the boxplot as a png file, does save the correct boxplot
png(file = "~/Desktop/Data_science/OUTPUT_FIGURES/boxplotSC.png", width = 800, height = 400)
boxplot(Out, col="plum", ylab="R2") ## plotting the results in purple col=plum
dev.off() 
## what are the average Out R2?
colMeans(Out) ##negative, suggesting the model predicts worse than the intercept model



#Test with the k-fold algorithm, now 20 folds
# Step 2 - K-fold Partition/Experiment
# setup the experiment
n <- nrow(SC) # the number of SC observations
K <- 20 # the number of `folds', increased to 20
# create a vector of fold memberships (random order)
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
# create an empty dataframe of results
Out <- data.frame(full=rep(NA,K)) 
# use a for loop to run the experiment
for(k in 1:K){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit regression on full sample
  rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
  
  ## get prediction: type=response so we have probabilities
  predfull <- predict(rfull, newdata=SC[-train,], type="response")
  
  ## calculate and log R2
  Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
  
  ## print progress
  cat(k, " ")
}

#saving the test boxplot aswell
png(file = "~/Desktop/Data_science/OUTPUT_FIGURES/boxplotSC2.png", width = 800, height = 400)
boxplot(Out, col="plum", ylab="R2")
dev.off() 
## what are the average Out R2?
colMeans(Out)

# average out R2 still negative, but not as low. First mean -4.33, now -2,76
#conclusion, results seem to be robust, model predicts worse than the null model



#Forward stepwise regression
#Continues until the AIC becomes lower after adding another covariate.
null <- glm(FAIL~1, data=SC)##starts with a null model, only the intercept
fwd <- step(null, scope=formula(full), dir="forward")
length(coef(fwd)) #number of selected coefficients by the forward stepwise regression
#69 coefficients


#test what happens with backward regression, should not perform well because of multicollinearity and p values are from an overfit model
full_model <- glm(FAIL ~ ., data = SC, family = binomial)
# Perform backward stepwise selection starting from the full model
backward_model <- step(full_model, direction = "backward")
# Count the number of coefficients in the final model
length(coef(backward_model))
##takes extremely long, and estimates p-values on overfit model

#Complexity penalty for regularization (LASSO)
#matrix is needed to install gamlr
install.packages("Matrix")
library(Matrix)
library(gamlr)

## Browsing History. 
## web has 3 colums: [machine] id, site [id], [# of] visits
#Reads the browser domain csw into the web dataframe
web <- read.csv("~/Desktop/Data_science/Data/browser-domains.csv")
## Read in actual website names, into a character vector called sitenames
sitenames <- scan("~/Desktop/Data_science/Data/browser-sites.txt", what="character")
#Relabels site column as a factor using the levels and labels from sitenames vector
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)
## also convert factor to  id
web$id <- factor(web$id, levels=1:length(unique(web$id)))
## get total visits per-machine and % of time on each site
## tapply(a,b,c) does c(a) for every level of factor b.
machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
#Calculates the percentages of visits for each row in the dataframe, relative to the machine's total visits. 
visitpercent <- 100*web$visits/machinetotals[web$id] #The percentage is indexed to match the total visits with corresponding machine ID.


## stores this data in a sparse matrix, called xweb
#i indicates the row, derived from numeric representation of machine ID
#j indicates the column, representation of the website ID
#x value representing the visit percentages 
#dims, specifiesa vector containing number of levels for machine id and websites. 
#dimnames specifies the demension names for the sparse matrix. first dimension is id, second site. 
xweb <- sparseMatrix(
  i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,
  dims=c(nlevels(web$id),nlevels(web$site)), #n levels function counts the number of levels in the factor variable
  dimnames=list(id=levels(web$id), site=levels(web$site))) #levels function gets the unique categories of the factor variable

# what sites did household 1 visit? shows non zero values in the first row of the sparse matrix
head(xweb[1, xweb[1,]!=0])
#atdmt.com yahoo.com msn.com google.com aol.com questionmarket.com
tail(xweb[1, xweb[1,]!=0])
plot(xweb)

# Step 1: Calculate total percentage of visits for each website
total_percentage <- colSums(xweb)

# Step 2: Identify the top websites
top_websites <- head(order(total_percentage, decreasing = TRUE), 10)  # Adjust 10 to the desired number of top websites

# Display the results
top_websites_data <- data.frame(Website = names(total_percentage[top_websites]), Total_Percentage = total_percentage[top_websites])
print(top_websites_data)


## now read in the spending data 
yspend <- read.csv("~/Desktop/Data_science/Data/browser-totalspend.csv", row.names=1)  # us 1st column as row names
yspend <- as.matrix(yspend) ## good practice to move from dataframe to matrix

#running this set until betamin gives different outcomes each times, because it creates different folds each time
## run a lasso path plot and saving the png file
spender <- gamlr(xweb, log(yspend), verb=TRUE); spender
png(file = "~/Desktop/Data_science/OUTPUT_FIGURES/spender.png", width = 800, height = 400)
plot(spender) ## path plot
dev.off()

#running this set until betamin gives different outcomes each times, because it creates different folds each time
## run a lasso path plot and saving the png file
#K-fold cross validation for LASSO
cv.spender <- cv.gamlr(xweb, log(yspend))
png(file = "~/Desktop/Data_science/OUTPUT_FIGURES/CVspender.png", width = 800, height = 400)
plot(cv.spender)
dev.off()
##Extract Coefficients for Minimum Cross-Validation Error:
betamin = coef(cv.spender, select="min"); betamin

# Aikaike's Info Criterion AIC=deviance + 2df (n-df)
#can be used for model selection, lower AIC means being closer to the truth. But could lead to overfitting if cost df is not large enough
head(AIC(spender)) #shows first six LASSO model AIC values of spender


