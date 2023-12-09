# KNN Example
library(MASS)
data(fgl) #loading the fgl dataset
dim(fgl) #shows the dimensions 214 rows and 10 columns
head(fgl, n = 2) #shows the first 2 rows of the fgl dataset

# KNN Example
#creating and saving different plots of the type of glass and material
png(file = "~/Desktop/Data_science/OUTPUT_FIGURES/KNNplot.png", width = 800, height = 400)
par(mfrow=c(2,3)) #creates a layout with 2 rows and 3 columns for the variables
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
dev.off()

x <- scale(fgl[,1:9]) # column 10 is class label, scale converts to mean 0 sd 1
apply(x,2,sd) # apply function sd to columns of x

library(class) #has knn function 
#first test
test <- sample(1:214,10) #draw a random sample of 10 rows 
nearest1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=1)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)
data.frame(fgl$type[test],nearest1,nearest5)
##first time run, results of nearest 1 are best
##seconnd time, predicts evenly well
##third time evenly well
#method seems not to be that trushworthy for all glass

#test 2, more rows. 
test <- sample(1:214,20) #draw a random sample of 20 rows 
nearest1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=1)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)
data.frame(fgl$type[test],nearest1,nearest5)
#no large difference in predictability on scale of 20 rows

#test 3, 
test <- sample(1:214,10) #draw a random sample of 10 rows 
nearest1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=1)
nearest3 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=3)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)
nearest10 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=10)
nearest15 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=15)
data.frame(fgl$type[test],nearest1,nearest3,nearest5,nearest10,nearest15)
#increasing numbers of nearest, results in more WinF. Does not predict better. Seems that WinF and WinNF, seems to be overly represented


# Classification Example 
#read and store German Credit Data
credit <- read.csv("~/desktop/Data_science/Data/credit.csv")

## re-level the credit history and checking account status, converts history variable to factor with levels, good, good, poor, poor, terrible
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")

## a few others, converts foreign variable to a factor with levels foreign and german
#also creates new variable rent
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")

#converts purpose to factor with levels, newcar, usedcar, goods/repair (4x), edu, biz(2x)
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

#only saving the columns, default, duration, amount, installment, age, history, purpose, foreign and rent
credit <- credit[,c("Default", "duration", "amount",
                    "installment", "age", "history",
                    "purpose", "foreign", "rent")]
#shows the first 6 rows of the column stated above
head(credit)
#shows the number of rows and the number of columns, 1000 and 9
dim(credit)


#creating the naref function as specified by taddy, used to create separate dummies so NA is the reference level
xnaref <- function(x){
  if (is.factor(x))
    if(!is.na(levels(x)[1]))
      x<- factor(x,levels=c(NA, levels(x)), exclude = NULL)
return(x)}

naref <- function(DF) {
  if(is.null(dim(DF))) return(xnaref(DF))
  if(!is.data.frame(DF))
    stop("You need to give me a data.frame or a factor")
  DF <- lapply(DF, xnaref)
return(as.data.frame(DF)) }

library(gamlr)
credx <- sparse.model.matrix(Default ~ . ^ 2, data=naref(credit)); colnames(credx)


default <- credit$Default
credscore <- cv.gamlr(credx, default, family="binomial")

#Creating a regularization path where the AIC chooses the model, and OOS prediction performance. Also, the CV-min on the right plot
png(file = "~/Desktop/Data_science/OUTPUT_FIGURES/creditscore.png", width = 800, height = 400)
par(mfrow=c(1,2))
plot(credscore$gamlr)
plot(credscore)
dev.off()
  
aic_values <- credscore$cvm

# Find the value of lambda that minimizes AIC
best_lambda <- credscore$lambda.min
print(aic_values)
cat("Optimal Lambda (min AIC):", best_lambda, "\n")
result <- log(0.014)
print(result) #lambda -4.2

#CV min 
sum(coef(credscore, s="min")!=0) # min 20 coefficients
#AICc 21
sum(coef(credscore$gamlr)!=0) # AICc 21 coefficients
#AIC, 21
sum(coef(credscore$gamlr, s=which.min(AIC(credscore$gamlr)))!=0) # AIC
# the OOS R^2, 0,097, model explains about 9.7%, but better than the intercept model
1 - credscore$cvm[credscore$seg.min]/credscore$cvm[1]


## What are the underlying default probabilities
## In sample probability estimates
pred <- predict(credscore$gamlr, credx, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting
png(file = "~/Desktop/Data_science/OUTPUT_FIGURES/defaultprob.png", width = 800, height = 400)
boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue"))
dev.off()
#shows a lot of overlap between probabilities for true defautls and nondefaults.
#this means there will be false positives and false negatives


# Classification Rule 
rule <- 1/5 # move this around to see how these change
sum( (pred>rule)[default==0] )/sum(pred>rule) ## 0,6 false positive rate at 1/5 rule
sum( (pred<rule)[default==1] )/sum(pred<rule) ## 0,077 false negative rate at 1/5 rule

sum( (pred>rule)[default==1] )/sum(default==1) ## sensitivity 0,9, 90% correctly classifies the group that defaults

sum( (pred<rule)[default==0] )/sum(default==0) ## specificity 0,39, but not all people that would have paid back will get a loan



# Classification Rule test 1, higher costs, more false positives
rule <- 1/10 # move this around to see how these change
sum( (pred>rule)[default==0] )/sum(pred>rule) ## 0,68 false positive rate at 1/10 rule
sum( (pred<rule)[default==1] )/sum(pred<rule) ## 0,035 false negative rate at 1/10 rule

sum( (pred>rule)[default==1] )/sum(default==1) ## sensitivity 0,99, almost perfectly classifies people that will default

sum( (pred<rule)[default==0] )/sum(default==0) # 0,077, way less people will get a loan for this rule.

# Classification Rule  test 2, lower costs at default, far less false positives, but more false negatives. Would be costly to apply this rule
rule <- 1/2 # move this around to see how these change
sum( (pred>rule)[default==0] )/sum(pred>rule) ## 0,32 false positive rate at 1/2 rule
sum( (pred<rule)[default==1] )/sum(pred<rule) ## 0,25 false negative rate at 1/2 rule
sum( (pred>rule)[default==1] )/sum(default==1) ## sensitivity 0,26, drops drastically
sum( (pred<rule)[default==0] )/sum(default==0) # 0,95

## OOS ROC curve
# refit the model using only 1/2 of data
test <- sample.int(1000,500)
credhalf <- gamlr(credx[-test,], default[-test], family="binomial")
predoos <- predict(credhalf, credx[test,], type="response")
defaultoos <- default[test]
#calculate roc for 1/5


#Creates in and out of sample a roc curve 
source("roc.R")
png(file="~/Desktop/Data_science/OUTPUT_FIGURES/ROCCurve.png", width=600, height=350)
par(mai=c(.9,.9,.2,.1), mfrow=c(1,2))
roc(p=pred, y=default, bty="n", main="in-sample")
## our 1/5 rule cutoff
points(x= 1-mean((pred<.2)[default==0]), 
       y=mean((pred>.2)[default==1]), 
       cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred<.5)[default==0]), 
       y=mean((pred>.5)[default==1]), 
       cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")
roc(p=predoos, y=defaultoos, bty="n", main="out-of-sample")
## our 1/5 rule cutoff
points(x= 1-mean((predoos<.2)[defaultoos==0]), 
       y=mean((predoos>.2)[defaultoos==1]), 
       cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((predoos<.5)[defaultoos==0]), 
       y=mean((predoos>.5)[defaultoos==1]), 
       cex=1.5, pch=20, col='blue') 
dev.off()

#saving mosaic plot for credit history against loan outcome
png(file="~/Desktop/Data_science/OUTPUT_FIGURES/Mosaicplot.png", width=600, height=350)
par(mai=c(.8,.8,.1,.1))
plot(factor(Default) ~ history, data=credit, col=c(8,2), ylab="Default") 
dev.off()
##default rate decreases for worse credit history, this is because only people with good history can get large loans. 

# Multinomial Logistic Regression
# LASSO penalized multinomial regression example
install.packages(glmnet)
library(glmnet)
xfgl <- sparse.model.matrix(type~.*RI, data=fgl)[,-1] #Design matrix includes chemical composition variables and all their interactions with refractive index (RI).
gtype <- fgl$type #extracts column type from fgl dataset
glassfit <- cv.glmnet(xfgl, gtype, family="multinomial") #cross validation experiments
glassfit
png(file="~/Desktop/Data_science/OUTPUT_FIGURES/glassfit.png", width=600, height=350)
plot(glassfit)
dev.off()
##plots the OOS variance results across folds. lowest deviance is around 2.0 and the null model 3.0

#Shows OOS deviance across CV folds, for all types
png(file="~/Desktop/Data_science/OUTPUT_FIGURES/glassfitglm.png", width=600, height=350)
par(mfrow=c(2,3), mai=c(.6,.6,.4,.4)) 
plot(glassfit$glm, xvar="lambda")
dev.off()

B  <- coef(glassfit, select="min"); B ## extract coefficients for lambda that minimalize CV error, for the classes
B <- do.call(cbind, B)  #combine the matrices into matrix B
colnames(B) <- levels(gtype) # column names dropped in previous command. This command adds them back.

##calculates the difference in coefficients for predictor Mg between classes WinNF and WinF
DeltaBMg <- B["Mg", "WinNF"] - B["Mg", "WinF"]; DeltaBMg; #B is a matrix. Fixed Row. Vary Columns. k is Mg, a is WinNF, b is WinF. 
exp(DeltaBMg);
1 - exp(DeltaBMg) #calculates the probability of being in class WinF instead of in WinNF

#predicts probabilities for each class based on model glassfit
probfgl <- predict(glassfit, xfgl, type="response"); dim(probfgl); head(probfgl,n=2); tail(probfgl,n=2)
#gives in-sample probabilities. Note: this is nXKX1 array. Need nXK array. To convert: 
probfgl <- drop(probfgl); #use dim(probfgl) to check dim is 214 by 6
n <- nrow(xfgl)
#extracting predicted probabilitiees for the true class of the observations
trueclassprobs <- probfgl[cbind(1:n, gtype)]; head(trueclassprobs,n=3); tail(trueclassprobs,n=3) 
#for each obs there is one probability that corresponds to realized shard for that obs. Last command extracts those probabilities. 
#Note use of a matrix to index a matrix.

#creates bar plot to show predicted probabilities for true class of each observation
png(file="~/Desktop/Data_science/OUTPUT_FIGURES/glassfitprobs.png", width=600, height=350)
plot(trueclassprobs ~ gtype, col="lavender", varwidth=TRUE,
     xlab="glass type", ylab="prob( true class )") 
dev.off()



