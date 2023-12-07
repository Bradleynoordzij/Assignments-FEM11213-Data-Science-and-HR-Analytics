#importing the OJ.csv dataset
oj <- read.csv("~/Desktop/Data_science/Data/oj.csv")
head(oj, n=5) #showing the first 5 rows of the data set
tail(oj, n=5) #shows the last 5 rows

#Generalized linear model, with logarithmic link function
glm(log(sales) ~ brand + log(price), data=oj)
#The response variable is the logarithmic change in sales
#Brand and log change in price are used as predictors

reg <- glm(log(sales) ~ brand + log(price), data=oj) #to store the data in a list
names(reg) #shows all the names of the coefficients in the list

#same regression as above, excludes brand dominicks
glm(log(sales) ~ brand + log(price), data=oj)

#this is how the glm command creates dummies, shows the head and tail (6rows each)
x <- model.matrix(~ brand + log(price), data=oj); head(x); tail(x)

#Change base group
#converts the brand variable to a factor,this is used to deal with categorical variables
oj$brand = as.factor(oj$brand) 
#this creates model matrix x, The relationship is established, so the  response variable (log sales) can be modelled as a linear combination of brand and the log chang of price.
x <- model.matrix(~ brand + log(price), data=oj); head(x) 
#new factor mybrand is created, this is the original brand variable, but relevelled so "tropicana" is the new baseline category
oj$mybrand = relevel(oj$brand, "tropicana") 
#model matrix x now contains the new mybrand order instead of the old brand order. 
x <- model.matrix(~ mybrand + log(price), data=oj); head(x) #showing first 6 rows, now we see that the baseline category has changed

#same test, now with base category minute.maid
oj$brand = as.factor(oj$brand)
levels(oj$brand) #shows that dominicks is the reference
x <- model.matrix(~ brand + log(price), data=oj); head(x)
oj$mybrand = relevel(oj$brand, "minute.maid")
levels(oj$mybrand) #now is minute.maid the reference
x <- model.matrix(~ mybrand + log(price), data=oj); head(x)

#less restrictive regressions: 
glm(log(sales) ~ log(price)*brand*feat, data=oj)
#log sales as response variable, log price, brand and features as predictive variables. Including interaction terms
#the feat variable is a dummy variable that denotes whether a brand is promoted at that time


#logistic regression
#opens spam email data
email <- read.csv("~/Desktop/Data_science/Data/spam.csv")
#shows rows and columns, 4601 rows, 58 columns 
dim(email) 
#shows column names
colnames(email) 

#fits a logistic regression, spam is a binary outcome, this is also specified under family=binomial.
#all vriables in the dataset are used as predictors for the response variable spam
#the data originates form the email dataset
glm(spam ~ ., data=email, family='binomial')

#The result of the logistic regression from above is stored in the data object spammy
spammy <- glm(spam ~ ., data=email, family='binomial')

#this is used to extract coefficients of the fitted model stored in spammy, specifically for word_free
coef(spammy)["word_free"]; 
#this computes the exponentiated coefficient. The odds ratio
exp(coef(spammy)["word_free"])# returns 4.67, would mean that a mail containing th word free would increase the odds of being spam by a factor of 4.67
#Same is done the word george, now decreases by factor of 300 
coef(spammy)["word_george"]; exp(coef(spammy)["word_george"]); 1/exp(coef(spammy)["word_george"])
#1 more test word people
coef(spammy)["word_people"]; exp(coef(spammy)["word_people"]);1/exp(coef(spammy)["word_people"]) #factor 0,38 - decrease by factor 2.6
#tests whether the first and 4000th observation would be spam, predicted on basis of the coefficients in spammy
predict(spammy, newdata = email[c(1,4000),], type="response")
#first observation 88% chance of being spam, 4000th 15% chance

#Deviance and likelihood
#calculates the deviance of the spammy regression output. the deviance measures how far the fitted modle deviates from the saturated model
summary(spammy)$deviance
#calculates the null deviance, this measures how far the basic model, without additional coefficients is from the perfect saturated model
summary(spammy)$null.deviance

#calculates the deviance and null deviance and stores the values, then calculates the R2 by computing 1 - (deviance / null deviance)
D <- summary(spammy)$deviance; D
D0 <- summary(spammy)$null.deviance; D0
R2 <- 1 - D/D0; R2 ## result is about 75%
