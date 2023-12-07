#Practice file while using the survery_response_data.csv

#To load the dataset survey response data, which is stored as a csv file
CEO_Diary <- read.csv("~/Desktop/Data_science/Data/survey_response_data.csv")

#shows the dataset, similar to browse in stata
View(CEO_Diary)

#shows rows 1 until 15, and colums 1 -5 and 37, 39,40
CEO_Diary[1:15,c(1:5,37, 39, 40)]

# shows the type / class of a variable, in this case all characters
apply(CEO_Diary,2,class)

#to count the rows
nrow(CEO_Diary)

#Summary statistics of all variables in the dataset
summary(CEO_Diary)

#summary statistics of first 5 columns
summary(CEO_Diary[1:5])

#creating directory to store figures
dir.create("~/Desktop/Data_science/OUTPUT_FIGURES")

##shows the plot 
barplot(prop.table(table(CEO_Diary$type)))
##number of occurences of each type 
table(CEO_Diary$type)

#create a PNG file to save a barplot of the 
png(file="~/Desktop/Data_science/OUTPUT_FIGURES/CEOTypes.png", width = 800, height=300)
par(mar=c(9,3,1,1))
barplot(prop.table(table(CEO_Diary$type)), las=2)
dev.off()

#test for smaller image
png(file="~/Desktop/Data_science/OUTPUT_FIGURES/CEOTypes2.png", width = 500, height=200)
par(mar=c(9,3,1,1))
barplot(prop.table(table(CEO_Diary$type)), las=2)
dev.off()

#test with different marges,results in labels falling off
png(file="~/Desktop/Data_science/OUTPUT_FIGURES/CEOTypes3.png", width = 800, height=300)
par(mar=c(5,5,2,1))
barplot(prop.table(table(CEO_Diary$type)), las=2)
dev.off()

#test what las=2 does, by setting to las=1, results in horizontal labels, many fall off
png(file="~/Desktop/Data_science/OUTPUT_FIGURES/CEOTypes4.png", width = 800, height=300)
par(mar=c(9,3,1,1))
barplot(prop.table(table(CEO_Diary$type)), las=1)
dev.off()
#las=0, appears to be the default, with labels parallel to axis.
#In this case las=2 with vertical labels in preferred
png(file="~/Desktop/Data_science/OUTPUT_FIGURES/CEOTypes5.png", width = 800, height=300)
par(mar=c(9,3,1,1))
barplot(prop.table(table(CEO_Diary$type)), las=0)
dev.off()

## this combination creates a logistic regression for response variable strategy, using control variables consultants and politicians. The summary(fit) code shows the output of the estimated regression coefficients 
fit <-glm(strategy ~ consultants + politicians, data=CEO_Diary); summary fit
summary(fit)
