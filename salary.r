columnnames <- c("Age","Work Class","Final Weight","Education","Education Years","Marital Status", "Occupation", "Relationship Status", "Race", "Sex", "Capital Gains Earnings", "Capital Losses", "Hours per Week", "Native Country", "Salary Band")
trainSalary <- read.csv("C:/Users/Administrator/Documents/R Scripts/censusData_train.csv", header = FALSE, sep = ",", strip.white = TRUE, col.names = columnnames,na.strings = "?", stringsAsFactors = TRUE)

cleanTrain <- trainSalary[!is.na(trainSalary$Work.Class) && !is.na(trainSalary$Occupation)]
cleanTrain <- cleanTrain[!is.na(cleanTrain$Native.Country),]
cleanTrain$Final.Weight = NULL

boxplot (Age ~ Salary.Band, data = cleanTrain, main = "Age distribution for the Salary Bands",  xlab = "Income Levels", ylab = "Age", col = "green3")

boxplot (Hours.per.Week ~ Salary.Band, data = cleanTrain, main = "Hours Per Week distribution for the Salary Bands", xlab = "Salary Bands", ylab = "Hours Per Week", col = "blue")

qplot (Salary.Band, data = cleanTrain, fill = Work.Class) + facet_grid (. ~ Work.Class)

fit1 <-rpart(Salary.Band ~ Age + Work.Class + Education + Education.Years + Marital.Status + Occupation + Relationship.Status + Race + Capital.Gains.Earnings + Capital.Losses + Hours.per.Week + Native.Country + Salary.Band, data = cleanTrain, method = "class")
fit2 <-rpart(Salary.Band ~., data = cleanTrain, method = "class")