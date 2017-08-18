train <- read.csv("C:/Users/Administrator/Documents/R Scripts/train.csv")
test <-read.csv("C:/Users/Administrator/Documents/R Scripts/test.csv")
ls()
str(train)
head(train)

summary(train)
counts <- table(train$SalePrice)
barplot(counts, main="Price Distribution", xlab="Price of the house")
des <- summary(train$SalePrice)

ncol(train)
nrow(train)

train <- train[2:length(train)]
colnames(train)

names <- colnames(train)
values <- t(colSums(is.na(train)))
NA_cols <- which(values>0)
values <- values[NA_cols]
NA_names <- names[NA_cols]
barplot(values[1:length(values)],horiz = TRUE,main="Missing Values",names.arg = NA_names,las=2)
barplot(values[1:length(values)],main="Missing Values",names.arg = NA_names,las=2)

plot(train$MSZoning)
plot(train$Street)
plot(train$Neighborhood,las=2)

plot(train$YearBuilt)
plot(train$MSSubClass)

classes <- t(sapply(train, class))
int_col <- which(classes == "integer")
int_names <- names[int_col]

library(corrplot)
M <- cor(train[int_names])
corrplot(M, method="circle")
corrplot(M, method="number")
plot(train$OverallQual,train$SalePrice,type="p",col="blue")
plot(train$GrLivArea,train$GarageCars,type="p",col="blue")

null_to_none <- function(X){
  X <- as.character(X)
  X[which(is.na(X))] <- "None"
  X <- as.factor(X)
  return(X)
}

train$Alley <- null_to_none(train$Alley)
train$Fence <- null_to_none(train$Fence)
train$PoolQC <- null_to_none(train$PoolQC)
train$MiscFeature <- null_to_none(train$MiscFeature)

mean_LotFrontage <- as.integer(mean(train$LotFrontage, na.rm = TRUE))
train$LotFrontage[which(is.na(train$LotFrontage))] <- mean_LotFrontage
train$LotFrontage

plot(train$MasVnrType)
train$MasVnrType <- null_to_none(train$MasVnrType)


garage <-  c("GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond")
head(train[garage])
train[garage]
train$GarageType <- null_to_none(train$GarageType)

train$GarageYrBlt[which(is.na(train$GarageYrBlt))] <- 0

train$GarageFinish <- null_to_none(train$GarageFinish)

train$GarageQual <- null_to_none(train$GarageQual)

train$GarageCond <- null_to_none(train$GarageCond)

train$MasVnrArea <- as.numeric(train$MasVnrArea)
mean_MasVnrArea <- as.integer(mean(train$MasVnrArea, na.rm = TRUE))
train$MasVnrArea[is.na(train$MasVnrArea)] <- mean_MasVnrArea
train$MasVnrArea

plot(train$BsmtQual)
train$BsmtQual <- null_to_none(train$BsmtQual)
train$BsmtCond <- null_to_none(train$BsmtCond)
train$BsmtExposure <- null_to_none(train$BsmtExposure)
train$BsmtFinType1 <- null_to_none(train$BsmtFinType1)
train$BsmtFinType2 <- null_to_none(train$BsmtFinType2)

train$Electrical <- null_to_none(train$Electrical)

train$FireplaceQu <- null_to_none(train$FireplaceQu)

Xf <- train[1:length(train)-1]
y <- train$SalePrice
Zf <- train[sample(nrow(Xf)),]
cdata <- data.frame(cbind(Xf, Zf, y))

library(randomForest)
rf <- randomForest(y~., data=cdata, ntree=50)
VIMs <- importance(rf, type=2)
p <- ncol(Xf)
VIMs.unb <- VIMs[1:p,] - VIMs[(p+1):(2*p),]

VIMs.unb <- function(k){
  set.seed(k)
  Zf <- Xf[sample(nrow(Xf)),]
  dtset.pseudo <- data.frame(cbind(Xf,Zf,y))
  rf <- randomForest(y ~ ., data=dtset.pseudo, ntree=50)
  VIMs <- importance(rf, type=2)
  VIMs[1:p,] - VIMs[(p+1):(2*p),]
}


library(snowfall)
sfInit(parallel=TRUE, cpus=6, type="SOCK")
sfLibrary(randomForest)
sfExport("Xf", "y", "p")
VIMs.list <- sfLapply(x=1:100, VIMs.unb) # Takes some minutes...
sfStop()

VIMs <- t(matrix(unlist(VIMs.list),p))
GINI.unb <- apply(VIMs,2,mean)

idx <- order(GINI.unb,decreasing=T)
Xs <- Xf[,idx[1:13]]
vm <- c(VIMs[,idx])
grp <- c(t(matrix(rep(1:ncol(VIMs),nrow(VIMs)),ncol(VIMs))))
dt <- data.frame(vm,grp=factor(grp))
ggplot(dt, aes(grp,vm)) + geom_boxplot(outlier.size = 0)+
  scale_x_discrete(breaks=c(1,100,200,300,400,p), name="") +
  scale_y_continuous(name="Gini VIM corrected")+
  geom_hline(yintercept=0, colour="red", lty=2, lwd=1)+
  theme(text=element_text(size = 24))

dt <- data.frame(id=1:50,
                 VIM=GINI.unb[idx[1:50]],
                 grp=c(rep(1,4),rep(2,9),rep(3,50-4-9)),
                 names=c(names(Xs),rep("",50-13)),
                 cols= c(rep("red",4),rep("blue",9),rep("gray50",50-4-9)))

ggplot(dt, aes(x=id, y=VIM, label=names, colour=cols))+
  geom_point() + scale_colour_discrete(l=60)+ scale_fill_identity() +
  geom_text(angle = 45,hjust=-.05, vjust=0, size=4.2)+
  scale_y_continuous(name="Gini VIM corrected",limits=c(0,3.05))+
  scale_x_continuous(name="")+
  theme(legend.position="none",text=element_text(size = 24))

Xs <- Xf[,idx[1:13]]
save(Xs,y,file="selected_covariates.RData")

head(Xs)

names_f <- c("OverallQual", "GrLivArea", "Neighborhood", "GarageCars", "ExterQual", 
             "TotalBsmtSF", "X1stFlrSF", "GarageArea", "KitchenQual", "X2ndFlrSF", 
             "BsmtQual", "YearBuilt", "BsmtFinSF1")

Xs <- train[names_f]

train_data <- Xs[1:1000,names(Xs)]
test_data <- Xs[1000:1460,names(Xs)]
train_label <- y[1:1000]
test_label <- y[1000:1460]

library(randomForest)
model <- randomForest(train_label ~., data = train_data, method = "anova", ntree = 300,
                      mtry = 26,replace = F,nodesize = 1,importance = T)
predict <- predict(model, test_data)
RMSE <- RMSE(predict, test_label)
RMSE <- round(RMSE, digits = 3)
head(predict)
head(test_label)

model2 <- lm(train_label~.,train_data)
predict2 <- predict(model2,test_data)
RMSE2 <- RMSE(predict2, test_label)
RMSE2 <- round(RMSE2, digits = 3)
head(predict2)
head(test_label)

library(e1071)
model3 <- svm(train_label ~ ., train_data, cost = 64, epsilon = 0.01)
predict3 <- predict(model3, newdata = test_data)
RMSE3 <- RMSE(predict3, test_label)
RMSE3 <- round(RMSE3, digits = 3)
head(predict3)
head(test_label)

library(gbm)
model4 <- gbm(train_label ~., data = train_data, distribution = "laplace",
              shrinkage = 0.05,
              interaction.depth = 5,
              bag.fraction = 0.66,
              n.minobsinnode = 1,
              cv.folds = 100,
              keep.data = F,
              verbose = F,
              n.trees = 300)
predict4 <- predict(model4, newdata = test_data)
RMSE4 <- RMSE(predict4, test_label)
RMSE4 <- round(RMSE4, digits = 3)
head(predict4)
head(test_label)

p1 <- (predict-train_label)*(predict-train_label)
p2 <- (predict2-train_label)*(predict2-train_label)
p3 <- (predict3-train_label)*(predict3-train_label)
p4 <- (predict4-train_label)*(predict4-train_label)
plot(p1,type="l",col="red")
par(new=TRUE)
plot(p2,type="l",col="blue")
par(new=TRUE)
plot(p3,type="l",col="green")
par(new=TRUE)
plot(p4,type="l",col="orange")

plot(predict,type="p",col="red")
par(new=TRUE)
plot(train_label,type="p",col="blue")

plot(predict2,type="p",col="red")
par(new=TRUE)
plot(train_label,type="p",col="blue")

plot(predict3,type="p",col="red")
par(new=TRUE)
plot(train_label,type="p",col="blue")

plot(predict4,type="p",col="red")
par(new=TRUE)
plot(train_label,type="p",col="blue")

names_a <- c("Decision Tree","Regression","SVM","Boosting")
MS <- c(RMSE,RMSE2,RMSE3,RMSE4)
barplot(MS,main="Model Selection",names.arg = names_a,las=1)
prediction <- predict(model4, test)
write.csv(prediction, file = "submission2.csv")
