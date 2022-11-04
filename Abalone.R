install.packages('PivotalR')
library(PivotalR)
library(dplyr)
library(corrplot)
library(caret)
library(class)
library(e1071)

# ______________________________ Data Exploration ______________________________
abalone = abalone
str(abalone)
head(abalone)

# ______________________________ Data Preparation ______________________________
# remove incomplete observations
abalone = na.omit(abalone)
# remove column "id"
abalone = select(abalone,-id)
# create new column
abalone$infant = as.factor(ifelse(abalone$sex=='I',1,0))

# retain numeric variables
abalone_numeric = abalone %>% select_if(is.numeric)
# corrplot to find variables with highest linear correlation
corrplot(cor(abalone_numeric), type="upper", method="color",addCoef.col ="black",number.cex = 0.6)

# boxplot of number of rings by sex
abalone %>% ggplot(aes(x=sex,y=rings)) + geom_boxplot()
# summary statistics for number of rings
summary(abalone[abalone$infant == 1,]$rings)
summary(abalone[abalone$infant == 0,]$rings)


# selecting required columns for linear regression
abalone_lr = abalone %>% select(length,diameter,height,whole,rings)
# selecting required columns for classification
abalone_clean = abalone %>% select(length,diameter,height,whole,infant)

#______________________________ Linear Regression ______________________________
# separate dataset into training and test set
set.seed(100)
dt = sample(nrow(abalone_lr),nrow(abalone_lr)*0.8)
train_lr = abalone_lr[dt,]
test_lr = abalone_lr[-dt,]

# creating linear regression model
lmodel = lm(rings~.,data=train_lr)
summary(lmodel)
# making predictions
pred_lr = predict(lmodel,test_lr)

# visualizing model performance
plot(test_lr$rings,pred_lr,main='Predicted number of rings against actual number of rings')
abline(0,1,col='red')
# evaluating model performance
RMSE(test_lr$rings,pred_lr) #2.56
MAE(test_lr$rings,pred_lr) #1.86

#_____________________________ Logistic Regression _____________________________
# separate dataset into training and test set
set.seed(500)
dt = sample(nrow(abalone_clean), nrow(abalone_clean)*0.8)
train = abalone_clean[dt,]
test = abalone_clean[-dt,]

# creating logistic regression model
mlogit <- glm(infant~., data = train, family = "binomial")
summary(mlogit)

# making predictions
Pred.p <-predict(mlogit, newdata =test, type = "response")
# the value 0.5 is used as a decision boundary
y_pred_num <-ifelse(Pred.p > 0.5, 1, 0)
y_pred <-factor(y_pred_num, levels=c(0, 1))

# model accuracy
mean(y_pred ==test$infant)
# create the confusion matrix with row:y_pred, col:y
table(y_pred,test$infant)

# _______________________________ KNN regression _______________________________
# separate dataset into training and test set
set.seed(500)
dt = sample(nrow(abalone_clean),nrow(abalone_clean)*0.8)
train = abalone_clean[dt,]
test = abalone_clean[-dt,]

# creating empty vector of length 50 to store the values for model accuracy from k = 1 up to k = 50
ac<-rep(0, 50)
for (i in 1:50) {
    set.seed(500)
    knn.i = knn(train[,1:4], test[,1:4], cl=train$infant, k=i)
    ac[i] = mean(knn.i == test$infant)
    cat("k=",i,"accuracy=",ac[i],"\n")
}
# Accuracy Plot
plot(ac,type="b",xlab="K",ylab="Accuracy")

range(ac) # 0.7392344 0.8253589


# best k value = 13

# making predictions
set.seed(500)
knn.13 = knn(train[,1:4], test[,1:4], cl=train$infant, k=13)

# model accuracy
mean(knn.13 == test$infant) # 0.8253589
# create the confusion matrix with row:y_pred, col:y
table(knn.13 , test$infant)

# _____________________________ SVM Classification _____________________________
set.seed(500)
dt = sample(1: nrow(abalone_clean), size=nrow(abalone_clean)*0.8)
train = abalone_clean[dt,]
test = abalone_clean[-dt,]

# run model with linear kernel
set.seed(500)
m.svm = svm(infant~., data = train, kernel = "linear")
summary(m.svm)
# making predictions
pred.svm <- predict(m.svm, newdata=test[,1:4])

# model accuracy
mean(pred.svm == test$infant)
# create the confusion matrix with row:y_pred, col:y
table(pred.svm,test$infant)

# run model with radial kernel
set.seed(500)
m.svm.tune = tune.svm(infant~., data=train, kernel="radial", cost=10^(-1:2), gamma=c(.1,.5,1,2))
summary(m.svm.tune)

# select the best possible model
best.svm = m.svm.tune$best.model
# making predictions
pred.svm.tune = predict(best.svm, newdata=test[,1:4]) 
# model accuracy
mean(pred.svm.tune ==test$infant)
# create the confusion matrix with row:y_pred, col:y
table(pred.svm.tune, test$infant)
