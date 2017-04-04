library(readxl) #for reading .xlsx file
library(rpart)
library(rpart.plot)
library(caTools) #for sample.split()
library(ROCR) 
library(DMwR) #for imbalanced dataset
library(randomForest)
library(arulesViz)
library(FSelector) #for data binning using information.gain
library(factoextra)#for PCA


b <- read_excel("F:/dataset/auto-insutance/Dataset.xlsx")

b$State = as.factor(b$State)
b$Response=as.factor(b$Response)
b$Coverage=as.factor(b$Coverage)
b$Education=as.factor(b$Education)
b$EmploymentStatus=as.factor(b$EmploymentStatus)
b$Gender=as.factor(b$Gender)
b$`Location Code` =as.factor(b$`Location Code` )
b$`Marital Status`=as.factor(b$`Marital Status`)
b$`Policy Type`=as.factor(b$`Policy Type`)
b$Policy = as.factor(b$Policy)
b$`Renew Offer Type`=as.factor(b$`Renew Offer Type`)
b$`Sales Channel`=as.factor(b$`Sales Channel`)
b$`Vehicle Class`=as.factor(b$`Vehicle Class`)
b$`Vehicle Size` =as.factor(b$`Vehicle Size`)
b$`Customer Lifetime Value` =as.factor(b$`Customer Lifetime Value`)


b = b[,-7]
b = b[,-1]
split <- sample.split(b$Response, SplitRatio = 0.75)
train <- subset(b, split == T)
test <- subset(b, split == F)


#base CART model
model <- rpart(Response ~ ., data = train , method = "class" )
prp(model)
guess = predict(model , newdata = test , type = "prob")#type = prob will give the probability of that data point lying in that class.  
pred = prediction(guess[,2] , test$Response )
pref = performance( pred , "tpr" , "fpr" )
plot(pref)
table(test$Response , guess[,2])
test$Response = as.factor(test$Response)
auc.tmp = performance(pred,"auc")
auc = as.numeric(auc.tmp@y.values)

#applying SMOTE to handel imbalance dataset
train_new = SMOTE (Response ~ . , train , perc.over= 400 , perc.under = 200)#SMOTE handles imbalance data.
table(train$Response)
#applying CART to SMOTE'd dataset
new_model = rpart(Response ~ . , data = train_new , method = "class")
new_guess = predict(new_model , newdata = test , type = "prob")
new_pred =  prediction( new_guess[,2] , test$Response)
new_perf = performance( new_pred , "tpr" , "fpr")  
plot(new_perf)  
auc.tmp = performance(new_pred,"auc")
auc = as.numeric(auc.tmp@y.values)
table(test$Response ,new_guess[,2]>0.6)

#applying randomforest
names(train_new) = make.names(names(train_new))
names(test) = make.names(names(test))#making column name legal ,i.e.removing the space and other illegal characters from column name.
modelforest = randomForest(Response ~ . , data = train_new , na.action = na.omit )
predforest = predict(modelforest , newdata = test , type = "prob")
forestpred = prediction(predforest[,2] , test$Response)
graph = performance(forestpred , "tpr" , "fpr")
plot(graph)
auc.tmp = performance(forestpred,"auc")
auc = as.numeric(auc.tmp@y.values)
table(test$Response , predforest[,2]>0.5)

#binning of dataset , i.e finding the correaltion of features with the target variable .
weights = information.gain(Response ~ . , b )
subset <- cutoff.biggest.diff(weights)

#randomForest with variables of high correaltion with target variable
names(train_new) = make.names(names(train_new))
names(test) = make.names(names(test))#making column name legal ,i.e.removing the space and other illegal characters from column name.
modelforest = randomForest(Response ~ Renew.Offer.Type + EmploymentStatus + Income + Total.Claim.Amount , data = train_new , na.action = na.omit )
predforest = predict(modelforest , newdata = test , type = "prob")
forestpred = prediction(predforest[,2] , test$Response)
graph = performance(forestpred , "tpr" , "fpr")
plot(graph)
auc.tmp = performance(forestpred,"auc")
auc = as.numeric(auc.tmp@y.values)
table(test$Response , predforest[,2]>0.662)

train_new$State = as.numeric(train_new$State)
train_new$Customer.Lifetime.Value = as.numeric(train_new$Customer.Lifetime.Value)
train_new$Response = as.numeric(train_new$Response)
train_new$Coverage = as.numeric(train_new$Coverage)
train_new$Education = as.numeric(train_new$Education)
train_new$EmploymentStatus = as.numeric(train_new$EmploymentStatus)
train_new$Gender = as.numeric(train_new$Gender)
train_new$Income = as.numeric(train_new$Income)
train_new$Location.Code = as.numeric(train_new$Location.Code)
train_new$Marital.Status = as.numeric(train_new$Marital.Status)
train_new$Monthly.Premium.Auto = as.numeric(train_new$Monthly.Premium.Auto)
train_new$Months.Since.Last.Claim = as.numeric(train_new$Months.Since.Last.Claim)
train_new$Months.Since.Policy.Inception = as.numeric(train_new$Months.Since.Policy.Inception)
train_new$Number.of.Open.Complaints = as.numeric(train_new$Number.of.Open.Complaints)
train_new$Number.of.Policies = as.numeric(train_new$Number.of.Policies)
train_new$Policy.Type = as.numeric(train_new$Policy.Type)
train_new$Policy = as.numeric(train_new$Policy)
train_new$Renew.Offer.Type = as.numeric(train_new$Renew.Offer.Type)
train_new$Sales.Channel = as.numeric(train_new$Sales.Channel)
train_new$Total.Claim.Amount = as.numeric(train_new$Total.Claim.Amount)
train_new$Vehicle.Class = as.numeric(train_new$Vehicle.Class)
train_new$Vehicle.Size = as.numeric(train_new$Vehicle.Size)

test$State = as.numeric(test$State)
test$Customer.Lifetime.Value = as.numeric(test$Customer.Lifetime.Value)
test$Response = as.numeric(test$Response)
test$Coverage = as.numeric(test$Coverage)
test$Education = as.numeric(test$Education)
test$EmploymentStatus = as.numeric(test$EmploymentStatus)
test$Gender = as.numeric(test$Gender)
test$Income = as.numeric(test$Income)
test$Location.Code = as.numeric(test$Location.Code)
test$Marital.Status = as.numeric(test$Marital.Status)
test$Monthly.Premium.Auto = as.numeric(test$Monthly.Premium.Auto)
test$Months.Since.Last.Claim = as.numeric(test$Months.Since.Last.Claim)
test$Months.Since.Policy.Inception = as.numeric(test$Months.Since.Policy.Inception)
test$Number.of.Open.Complaints = as.numeric(test$Number.of.Open.Complaints)
test$Number.of.Policies = as.numeric(test$Number.of.Policies)
test$Policy.Type = as.numeric(test$Policy.Type)
test$Policy = as.numeric(test$Policy)
test$Renew.Offer.Type = as.numeric(test$Renew.Offer.Type)
test$Sales.Channel = as.numeric(test$Sales.Channel)
test$Total.Claim.Amount = as.numeric(test$Total.Claim.Amount)
test$Vehicle.Class = as.numeric(test$Vehicle.Class)
test$Vehicle.Size = as.numeric(test$Vehicle.Size)

apca = prcomp(na.omit(train_new) , center = TRUE, scale = TRUE  )
names(apca)
biplot(apca, scale = 0)
stddev = apca$sdev
prvar = stddev^2
percvar = prvar / sum(prvar)

#scree plot
plot(prvar, xlab = "Principal Component",ylab = "Proportion of Variance Explained" , type = "b")
#cumulative scree plot
plot(cumsum(prvar), xlab = "Principal Component",ylab = "Cumulative Proportion of Variance Explained",type = "b")

#add a training set with principal components
train.data <- data.frame(Resp = train_new$Response, apca$x)
#we are interested in first 20 PCAs
train.data <- train.data[,1:20]
apca$x = as.numeric(apca$x)
apca$sdev = as.numeric(apca$sdev)
apca$rotation = as.numeric(apca$rotation)
apca$center = as.numeric(apca$center)
apca$scale = as.numeric(apca$scale)

#transform test into PCA
test.data <- predict(apca, newdata = test)
test.data <- as.data.frame(test.data)
#select the first 30 components
test.data <- test.data[,1:20]
#make prediction on test data
rpart.prediction <- predict(modelforest , test)



