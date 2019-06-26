#Black Friday Sales Prediction

#Loading libraries
library(data.table)
library(dplyr)
library(caret)
library(ggplot2)
#install.packages('corrplot')
library(corrplot)
library(h2o)

#Reading the training and test datasets
training_set = fread("train.csv",stringsAsFactors = T)
test_set = fread("test.csv",stringsAsFactors = T)
submission = fread("Sample_Submission_Tm9Lura.txt")
training_set$User_ID=as.character(training_set$User_ID)
test_set$User_ID=as.character(test_set$User_ID)
training_set$Product_ID=as.character(training_set$Product_ID)
test_set$Product_ID=as.character(test_set$Product_ID)

#Features of Data
names(training_set)
names(test_set)

summary(training_set)
summary(test_set)
#We observe that max value of Product_Category_1 is 20 whereas for others is 18. Appears to be noise.

#Combining training and test data for analysis
test_set[,Purchase := mean(training_set$Purchase)]
c <- list(training_set,test_set)
dataset = rbindlist(c) # Using rblindlist because it is faster than rbind
sapply(dataset,class)


#Exploratory Data Analysis
#Analyzing Gender varaible
dataset[,prop.table(table(Gender))]
ggplot(dataset%>%group_by(Gender)%>%summarise(Count = n())) +
  geom_bar(aes(Gender,Count),stat="identity",fill="coral1") +
  geom_label(aes(Gender,Count,label=Count),vjust=0.1)

#Analyzing Age varaible
dataset[,prop.table(table(Age))]
ggplot(dataset%>%group_by(Age)%>%summarise(Count = n())) +
  geom_bar(aes(Age,Count),stat="identity",fill="coral1") +
  geom_label(aes(Age,Count,label=Count),vjust=0.1)

#Analyzing City Category varaible
dataset[,prop.table(table(City_Category))]
ggplot(dataset%>%group_by(City_Category)%>%summarise(Count = n())) +
  geom_bar(aes(City_Category,Count),stat="identity",fill="coral1") +
  geom_label(aes(City_Category,Count,label=Count),vjust=0.1)

#Analyzing Stay in Current Years varaible
dataset[,prop.table(table(Stay_In_Current_City_Years))]
ggplot(dataset%>%group_by(Stay_In_Current_City_Years)%>%summarise(Count = n())) +
  geom_bar(aes(Stay_In_Current_City_Years,Count),stat="identity",fill="coral1") +
  geom_label(aes(Stay_In_Current_City_Years,Count,label=Count),vjust=0.1)

#Age vs Gender
ggplot(training_set,aes(Age,fill=Gender)) + geom_bar(position="dodge")
#Age vs City Category
ggplot(training_set,aes(Age,fill=City_Category)) + geom_bar(position="dodge")
#Gender vs Product Category1
ggplot(training_set,aes(Product_Category_1,fill=Gender)) + geom_bar(position="dodge")
#Gender vs Product Category2
ggplot(training_set,aes(Product_Category_2,fill=Gender)) + geom_bar(position="dodge")
#Gender vs Product Category3
ggplot(training_set,aes(Product_Category_3,fill=Gender)) + geom_bar(position="dodge")

#Gender vs Purchase
ggplot(training_set)+geom_boxplot(aes(Gender,Purchase),fill="#1F3552", alpha=0.7, outlier.color = "#1F3552",
                                  outlier.shape = 20)

#Age vs Purchase
ggplot(training_set)+geom_boxplot(aes(Age,Purchase),fill="#1F3552", alpha=0.7, outlier.color = "#1F3552",
                                  outlier.shape = 20)

#Age + Gender vs Purchase
#Gender vs Purchase
ggplot(training_set,aes(Age,Purchase))+geom_boxplot(aes(fill=Gender), alpha=0.7, outlier.color = "#1F3552",
                                  outlier.shape = 20)

#Data Manipulation
#Treating missing values
#Earlier we observed that Product Category 2 and 3 had a lot of missing values. Take them as category 0
dataset[,Product_Category_2 := ifelse(is.na(Product_Category_2),0,Product_Category_2)]
dataset[,Product_Category_3 := ifelse(is.na(Product_Category_3),0,Product_Category_3)]


#Feature Engineering

#Categorize the purchasing power based on Occupation and Age
a<- aggregate(x= training_set$Purchase, by= list(training_set$Occupation,training_set$Age), FUN = median )
colnames(a) = c("Occupation", "Age", "Purchase_Median")
a=as.data.table(a)
ggplot(a)+geom_histogram(aes(Purchase_Median), binwidth = 500)
a[,Spending := ifelse(Purchase_Median>=9500, "High", ifelse(Purchase_Median>=8000 & Purchase_Median<9500,
                      "Medium",ifelse(Purchase_Median<8000,"Low","Undefined")))]
a=a[,-c("Purchase_Median")]

#Categorize the purchasing power based on Product ID
b<- aggregate(x= training_set$Purchase, by= list(training_set$Product_ID), FUN = median )
colnames(b) = c("Product_ID", "Purchase_Median")
b=as.data.table(b)
ggplot(b)+geom_histogram(aes(Purchase_Median), binwidth = 500)
summary(b$Purchase_Median)
b[,Cost := ifelse(Purchase_Median>=20000, "VeryCostly", ifelse(Purchase_Median>=15000 & Purchase_Median<20000,
                  "Costly",ifelse(Purchase_Median>=10000 & Purchase_Median<15000,"Average","Cheap")))]
b=b[,-c("Purchase_Median")]
b$Product_ID <- as.character(b$Product_ID)


#Categorize the purchasing power based on CityCategory StayinCurrentCityYears
d<- aggregate(x= training_set$Purchase, by= list(training_set$City_Category, training_set$Stay_In_Current_City_Years), FUN = median )
colnames(d) = c("City_Category", "Stay_In_Current_City_Years","Purchase_Median")
d=as.data.table(d)
ggplot(d)+geom_histogram(aes(Purchase_Median), binwidth = 500)
summary(d$Purchase_Median)
d[,Category := ifelse(Purchase_Median>8300, "A", ifelse(Purchase_Median<=8300,"B","Undefined"))]
d=d[,-c("Purchase_Median")]

#Combine these newly created features to the combined dataset
dataset = full_join(dataset,a,by=c("Occupation", "Age"))
dataset = full_join(dataset,b,by="Product_ID")
dataset<- full_join(dataset,d,by=c("City_Category", "Stay_In_Current_City_Years"))

#After joining cost will have some NA because there are product id which are present in Test but not Training
dataset$Cost[is.na(dataset$Cost)] = "Cheap"

# Converting 4+ to 4 in Stay_In_Current_City_Years
levels(dataset$Stay_In_Current_City_Years)[levels(dataset$Stay_In_Current_City_Years) == "4+"] <- "4"

#Recoding Age Groups
levels(dataset$Age)[levels(dataset$Age) == "0-17"] <- 0
levels(dataset$Age)[levels(dataset$Age) == "18-25"] <- 1
levels(dataset$Age)[levels(dataset$Age) == "26-35"] <- 2
levels(dataset$Age)[levels(dataset$Age) == "36-45"] <- 3
levels(dataset$Age)[levels(dataset$Age) == "46-50"] <- 4
levels(dataset$Age)[levels(dataset$Age) == "51-55"] <- 5
levels(dataset$Age)[levels(dataset$Age) == "55+"] <- 6

#Checking user count and product count
#Higher user count suggests that a particular user has purchased products multiple times. 
#High product count suggests that a product has been purchased many a times, which shows its popularity.
# dataset[,User_Count := .N, by=User_ID]
# dataset[,Product_Count := .N, by=Product_ID]

#One Hot Encoding of City_Category
#install.packages("dummies")
dataset$Spending <- as.factor(dataset$Spending)
dataset$Cost <- as.factor(dataset$Cost)
dataset$Category <- as.factor(dataset$Category)
dataset$Gender = as.numeric(as.factor(dataset$Gender))-1
dataset$Age = as.numeric(dataset$Age)
dataset$Occupation <- as.numeric(dataset$Occupation)
dataset$Stay_In_Current_City_Years <- as.numeric(dataset$Stay_In_Current_City_Years)
dataset$Marital_Status <- as.numeric(dataset$Marital_Status)
dataset$Product_Category_1 <- as.integer(dataset$Product_Category_1)
dataset$Product_Category_2 <- as.integer(dataset$Product_Category_2)
dataset$Product_Category_3 <- as.integer(dataset$Product_Category_3)

library(dummies)
dataset <- dummy.data.frame(dataset,names=c("City_Category", "Spending", "Cost", "Category"),sep="_")
dataset=as.data.table(dataset)

#All factor variables should be numeric
sapply(dataset,class)


#Splitting combined data to training and test set
training_set = dataset[1:nrow(training_set)]
test_set = dataset[(nrow(training_set)+1):nrow(dataset)]
test_set[,Purchase := NULL]

#Analyzing Correlated Variables
cor_train = cor(training_set[,-c("User_ID","Product_ID")])
corrplot(cor_train, method="pie", type="lower", tl.cex=0.9)

#Model Building using H2O

localH2O = h2o.init(nthreads = -1) #Start H2o
#Transfer the data models to h2o
training_set_h2o=as.h2o(training_set)
test_set_h2o=as.h2o(test_set)

target <- 14
predictor <- c(3:13,15:23)

#Appying Regression Model
regressor = h2o.glm(y=target,x=predictor,training_frame=training_set_h2o, family="gaussian")
h2o.performance(regressor)
#making predictions
y_pred = as.data.frame(h2o.predict(regressor,test_set_h2o))
subreg = data.frame(User_ID = test_set$User_ID, Product_ID=test_set$Product_ID,
                    Purchase=y_pred$predict)
write.csv(subreg,file = "Basic Regression",row.names=F)

#Random Forest
rforest.model = h2o.randomForest(y=target,x=predictor,training_frame=training_set_h2o,ntrees=1001, 
                                 mtries=3, max_depth = 4, seed=1122)
h2o.performance(rforest.model)
#feature Importance
h2o.varimp(rforest.model)
#making predictions
y_pred_rf = as.data.frame(h2o.predict(rforest.model,test_set_h2o))
submission_rf = data.frame(User_ID = test_set$User_ID, Product_ID=test_set$Product_ID,
                    Purchase=y_pred_rf$predict)
write.csv(submission_rf,file = "Random Forest",row.names=F)

#Gradient Bossting with H2o
#Selecting paramters
grid <- h2o.grid(hyper_params = list(max_depth=c(4,6,8,12,16)), 
                 search_criteria=list(strategy = "Cartesian"),
                 algorithm = "gbm", y=target,x=predictor, grid_id = "depth_grid",
                 training_frame=training_set_h2o, learn_rate=0.05, ntrees=500, stopping_metric="RMSE", 
                 stopping_tolerance = 1e-4, score_tree_interval =10, stopping_rounds = 30, seed=1234)
sorted_grid = h2o.getGrid("depth_grid",sort_by = "RMSE",decreasing = TRUE)

gbm.model = h2o.gbm(y=target,x=predictor,training_frame=training_set_h2o,ntrees=200,                              
                    max_depth = 18, learn_rate=0.1, seed=1122)

h2o.performance(gbm.model)
#making predictions
y_pred_gbm = as.data.frame(h2o.predict(gbm.model,test_set_h2o))
submission_gbm = data.frame(User_ID = test_set$User_ID, Product_ID=test_set$Product_ID,
                           Purchase=y_pred_gbm$predict)
write.csv(submission_gbm,file = "GBM",row.names=F)

#Deep Learning
# hyper_params = list(hidden=list(c(256,256), c(512,512),c(1024,1024)),epochs=c(30,50,70))
# grid <- h2o.grid(hyper_params = hyper_params, 
#                  algorithm = "deeplearning", y=target,x=predictor, grid_id = "depth_grid",
#                  training_frame=training_set_h2o, stopping_metric="RMSE", 
#                  stopping_tolerance = 1e-4, stopping_rounds = 30, score_training_samples = 0,seed=1234)
# sorted_grid = h2o.getGrid("depth_grid",sort_by = "RMSE",decreasing = TRUE)


dlearning.model = h2o.deeplearning(y=target,x=predictor,training_frame = training_set_h2o,
                                   epochs =50, hidden =c(1024,512,512,128,128,64,64,32,32),
                                   activation="Rectifier", score_training_samples = 0, seed = 1122)
h2o.performance(dlearning.model)
#making predictions
y_pred_dl = as.data.frame(h2o.predict(dlearning.model,test_set_h2o))
submission_dl = data.frame(User_ID = test_set$User_ID, Product_ID=test_set$Product_ID,
                            Purchase=y_pred_dl$predict)
write.csv(submission_gbm,file = "Deep Learning",row.names=F)

# Creating Submission File
predict <- 0.5*y_pred_gbm +0.5*y_pred_dl
submission <- dataset[,c("User_ID","Product_ID")]
submission<- cbind(submission, predict)
colnames(submission)[3] <- "Purchase"
write.csv(submission,file = "Submission.csv", row.names = F)
