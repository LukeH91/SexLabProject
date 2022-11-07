###################################
# SexLab Machine Learning Project #
###################################


### PART 1: SETUP

# performs initial setup, installs and initialises required packages
knitr::opts_chunk$set(echo = TRUE)
if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret",
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table",
                                          repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs",
                                      repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra",
                                      repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart",
                                         repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc",
                                     repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot",
                                     repos = "http://cran.us.r-project.org")
library(rpart)
library(tidyverse)
library(caret)
library(data.table)
library(dslabs)
library(gridExtra)
library(rpart)
library(Hmisc)
library(rpart.plot)

# Downloads and loads the sexlab dataset from my github repository
sexlabdata <- read.csv(
  "https://raw.githubusercontent.com/LukeH91/SexLabProject/main/sexlab-r-datasheet.csv", 
                      head=TRUE, sep=",",encoding="utf-8")


# changes the column names to more R-friendly ones
colnames(sexlabdata) <- c("id", "sexual_orientation", "pupil_dilation",
                          "subjective_ratings", "self_reported_gnc", "observer_rated_gnc")

# convert two columns from character to numeric for analyses
sexlabdata$pupil_dilation <- as.numeric(sexlabdata$pupil_dilation)
sexlabdata$subjective_ratings <- as.numeric(sexlabdata$subjective_ratings)



### PART 2: DATA EXPLORATION AND VISUSALISATION

# print all column names:
cat("Column Names: ",names(sexlabdata[1:3]))
names(sexlabdata[4:6])

# print number of rows:
cat("Number of Participants:",dim(sexlabdata)[1])

# histogram of distribution of sexual orientations
sexlabdata %>%
  ggplot(aes(x=sexual_orientation)) + 
  stat_bin(binwidth=0.5,color="black",fill="darkgreen") +
  scale_x_continuous(breaks=0:7) +
  ggtitle("Distribution of Sexual Orientations in the Sample") +
  xlab("Sexual Orientation") + 
  ylab("Number of Men")

# array of four graphs showing the linear relationship between SO and the predictive variables
# trend line showing the relationship between sexual orientation and pupil dilation
plot_pd <- sexlabdata %>% ggplot(aes(sexual_orientation, pupil_dilation)) +
  geom_point(position=position_jitter(w=0.15,h=0)) +
  scale_x_continuous(breaks=0:7) +
  geom_smooth(color="darkred",method="lm") + 
  ggtitle("Scatterplot of Sexual Orientation and Pupil Dilation") +
  xlab("Sexual Orientation") +
  ylab("Pupil Dilation")
# trend line showing the relationship between sexual orientation and subjective ratings
plot_sr <- sexlabdata %>% ggplot(aes(sexual_orientation, subjective_ratings)) +
  geom_point(position=position_jitter(w=0.15,h=0)) +
  scale_x_continuous(breaks=0:7) +
  geom_smooth(color="darkblue",method="lm") + 
  ggtitle("Scatterplot of Sexual Orientation and Subjective Ratings") +
  xlab("Sexual Orientation") +
  ylab("Subjective Ratings")
# trend line showing the relationship between sexual orientation and self-reported GNC
plot_sgnc <- sexlabdata %>% ggplot(aes(sexual_orientation, self_reported_gnc)) +
  geom_point(position=position_jitter(w=0.15,h=0)) +
  scale_x_continuous(breaks=0:7) +
  geom_smooth(color="darkgreen",method="lm") + 
  ggtitle("Scatterplot of Sexual Orientation and Self-Reported Gender Nonconformity") +
  xlab("Sexual Orientation") +
  ylab("Self-Reported GNC")
# trend line showing the relationship between sexual orientation and observer-rated GNC
plot_ognc <- sexlabdata %>% ggplot(aes(sexual_orientation, observer_rated_gnc)) +
  geom_point(position=position_jitter(w=0.15,h=0)) +
  scale_x_continuous(breaks=0:7) +
  geom_smooth(color="green",method="lm") + 
  ggtitle("Scatterplot of Sexual Orientation and Observer-Rated Gender Nonconformity") +
  xlab("Sexual Orientation") +
  ylab("Observer-Rated GNC")
grid.arrange(plot_pd,plot_sr,plot_sgnc,plot_ognc)

# array of the same four graphs, this time with non-linear fit lines
nlplot_pd <- sexlabdata %>% ggplot(aes(sexual_orientation, pupil_dilation)) +
  geom_point(position=position_jitter(w=0.15,h=0)) +
  scale_x_continuous(breaks=0:7) +
  geom_smooth(color="darkred") + 
  ggtitle("SO x Pupil Dilation") +
  xlab("Sexual Orientation") +
  ylab("Pupil Dilation")
nlplot_sr <- sexlabdata %>% ggplot(aes(sexual_orientation, subjective_ratings)) +
  geom_point(position=position_jitter(w=0.15,h=0)) +
  scale_x_continuous(breaks=0:7) +
  geom_smooth(color="darkblue") + 
  ggtitle("SO x Subjective Ratings") +
  xlab("Sexual Orientation") +
  ylab("Subjective Ratings")
nlplot_sgnc <- sexlabdata %>% ggplot(aes(sexual_orientation, self_reported_gnc)) +
  geom_point(position=position_jitter(w=0.15,h=0)) +
  scale_x_continuous(breaks=0:7) +
  geom_smooth(color="darkgreen") + 
  ggtitle("SO x Self-Reported GNC") +
  xlab("Sexual Orientation") +
  ylab("Self-Reported GNC")
nlplot_ognc <- sexlabdata %>% ggplot(aes(sexual_orientation, observer_rated_gnc)) +
  geom_point(position=position_jitter(w=0.15,h=0)) +
  scale_x_continuous(breaks=0:7) +
  geom_smooth(color="green") + 
  ggtitle("SO x Observer-Rated GNC") +
  xlab("Sexual Orientation") +
  ylab("Observer-Rated GNC")
grid.arrange(nlplot_pd,nlplot_sr,nlplot_sgnc,nlplot_ognc)

# create set of pearson correlations, p values and Ns for all columns
correlations <- rcorr(as.matrix(sexlabdata),type="spearman")
# save each set as its own data frame
r <- data.frame(correlations[1])
p <- data.frame(correlations[3])
n <- data.frame(correlations[2])
# combine relevant numbers from each one into a table
# create data frame to show all relevant values easily
corrtable <- data.frame(c("r", "p"),
                        c(r[3,2],p[3,2]),
                        c(r[4,2],p[4,2]),
                        c(r[5,2],p[5,2]),
                        c(r[6,2],p[6,2]))
colnames(corrtable) <- c("", "Pupil Dilation","Subjective Ratings",
                         "Self-Reported GNC","Observer-Rated GNC")
# round all numbers in data frame to 3 decimal places
is.num <- sapply(corrtable, is.numeric)
corrtable[is.num] <- lapply(corrtable[is.num], round, 3)
# print data frame
print(corrtable)


### PART 3: MODEL TRAINING

# data preparation:
# sets seed
set.seed(1, sample.kind = "Rounding")
# split the data into training and test sets
test_index <- createDataPartition(y = sexlabdata$sexual_orientation, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- sexlabdata[-test_index,]
test_set <- sexlabdata[test_index,]
# verify that the same id does not appear in both sets
train_set$id %in% test_set$id

#### Model 1: Linear Regression

# variables will be added to the model in descending order of predictive power
# LR model 1: PD
# model is fitted using the training set
fit1 <- lm(sexual_orientation ~ pupil_dilation, data = train_set)
# predictions are generated on the test set
y_hat1 <- predict(fit1,newdata=test_set)
# summary of model is shown
summary(fit1)

# LR model 2: PD/SR
# model is fitted using the training set
fit2 <- lm(sexual_orientation ~ pupil_dilation + subjective_ratings, data = train_set)
# predictions are generated on the test set
y_hat2 <- predict(fit2,newdata=test_set)
# summary of model is shown
summary(fit2)

# LR model 3: PD/SR/SRG
# model is fitted using the training set
fit3 <- lm(sexual_orientation ~ pupil_dilation + subjective_ratings +
             self_reported_gnc, data = train_set)
# predictions are generated on the test set
y_hat3 <- predict(fit3,newdata=test_set)
# summary of model is shown
summary(fit3)

# LR model 4: PD/SR/SRG/ORG
# model is fitted using the training set
fit4 <- lm(sexual_orientation ~ pupil_dilation + subjective_ratings +
             self_reported_gnc + observer_rated_gnc, data = train_set)
# predictions are generated on the test set
y_hat4 <- predict(fit4,newdata=test_set)
# summary of model is shown
summary(fit4)

# create results table, add 4 linear regression R^2 values to it
resultstable <- data.frame(c("PD", "PD/SR", "PD/SR/SRG", "PD/SR/SRG/ORG"),
                           c("0.1391","0.2369","0.2344","0.2327"),
                           c("","","",""),
                           c("","","",""))
colnames(resultstable) <- c("Model", "LR (R^2)","KNN (Acc)","Class (Acc)")
# print results table so far
print(resultstable)


#### Model 2: KNN

# create categorical sexual orientation variable through binning
knndata <- sexlabdata %>% 
  mutate(so_category=ifelse(sexual_orientation %in% c(0.0, 0.5, 1.0), "straight",
                     ifelse(sexual_orientation %in% c(1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5), "bi", "gay")))
# turn this new categorical variable into a factor
knndata$so_category <- as.factor(knndata$so_category)
# drop the old sexual orientation variable to prevent it being used for predictions
knndata <- select(knndata,-sexual_orientation)
# drop id variable for the same reason
knndata <- select(knndata,-id)
# sets seed
set.seed(1, sample.kind = "Rounding")
# split the new dataset into training and test sets
test_index <- createDataPartition(y = knndata$so_category, times = 1,
                                  p = 0.2, list = FALSE)
knn_train_set <- knndata[-test_index,]
knn_test_set <- knndata[test_index,]
# sets parameters for cross-validation of knn models
trctrl <- trainControl(method = "repeatedcv", number = 20, repeats = 5)

# examine distribution of groups
summary(knndata$so_category)

# KNN model 1: PD
# knn model is fitted
knn_fit1 <- train(so_category ~pupil_dilation, 
                  data = knn_train_set, method = "knn", trControl=trctrl,
                  preProcess = c("center", "scale"), tuneLength = 10)
# shows accuracy of knn model in cross-validation
knn_fit1
# use trained model to predict test set
test_pred1 <- predict(knn_fit1, newdata = knn_test_set)
# confusion matrix showing the results
knncf1 <- confusionMatrix(test_pred1, knn_test_set$so_category)

# KNN model 2: PD/SR
# knn model is fitted
knn_fit2 <- train(so_category ~pupil_dilation + subjective_ratings, 
                 data = knn_train_set, method = "knn", trControl=trctrl,
                 preProcess = c("center", "scale"), tuneLength = 10)
# shows accuracy of knn model in cross-validation
knn_fit2
# use trained model to predict test set
test_pred2 <- predict(knn_fit2, newdata = knn_test_set)
# confusion matrix showing the results
knncf2 <- confusionMatrix(test_pred2, knn_test_set$so_category)


# KNN model 3: PD/SR/SRG
# knn model is fitted
knn_fit3 <- train(so_category ~pupil_dilation + subjective_ratings + self_reported_gnc, 
                 data = knn_train_set, method = "knn", trControl=trctrl,
                 preProcess = c("center", "scale"), tuneLength = 10)
# shows accuracy of knn model in cross-validation
knn_fit3
# use trained model to predict test set
test_pred3 <- predict(knn_fit3, newdata = knn_test_set)
# confusion matrix showing the results
knncf3 <- confusionMatrix(test_pred3, knn_test_set$so_category)

# knn model 4: PD/SR/SRG/ORG
# knn model is fitted
knn_fit4 <- train(so_category ~., data = knn_train_set, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
# shows accuracy of knn model in cross-validation
knn_fit4
# use trained model to predict test set
test_pred4 <- predict(knn_fit4, newdata = knn_test_set)
# confusion matrix showing the results
knncf4 <- confusionMatrix(test_pred4, knn_test_set$so_category)


# add KNN models to results table
resultstable <- data.frame(c("PD", "PD/SR", "PD/SR/SRG", "PD/SR/SRG/ORG"),
                           c("0.1391","0.2369","0.2344","0.2327"),
                           c(knncf1$overall["Accuracy"], knncf2$overall["Accuracy"],
                             knncf3$overall["Accuracy"], knncf4$overall["Accuracy"]),
                           c("","","",""))
colnames(resultstable) <- c("Model", "LR (R^2)","KNN (Acc)","Class (Acc)")
# print results table so far
print(resultstable)


#### Model 3: Classification Tree

# Classification model 1: PD
# classification model is fitted and plotted
rpartfit1 <- rpart(so_category~pupil_dilation, data = knn_train_set, method = 'class')
rpart.plot(rpartfit1)
# fitted model used to predict test set
classpredict1 <- predict(rpartfit1,newdata=knn_test_set, type ="class")
# confusion matrix created and printed
classcf1 <- confusionMatrix(classpredict1, knn_test_set$so_category)
classcf1

# Classification model 2: PD/SR
# classification model is fitted and plotted
rpartfit2 <- rpart(so_category~pupil_dilation+subjective_ratings,
                   data = knn_train_set, method = 'class')
rpart.plot(rpartfit2)
# fitted model used to predict test set
classpredict2 <- predict(rpartfit2,newdata=knn_test_set, type ="class")
# confusion matrix created and printed
classcf2 <- confusionMatrix(classpredict2, knn_test_set$so_category)
classcf2

# Classification model 3: PD/SR/SRG
# classification model is fitted
rpartfit3 <- rpart(so_category~pupil_dilation+subjective_ratings+
                     self_reported_gnc, data = knn_train_set, method = 'class')
rpart.plot(rpartfit3)
# fitted model used to predict test set
classpredict3 <- predict(rpartfit3,newdata=knn_test_set, type ="class")
# confusion matrix created and printed
classcf3 <- confusionMatrix(classpredict3, knn_test_set$so_category)
classcf3

# Classification model 4: PD/SR/SRG/ORG
# classification model is fitted
rpartfit4 <- rpart(so_category~., data = knn_train_set, method = 'class')
rpart.plot(rpartfit4)
# fitted model used to predict test set
classpredict4 <- predict(rpartfit4,newdata=knn_test_set, type ="class")
classcf4 <- confusionMatrix(classpredict4, knn_test_set$so_category)
# confusion matrix created and printed
classcf4

# add classification models to results table
resultstable <- data.frame(c("PD", "PD/SR", "PD/SR/SRG", "PD/SR/SRG/ORG"),
                           c("0.1391","0.2369","0.2344","0.2327"),
                           c(knncf1$overall["Accuracy"], knncf2$overall["Accuracy"],
                             knncf3$overall["Accuracy"], knncf4$overall["Accuracy"]),
                           c(classcf1$overall["Accuracy"],classcf2$overall["Accuracy"],
                             classcf4$overall["Accuracy"],classcf4$overall["Accuracy"]))
colnames(resultstable) <- c("Model", "LR (R^2)","KNN (Acc)","Class (Acc)")
# print final results table
print(resultstable)
