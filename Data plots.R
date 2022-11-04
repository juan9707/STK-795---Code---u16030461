library(class) # Used for k-NN
library(forcats)
library(groupdata2)
library(dplyr)
library(ggplot2)
library(hydroGOF) # rmse()
require(caret) # Confusion matrix
require(pacman)
require(ghyp)
require(stats)
require(Bessel)
require(devtools)
require(tseries)
require(dplyr)
require(psych)
require("psych")
require(corrplot) # Package for correlation plot
require(car)
library(readxl)
require(ltm)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library("solitude")
library("mlbench")
library(rsample) # Used to obtain training and testing set
library(uwot)
library(likert)
library(mediation)
library("ggpmisc")
library("gridExtra")
library(data.table)
library(tidySEM)
library(DescTools)
library(MASS)
library("isotree") 
library(readr)
library(GGally)
library(lmtest)
require(stats)
require(Bessel)
require(data.table)
require(sjmisc)
require(DMwR2)
library(rpart) # Used for the CART algorithm
library(partykit)
library(RWeka) # Package used for computing decision trees
library(lessR)
library(forcats)
library(bios2mds)
library(caret)
library(ROCR)
library(pROC) # Used for ROC curves
library(EFAtools)
library(mltools)
library(RColorBrewer)
Hear <- read.csv("C:/Users/Juan Ferreira/Desktop/Research/heart.csv")

par(mfrow = c(1, 1))
colSums(is.na(Hear))

which(colSums(is.na(Hear))>0)

names(which(colSums(is.na(Hear))>0))


nrow(Hear)
Hear$Sex <- ifelse(Hear$Sex == "M", 0, 1) #0 = Male and 1 = Female
Hear$ExerciseAngina <- ifelse(Hear$ExerciseAngina == "N", 0, 1) 
#0 = No and 1 = Y
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
Hear$RestingECG <- as.character(Hear$RestingECG)

Hear$RestingECG <- factor(Hear$RestingECG,
                          levels = c("Normal", "LVH", "ST"));

Hear$RestingECG <- as.numeric(Hear$RestingECG)
Hear$ST_Slope <- ifelse(Hear$ST_Slope == "Up", 0, 1) 
#0 = Normal and 1 = coronary ischemia.
typeof(Hear$ChestPainType)


Hear$ChestPainType <- fct_collapse(Hear$ChestPainType, Normal = "NAP", 
                                   S1 = c("ASY","ATA"), S2 = "TA")
Hear$ChestPainType <- as.numeric(Hear$ChestPainType)
######################### K-NN ##########################
# Create a normalisation function:
norm_rank <- function(a) {(a - 1 )/ (2)}
normalize <-function(x) { (x -min(x))/(max(x)-min(x))   }
Hear$RestingECG <- norm_rank(Hear$RestingECG)
Hear$ChestPainType <- norm_rank(Hear$ChestPainType)
Hear$Age <- normalize(Hear$Age)
Hear$RestingBP <- normalize(Hear$RestingBP)
Hear$Cholesterol <- normalize(Hear$Cholesterol)
Hear$MaxHR <- normalize(Hear$MaxHR)
Hear$Oldpeak <- normalize(Hear$Oldpeak)

set.seed(123)
Subse <- Hear[,1:11]
datasplit <- sample(1:nrow(Subse), 0.75 * nrow(Subse))
# extract the training set

trai <- Subse[datasplit,]
tes <- Subse[-datasplit,]

# extract the 10th column of train dataset because it will be used as 

targetcategory <- Hear[datasplit,12]
targetcategory

testcategory <- Hear[-datasplit,12]
seed = 123
set.seed(seed)
head(Hear)


set.seed(123)
testpred <- knn(trai,tes,cl=targetcategory,k=2)
dfpred=data.frame(testcategory,testpred)
count = 0
act <- ifelse(testpred == testcategory, 1, 0)
actualpre <- sum(act) / length(testcategory)
actualpre
##create confusion matrix
table <- table(testcategory,testpred)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(table)

Hear$HeartDisease <- as.factor(Hear$HeartDisease)
confusionMatrix(as.factor(testcategory), as.factor(testpred), 
                positive = "1", mode = "everything")
MCC <- mcc(preds = as.factor(testpred),actuals = as.factor(testcategory))
MCC <- mcc(TP = 102, FP = 20, TN = 83, FN = 25)
MCC
### k-NN with 0.5 cut-off ###
Heart <- read.csv("C:/Users/Juan Ferreira/Desktop/Research/heart.csv")

par(mfrow = c(1, 1))
colSums(is.na(Heart))

which(colSums(is.na(Heart))>0)

names(which(colSums(is.na(Heart))>0))
summary(Heart, Heart.measures = TRUE)

ggpairs(cbind(Heart, Cluster=as.factor(Heart$HeartDisease)),
        columns = c(1,4,5,8, 10), aes(colour=Cluster, alpha=0.05),
        lower=list(continuous="points"),
        axisLabels="none", switch="both") +
  theme_bw()
Heart <- subset(Heart, RestingBP != 0)
Heart <- subset(Heart, Cholesterol != 0)

cat <- data.frame(Heart$HeartDisease)
Pie_Plot <- factor(Heart$HeartDisease)
par(mfrow = c(1, 2))
PieChart(Pie_Plot, data = cat, hole = 0,
         fill = brewer.pal(3, "Greys"),
         labels_cex = 1.2)

frq_table <- table (Heart$HeartDisease)
barplot(frq_table, ylim = c(0, 450), col = brewer.pal(2, "Greys"))
plot(1,1)
legend(.6, 1, legend=c("No", "Yes"), 
       fill = brewer.pal(3, "Greys"),
       title = "Underlying Heart Condition:"
)


nrow(Heart)
Heart$Sex <- ifelse(Heart$Sex == "M", 0, 1) #0 = Male and 1 = Female
Heart$ExerciseAngina <- ifelse(Heart$ExerciseAngina == "N", 0, 1) 
#0 = No and 1 = Y
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
Heart$RestingECG <- as.character(Heart$RestingECG)

Heart$RestingECG <- factor(Heart$RestingECG,
                           levels = c("Normal", "LVH", "ST"));

Heart$RestingECG <- as.numeric(Heart$RestingECG)
Heart$ST_Slope <- ifelse(Heart$ST_Slope == "Up", 0, 1) 
#0 = Normal and 1 = coronary ischemia.
typeof(Heart$ChestPainType)


Heart$ChestPainType <- fct_collapse(Heart$ChestPainType, Normal = "NAP", 
                                    S1 = c("ASY","ATA"), S2 = "TA")
Heart$ChestPainType <- as.numeric(Heart$ChestPainType)
######################### K-NN ##########################
# Create a normalisation function:
norm_rank <- function(a) {(a - 1 )/ (2)}
normalize <-function(x) { (x -min(x))/(max(x)-min(x))   }
Heart$RestingECG <- norm_rank(Heart$RestingECG)
Heart$ChestPainType <- norm_rank(Heart$ChestPainType)
Heart$Age <- normalize(Heart$Age)
Heart$RestingBP <- normalize(Heart$RestingBP)
Heart$Cholesterol <- normalize(Heart$Cholesterol)
Heart$MaxHR <- normalize(Heart$MaxHR)
Heart$Oldpeak <- normalize(Heart$Oldpeak)
DM <- cor(Heart)
corrplot(DM, method = "number")
Heart <- Heart[-9]
nrow(Heart)
DM <- cor(Heart)
corrplot(DM, method = "number")
Heart <- Heart[-9]
DM <- cor(Heart)
corrplot(DM, method = "number")
set.seed(123)
Subset <- Heart[,1:9]
data_split <- sample(1:nrow(Subset), 0.75 * nrow(Subset))
# extract the training set
Heart
train <- Subset[data_split,]
test <- Subset[-data_split,]

# extract the 10th column of train dataset because it will be used as 

target_category <- Heart[data_split,10]
target_category

test_category <- Heart[-data_split,10]
seed = 123
set.seed(seed)
head(Heart)

# Determining the number of clusters, k
SCREE(
  DM,
  eigen_type = c("PCA", "SMC", "EFA"),
  use = c("pairwise.complete.obs", "all.obs", "complete.obs", "everything",
          "na.or.complete"),
  cor_method = c("pearson", "spearman", "kendall"),
  n_factors = 1
)

nrow(train)
nrow(test)
nrow(target_category)

abline(h=1, col="indianred3", type = "l", lty = 2)
set.seed(123)
test_pred <- knn(train,test,cl=target_category,k=2)
df_pred=data.frame(test_category,test_pred)
count = 0
act <- ifelse(test_pred == test_category, 1, 0)
actual_pred <- sum(act) / length(test_category)
actual_pred
##create confusion matrix
table <- table(test_category,test_pred)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(table)

Heart$HeartDisease <- as.factor(Heart$HeartDisease)
confusionMatrix(as.factor(test_category), as.factor(test_pred), 
                positive = "1", mode = "everything")

######################### CART using Info Gain ##########################
Heart_0 <- read.csv("C:/Users/Juan Ferreira/Desktop/Research/heart.csv")
Heart_0 <- subset(Heart_0, RestingBP != 0)
Heart_0 <- subset(Heart_0, Cholesterol != 0)
Heart_0$HeartDisease <- ifelse(Heart_0$HeartDisease == 0, "no", "yes")
Heart_0$Sex <- ifelse(Heart_0$Sex == "M", 0, 1) 
#0 = Male and 1 = Female
Heart_0$ExerciseAngina <- ifelse(Heart_0$ExerciseAngina == "N", 0, 1) 
#0 = No and 1 = Y
Heart_0$RestingECG <- as.character(Heart_0$RestingECG)
Heart_0$RestingECG <- factor(Heart_0$RestingECG,
                           levels = c("Normal", "LVH", "ST"));

Heart_0$RestingECG <- as.numeric(Heart_0$RestingECG)
Heart_0$ST_Slope <- ifelse(Heart_0$ST_Slope == "Up", 0, 1) 
#0 = Normal and 1 = coronary ischemia.
typeof(Heart_0$ChestPainType)


Heart_0$ChestPainType <- fct_collapse(Heart_0$ChestPainType, 
                                      Normal = "NAP", 
                                      S1 = c("ASY","ATA"), S2 = "TA")
Heart_0$ChestPainType <- as.numeric(Heart_0$ChestPainType)
nrow(Heart_0)
set.seed(123)
  Subs <- Heart_0[,1:11]
  data_split_0 <- sample(1:nrow(Subs), 0.75 * nrow(Subs))
  ##extract training set
  
  train_0 <- Subs[data_split_0,]
  head(train_0)
  dim(train_0)
  test_0 <- Subs[-data_split_0,]
  nrow(test_0)
  head(test_0)
  t_0 <- Heart_0[data_split_0,1:12]

# extract the 12th column of train dataset because it will be used as 'cl' 
# argument  in knn function.
target_category_0 <- Heart_0[data_split_0,12]

test_category_0 <- Heart_0[-data_split_0,12]

set.seed(123)
c = t_0$HeartDisease
# CART using the gain ratio as a splitting measure
test_pred_0 <- rpart(as.factor(c)~., data=train_0, method = "class", 
                     parms = list(prior = c(0.65,0.35), split = "information"))

plot(test_pred_0)
text(test_pred_0)
test_pred_0
test_category_0 <- ifelse(test_category_0 == "yes", 1, 0)
t_0 <- ifelse(t_0 == "yes", 1, 0)
p <- predict(test_pred_0, test_0)
p
split <- sqrt((2 * log(1/0.05))/ length(test_category_0))
split
j <- ifelse(p[,2] > split, 1, 0)
j
plot(test_pred_0)
text(test_pred_0)
confusionMatrix(as.factor(j), as.factor(test_category_0),
                positive = "1", mode = "everything")
rmse(j, test_category_0)
act_0 <- ifelse(j == test_category_0, 1, 0)
actual_pred <- sum(act_0) / length(test_category_0)
actual_pred

######################### CART using Gini Index ##########################
Heart_1 <- read.csv("C:/Users/Juan Ferreira/Desktop/Research/heart.csv")
Heart_1 <- subset(Heart_1, RestingBP != 0)
Heart_1 <- subset(Heart_1, Cholesterol != 0)
Heart_1$HeartDisease <- ifelse(Heart_1$HeartDisease == 0, "no", "yes")
Heart_1$Sex <- ifelse(Heart_1$Sex == "M", 0, 1) 
Heart_1$ExerciseAngina <- ifelse(Heart_1$ExerciseAngina == "N", 0, 1)
Heart_1$RestingECG <- as.character(Heart_1$RestingECG)
Heart_1$RestingECG <- factor(Heart_1$RestingECG,
                             levels = c("Normal", "LVH", "ST"));

Heart_1$RestingECG <- as.numeric(Heart_1$RestingECG)
Heart_1$ST_Slope <- ifelse(Heart_1$ST_Slope == "Up", 0, 1) 
#0 = Normal and 1 = coronary ischemia.
typeof(Heart_1$ChestPainType)


Heart_1$ChestPainType <- fct_collapse(Heart_1$ChestPainType, 
                                      Normal = "NAP", S1 = c("ASY","ATA"),
                                      S2 = "TA")
Heart_1$ChestPainType <- as.numeric(Heart_1$ChestPainType)

Subs_1 <- Heart_1[,1:11]
data_split_1 <- sample(1:nrow(Subs_1), 0.75 * nrow(Subs_1))
##extract training set

train_1 <- Subs[data_split_1,]
head(train_1)
dim(train_1)
test_1 <- Subs[-data_split_1,]
nrow(test_1)
head(test_1)
t_1 <- Heart_1[data_split_1,1:12]
target_category_1 <- Heart_1[data_split_1,12]
test_category_1 <- Heart_1[-data_split_1,12]

set.seed(seed)
c_1 = t_1$HeartDisease
test_pred_1 <- rpart(as.factor(c_1)~., data=train_1, method = "class", 
                     parms = list(prior = c(0.75,0.25), split = "gini"))

plot(test_pred_1)
text(test_pred_1)
test_category_1 <- ifelse(test_category_1 == "yes", 1, 0)
t_1 <- ifelse(t_1 == "yes", 1, 0)
p_1 <- predict(test_pred_1, test_1)
bound <- sqrt((8*log(1/0.05))/ nrow(Subs_1)) #McDiarmid's bound 
k <- ifelse(p_1[,2] > bound, 1, 0)
# Creating the confusion matrix
confusionMatrix(as.factor(k), as.factor(test_category_1), 
                positive = "1", mode = "everything")
rmse(k, test_category_1)
act_1 <- ifelse(k == test_category_1, 1, 0)
actual_pred_1 <- sum(act_1) / length(test_category_1)
actual_pred_1

######################### C4.5 DT ##########################
Heart_2 <- read.csv("C:/Users/Juan Ferreira/Desktop/Research/heart.csv")
Heart_2 <- subset(Heart_2, RestingBP != 0)
Heart_2 <- subset(Heart_2, Cholesterol != 0)
Heart_2$HeartDisease <- ifelse(Heart_2$HeartDisease == 0, "no", "yes")
Heart_2$Sex <- ifelse(Heart_2$Sex == "M", 0, 1) 
#0 = Male and 1 = Female
Heart_2$ExerciseAngina <- ifelse(Heart_2$ExerciseAngina == "N", 0, 1) 
#0 = No and 1 = Y
Heart_2$RestingECG <- as.character(Heart_2$RestingECG)
Heart_2$RestingECG <- factor(Heart_2$RestingECG,
                             levels = c("Normal", "LVH", "ST"));

Heart_2$RestingECG <- as.numeric(Heart_2$RestingECG)
Heart_2$ST_Slope <- ifelse(Heart_2$ST_Slope == "Up", 0, 1) 
#0 = Normal and 1 = coronary ischemia.
typeof(Heart_2$ChestPainType)


Heart_2$ChestPainType <- fct_collapse(Heart_2$ChestPainType, 
                                      Normal = "NAP", S1 = c("ASY","ATA"), 
                                      S2 = "TA")
Heart_2$ChestPainType <- as.numeric(Heart_2$ChestPainType)

Subs_2 <- Heart_2[,1:11]
data_split_2 <- sample(1:nrow(Subs_2), 0.75 * nrow(Subs_2))
##extract training set

train_2 <- Subs[data_split_2,]
head(train_2)
dim(train_2)
test_2 <- Subs[-data_split_2,]
nrow(test_2)
head(test_2)
t_2 <- Heart_2[data_split_2,1:12]
target_category_2 <- Heart_2[data_split_2,12]
test_category_2 <- Heart_2[-data_split_2,12]

set.seed(seed)
c_2 = t_2$HeartDisease
test_pred_2 <- J48(as.factor(c_2)~., data=train_2)
plot(test_pred_2)

test_category_2 <- ifelse(test_category_2 == "yes", 1, 0)
t_2 <- ifelse(t_2 == "yes", 1, 0)
p_2 <- predict(test_pred_2, test_2)
p_2 <- ifelse(p_2 == "no", 0, 1)
p_2 <- as.factor(p_2)

confusionMatrix(as.factor(p_2), as.factor(test_category_2),
                positive = "1", mode = "everything")
rmse(as.integer(p_2), test_category_2)
act_2 <- ifelse(as.numeric(p_2) == test_category_2, 0, 1)
actual_pred_2 <- sum(act_2) / length(test_category_2)
actual_pred_2

######################### C4.5 DT (error pruned) ##########################
Heart_3 <- read.csv("C:/Users/Juan Ferreira/Desktop/Research/heart.csv")
Heart_3 <- subset(Heart_3, RestingBP != 0)
Heart_3 <- subset(Heart_3, Cholesterol != 0)

Subs_3 <- Heart_3[,1:11]
data_split_3 <- sample(1:nrow(Subs_3), 0.75 * nrow(Subs_3))

train_3 <- Subs[data_split_3,]
head(train_3)
dim(train_3)
test_3 <- Subs[-data_split_3,]
nrow(test_3)
head(test_3)
t_3 <- Heart_3[data_split_3,1:12]
target_category_3 <- Heart_3[data_split_3,12]
test_category_3 <- Heart_3[-data_split_3,12]
test_category_3
set.seed(seed)
c_3 = t_3$HeartDisease
test_pred_3 <- J48(as.factor(c_3)~., data=train_3, 
                   control = Weka_control(R = TRUE))
#df_pred_0=data.frame(test_category_0,test_pred_1)
plot(test_pred_3)

p_3 <- predict(test_pred_3, test_3)

confusionMatrix(as.factor(p_3), as.factor(test_category_3), 
                positive = "1", mode = "everything")


act_3 <- ifelse(k == test_category_2, 1, 0)
actual_pred_3 <- sum(act_3) / length(test_category_3)
actual_pred_3

# Plotting the ROC curves
aa_0 <- roc(test_category, as.numeric(test_pred), direction="<")
aa_1 <- roc(test_category_0, j, direction="<")
aa_2 <- roc(test_category_1, k, direction="<")
aa_3 <- roc(test_category_2, as.numeric(p_2), direction="<")
aa_4 <- roc(test_category_3, as.numeric(p_3), direction="<")
aa_0
par(mfrow = c(1, 1))
plot(tpr ~ fpr,
     coords(aa_0, "all", ret = c("tpr", "fpr"), transpose = FALSE),
     type="b",      col="indianred3", lwd=3, main="ROC curves")
lines(tpr ~ fpr,
     coords(aa_1, "all", ret = c("tpr", "fpr"), transpose = FALSE),
     type="b",col="olivedrab3", lwd=3)
lines(tpr ~ fpr,
      coords(aa_2, "all", ret = c("tpr", "fpr"), transpose = FALSE),
      type="b",col="steelblue3", lwd=3)
lines(tpr ~ fpr,
      coords(aa_3, "all", ret = c("tpr", "fpr"), transpose = FALSE),
      type="b",col="slategray4", lwd=3)
lines(tpr ~ fpr,
      coords(aa_4, "all", ret = c("tpr", "fpr"), transpose = FALSE),
      type="b",col="gold3", lwd=3)
lines(0:1, 0:1)
points(0,1, col = "purple", lwd = 3.0)

lines(lines(0:1, 0:1))
legend(0.75, 0.3, legend=c("Perfect Classifier", "K-NN", "CART (info)", "CART (Gini)", 
                          "C4.5 (info)", "C4.5 (error)"), 
       fill = c("purple","indianred3","olivedrab4", "steelblue3", "slategray4", "gold3")
)

# Calculating MCC
MCC_0 <- mcc(preds = as.factor(test_pred),actuals = as.factor(test_category))
MCC_0 <- mcc(TP = 76, FP = 18, TN = 77, FN = 16)
MCC_0

MCC_1 <- mcc(preds = as.factor(j), actuals = as.factor(test_category_0))
MCC_1 <- mcc(TP = 83, FP = 18, TN = 75, FN = 11)
MCC_1

MCC_2 <- mcc(preds = as.factor(k),actuals = as.factor(test_category_1))
MCC_2 <- mcc(TP = 74, FP = 20, TN = 77, FN = 16)
MCC_2

MCC_3 <- mcc(preds = as.numeric(p_2),actuals = test_category_2)
MCC_3 <- mcc(TP = 82, FP = 18, TN = 79, FN = 8)
MCC_3

MCC_4 <- mcc(preds = as.numeric(p_3),actuals = test_category_3)
MCC_4 <- mcc(TP = 76, FP = 14, TN = 79, FN = 18)
MCC_4



Specificity = 0:1
FPR = 0:1
expon = function(x){
  exp(x)}
x <- 0:1
y <- jitter(x^2)


plot(FPR, Specificity)
lines(0:1, 0:1, col = "gold")
points(0, 1, col = "olivedrab4", lwd = 5)
abline(v = 0)
abline(h = 1)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

curve(x**0.05, add = TRUE, col = "steelblue3")
curve(x**0.15, add = TRUE, col = "indianred3")
legend(0.7, 0.3, legend=c("Better Performer", "Worse Performer", "Point of Best Fit", "Random Classifier"), 
       fill = c("steelblue3","indianred3", "olivedrab4", "Gold")
)

set.seed(sample.int(187,1))
test_pred_3_0 <- J48(as.factor(c_3)~., data=train_3, 
                   control = Weka_control(R = TRUE))
p_3_0 <- predict(test_pred_3, test_3)

err_metric=function(CM)
{
  TN =CM[1,1]
  TP =CM[2,2]
  FP =CM[1,2]
  FN =CM[2,1]
  FPR =(FP)/(FP+TN)
  TPR =(TP)/(FN+TP)
  print(paste("FPR: ",round(FPR,4)))
  print(paste("TPR: ",round(TPR,4)))
}

d3 <- confusionMatrix(as.factor(p_3_0), as.factor(test_category_3), 
                      positive = "1", mode = "everything")
err_metric(d3$table)
plot

points(0.1573, 0.8265)
points(0.1222, 0.7835)
points(0.0952, 0.8171)
points(0.0952, 0.8171)
points(0.1856, 0.8182)
points(0.1856, 0.8444)

