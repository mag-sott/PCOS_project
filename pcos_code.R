##########################################################
# Create pcos set, validation set (final hold-out test set)
##########################################################
options(digits = 3)
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(lubridate)
library(dslabs)
library(readxl)
library(readr)
library(corrplot)

# The original PCOS data set can be found under below links:
#https://www.kaggle.com/datasets/prasoonkottarathil/polycystic-ovary-syndrome-pcos

#Loading the data

path_1 <- "C:/Users/A529462/Desktop/R files/PCOS project/PCOS_data_without_infertility.xlsx"
pcos_1 <- read_excel(path_1, sheet = "Full_new")


path_2 <- "C:/Users/A529462/Desktop/R files/PCOS project/PCOS_infertility.csv"
pcos_2 <- read_csv(path_2)

#joining the files into one pcos data set

pcos <- merge(pcos_1, pcos_2, by = 'Sl. No')

#removing duplicate columns and the last empty column

pcos <- subset (pcos, select =  -c(`Patient File No..y`,`I   beta-HCG(mIU/mL).y`,
                                   `PCOS (Y/N).y`, `I   beta-HCG(mIU/mL).y`,
                                   `II    beta-HCG(mIU/mL).y`, `AMH(ng/mL).y`, `...45`))

#checking the data types of variables
str(pcos)

#there are numeric columns with data type chr - we change them to num

pcos$`II    beta-HCG(mIU/mL).x`<- as.numeric(pcos$`II    beta-HCG(mIU/mL).x`)
pcos$`AMH(ng/mL).x` <- as.numeric(pcos$`AMH(ng/mL).x`)

#checking for NA values

sapply(pcos, function(x) sum(is.na(x)))

#filling missing data with median value of the columns: Fast food (Y/N), 
#Marraige Status (Yrs), II    beta-HCG(mIU/mL).x, AMH(ng/mL)

pcos$`Fast food (Y/N)`[is.na(pcos$`Fast food (Y/N)`)] <- median(pcos$`Fast food (Y/N)`, na.rm = T)
pcos$`Marraige Status (Yrs)`[is.na(pcos$`Marraige Status (Yrs)`)] <- median(pcos$`Marraige Status (Yrs)`, na.rm = T)
pcos$`II    beta-HCG(mIU/mL).x`[is.na(pcos$`II    beta-HCG(mIU/mL).x`)] <- median(pcos$`II    beta-HCG(mIU/mL).x`, na.rm = T)
pcos$`AMH(ng/mL).x`[is.na(pcos$`AMH(ng/mL).x`)] <- median(pcos$`AMH(ng/mL).x`, na.rm = T)

#looking into data there is not much information for columns Sl. No and Patient File No..x 
#we drop them
pcos <- subset (pcos, select =  -c(`Sl. No`,`Patient File No..x`))

#Exploratory data analysis
#overview of all variables
summary(pcos)


# closer look to categorical data
#bar plots for categorical data

#PCOS(Y/N)
ggplot(pcos, aes(x=factor(`PCOS (Y/N).x`)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="PCOS(Y/N)", y = "count")

#proportion of patient with PCOS
mean(pcos$`PCOS (Y/N).x`=='1')

#Pregnant(Y/N)
ggplot(pcos, aes(x=factor(`Pregnant(Y/N)`)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Pregnant(Y/N)", y = "count")

#Weight gain(Y/N)
ggplot(pcos, aes(x=factor(`Weight gain(Y/N)`)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Weight gain(Y/N)", y = "count")

#Hair growth(Y/N)
ggplot(pcos, aes(x=factor(`hair growth(Y/N)`)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Hair growth(Y/N)", y = "count")

#Skin darkening (Y/N)
ggplot(pcos, aes(x=factor(`Skin darkening (Y/N)`)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Skin darkening (Y/N)", y = "count")

#Hair loss(Y/N)
ggplot(pcos, aes(x=factor(`Hair loss(Y/N)`)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Hair loss(Y/N)", y = "count")

#Pimples(Y/N)
ggplot(pcos, aes(x=factor(`Pimples(Y/N)`)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Pimples(Y/N)", y = "count")

#Fast food (Y/N)
ggplot(pcos, aes(x=factor(`Fast food (Y/N)`)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Fast food (Y/N)", y = "count")

#Reg.Exercise(Y/N)
ggplot(pcos, aes(x=factor(`Reg.Exercise(Y/N)`)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Reg.Exercise(Y/N)", y = "count")

#Blood Group 
ggplot(pcos, aes(x=factor(`Blood Group`)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Blood Group", y = "count")

#numerical data
#distribution of some numerical data

#Age (yrs)
ggplot(pcos, aes(x=`Age (yrs)`))+
  geom_histogram(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Age (yrs)", y = "count")

#Weight (Kg)
ggplot(pcos, aes(x=`Weight (Kg)`))+
  geom_histogram(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Weight (Kg)", y = "count")

#BMI
#adding additional lines for the BMI in norm values (18.5 - 24.9)
ggplot(pcos, aes(x=`BMI`))+
  geom_histogram(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="BMI", y = "count") +
  geom_vline(xintercept = 18.5,color="red") + geom_vline(xintercept = 24.9, color="red")

#Cycle length(days)
ggplot(pcos, aes(x=`Cycle length(days)`))+
  geom_histogram(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Cycle length(days)", y = "count")

#Marraige Status (Yrs)
ggplot(pcos, aes(x=`Marraige Status (Yrs)`))+
  geom_histogram(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="Marraige Status (Yrs)", y = "count")

#No. of aborptions
ggplot(pcos, aes(x=`No. of aborptions`))+
  geom_histogram(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + labs( x="No. of aborptions", y = "count")

## correlation matrix of all variables

cor_all <- round(cor(pcos),2)

cor_all

cor_pcos <- round(cor(pcos[ , colnames(pcos) != "PCOS (Y/N).x"],  # Calculate correlations
                pcos$`PCOS (Y/N).x`),2)
#only values above abs(0.25)


data.frame(cor_pcos) %>%
  rownames_to_column() %>%
  gather(key="variable", value="correlation", -rowname) %>%
  filter(abs(correlation) > 0.25) 



# dividing data into pcos_x and pcos_y
pcos$`PCOS (Y/N).x` <- factor(pcos$`PCOS (Y/N).x`)

y <- pcos$`PCOS (Y/N).x`

x <-subset(pcos, select = -`PCOS (Y/N).x`)

#dividing data into test and train set

set.seed(1)    
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_x <- x[test_index,]
train_x <- x[-test_index,]

test_y <- y[test_index]
train_y <- y[-test_index]



#logistic regression model
set.seed(1) 
train_glm <- train(train_x, train_y, method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)

#LDA model
set.seed(1)
train_lda <- train(train_x, train_y, method = "lda")
lda_preds <- predict(train_lda, test_x)
mean(lda_preds == test_y)

#QDA model

set.seed(5)
train_qda <- train(train_x, train_y, method = "qda")
qda_preds <- predict(train_qda, test_x)
mean(qda_preds == test_y)


# K-nearest neighbors model
set.seed(1)
train_knn <- train(train_x, train_y,
                   method = "knn")
knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)

# K-nearest neighbors model trained
set.seed(1)
tuning <- data.frame(k = seq(3, 21, 2))
train_knn_v <- train(train_x, train_y,
                     method = "knn", 
                     tuneGrid = tuning)
train_knn_v$bestTune

knn_preds_v <- predict(train_knn_v, test_x)
mean(knn_preds_v == test_y)



#Random forest model
set.seed(1)
tuning <- data.frame(mtry = c(3, 5, 7, 9))
train_rf <- train(train_x, train_y,
                  method = "rf",
                  tuneGrid = tuning,
                  importance = TRUE)
train_rf$bestTune


rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)

varImp(train_rf)
#ensamble

ensemble <- cbind(glm = glm_preds == "1", lda = lda_preds == "1", qda = qda_preds == "1", rf = rf_preds == "1", knn = knn_preds == "1", knn_v = knn_preds_v == "1")

ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "1", "0")
mean(ensemble_preds == test_y)

#All models together
models <- c("Logistic regression","LDA", "QDA", "Knn", "Knn with cross validation", "Random forest", "Ensemble")
accuracy <- c(mean(glm_preds == test_y),
              mean(lda_preds == test_y),
              mean(qda_preds == test_y),
              mean(knn_preds == test_y),
              mean(knn_preds_v == test_y),
              mean(rf_preds == test_y),
              mean(ensemble_preds == test_y))
data.frame(Model = models, Accuracy = accuracy)

