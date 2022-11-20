rm(list=ls())
getwd()
setwd("C:/Users/payal/OneDrive/Documents/541")
data <- read.csv("credit_risk_dataset.csv")
#Data perprocessing
data <- data.frame(data)
head(data)

na_count <-sapply(data, function(data) sum(length(which(is.na(data)))))
na_count
#Dropping Na values
data <- na.omit(data)
#Finding outliers
#pairs(data[,c(1,2,4,7,8)])
#scatterplot matrix clearly indicates outliers in Age, Income and Employement Length
#144 years in age seem a mistake in recording, on basis of plot, we will remove values
# for ages above 100 years
#Income has one clear outlier which might influence the result, hence dropping 
#Employement length above 60 years also seem unrelaistic, may be recording error
data <- subset(data, Age < 100)
data <- subset(data, Income < 400000)
data <- subset(data, Employement.length < 60)
table(data$default_on_file)
summary(data)
group <- NA
group[data$loan_status == 0 ] <- 1
group[data$loan_status == 1 ] <- 2
#pairs(data[,c(1,2,4,7)], col= c("dark green","red")[group],
      #main= "Scatterplot matrix of numerical variables and Loan status", 
      #legend = "top left")
#pairs(data[,c(8, 10,12)], col= c("dark green","red")[group],
      #main= "Scatterplot matrix of numerical variables and Loan status", 
      #legend = "top left")


library(ggplot2)
#Boxplots - to understand preliminary relationship
#relationship between Home ownership and loan status
group <- NA
group[data$loan_status == 0 ] <- "No default"
group[data$loan_status == 1 ] <- "default"
par(mfrow = c(2,3))
ggplot(data, aes(x=loan_grade, y= loan_percent_income, fill = group))+
  geom_boxplot()

ggplot(data, aes(x=factor(loan_grade), fill=group))+
  geom_bar(position = position_dodge())
ggplot(data, aes(x=factor(Home.ownership), fill=group))+
geom_bar(position = position_dodge())

ggplot(data, aes(x=factor(loan_intent), fill=group))+
  geom_bar(position = position_dodge())
ggplot(data, aes(x=factor(default_on_file), fill=group))+
  geom_bar(position = position_dodge())
ggplot(data, aes(x=factor(cred_hist_length), fill=group))+
  geom_bar(position = position_dodge())

set.seed(3)
n = nrow(data)
train <- sample(n, n*0.75, replace=F)
train_data <- data[train,]
test_data <- data[-train,]


library(ROCR)
library(plotROC)
library(pROC)
set.seed(3)
#multiple lr with all predictors

lr_m <- glm(loan_status~., family=binomial,data=train_data)


summary(lr_m)

plot(lr_m)




pred.prob <- predict(lr_m, test_data,type="response")
head(pred.prob)
pred.prob <- ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, test_data$loan_status)
Test_accuracy_all <- mean(pred.prob==test_data$loan_status)

predict1 <- predict(lr_m,test_data, type="response")


anova(lr_m, test="Chisq")

##From the above Anova test we see that certain predictors are more significant



##with subset
lr_subset <- glm(loan_status~ Age+Income+Home.ownership+loan_intent+
                   loan_grade+loan_amnt+loan_int_rate+ loan_percent_income,
                 data = train_data)


summary(lr_subset)

pred.prob <- predict(lr_subset, test_data,type="response")
head(pred.prob)
pred.prob <- ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, test_data$loan_status)
Test_accuracy_sub <- mean(pred.prob==test_data$loan_status)

anova(lr_subset, test="Chisq")
predict2 <- predict(lr_subset, test_data,type="response")


##Removing loan_interest rate also

lr_subset2 <- glm(loan_status~ Age+Income+Home.ownership+loan_intent+
                   loan_grade+loan_amnt+ loan_percent_income,
                 data = train_data)

summary(lr_subset2)

pred.prob <- predict(lr_subset2, test_data,type="response")
pred.prob <- ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, test_data$loan_status)
Test_accuracy_sub2 <- mean(pred.prob==test_data$loan_status)

predict3 <- predict(lr_subset2, test_data,type="response")
anova(lr_m, lr_subset, lr_subset2, test ="Chisq")
library(lmtest)
lrtest(lr_m, lr_subset, lr_subset2)

#Stepwise Variable Selection
library(MASS)
step.model <-  stepAIC(lr_m, trace = FALSE)
coef(step.model)
summary(step.model)
pred.prob.step <- predict(step.model, test_data,type="response")
pred.prob.step <- ifelse(pred.prob.step > 0.5, 1, 0)
table(pred.prob.step, test_data$loan_status)
Test_accuracy_step <- mean(pred.prob.step==test_data$loan_status)
Test_accuracy_step


predict4 <- predict(step.model, test_data,type="response")

##from the above analysis we see that model 

set.seed(100)
#LDA
library(MASS)
lda = lda(loan_status~., data = train_data)
lda.pred=predict(lda,test_data)
lda.class=lda.pred$class
accuracy_lda <-mean(lda.class==test_data$loan_status)

predict5 <- predict(lda,test_data)

table(lda.class,test_data$loan_status)
summary(lda)
lda

roc5 <- roc(test_data$loan_status ~ lda.pred$posterior[,2], 
            plot = TRUE, print.auc = TRUE)

library(ROCR)
#Tree
library(tree)
train_data$loan_status <- as.factor(train_data$loan_status)
test_data$loan_status <- as.factor(test_data$loan_status)
tree2 = tree(loan_status~.,data=train_data)
yhat.tree=predict(tree2, newdata=test_data, type="class")
accuracy_tree <- mean((yhat.tree==test_data$loan_status))

table( yhat.tree, test_data$loan_status)

tail(yhat.tree)
#yhat.tree=predict(tree2, newdata=test_data, type="responses")

#roc1 <- roc(test_data$loan_status ~ predict1, plot = TRUE, print.auc = TRUE)
#roc2 <- roc(test_data$loan_status ~ predict2, plot = TRUE, print.auc = TRUE)
#roc3 <- roc(test_data$loan_status ~ predict3, plot = TRUE, print.auc = TRUE)
#roc4 <- roc(test_data$loan_status ~ predict4, plot = TRUE, print.auc = TRUE)
#roc5 <- roc(test_data$loan_status ~ lda.pred$posterior[,2], 
   # plot = TRUE, print.auc = TRUE)
#roc(test_data$loan_status ~ yhat.tree, plot = TRUE, print.auc = TRUE)


#plot(roc1, xlim= c(1,0))
#lines(roc2, add=TRUE, col='red')
#lines(roc3, add= TRUE, col="blue")
#lines(roc4, add= TRUE, col="green")
#lines(roc5, add= TRUE, col="yellow")
#legend("topleft", legend=c("Lr All","Model2","Model3", "Model4", "LDA"),
 #      lty=1:2, cex=0.8)
accuracy <- cbind(Test_accuracy_all,Test_accuracy_sub,Test_accuracy_sub2, 
                  Test_accuracy_step, accuracy_lda,accuracy_tree)
accuracy

#plot(roc1)
#plot(roc5, main="Roc curve of LDA")

#plot(lr_subset)

#library(rpart.plot)
#prp(tree2)
#plot(tree2)
#text(tree2)
summary(tree2)
