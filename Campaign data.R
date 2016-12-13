
# ################################################ 


#Direct Marketing Campaign for a European Bank
#load the data


# ################################################ 

setwd(" ")
campaign_data<-read.csv("Campaign Data.csv",sep = ";")
View(campaign_data)
install.packages("data.table")
install.packages("Amelia")
install.packages("carrplot")
install.packages("gmodels")
install.packages("VIM")
install.packages("ggplot2")
install.packages("car")

library(VIM)
library(Amelia)
library(corrplot)
library(gmodels)
library(mice)
library(ROCR)
library(caret)
library(ROSE)
library(ggplot2)
library(Hmisc)
library(car)
library(caTools)

####Data Exploration####

#structure & summary of the data set

str(campaign_data) 
summary(campaign_data)

#to see top & bottom 5 rows

head(campaign_data)
tail(campaign_data)

#checking to structure for some random samples data

samp <-sample(1:nrow(campaign_data),10,replace= FALSE)
samp
campaign_data[samp,]

#delete the  NA in the dataset

campaign_data<- campaign_data[!(is.na(campaign_data) ), ]


# Checking for Duplicates

sum(duplicated(campaign_data))
describe(campaign_data)

#checking the strucutre of categorical variable

str(campaign_data$education)
levels(campaign_data$education)
table(campaign_data$education)
plot(table(campaign_data$education))

#Finding and removing/imputing missing values

campaign_data[campaign_data == "unknown"]= NA
sapply(campaign_data, function(x) sum(is.na(x)))
aggr_plot <- aggr(campaign_data, col=c('blue','green'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#converting factor to numeric 

campaign_data$age <- as.numeric(campaign_data$age)
campaign_data$duration <- as.numeric(campaign_data$duration)
campaign_data$campaign <- as.numeric(campaign_data$campaign)
campaign_data$pdays <- as.numeric(campaign_data$pdays)
campaign_data$previous <- as.numeric(campaign_data$previous)
campaign_data$emp.var.rate <- as.numeric(campaign_data$emp.var.rate)
campaign_data$cons.price.idx <- as.numeric(campaign_data$cons.price.idx)
campaign_data$cons.conf.idx <- as.numeric(campaign_data$cons.conf.idx)
campaign_data$euribor3m <- as.numeric(campaign_data$euribor3m)
campaign_data$nr.employed <- as.numeric(campaign_data$nr.employed)
str(campaign_data)

#Creating new Business variables

level1<- paste(rep("catg",5),seq(1:5),sep="-")
vari <-sample(level1,nrow(campaign_data),replace = TRUE)
str(vari)
vari <- as.factor(vari)
str(vari)
year <- ifelse(campaign_data$month %in% c("jan","feb","mar","apr","may","jun"),yes="1st half",no="2nd half")


#appending those variables to the data

campaign_data$newVariable <- vari
campaign_data$year <- year
str(campaign_data)

#imbalanced data

t1 <-subset(campaign_data,  campaign_data$y == 'yes')
nrow(t1)/nrow(campaign_data)

#finding the Distrubtion of data

#Age wrt last contact duration 
ggplot(campaign_data,aes(x=age,y=duration,color =y))+geom_point()+facet_wrap(~job,scales = "free_y")+labs(x="age",y="duration",title="age with duration of contact")
+theme(axis.text.x=element_text(angle=90))
#Age wrt campaign call during a campaign
ggplot(campaign_data,aes(x=age,y=campaign,color = y))+geom_point()+geom_smooth(method = lm)+geom_smooth(method=lm,aes(group=1))+
  facet_wrap(~job,scales = "free_y")+labs(x="age",y="campagin calls")+theme(axis.text.x=element_text(angle=90))
#type of job vs duration of call
ggplot(campaign_data,aes(x=job,y=duration))+geom_boxplot()+facet_wrap(~marital,scales="free_y")+
  labs(x="job",y="duration",title=" job vs duration of call")+theme(axis.text.x=element_text(angle=90))

#A stacked bar  comparing job vs martial vs Education

ggplot(campaign_data,aes(x=job,fill= marital))+geom_bar(position = "fill")+scale_fill_brewer(palette="Set2")+
facet_wrap(~education,scales="free_y")+
labs(x="age",title="Frequency distrubtion")+theme(axis.text.x=element_text(angle=90))  

#finding porportions for two categorical variables

edumar <- table(campaign_data$education,campaign_data$marital)
edumar
mosaicplot(edumar)

# Finding the Distrubtion of continous variables --------------------------

hist(campaign_data$age,col="red",freq = FALSE)
hist(campaign_data$pdays, breaks= 80,main = paste("histogram of no of days since last contact"))
CrossTable(campaign_data$pdays, campaign_data$y)
hist(campaign_data$campaign, breaks = 80, main = paste("histogram rep no of  contacts during the campaign "))#, xlab = campaign) 
CrossTable(campaign_data$campaign,campaign_data$y)
hist(campaign_data$previous)
hist(log(campaign_data$age ))
table <- table(campaign_data$y, campaign_data$default)
mosaicplot(table)
table <- table((as.factor(campaign_data$day_of_week)))
barchart(table)

# Splitting dataset into training and testing

set.seed(100)

# training data contains 75% of the data
#testing data contains 25% of the data

train.prop = 0.75
cases = sample(nrow(campaign_data), nrow(campaign_data)*train.prop)
campaign_train = campaign_data[cases,]
campaign_test = campaign_data[-cases,]
attach(as.data.frame(campaign_train))

##############Logistice Regression ##################

initial_model <- glm(y ~., data =campaign_train, family = binomial("logit"))   
summary(initial_model)
pred <- predict(initial_model, campaign_test[,-21], type = 'response')
pred <- ifelse(pred > 0.5, 1,0)
table(campaign_test$y, pred)
#Accuracy
#6514+418/(6514+418+536+233)
#0.9001  == 90.0%

## assuming a low threshold, acc to the ROC curve - to reduce the cost

FP_cost=-4
FN_cost= 60
x=seq(0,1,0.1)
for (i in x)
  {
   print(i)
   pred_res <- ifelse(pred > i, 1,0)
   confusion_matrix <- matrix(table(campaign_test$y, pred_res),2,2)
   #print(confusion_matrix)
   cost <- (confusion_matrix[1,2]*FP_cost)+ ( confusion_matrix[2,1]* FN_cost)
   print(cost)
   y <- c(y,cost)
   #print(min(y))
  }
typeof(y)
dim(y)
plot(as.list(y),as.list(x))
plot(y,  xlim=c(0, 1), ylim=c(1000, 7000))
############Analysing the results#######

library(ROCR)
prob <- predict(initial_model, newdata=campaign_test[,-21], type="response")
pred <- prediction(prob, campaign_test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

#random forest
#finding the important attributes

table(campaign_data$default)
initial_model1<- campaign_data[, c(2:7)]
set.seed(100)
imputed = complete(mice(initial_model1))
campaign_data[, c(2:7)] = imputed
write.csv(campaign_data, "campaign_imputed.csv")

# Classification

install.packages("randomForest")
library(randomForest)
campaign_data <- read.csv("campaign_imputed.csv")  

#Create train and validation sets from above

seg.rf <- campaign_train
seg.rf
seg.rf.class.all <- campaign_test
seg.rf.class.all
n = dim(campaign_test)[1]
seg.rf.class = rep(0,n)
for(i in 1:n){
  tmp= table(seg.rf.class.all$individual[i,])
  seg.rf.class[i]= names(tmp)[which.max(tmp)]
} 
seg.rf.class
table(campaign_test$y, seg.rf.class) #no 9272 yes 1139
CrossTable(campaign_test$y, seg.rf.class)
importance(seg.rf)
#total :10411 
no:
#variable imporance plot

library(gplots)
library(RColorBrewer)
yes = subset(campaign_data, campaign_data$y == "yes")
hist(yes$duration,breaks = 80, main = "Histogram of call duration who subscribed")
no = subset(campaign_data, campaign_data$y == "no")
hist(no$duration,breaks = 80, main = "Histogram of call duration who did not subscribe")



