## Libraries
library(ggplot2)
library(MASS)
library(pROC)
library(ROCR) 
library(leaps)
library(glmnet)


## Functions
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "green", ...)
}

box_plot_categories <- function(data, y){
  n_features = length(data)
  grid = round(sqrt(n_features))
  print(grid)
  par(mfrow=c(grid, grid))
  names = colnames(data)
  for (idx in c(1:n_features)) {
    plot(y ~ data[, idx], xlab=names[idx], main=c('Boxplot y ~ ', names[idx]))
  }
  par(mfrow=c(1, 1))
}

get.roc.recall.values <- function(pred_models, true_value, yes_plot=TRUE) {
  result <- data.frame(Thr.ROC=double(), Specificity=double(), Sensitivity=double(),
                       Thr.Prec.Rec=double(), Recall=double(), Precision=double())
  n_models = length(pred_models)
  if (yes_plot==TRUE) {
    png(filename="./curves.png")
    par(mfrow=c(n_models, 2))
  }
  for (pred in pred_models) {
    ### ROC
    roc.res <- roc(true_value, pred, levels=c("0", "1"))
    if (yes_plot==TRUE){
      plot(roc.res, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
    }
    best.roc  <- coords(roc.res, "best")
    
    ### PREC-REC
    pred.rec = prediction(pred, true_value)
    perf = performance(pred.rec, "prec", "rec")
    if (yes_plot==TRUE){
      plot(perf)
    }
    pr_cutoffs <- data.frame(cutrecall=perf@alpha.values[[1]], recall=perf@x.values[[1]], 
                             precision=perf@y.values[[1]])
    pr_cutoffs <- pr_cutoffs[pr_cutoffs$recall>0 &  pr_cutoffs$precision >0, ]
    best_recall <- pr_cutoffs[which.max(pr_cutoffs$recall + pr_cutoffs$precision), ]
    
    result[nrow(result) + 1,] = c(best.roc[1, 1], best.roc[1, 2], best.roc[1, 3], 
                                  best_recall[1, 1], best_recall[1, 2], best_recall[1, 3])
  }
  if(yes_plot==TRUE){
    dev.off()
  }
  par(mfrow=c(1, 1))
  return(result)
}

################################################
# Dataset
stroke_data <- read.csv('healthcare-dataset-stroke-data.csv')
View(stroke_data)
names(stroke_data)
summary(stroke_data)
sum(is.na(stroke_data))

####################
# 1. Preprocessing
#####################
stroke_data<-stroke_data[,-1]
stroke_data$gender<- as.factor(stroke_data$gender)
stroke_data$ever_married<-as.factor(stroke_data$ever_married)
stroke_data$work_type<-as.factor(stroke_data$work_type)
stroke_data$Residence_type<-as.factor(stroke_data$Residence_type)
stroke_data$smoking_status<-as.factor(stroke_data$smoking_status)
stroke_data$heart_disease<-as.factor(stroke_data$heart_disease)

# modifiche a bmi
stroke_data$bmi <- as.numeric(stroke_data$bmi)# A warning has to be shown for N/A values which has to be map in NAs values. In case this warning is not shown, update R version to 4.__
stroke_data<- na.omit(stroke_data)

summary(stroke_data)

##################################
attach(stroke_data)
##################################

#### Dataset biased 
table(stroke)
table(stroke)/dim(stroke_data)[1]
barplot(table(stroke)/dim(stroke_data)[1],
        xlab='Stroke',col = c('#F8766D','#00BFC4'))

############
## 2. Descriptive Statistic
############

# box plots
###########
par(mfrow=c(1,3))
boxplot(avg_glucose_level~stroke, xlab= 'stroke', 
        ylab = 'average glucose level', col = c('#F8766D','#00BFC4'))
boxplot(bmi~stroke, xlab = 'stroke', ylab = 'bmi',col = c('#F8766D','#00BFC4'))
boxplot(age~stroke, xlab='stroke' ,ylab = 'age',col = c('#F8766D','#00BFC4'))
par(mfrow=c(1,1))
# pair plot y~ relevant feature (da inserire)
#################
# age
plot(stroke~age)
stroke.less.35 <- stroke_data[stroke_data$age<35, 'stroke']
table(stroke.less.35 <- stroke_data[stroke_data$age<35, 'stroke'])#/length(stroke.less.35)
table(stroke.less.50 <- stroke_data[stroke_data$age>=35 & stroke_data$age<50 , 'stroke'])
table(stroke.major.50 <- stroke_data[stroke_data$age>=50 , 'stroke'])

table(heart_disease)
table(stroke_data[stroke_data$heart_disease==0, ]$stroke)
table(stroke_data[stroke_data$heart_disease==1, ]$stroke)
# Scatter plot
################
ggplot(stroke_data, aes(x = avg_glucose_level, y = bmi,col = as.factor(stroke))) +
  labs(x = "average glucose level", y = "bmi", color = "Stroke") + geom_point()
ggplot(stroke_data, aes(x = avg_glucose_level, y = age,col = as.factor(stroke))) + 
  labs(x = "average glucose level",y = "age", color = "Stroke") + geom_point()
ggplot(stroke_data, aes(x = bmi, y = age, col = as.factor(stroke))) +
  labs(x = "bmi", y = "age", color = "Stroke") +geom_point()

# 3. PAIR PLOTS WITH histogram and correlation matrix
#################################################
pairs(stroke_data, diag.panel=panel.hist, upper.panel=panel.cor)


# 4. Residual plots and diagnostic for logistic GLMs
#################################################

# 4.1 Full and Reduced Model
#####
mod.full <- glm(stroke~., data=stroke_data, family = binomial)
summary(mod.full)

par(mfrow=c(2,2))
plot(mod.full)
par(mfrow=c(1,1))

##### 
# Reduced model 1 : We remove at least all the features that have collinearity between each other (work_type, ever_married)
# and the residence type
mod.red1 <- glm(stroke ~ age + bmi + avg_glucose_level + hypertension + smoking_status + gender + heart_disease, family=binomial)
summary(mod.red1)
# Reduced model 2, Remove gender
mod.red2 <- glm(stroke ~ age + bmi + avg_glucose_level + hypertension + smoking_status  + heart_disease, family=binomial) #
summary(mod.red2)
# Final Reduced Model

mod.red <- glm(stroke~age + heart_disease + avg_glucose_level+ hypertension, data=stroke_data, family = binomial)
summary(mod.red)

par(mfrow=c(2,2))
plot(mod.red)
par(mfrow=c(1,1))
# We can see from the residual vs predicted values the presence of high non-linearity in the dataset.
# In the qqplot instead we see that residuals do no follow a normal distribution
# Instead in the standard deviance vs predicted we can see that homoscedasticity does not hold since the 
# line of the standard residual is not flat, hence even by standardizing the residual we end up having high
# variance among residuals.
# In the end by looking at the leverage plot, we see the presence of some sample with high leverage values (bottom right), which
# could influence the prediction of the model. 
# Buuut i don't know in which range of levarage value is considered to change a lot the prediction of the model.
# Furthermore R does not show the index of the sample with high leverage, i guess because a lot of values could change the prediction.
# Some outliers with high variance are: idx: 183, 246, 163

print(stroke_data[c("246","183","119"), ])
###################

#anova computation.
anova(mod.red, mod.full, test="Chisq")
# As expected from the anova test REJECTS that the complex model is more
# significant than the reduced one, since the p-value is not less than 5%.
# Hence the full model does not help with our prediction.
######################


#### 4.2 Interaction
####################
# le scelte fatte sono state guidate da p-value con almeno il punto
# mod.red is our starting model
summary(mod.red) # AIC=1384.6

# let's procede with the iteraction:
# 1. age*~
mod1 <- glm(stroke~age + avg_glucose_level+ heart_disease+ hypertension +
               age*heart_disease, family=binomial)
summary(mod1) # AIC = 1384

# 2. avg_glucose_level*~
mod2 <- glm(stroke~age + avg_glucose_level+ heart_disease+hypertension +
              avg_glucose_level*hypertension, family=binomial)
summary(mod2) # AIC = 1385.9

# 3. heart_disease*hypertension
mod3 <- glm(stroke~age + avg_glucose_level+ heart_disease+hypertension + 
            heart_disease*hypertension, family=binomial)
summary(mod3) # AIC = 1384.5

# 4. from mod.red we remove heart_disease and add
# 4.1 add age*~
mod4 <- glm(stroke~age + avg_glucose_level + hypertension + 
              age*hypertension, family=binomial)
summary(mod4) # winner for 4: AIC= 1386.2

# 4.2 add avg_glucose_level*hypertension
mod5 <- glm(stroke~age + avg_glucose_level + hypertension + 
              avg_glucose_level*hypertension, family=binomial)
summary(mod5)

# 5. sommiamo le 2 best: mod1 e mod3
mod6 <- glm(stroke~age + avg_glucose_level+ heart_disease+ hypertension +
              age*heart_disease + heart_disease*hypertension, family=binomial)
summary(mod6)

par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))

print(stroke_data[c("207","150","100"), ])

## 4.3 polynomial model
##############
mod.poly1 <- glm(stroke~age + heart_disease + avg_glucose_level+ hypertension+bmi+
                       I(bmi^2), family = binomial)
summary(mod.poly1)

mod.poly2 <- glm(stroke~age + heart_disease + avg_glucose_level+ hypertension+bmi+
                     + I(avg_glucose_level^2), family = binomial)
summary(mod.poly2)

mod.poly3 <- glm(stroke~age + heart_disease + avg_glucose_level+ hypertension+bmi+
                      I(bmi^2) + I(avg_glucose_level^2), family = binomial)
summary(mod.poly3)

# 5. LDA
###########################################
#                   LDA
# Assumption: samples are normally distributed and have same variance in every class => strong assumption.
############################################

lda.fit <- lda(stroke ~ age + bmi + avg_glucose_level + hypertension +  gender
               + smoking_status +  Residence_type + heart_disease)
lda.pred <- predict(lda.fit)
table(lda.pred$class, stroke)
lda.pred.stroke <- lda.pred$posterior[, 2]

# 6.
###########################################
#                   QDA
# Assumption: sample are normally distributed BUT NOT SAME variance among classes.
############################################

qda.fit <- qda(stroke~age+bmi+avg_glucose_level+hypertension+heart_disease+smoking_status, data = stroke_data)
# ERROR rank deficiency, i.e. some variables 
# are collinear and one or more covariance matrices cannot be inverted to obtain the estimates in group 1 (Controls)!
qda.pred <- predict(qda.fit, stroke_data)
qda.pred.stroke <- qda.pred$posterior[, 2]

table(qda.pred$class, stroke)

# 7.
###########################################
#             ROC and RECALL-PRECISION CURVES
############################################


# AIC
############################
# AIC ? piu adeguato ad una realta sconosciuta e a dimensionalita elevata
AIC(mod.red)
AIC(mod1)

#con AIC ----> vince mod2 a conferma che AIC e mallow sono scelgono lo stesso modello

#AIC e BIC non ho utilizzato le formule del codice del professore 
#perche' in quel caso (dataset Credit) gli errori seguivano un andamento gaussiano

#quindi non ho potuto usare log(RSS/n) come valore della funzione di log-verosomiglianza
#ho usato direttamente la funzione di R

#!!! WARNING: QUI C'E' UN PROBLEMA
#R^2 VIENE NEGATIVO DI CONSEGUENZA ANCHE R^2adjusted!
#OVVIAMENTE NON VA BENE PERCHE' IL COEFF. DI DETERMINAZIONE R^2
#DEVE ESSERE COMPRESO TRA 0 E 1 
#INFATTI NEI MODELLI REGRESSIONE LOGISTICA/NON LINEARE NON E' POSSIBILE CALCOLARE L'R^2.
#SI UTILIZZA UN COSIDETTO "PSEUDO R QUADRO", TIPO R^2 DI COX O DI SNELL, CHE CON REOVERATO NON ABBIAMO FATTO
#PERCIO' DOBBIAMO LASCIARE PERDERE QUEST'ULTIMO METODO


################# TRAINING AND VALIDATION SETS ############
# sum(stroke_data$stroke==0) --> 4700
# sum(stroke_data$stroke==1) --> 209
# 75% train: 3682 - 25% test
# di questo 75% ne prendiamo 120 di stroke
# 3562 without stroke
no.strokes.data <- stroke_data[stroke == 0, ]
rnd.idx.no.strokes <- sample(c(1:dim(no.strokes.data)[1]))

yes.strokes.data <- stroke_data[stroke == 1, ]
rnd.idx.yes.strokes <- sample(c(1:dim(yes.strokes.data)[1]))

##TRAINING SET
training.set <- no.strokes.data[rnd.idx.no.strokes[1:3522], ]
training.set <- rbind(training.set, yes.strokes.data[rnd.idx.yes.strokes[1:160], ])
shuffle <- sample(nrow(training.set)) #shuffle the dataset, since the strokes are added in the last positions
training.set <- training.set[shuffle, ]
training.set
dim(training.set)

##VALIDATION SET
val.set <- no.strokes.data[rnd.idx.no.strokes[3523:4700], ]
val.set <- rbind(val.set, yes.strokes.data[rnd.idx.yes.strokes[161:209], ])
shuffle <- sample(nrow(val.set)) #shuffle the dataset, since the strokes are added in the last positions
val.set <- val.set[shuffle, ]

attach(training.set)

#mod.red.train
mod.red.train <- glm(stroke~age + heart_disease + avg_glucose_level+ hypertension, 
                     data=training.set, family = binomial)
mod.red.train.pred <- predict(mod.red.train, data=training.set, type="response")

mod1.train <- glm(stroke~age + avg_glucose_level+ heart_disease+ hypertension + 
                    age*heart_disease, data=training.set, family=binomial)
mod1.train.pred <- predict(mod1.train, data=training.set, type="response")

lda.fit.train <- lda(stroke~age+bmi+avg_glucose_level+hypertension+smoking_status+
                       Residence_type + heart_disease, data=training.set) 
lda.fit.train.pred <- predict(lda.fit.train, data=training.set)
lda.fit.train.pred <- lda.fit.train.pred$posterior[, 2]


qda.fit.train <- qda(stroke~age+bmi+avg_glucose_level+hypertension+heart_disease+
                       smoking_status, data = training.set)
qda.fit.train.pred <- predict(qda.fit.train, data=training.set)
qda.fit.train.pred<- qda.fit.train.pred$posterior[, 2]

# The interaction qda, which has the same variable of the logistic interaction model,
# improved a lot the result in the training set, but in the validation test 
# it does perform the same as the previous qda
qda.inter.fit.train <- qda(stroke~age + avg_glucose_level+ heart_disease+ hypertension + 
                             age*heart_disease, data = training.set)
qda.inter.fit.train.pred <- predict(qda.inter.fit.train, data=training.set)
qda.inter.fit.train.pred<- qda.inter.fit.train.pred$posterior[, 2]

###########

res = get.roc.recall.values(list(mod.red.train.pred, mod1.train.pred, lda.fit.train.pred, 
                                 qda.fit.train.pred, qda.inter.fit.train.pred), training.set$stroke,
                            yes_plot=FALSE)
print(res)
recall_thresholds = res$Thr.Prec.Rec 
roc_thresholds = res$Thr.ROC


#################### RECALL prediction for all models
mod.red.train.pred.class <- as.numeric(mod.red.train.pred >= roc_thresholds[1])
table(training.set$stroke,mod.red.train.pred.class, dnn=c('ground thruth','reduced model 
            predicted'))

mod.red.train.pred.class <- as.numeric(mod.red.train.pred >= recall_thresholds[1])
table(training.set$stroke, mod.red.train.pred.class, dnn=c('ground thruth','reduced model
            predicted'))

mod1.train.pred.class <- as.numeric(mod1.train.pred >= roc_thresholds[2])
table(training.set$stroke, mod1.train.pred.class, dnn=c('ground thruth','mod1 
            predicted'))

mod1.train.pred.class <- as.numeric(mod1.train.pred >= recall_thresholds[2])
table(training.set$stroke, mod1.train.pred.class, dnn=c('ground thruth','mod1 
            predicted'))

lda.fit.train.pred.class <- as.numeric(lda.fit.train.pred>=recall_thresholds[3])
table(training.set$stroke, lda.fit.train.pred.class, dnn=c('ground thruth','LDA 
            predicted'))

qda.fit.train.pred.class <- as.numeric(qda.fit.train.pred>= recall_thresholds[4])
table(training.set$stroke, qda.fit.train.pred.class, dnn=c('ground thruth','QDA 
            predicted'))

####VALIDATION ORA FUNZIONA OTTIMAMENTE PER MOD.RED E MOD1
####C'e da fare per lda e qda

mod.red.val <- predict(mod.red.train, val.set, type="response")
mod.red.val.class <- as.numeric(mod.red.val >= recall_thresholds[1])
table(val.set$stroke, mod.red.val.class,  dnn=c('ground thruth','reduced model 
            predicted'))

mod1.val <- predict(mod1.train, val.set, type="response")
mod1.val.class <- as.numeric(mod1.val >= recall_thresholds[2])
table(val.set$stroke, mod1.val.class, dnn=c('ground thruth','interact model 
            predicted'))

lda.val <- predict(lda.fit.train, val.set)
lda.val <- lda.val$posterior[, 2]
lda.val.class = as.numeric(lda.val >= recall_thresholds[3])
table(val.set$stroke, lda.val.class, dnn=c('ground thruth','LDA 
            predicted'))

qda.val <- predict(qda.fit.train, val.set)
qda.val <- qda.val$posterior[, 2]
qda.val.class <- as.numeric(qda.val >= recall_thresholds[4])
table(val.set$stroke, qda.val.class, dnn=c('ground thruth','QDA 
            predicted'))

# The interaction qda, which has the same variable of the logistic interaction model,
# improved a lot the result in the training set, but in the validation test 
# it does perform the same as the previous qda
qda.inter.val <- predict(qda.inter.fit.train, val.set)
qda.inter.val <- qda.inter.val$posterior[, 2]
qda.inter.val.class = as.numeric(qda.inter.val >= recall_thresholds[5])
table(val.set$stroke, qda.inter.val.class)


###########################################################################
###########################################################################


#######################################################
# Ridge & Lasso
#########################################################


X <- model.matrix(stroke~.,stroke_data)
X <- X[,-1]
y <- stroke_data$stroke
grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(X, y, alpha=0, lambda=grid)

predict(ridge.mod, s=50, type="coefficients")

cv.out <- cv.glmnet(X, y, alpha=0, nfolds=10, lambda=grid)
plot(cv.out)
#lambda.min is the value of ?? that gives minimum mean 
# cross-validated error, while lambda.1se is the value of ??
# that gives the most regularized model such that the cross-validated 
# error is within one standard error of the minimum.

coef(ridge.mod)

cv.out$lambda[1:10]
summary(cv.out$lambda)

cv.out$cvm[1:10]
cv.out$cvsd[1:10]

i.bestlam <- which.min(cv.out$cvm)
i.bestlam 
bestlam <- cv.out$lambda[i.bestlam]
bestlam
cv.out$cvm[i.bestlam]

#bestlam <- cv.out$lambda.min
#bestlam

########## Lasso ################
# Lasso yields to sparse models -that is models that involve 
# only a subset of of the variables.
cv.out.l <- cv.glmnet(X, y, alpha=1, nfolds=10, lambda=grid)
plot(cv.out.l)

lasso.mod <- glmnet(X, y, alpha=1, lambda=grid)
coef(lasso.mod)

cv.out.l$lambda[1:10]
summary(cv.out.l$lambda)

cv.out.l$cvm[1:10]
cv.out.l$cvsd[1:10]

i.bestlam <- which.min(cv.out.l$cvm)
i.bestlam 
bestlam <- cv.out.l$lambda[i.bestlam]
bestlam
