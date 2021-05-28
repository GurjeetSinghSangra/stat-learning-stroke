# dataset
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

# modifiche a bmi
stroke_data$bmi <- as.numeric(stroke_data$bmi)# A warning has to be shown for N/A values which has to be map in NAs values. In case this warning is not shown, update R version to 4.__
stroke_data<- na.omit(stroke_data)

summary(stroke_data)

###
### Functions
#########################

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

##################################
attach(stroke_data)
##################################

#### Dataset biased 
par(mfrow=c(1, 1))

table(stroke)
table(stroke)/dim(stroke_data)[1]
barplot(table(stroke)/dim(stroke_data)[1],
        xlab='probability to have a stroke')

############
## 2. Descriptive Statistic
############

# box plots
###########
boxplot(avg_glucose_level~stroke, xlab= 'stroke', 
        ylab = 'average glucose level')
boxplot(bmi~stroke, xlab = 'stroke', ylab = 'bmi')
boxplot(age~stroke, xlab='stroke' ,ylab = 'age')
#boxplot(heart_disease~stroke, xlab='stroke' ,ylab = 'heart_disease')
boxplot(avg_glucose_level~heart_disease)

# pair plot y~ relevant feature
#################
par(mfrow=c(1,1))
#age
plot(stroke~age)
stroke.less.35 <- stroke_data[stroke_data$age<35, 'stroke']
table(stroke.less.35 <- stroke_data[stroke_data$age<35, 'stroke'])#/length(stroke.less.35)
table(stroke.less.35 <- stroke_data[stroke_data$age>=35 & stroke_data$age<50 , 'stroke'])
table(stroke.less.35 <- stroke_data[stroke_data$age>=50 , 'stroke'])

par(mfrow=c(2,1))
plot(stroke~bmi)
plot(stroke~avg_glucose_level)
par(mfrow=c(1,1))

# Scatter plot
################
#plot(avg_glucose_level,bmi, pch=16,col=as.factor(stroke))
library(ggplot2)
ggplot(stroke_data, aes(x = avg_glucose_level, y = bmi,
                        col = as.factor(stroke))) + geom_point()
ggplot(stroke_data, aes(x = avg_glucose_level, y = age,
                        col = as.factor(stroke))) + geom_point()
ggplot(stroke_data, aes(x = bmi, y = age,
                        col = as.factor(stroke))) + geom_point()
par(mfrow=c(1,1))

# 3. PAIR PLOTS WITH histogram and correlation matrix
#################################################

#pairs(stroke_data[, c(2, 3, 4, 8, 9, 10, 11)], diag.panel=panel.hist, upper.panel=panel.cor)
pairs(stroke_data, diag.panel=panel.hist, upper.panel=panel.cor)
# Result pairswise plot with matrix correlation: Strong collinearity among:
# age, worktype, and ever_married => When fitting they do not help and also are redundant dummy variable.

# 4. Residual plots and diagnostic for logistic GLMs
#################################################

# a.Full Model
#####
mod.full <- glm(stroke~., data=stroke_data, family = binomial)
summary(mod.full)
mod.full.resid <- residuals(mod.full, type="deviance") # because we have a binary response
predicted <- predict(mod.full, type = "link")

par(mfrow=c(1,2))
plot(mod.full.resid~predicted)
abline(h=0, col='red')
qqnorm(mod.full.resid)
qqline(mod.full.resid, col='red')

######################

par(mfrow=c(2,2))
plot(mod.full)
par(mfrow=c(1,1))

####################
# Basic feature selection using T statistics
##### 
# Reduced model 1 : We remove at least all the features that have collinearity between each other (work_type, ever_married)
# and the residence type
mod.red1 <- glm(stroke ~ age + bmi + avg_glucose_level + hypertension + smoking_status + gender + heart_disease, family=binomial) #
summary(mod.red1)
# Reduced model 2, Remove gender
mod.red2 <- glm(stroke ~ age + bmi + avg_glucose_level + hypertension + smoking_status  + heart_disease, family=binomial) #
summary(mod.red2)
# Final Reduced Model (with age, hypertension, avg_glucose_level, heart_disease )
#####
mod.red <- glm(stroke~age + heart_disease + avg_glucose_level+ hypertension, data=stroke_data, family = binomial)
summary(mod.red)

mod.red.resid <- residuals(mod.red, type="deviance")
predicted <- predict(mod.red, type = "link")

par(mfrow=c(1,2))
plot(mod.red.resid~predicted)
abline(h=0, col='red')
qqnorm(mod.red.resid)
qqline(mod.red.resid, col='red')

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

View(stroke_data[c("246","183","119"), ]) 
###################

#anova computation.
anova(mod.full, mod.red, test="Chisq")
# As expected from the anova test REJECTS that the complex model is more
# significant than the reduced one, since the p-value is not less than 5%.
# Hence the full model does not help with our prediction.
######################

# F-statistic to see variance
###################

var.test(age,avg_glucose_level) # low p-value -> relation
var.test(age, hypertension) # low p-value -> relation
var.test(hypertension,avg_glucose_level) # low p-value -> relation
var.test(age, heart_disease) # low p-value -> relation
var.test(avg_glucose_level,heart_disease) # low p-value, very high F -> relation?
var.test(age,bmi)

#### c. Mixed Model AND Interaction
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

View(stroke_data[c("207","150","100"), ])

## d. polynomial model
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
# Any polynomial term does not improve the result

# 5. LDA
###########################################
#                   LDA
# Assumption: samples are normally distributed and have same variance in every class => strong assumption.
############################################
library(MASS)
lda.fit <- lda(stroke~age+bmi+avg_glucose_level+hypertension+work_type+gender
               +smoking_status+ever_married+Residence_type + heart_disease)
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

# get the reduced model 

#mod1 <- glm(stroke~age + avg_glucose_level+ heart_disease+ hypertension +
#             age*heart_disease, family=binomial)
#mod3 <- glm(stroke~age + avg_glucose_level+ heart_disease+hypertension + 
#             heart_disease*hypertension, family=binomial)
mod.red <- glm(stroke~age + heart_disease + avg_glucose_level+ hypertension, data=stroke_data, family = binomial)
summary(mod.red)
mod.red.probs <- predict(mod.red,type="response")
# ROC curve
library(pROC)
library(ROCR) # Precision plot

################ SUPER MEGA FUNZIONE CHE CALCOLA TUTTO #################
get.roc.recall.values <- function(pred_models, true_value) {
  result <- data.frame(Thr.ROC=double(), Specificity=double(), Sensitivity=double(),
                       Thr.Prec.Rec=double(), Recall=double(), Precision=double())
  n_models = length(list(mod.red.probs,lda.pred.stroke, qda.pred.stroke))
  par(mfrow=c(n_models, 2))
  for (pred in pred_models) {
    ### ROC
    roc.res <- roc(true_value, pred, levels=c("0", "1"))
    plot(roc.res, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
    
    best.roc  <- coords(roc.res, "best")
    ###
    
    ### PREC-REC
    pred.rec = prediction(pred, true_value)
    perf = performance(pred.rec, "prec", "rec")
    plot(perf)
    pr_cutoffs <- data.frame(cutrecall=perf@alpha.values[[1]], recall=perf@x.values[[1]], 
                             precision=perf@y.values[[1]])
    pr_cutoffs <- pr_cutoffs[pr_cutoffs$recall>0 &  pr_cutoffs$precision >0, ]
    best_recall <- pr_cutoffs[which.min(pr_cutoffs$recall + pr_cutoffs$precision), ]
    
    result[nrow(result) + 1,] = c(best.roc[1, 1], best.roc[1, 2], best.roc[1, 3], 
                                  best_recall[1, 1], best_recall[1, 2], best_recall[1, 3])
  }
  par(mfrow=c(1, 1))
  return(result)
}

res = get.roc.recall.values(list(mod.red.probs,lda.pred.stroke, qda.pred.stroke), stroke)
print(res)
recall_thresholds = res$Thr.Prec.Rec # precision-recall
roc_thresholds = res$Thr.ROC

#################### RECALL prediction for all models
mod.red.probs.class = as.numeric(mod.red.probs >= recall_thresholds[1])
table(mod.red.probs.class, stroke)
mod.red.probs.class = as.numeric(mod.red.probs >= roc_thresholds[1])
table(mod.red.probs.class, stroke)

lda.stroke.class = as.numeric(lda.pred.stroke >= recall_thresholds[2])
table(lda.stroke.class, stroke)
lda.stroke.class = as.numeric(lda.pred.stroke >= roc_thresholds[2])
table(lda.stroke.class, stroke)

qda.stroke.class = as.numeric(qda.pred.stroke >= recall_thresholds[3])
table(qda.stroke.class, stroke)
qda.stroke.class = as.numeric(qda.pred.stroke >= roc_thresholds[3])
table(qda.stroke.class, stroke)

############################## MAYBE TO REMOVE EVERYTHING BELOW ########################
par(mfrow=c(1,1))
# levels=controls (0's) as first element and cases (1's) as second
roc.out <- roc(stroke, mod.red.probs, levels=c("0", "1"))
# plot
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
# suggested threshold
coords(roc.out, "best")

pred.recall = prediction(mod.red.probs, stroke)
perf = performance(pred.recall, "prec", "rec")
plot(perf)
par(mfrow=c(1,1))
pr_cutoffs <- data.frame(cut=perf@alpha.values[[1]], recall=perf@x.values[[1]], 
                      precision=perf@y.values[[1]])
pr_cutoffs[pr_cutoffs$recall>0.6, ]
best_recall = pr_cutoffs[which.min(pr_cutoffs$recall + pr_cutoffs$precision), ] #da controllare insieme

#RESOCONTO
#ho provato sia il modello ridotto sia alcuni dei modelli con interazione (vi ho lasciato i tre i migliori, 
# due sono commentati per fare le prove)
# AUC ~= 0.85, quindi direi che secondo la teoria il test e' moderatamente/abbastanza accurato 
# (potere discriminante del test) si ottiene di conseguenza un thresold molto piccolo circa 0.037

# nel caso dell'ictus ? meglio avere FP rather than FN -> FNR basso
# Allora ieri guardando i plot della ROC curve io e francesca c'eravamo posti due domande sui risultati.
# Siccome siamo in un problema medico di predizione di ictus di un paziente oppure no, alla fine la ROC curve non e` molto d'aiuto perche massimizza i true positive con i true predicted. 
# Questo significa che possibilmenete gli errori di false negativo possono incrementare.
# In ambito del nostro problema e in generale in ambito medico se il nostro modello predice una persona senza ictus quando invece lo presenta, eh  questa e` un errore piu grave rispetto a un false positive.
# Noi vorremmo invece che il false negative sia basso e quindi considerato. 
# Insomma significa che dobbiamo usare la Precision Recall curve.



###MODEL SELECTION 

##################################################
# Reduced model vx the best mixed model
###################################################

#1
# AIC
############################
# AIC ? pi? adeguato ad una realt? sconosciuta e a dimensionalit? elevata
AIC(mod.red)
AIC(mod1)

#2
# BIC
############################
# BIC viene utilizzato per modelli semplici e "veri"
BIC(mod.red)
BIC(mod1)
BIC(mod3)

#con AIC ----> vince mod2 a conferma che AIC e mallow sono scelgono lo stesso modello
#con BIC ----> vince mod.red


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


#######################################################
# Ridge & Lasso
#########################################################

library(glmnet)
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

##### best subset selection ########
library(leaps)
best.mod <- regsubsets(stroke~., stroke_data)
summary(best.mod)

# best model includes: age, avg_glucose_level, heart_disease, hypertension 
# as relevant variables

reg.summary <- summary(best.mod, nvar=5)
names(reg.summary)
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="number of variables", ylab= "RSS", type='l')
plot(reg.summary$adjr2, xlab = 'number of variables', ylab = 'Adjusted RSq', type = 'l')
par(mfrow=c(1,1))

# model with max adjr2 and identify it on the plot:
which.max(reg.summary$adjr2) 
points(8, reg.summary$adjr2[8], col='red', pch=20)

plot(best.mod, scale='r2')
plot(best.mod, scale='adjr2')
plot(best.mod, scale='Cp')
plot(best.mod, scale='bic')

coef(best.mod, 8)




################# TRAINING AND VALIDATION SETS ############
# sum(stroke_data$stroke==0) --> 4700
# sum(stroke_data$stroke==1) --> 209
# 75% train: 3682 - 25% test
# di questo 75% ne prendiamo 120 di stroke
# 3562 without stroke
no.strokes.data <- stroke_data[stroke == 0, ]
no.strokes.data
rnd.idx.no.strokes <- sample(c(1:dim(no.strokes.data)[1]))
rnd.idx.no.strokes

yes.strokes.data <- stroke_data[stroke == 1, ]
yes.strokes.data
rnd.idx.yes.strokes <- sample(c(1:dim(yes.strokes.data)[1]))
rnd.idx.yes.strokes

##TRAINING SET
training.set <- no.strokes.data[rnd.idx.no.strokes[1:3562], ]
training.set <- rbind(training.set, yes.strokes.data[rnd.idx.yes.strokes[1:120], ])
shuffle <- sample(nrow(training.set)) #shuffle the dataset, since the strokes are added in the last positions
training.set <- training.set[shuffle, ]
training.set
dim(training.set)

##VALIDATION SET
val.set <- no.strokes.data[rnd.idx.no.strokes[3563:4700], ]
val.set <- rbind(val.set, yes.strokes.data[rnd.idx.yes.strokes[121:209], ])
shuffle <- sample(nrow(val.set)) #shuffle the dataset, since the strokes are added in the last positions
val.set <- val.set[shuffle, ]
val.set
dim(val.set)

#attach(training.set)

#mod.red.train
mod.red.train <- glm(stroke~age + heart_disease + avg_glucose_level+ hypertension, 
                     data=training.set, family = binomial)
mod.red.train.pred <- predict(mod.red.train, data=training.set, type="response")
par(mfrow=c(2,2))
plot(mod.red.train)
par(mfrow=c(1,1))

mod1.train <- glm(stroke~age + avg_glucose_level+ heart_disease+ hypertension + 
                    age*heart_disease, data=training.set, family=binomial)
mod1.train.pred <- predict(mod1.train, data=training.set, type="response")
par(mfrow=c(2,2))
plot(mod1.train)
par(mfrow=c(1,1))

lda.fit.train <- lda(stroke~age+bmi+avg_glucose_level+hypertension+smoking_status+
                       Residence_type + heart_disease, data=training.set)  #se metto gender mi dice "varabile costante"
lda.fit.train.pred <- predict(lda.fit.train, data=training.set)
lda.fit.train.pred <- lda.fit.train.pred$posterior[, 2]
#plot(lda.fit.train)

qda.fit.train <- qda(stroke~age+bmi+avg_glucose_level+hypertension+heart_disease+
                       smoking_status, data = training.set)
qda.fit.train.pred <- predict(qda.fit.train, data=training.set)
qda.fit.train.pred<- qda.fit.train.pred$posterior[, 2]
#plot(qda.fit.train)


###########

res = get.roc.recall.values(list(mod.red.train.pred, mod1.train.pred, lda.fit.train.pred, 
                                 qda.fit.train.pred), training.set$stroke)
print(res)
recall_thresholds = res$Thr.Prec.Rec 
roc_thresholds = res$Thr.ROC


#################### RECALL prediction for all models
mod.red.train.pred.class = as.numeric(mod.red.train.pred >= roc_thresholds[1])
table(mod.red.train.pred.class,training.set$stroke) #bad
mod.red.train.pred.class = as.numeric(mod.red.train.pred >= recall_thresholds[1])
table(mod.red.train.pred.class, training.set$stroke)#good

mod1.train.pred.class = as.numeric(mod1.train.pred >= roc_thresholds[2])
table(mod1.train.pred.class,training.set$stroke)
mod1.train.pred.class = as.numeric(mod1.train.pred >= recall_thresholds[2])
table(mod1.train.pred.class, training.set$stroke)

lda.fit.train.pred.class = as.numeric(lda.fit.train.pred  >= roc_thresholds[3])
table(lda.fit.train.pred.class,training.set$stroke)
lda.fit.train.pred.class = as.numeric(lda.fit.train.pred  >= recall_thresholds[3])
table(lda.fit.train.pred.class, training.set$stroke)

qda.fit.train.pred.class = as.numeric(qda.fit.train.pred  >= roc_thresholds[4])
table(qda.fit.train.pred.class,training.set$stroke)
qda.fit.train.pred.class = as.numeric(qda.fit.train.pred  >= recall_thresholds[4])
table(qda.fit.train.pred.class, training.set$stroke)


####VALIDATION ORA FUNZIONA OTTIMAMENTE PER MOD.RED E MOD1
####C'è da fare per lda e qda

mod.red.val <- predict(mod.red.train,val.set, type="response")
#length(mod.red.val)
#print(length(mod.red.train.pred))
mod.red.val.class = as.numeric(mod.red.val >= recall_thresholds[1])
mod.red.val.class
table(mod.red.val.class, val.set$stroke)

mod1.val <- predict(mod1.train, val.set, type="response")
#print(length(mod1.val))
#print(length(mod1.train.pred))
mod1.val.class = as.numeric(mod1.val >= recall_thresholds[2])
table(mod1.val.class, val.set$stroke)





