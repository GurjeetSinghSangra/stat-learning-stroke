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
boxplot(avg_glucose_level, xlab= 'average glucose level')
boxplot(bmi, xlab = 'body mass index')
boxplot(age, xlab = 'age')
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
mod.red1 <- glm(stroke ~ age + bmi + avg_glucose_level + hypertension + smoking_status + gender, family=binomial) #
summary(mod.red1)
# Reduced model 2, Remove gender
mod.red2 <- glm(stroke ~ age + bmi + avg_glucose_level + hypertension + smoking_status, family=binomial) #
summary(mod.red2)
# Final Reduced Model (with age, hypertension, avg_glucose_level, bmi which are the variables with the highest level of significance)
#####
mod.red <- glm(stroke~age + bmi + avg_glucose_level+ hypertension, data=stroke_data, family = binomial)
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

View(stroke_data[c("119","183","246"), ]) #very strange and rare case, but have high bmi value with low glucose. The 163 has age 1.32, why?
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

#### c. Mixed Model AND INTERACTIONS
####################
# we put a threshold for rejecting the variables on 0.1
mod1 <- glm(stroke~age, family=binomial)
summary(mod1)

mod2 <- glm(stroke~age+avg_glucose_level+age*avg_glucose_level, family = binomial)
summary(mod2)

# remove age*avg_glucose_level and avg_glucose_level
# add hypertension
mod3 <-glm(stroke ~ age + hypertension, family = binomial)
summary(mod3) 

# add hypertension*avg_glucose_level+hypertension*age
mod4 <- glm(stroke~age+hypertension+
              hypertension*avg_glucose_level+hypertension*age,
             family = binomial)
summary(mod4)

# remove hypertension*avg_glucose_level+hypertension*age 
# add heart_disease
mod5 <- glm(stroke~age + hypertension +heart_disease, family = binomial)
summary(mod5)

# add hypertension*age + heart_disease*avg_glucose_level+ heart_disease*hypertension
mod6 <- glm(stroke~age + hypertension +
              heart_disease + heart_disease*avg_glucose_level+
              heart_disease*hypertension+ heart_disease*age, family = binomial)
summary(mod6)

# remove heart_disease, heart_disease*hypertension + heart_disease*age
# add bmi
mod7 <- glm(stroke~age + hypertension + heart_disease*avg_glucose_level+ bmi, family = binomial)
summary(mod7)

# remove bmi, heart_disease*avg_glucose_level
# add bmi*age and bmi*hypertension (because most relevant variables)
mod8 <- glm(stroke~ age + hypertension +
              heart_disease*hypertension + heart_disease*age + bmi*age +
              bmi*hypertension, family = binomial)
summary(mod8)

# remove bmi*hypertension,  bmi*age, heart_disease*hypertension
mod9 <- glm(stroke~ age + avg_glucose_level + hypertension+ heart_disease*age, family = binomial)
summary(mod9)

# add smoking status
mod10 <- glm(stroke~ age + avg_glucose_level + hypertension+ heart_disease*age+
               smoking_status, family = binomial)
summary(mod10)

# remove smoking status
# add 
mod11 <- glm(stroke~ age + avg_glucose_level + hypertension+ heart_disease*age+
               Residence_type, family = binomial)
summary(mod11)


# add gender 
mod12 <- glm(stroke~ age + avg_glucose_level + hypertension+ heart_disease*age+
gender, family = binomial)
summary(mod12)

# add gender 
mod13 <- glm(stroke~ age + avg_glucose_level + hypertension+ heart_disease*age+
               ever_married, family = binomial)
summary(mod13)

par(mfrow=c(2,2))
plot(mod9)
par(mfrow=c(1,1))

## 3. polynomial model
##############
mod.red.poly1 <- glm(stroke~age + bmi + avg_glucose_level+ hypertension+
                       I(bmi^2), family = binomial)
summary(mod.red.poly1)

mod.red.poly2 <- glm(stroke~age + bmi + avg_glucose_level+ hypertension
                     + I(avg_glucose_level^2), family = binomial)
summary(mod.red.poly2)

mod.red.poly <- glm(stroke~age + bmi + avg_glucose_level+ hypertension+
                      I(bmi^2) + I(avg_glucose_level^2), family = binomial)
summary(mod.red.poly)
# Any polynomial term does not improve the result

# 5. LDA
###########################################
#                   LDA
# Assumption: samples are normally distributed and have same variance in every class => strong assumption.
############################################
library(MASS)
lda.fit <- lda(stroke~age+bmi+avg_glucose_level+hypertension+work_type+gender+smoking_status)
lda.pred <- predict(lda.fit)
table(lda.pred$class, stroke)

# 6.
###########################################
#                   QDA
# Assumption: sample are normally distributed BUT NOT SAME variance among classes.
############################################

qda.fit <- qda(stroke~age+bmi+avg_glucose_level+hypertension+heart_disease+smoking_status, data = stroke_data)
# ERROR rank deficiency, i.e. some variables 
# are collinear and one or more covariance matrices cannot be inverted to obtain the estimates in group 1 (Controls)!
qda.pred <- predict(qda.fit, stroke_data)
#TODO: Calculate deviance and standardize deviance

table(qda.pred$class, stroke)

# 7.
###########################################
#             ROC and RECALL-PRECISION CURVES
############################################

# get the reduced model 

#mod.red <- glm(stroke~avg_glucose_level+hypertension+age+hypertension*age + heart_disease*avg_glucose_level+heart_disease*hypertension + bmi*age+bmi*avg_glucose_level, family = binomial  )
#mod.red <- glm(stroke~age+avg_glucose_level +age*avg_glucose_level+hypertension+heart_disease+hypertension*age + heart_disease*avg_glucose_level+heart_disease*hypertension, data=stroke_data, family = binomial)
mod.red <- glm(stroke~age + avg_glucose_level + hypertension + bmi, data=stroke_data, family = binomial)
summary(mod.red)
mod.red.probs <- predict(mod.red,type="response")
# check the coding of contrasts
contrasts(stroke)
# review of the proportion between 1 and 0
table(stroke)/length(stroke)

# ROC curve
library(pROC)
library(ROCR) # Precision plot

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
best_recall = pr_cutoffs[which.min(pr_cutoffs$recall + pr_cutoffs$precision), "cut"] #da controllare insieme

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

get.roc.recall.values <- function(pred_models, true_value) {
  result <- vector(mode = "list", length = length(pred_models))
  for (pred in pred_model) {
    roc.res <- roc(true_value, pred, levels=c("0", "1"))
    tmp.res  <- c(result, coords(roc.res, "best"))
    pred.rec = prediction(mod.red.probs, stroke)
    perf = performance(pred.rec, "prec", "rec")
    pr_cutoffs <- data.frame(cutrecall=perf@alpha.values[[1]], recall=perf@x.values[[1]], 
                             precision=perf@y.values[[1]])
    best_recall <- pr_cutoffs[which.min(pr_cutoffs$recall + pr_cutoffs$precision), ]
    data.frame(best_recall)
  }
}
 