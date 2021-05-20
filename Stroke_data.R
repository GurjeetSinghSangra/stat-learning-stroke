# dataset
stroke_data <- read.csv('healthcare-dataset-stroke-data.csv')
View(stroke_data)
names(stroke_data)
summary(stroke_data)
sum(is.na(stroke_data))

# trasformazione variabili categoriche
stroke_data<-stroke_data[,-1]
stroke_data$gender<- as.factor(stroke_data$gender)
stroke_data$ever_married<-as.factor(stroke_data$ever_married)
stroke_data$work_type<-as.factor(stroke_data$work_type)
stroke_data$Residence_type<-as.factor(stroke_data$Residence_type)
stroke_data$smoking_status<-as.factor(stroke_data$smoking_status)

# modifiche a bmi
stroke_data$bmi <- as.numeric(stroke_data$bmi)
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

barplot(table(stroke_data$stroke)/dim(stroke_data)[1],
        xlab='probability to have a stroke')
############
# box plots
boxplot(avg_glucose_level, xlab= 'average glucose level')
boxplot(bmi, xlab = 'body mass index')
boxplot(age, xlab = 'age')
##################################
################################# Scatter plot ###################
#plot(avg_glucose_level,bmi, pch=16,col=as.factor(stroke))
ggplot(stroke_data, aes(x = avg_glucose_level, y = bmi,
                        col = as.factor(stroke))) + geom_point()
ggplot(stroke_data, aes(x = avg_glucose_level, y = age,
                        col = as.factor(stroke))) + geom_point()
ggplot(stroke_data, aes(x = bmi, y = age,
                        col = as.factor(stroke))) + geom_point()
par(mfrow=c(1,1))
#boxplot(age~avg_glucose_level)
############################## pair plot y~age
par(mfrow=c(1,1))
plot(stroke~age)
par(mfrow=c(2,1))
plot(stroke~bmi)
plot(stroke~avg_glucose_level)
par(mfrow=c(1,1))
##################################
# PAIR PLOTS WITH histogram and correlation matrix
pairs(stroke_data, diag.panel=panel.hist, upper.panel=panel.cor)
pairs(stroke_data[, c(2, 3, 4, 8, 9, 10, 11)], diag.panel=panel.hist, upper.panel=panel.cor)

# RESIDUAL PLOTS AND DIAGNOSTICS FOR LOGISTIC GLMs
#########################################
#full model
#####
mod.full <- glm(stroke_data$stroke~., data=stroke_data, family = binomial)
summary(mod.full)
mod.full.resid <- residuals(mod.full, type="deviance") # because we have a binary response
predicted <- predict(mod.full, type = "link")

par(mfrow=c(1,2))
plot(mod.full.resid~predicted)
abline(h=0)
qqnorm(mod.full.resid)
qqline(mod.full.resid)
######################
par(mfrow=c(2,2))
plot(mod.full)
par(mfrow=c(1,1))

##############################################
#reduced model with age, hypertension, avg_glucose_level which are the variables with the highest level of significance
#####
mod.red <- glm(stroke_data$stroke~stroke_data$age + stroke_data$avg_glucose_level+ stroke_data$hypertension, data=stroke_data, family = binomial)
summary(mod.red)

mod.red.resid <- residuals(mod.red, type="deviance") # because we have a binary response
predicted <- predict(mod.red, type = "link")

par(mfrow=c(1,2))
plot(mod.red.resid~predicted)
abline(h=0)
qqnorm(mod.red.resid)
qqline(mod.red.resid)
######################
par(mfrow=c(2,2))
plot(mod.red)
par(mfrow=c(1,1))

#anova computation
anova(mod.full,mod.red)


# how to detect Other gender
# stroke_data[stroke_data$gender=='Other',]

#############################
# PAIRWISE INTERACTION BETWEEN: avg_glucose, age, bmi
#############################

attach(stroke_data)
int.mod <- glm(stroke~bmi + age + avg_glucose_level+ hypertension + gender + smoking_status + work_type+
      heart_disease+ age*ever_married   , family = binomial)


int.mod <- glm(stroke~bmi + age + avg_glucose_level+ hypertension + gender + smoking_status +
      heart_disease + bmi * avg_glucose_level , family = binomial)

summary(int.mod)

red1 = glm(stroke~age, family=binomial)
summary(red1)
red2 = glm(stroke~age + smoking_status + age*smoking_status, family = binomial)
summary(red2)
red3 = glm(stroke~bmi+age + smoking_status + age*smoking_status, family=binomial)
summary(red3)

###########################################
#                   LDA
#       Assumption: sample normally distributed and same variances.
############################################
lda.fit <- lda(stroke~age+bmi+avg_glucose_level+hypertension+work_type+gender+smoking_status)
lda.pred <- predict(lda.fit)
table(lda.pred$class, stroke)

###########################################
#                   QDA
#       Assumption: sample normally distributed and BOT NOT SAME variance among samples.
############################################
qda.fit <- qda(stroke~age+bmi+avg_glucose_level+hypertension+heart_disease+smoking_status, data = stroke_data)
# ERROR rank deficiency, i.e. some variables 
# are collinear and one or more covariance matrices cannot be inverted to obtain the estimates in group 1 (Controls)!
qda.pred <- predict(qda.fit, stroke_data)
table(qda.pred$class, stroke)

###################
# F-statistic to see correlations
###################
var.test(age,avg_glucose_level) # low p-value -> relation
var.test(age, hypertension) # low p-value -> relation
var.test(hypertension,avg_glucose_level) # low p-value -> relation
var.test(age, heart_disease) # low p-value -> relation
var.test(avg_glucose_level,heart_disease) # low p-value, very high F -> relation?

#### forward model
####################
mod1 <- glm(stroke~age, family=binomial)
summary(mod1)

mod2 <- glm(stroke~age+avg_glucose_level+age*avg_glucose_level, family = binomial)
summary(mod2) # -> age no correlated

mod3 <-glm(stroke~avg_glucose_level+age*avg_glucose_level+hypertension,
            family = binomial)
summary(mod3) 

mod4 <- glm(stroke~avg_glucose_level+age*avg_glucose_level+hypertension+
              hypertension*avg_glucose_level+hypertension*age,
             family = binomial)
summary(mod4)

mod5 <- glm(stroke~avg_glucose_level+age*avg_glucose_level+hypertension+
            heart_disease+hypertension*age, family = binomial)
summary(mod5)

mod6 <- glm(stroke~avg_glucose_level+age*avg_glucose_level+hypertension+
              heart_disease+hypertension*age + heart_disease*avg_glucose_level+
              heart_disease*hypertension, family = binomial)
summary(mod6)

mod7 <- glm(stroke~avg_glucose_level+age*avg_glucose_level+hypertension+
              hypertension*age + heart_disease*avg_glucose_level+
              heart_disease*hypertension)
summary(mod7)

