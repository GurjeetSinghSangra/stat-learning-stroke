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

pairs(stroke_data, diag.panel=panel.hist, upper.panel=panel.cor)

full_mod <- glm(stroke~.,data = stroke_data, family = binomial)
summary(full_mod)

# residuals plots in R
par(mfrow=c(2,2))
plot(full_mod)
par(mfrow=c(1,1))

# how to detect Other gender
# stroke_data[stroke_data$gender=='Other',]

# box plots
boxplot(stroke_data$avg_glucose_level)
boxplot(stroke_data$bmi)
boxplot(stroke_data$age)

