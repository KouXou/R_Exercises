library(car)
library(leaps)

# Function to read Data
read_data <- function(file_path) {
  d <- read.table(file_path, header = TRUE, dec = ".",  row.names = 1) 
  return(d)
}

# Function to plot Data
plot_data <- function(in_data) {
  pairs(in_data)
  summary(in_data)
}

# 3.ii
print("3.ii")
ozone_data <- read_data('./ozone.txt')
head(ozone_data)

#3.iii
print("3.iii")
plot_data(ozone_data)

maxO3 <- ozone_data[,1]
T9 <- ozone_data[,2]
T12 <- ozone_data[,3]
T15 <- ozone_data[,4]
lm1=lm(maxO3~T9,data=ozone_data) 
#summary(lm1)
lm2=lm(maxO3~T12,data=ozone_data) 
#summary(lm2)
lm3=lm(maxO3~T15,data=ozone_data) 
#summary(lm3)
lm1
lm2
lm3

#scatterplotMatrix(ozone_data,var.labels = TRUE, #smoother.args=list(lty=2),
#main="Scatter Plot Matrix")


#3.iv
print("3.iv")

# Linear Regression Example
# fit <- lm(maxO3 ~ T9 + T12 + T15, data=ozone_data)
summary(lm3) # show results

#https://www.statmethods.net/stats/regression.html
# Other useful functions

# coefficients(fit) # model coefficients

#confint(fit, level=0.95) # CIs for model parameters
#fitted(fit) # predicted values

# residuals(fit) # residuals
# anova(fit) # anova table
# vcov(fit) # covariance matrix for model parameters
# influence(fit) # regression diagnostics
print(paste('Παράμετροι : ', summary(lm3)$coef[1:2]))
print(paste('Συντελεστής Προσδιορισμού : ', summary(lm3)$r.squared ))
print(paste('Προσαρμοσμένος Συντελεστής Προσδιορισμού : 0.5963 '))

#3.v
print("3.v")
# plot(lm1)
# plot(lm2)
plot(maxO3 ~ T15)
abline(lm3)

res<-residuals(lm3)
qqnorm(res)
qqline(res)

leaps<-regsubsets(maxO3 ~ T9 + T12 + T15 + Ne9 + Ne12 + Ne15 + Wx9 + Wx12 + Wx15 + maxO3y
                  , data=ozone_data, nbest=1)
plot(leaps, scale="adjr2")

     
#3.vi
print("3.vi")
new_data.maxO3 <- 70
new_data.T12 <- 19
new_data.Ne9 <- 8
new_data.Wx9 <- 2.05

newx<-matrix(c(new_data.T12, new_data.Ne9, new_data.Wx9, new_data.maxO3), nrow = 1)
colnames(newx)<- c('T12', 'Ne3', 'Wx9', 'maxO3n')
newdata<-data.frame(newx)
pr<-predict(lm2, newdata, interval = 'pred')

print(paste('Προβλεπόμενη τιμή: ', pr[1]))
