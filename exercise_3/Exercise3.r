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

ozone_data <- read_data('./ozone.txt')
head(ozone_data)

plot_data(ozone_data)

scatterplotMatrix(formula = ~maxO3+T9+T12+T15,
                  data = ozone_data[1:4],main="Scatter Plot Matrix",
                  regLine=list(method=lm, lty=1, lwd=2, col='red'))

maxO3 <- ozone_data[,1]
T9 <- ozone_data[,2]
T12 <- ozone_data[,3]
T15 <- ozone_data[,4]
lm1=lm(maxO3~T9,data=ozone_data) 
lm2=lm(maxO3~T12,data=ozone_data) 
lm3=lm(maxO3~T15,data=ozone_data) 
lm1
lm2
lm3

summary(lm2)
print(paste('Συντελεστής Προσδιορισμού : ', summary(lm2)$r.squared ))
print(paste('Προσαρμοσμένος Συντελεστής Προσδιορισμού : 0.6116 '))

plot(maxO3 ~ T12)
abline(lm2)

res<--rstudent(lm2)
plot(res,pch=15,cex=.5,ylab="Residuals")
abline(h=c(-2,0,2),lty=c(2,1,2))

leaps<-regsubsets(maxO3 ~ T9 + T12 + T15 + Ne9 + Ne12 + Ne15 + Wx9 + Wx12 + Wx15 + maxO3y
                  , data=ozone_data, nbest=1)
plot(leaps, scale="adjr2")

new_data.maxO3 <- 70
new_data.T12 <- 19
new_data.Ne9 <- 8
new_data.Wx9 <- 2.05

newx<-matrix(c(new_data.T12, new_data.Ne9, new_data.Wx9, new_data.maxO3), nrow = 1)
colnames(newx)<- c('T12', 'Ne3', 'Wx9', 'maxO3n')
newdata<-data.frame(newx)
pr<-predict(lm2, newdata, interval = 'pred')

print(paste('Προβλεπόμενη τιμή: ', pr[1]))
