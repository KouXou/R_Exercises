library(ggplot2)
library(car)
library(leaps)
library(corrplot)

# Function to read Data
read_data <- function(file_path) {
  d <- read.table(file_path, header = TRUE,  sep = " ", dec = ".") 
  return(d)
}

data(iris)

# Order By Petal.Width
iris_order_by_petal_width<-iris[order(iris$Petal.Width),]
# Order by Petal.Length
iris_order_by_petal_length<-iris[order(iris$Petal.Length),]

# iris_order_by_sepal_width<-iris[order(iris$Sepal.Width),]
# iris_order_by_sepal_length<-iris[order(iris$Sepal.Length),]

ggplot(iris_order_by_petal_width, aes(x=Petal.Width,fill=Species, color=Species)) + 
geom_histogram(position="identity", alpha=0.5, binwidth = 0.1)


ggplot(iris_order_by_petal_length, aes(x=Petal.Length,fill=Species, color=Species)) + 
geom_histogram(position="identity", alpha=0.5, binwidth = 0.1)

op <- par(bg = "grey")
matplot(iris$Sepal.Length, iris[,-c(1,5)], pch = "123",xlab = "Sepal Length",
        ylab = "Sepal Width + Petal Length + Petal Width",col= rainbow(4),
       main = "Scatter Plot: Sepal Length ~ Sepal Width + Petal Length + Petal Width")
legend("topleft",1, 95, legend =c("Sepal Width", "Petal Length","Petal Width"),pch = "123",
        col = rainbow(4))
par(op)

babies_file_path<- './babies.data.txt'
babies_data <- read_data(babies_file_path) 
babies_data <- babies_data[, -c(3,5,6)]
head(babies_data[])

scatterplotMatrix(formula = ~birth.weight+gestation+mom.age+mom.smokes,
                  data = babies_data,main="Scatter Plot Matrix",
                  regLine=list(method=lm, lty=1, lwd=2, col='red'))

leaps <-regsubsets(birth.weight ~ gestation+mom.age+mom.smokes, data=babies_data, nbest=4)
plot(leaps, scale="adjr2")

input <- babies_data[,c("gestation", "mom.age", "mom.smokes")]

birth.weight <- babies_data[,c("birth.weight")]
model <- lm(birth.weight~gestation+mom.age+mom.smokes, data = input)

summary(model)

a <- coef(model)
print('Παράμετροι')
print(a)


cor(babies_data, birth.weight)
res <-cor(babies_data)
corrplot(res, order = "hclust", 
         tl.col = "black", tl.srt = 45)
print('O gestation είναι ο βασικός παράγοντας που επιδρά στο birth.weight')
