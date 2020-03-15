library(ggplot2)
library(car)

# Function to read Data
read_data <- function(file_path) {
  d <- read.table(file_path, header = TRUE, dec = ".",  row.names = 1) 
  return(d)
}

#5.A
data(iris)
head(iris)
iris_order_by_sepal_width<-iris[order(iris$Sepal.Width),]
hist(iris_order_by_sepal_width$Sepal.Width, main="Histogram of Sepal Width",
     xlab="Sepal Width", col="darkorchid", freq=FALSE)

iris_order_by_sepal_length<-iris[order(iris$Sepal.Length),]
hist(iris_order_by_sepal_length$Sepal.Length, main="Histogram of Sepal Length",
     xlab="Sepal Length",  col="red", freq=FALSE)
head(iris_order_by_sepal_length)
iris_order_by_petal_width<-iris[order(iris$Petal.Width),]
hist(iris_order_by_petal_width$Petal.Width, main="Histogram of Petal Width",
     xlab="Petal Width",  col="darkorchid", freq=FALSE)

iris_order_by_petal_length<-iris[order(iris$Petal.Length),]
hist(iris_order_by_petal_length$Petal.Length, main="Histogram of Petal Length",
     xlab="Petal Length", col="darkorchid", freq=FALSE)

plot(iris$Petal.Width, iris$Petal.height)

#5.A 
# I
ggplot(iris_order_by_sepal_width, aes(x=Petal.Length, y= Petal.Width,fill=Species, color=Species)) +
#   geom_histogram(position="identity", alpha=0.5) +
geom_point(position="identity")

# ggplot(iris, aes(x=Sepal.Length,fill=Species, color=Species)) + 
# geom_point(position="identity", color="blue", alpha=.5)

#   geom_histogram(position="identity", alpha=0.5)
ggplot(iris_order_by_sepal_width, aes(x=Petal.Length, y= Petal.Width,fill=Species, color=Species)) +
geom_line()

#  II
op <- par(bg = "grey")
matplot(iris$Sepal.Length, iris[,-c(1,5)], pch = "123",xlab = "Sepal Length",
        ylab = "Sepal Width + Petal Length + Petal Width",col= rainbow(4),
       main = "Scatter Plot: Sepal Length ~ Sepal Width + Petal Length + Petal Width")
legend("topleft",1, 95, legend =c("Sepal Width", "Petal Length","Petal Width"),pch = "123",
        col = rainbow(4))
par(op)

#5.B
babies_file_path<- './babies.data.txt'
babies_data <- read.table(babies_file_path, header = TRUE,  sep = " ", dec = ".") 
head(babies_data)

# scatterplotMatrix()
scatterplotMatrix(formula = ~birth.weight+gestation+not.first.born+mom.age+mom.height+mom.weight+mom.smokes,
                 data = babies_data)

input <- babies_data[,c("gestation", "not.first.born", "mom.age", "mom.height", "mom.weight", "mom.smokes")]
birth.weight <- babies_data[,c("birth.weight")]
model = lm(birth.weight~gestation+not.first.born+mom.age+mom.height+mom.weight+mom.smokes, data = input)
# Show the model.
summary(model)

a <- coef(model)
print('Παράμετροι')
print(a)


library(corrplot)
cor(babies_data, birth.weight)
res <-cor(babies_data)
corrplot(res, order = "hclust", 
         tl.col = "black", tl.srt = 45)
print('O gestation είναι ο βασικός παράγοντας που επιδρά στο birth.weight')
