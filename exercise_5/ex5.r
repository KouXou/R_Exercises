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

#5.B
babies_file_path<- './babies.data.txt'
babies_data <- read.table(babies_file_path, header = TRUE,  sep = " ", dec = ".") 
head(babies_data)

input <- babies_data[,c("gestation", "not.first.born", "mom.age", "mom.height", "mom.weight", "mom.smokes")]
birth.weight <- babies_data[,c("birth.weight")]
model = lm(birth.weight~gestation+not.first.born+mom.age+mom.height+mom.weight+mom.smokes, data = input)
# Show the model.
summary(model)

# Get the Intercept and coefficients as vector elements.
cat("# # # # The Coefficient Values # # # ","\n")

a <- coef(model)[1]
print(a)

library(corrplot)
res <-cor(babies_data)
corrplot(res, order = "hclust", 
         tl.col = "black", tl.srt = 45)


scatterplotMatrix(input,var.labels = TRUE, #smoother.args=list(lty=2),
main="Scatter Plot Matrix")


