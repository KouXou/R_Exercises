#5.A
data(iris)

sepal_width =iris$Sepal.Width
hist(sepal_width, main="Histogram of Sepal Width", xlab="Sepal Width", col="darkorchid", freq=FALSE)

sepal_length<-iris$Sepal.Length
hist(sepal_length, main="Histogram of Sepal Length", xlab="Sepal Length",  col="darkorchid", freq=FALSE)

petal_width<-iris$Petal.Width
hist(petal_width, main="Histogram of Petal Width", xlab="Petal Width",  col="darkorchid", freq=FALSE)

petal_length<-iris$Petal.Length
hist(petal_length, main="Histogram of Petal Length", xlab="Petal Length", col="darkorchid", freq=FALSE)

#5.B
babies_data <- read.table('./babies.data.txt', header = TRUE,  sep = " ", dec = ".") 
input <- babies_data[,c("gestation", "not.first.born", "mom.age", "mom.height", "mom.weight", "mom.smokes")]
birth.weight <- babies_data[,c("birth.weight")]
model = lm(birth.weight~gestation+not.first.born+mom.age+mom.height+mom.weight+mom.smokes, data = input)
# Show the model.
print(model)

# Get the Intercept and coefficients as vector elements.
cat("# # # # The Coefficient Values # # # ","\n")

a <- coef(model)[1]
print(a)

library(corrplot)
res <-cor(babies_data)
corrplot(res, order = "hclust", 
         tl.col = "black", tl.srt = 45)

