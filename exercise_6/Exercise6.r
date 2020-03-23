library('dplyr')
library('foreign')
library('caret')
library('klaR')
library('fpc')
library('reshape')

file_path<-'./ionoshere.txt'
data <- read.arff(file=file_path)

df<- as.data.frame(data)
print(paste('Το σύνολο των γραμμών του πίνακα που διαβάσαμε είναι : ', dim(df)[1]))
print(paste('Το σύνολο των στηλών του πίνακα που διαβάσαμε είναι : ', dim(df)[2]))

k_fold <- function(k) {
    
    # define training control
    train_control <- trainControl(method="cv", number=k)
    
    # Train the model
    model <- train(class ~., data = df, method = "naive_bayes",
               trControl = train_control)
    
    print(model)
    print('----------------------------------------------------')
}

resdf<-mutate(df,class= ifelse(class == 'g', 1, 0))

for( k in 2:9){
  k_fold (k)
}

file_path_vowel<-'./vowel.txt'
vowel <- read.arff(file=file_path_vowel)

names(vowel)[names(vowel) == "Train or Test"] <- "Train_or_Test"
names(vowel)[names(vowel) == "Speaker Number"] <- "Speaker_Number"
vowel_s<-dplyr::select(vowel,-(Train_or_Test:Sex))

clustering.ch <- kmeansruns(vowel_s[,1:10], krange=1:12, criterion="ch") # Calinski-Harabasz Index 
clustering.ch

clustering.asw <- kmeansruns(vowel_s[,1:10], krange=1:12, criterion="asw") # average silhouette width
clustering.asw

critframe <- data.frame(k=1:12, ch=scale(clustering.ch$crit),asw=scale(clustering.asw$crit))
critframe <- melt(critframe, id.vars=c("k"),variable.name="measure",value.name="score")

ggplot(critframe, aes(x=k, y=value, color=variable)) +
geom_point(aes(shape=variable)) + geom_line(aes(linetype=variable)) +
scale_x_continuous(breaks=1:12, labels=1:12)
summary(clustering.ch)

class_to_number <- function(name) {
  class_nbr1 <- 
    case_when(
        name == 'hid' ~ 0,
        name == 'hId' ~ 1,
        name == 'hEd' ~ 2,
        name == 'hAd' ~ 3,
        name == 'hYd' ~ 4,
        name == 'had' ~ 5,
        name == 'hOd' ~ 6,
        name == 'hod' ~ 7,
        name == 'hUd' ~ 8,
        name == 'hud' ~ 9,
        name == 'hed' ~ 10
    )
  return(class_nbr1)
}


vowel_s<-mutate(vowel_s,Class= class_to_number(Class))

clustering.ch <- kmeansruns(vowel_s[,1:11], krange=1:12, criterion="ch") # Calinski-Harabasz Index 
clustering.ch

clustering.asw <- kmeansruns(vowel_s[,1:11], krange=1:12, criterion="asw") # average silhouette width
clustering.asw

critframe <- data.frame(k=1:12, ch=scale(clustering.ch$crit),asw=scale(clustering.asw$crit))
critframe <- melt(critframe, id.vars=c("k"),variable.name="measure",value.name="score")

ggplot(critframe, aes(x=k, y=value, color=variable)) +
geom_point(aes(shape=variable)) + geom_line(aes(linetype=variable)) +
scale_x_continuous(breaks=1:12, labels=1:12)
summary(clustering.ch)
