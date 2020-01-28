library('dplyr')
library('foreign')
library('caret')
library('klaR')
library('fpc')
library('reshape')

# 6.1 - IONOSHERE DATA SET
file_path<-'/home/kostas/Documents/Msc/Lessons 1st/Data Mining/Ergasia Filip/mf_lab/ionoshere.txt'
data <- read.arff(file=file_path)

df<- as.data.frame(data)
head(df)
dim(df)

k<-9
# mutate(dat, Ctr_abbr = ifelse(Country == "Canada", "CAN", "USA"))
resdf<-mutate(df,class= ifelse(class == 'g', 1, 0))

# define training control
train_control <- trainControl(method="cv", number=10)
# train_control
# resdf

# train the model
# model <- train(class~., data=df,  method="lm",trControl=train_control)
# summarize results
# print(model)

# Train the model
model <- train(class ~., data = df, method = "regLogistic",
               trControl = train_control)
# Summarize the results
print(model)

# train the model
# model <- train(class ~., data = resdf, trControl=train_control, method="nb")

# iris

# 6.2 - VOWEL DATA SET
file_path_vowel<-'/home/kostas/Documents/Msc/Lessons 1st/Data Mining/Ergasia Filip/mf_lab/vowel.txt'
vowel <- read.arff(file=file_path_vowel)
head(vowel)

# 6.2.1
colnames(vowel)
names(vowel)[names(vowel) == "Train or Test"] <- "Train_or_Test"
names(vowel)[names(vowel) == "Speaker Number"] <- "Speaker_Number"
# vowel
vowel_s<-dplyr::select(vowel,-(Train_or_Test:Sex))
head(vowel_s) 
# vowel_s

# 6.2.2
kvalues<-c(1,2,3,4,5,6,7,8,9,10,11,12)

kvalues

for (k in 1:12){
    print(kmeans(vowel_s[,1:10], k, nstart=100, iter.max=100))
}

#  kmeansruns to pick the best k
clustering.ch <- kmeansruns(vowel_s[,1:10], krange=1:12, criterion="ch") # Calinski-Harabasz Index 
clustering.ch

clustering.asw <- kmeansruns(vowel_s[,1:10], krange=1:12, criterion="asw") # average silhouette width
clustering.asw

critframe <- data.frame(k=1:12, ch=scale(clustering.ch$crit),asw=scale(clustering.asw$crit))
critframe <- melt(critframe, id.vars=c("k"),variable.name="measure",value.name="score")
critframe
ggplot(critframe, aes(x=k, y=value, color=variable)) +
geom_point(aes(shape=variable)) + geom_line(aes(linetype=variable)) +
scale_x_continuous(breaks=1:12, labels=1:12)
summary(clustering.ch)

head(vowel_s)

# 6.2.3
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
head(vowel_s)

# vowel_b<-mutate(vowel_b,Sex= ifelse(Sex == 'Male', 0, 1))
for (k in 1:12){
#     print('YOLO')
    print(kmeans(vowel_s[,1:11], k, nstart=100, iter.max=100))
# #     pclusters <- kmeans(vowel_s[,], k, nstart=100, iter.max=100)
# #     pclusters
}


#  kmeansruns to pick the best k
clustering.ch <- kmeansruns(vowel_s[,1:11], krange=1:12, criterion="ch") # Calinski-Harabasz Index 
clustering.ch

clustering.asw <- kmeansruns(vowel_s[,1:11], krange=1:12, criterion="asw") # average silhouette width
clustering.asw

critframe <- data.frame(k=1:12, ch=scale(clustering.ch$crit),asw=scale(clustering.asw$crit))
critframe <- melt(critframe, id.vars=c("k"),variable.name="measure",value.name="score")
critframe
ggplot(critframe, aes(x=k, y=value, color=variable)) +
geom_point(aes(shape=variable)) + geom_line(aes(linetype=variable)) +
scale_x_continuous(breaks=1:12, labels=1:12)
summary(clustering.ch)


