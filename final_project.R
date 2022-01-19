## ---- warning=FALSE,message=FALSE,error=FALSE, results='hide'----------------------------------------------------------
library(funModeling) 
library(tidyverse) 
library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)
library(Hmisc)
library(missForest)
library(mice)
library(ROSE)
library(cluster)
library(ClusterR)
library(caTools)
library(caret)
library(knitr)
library(Amelia)
library(rpart)
library(ggcorrplot)
library(factoextra)
library(class)


## ---- warning=FALSE----------------------------------------------------------------------------------------------------
ratings <- fread("BookDataset/ratings.csv", sep = ";")
head(ratings, n=5)


## ----warning=FALSE-----------------------------------------------------------------------------------------------------
users <- fread("BookDataset/users.csv", sep = ";")
head(users, n=5)


## ---- warning=FALSE, message=FALSE, error=FALSE------------------------------------------------------------------------
books <- fread("BookDataset/books.csv", sep = ";")
head(books, n=5)


## ----------------------------------------------------------------------------------------------------------------------
n_distinct(ratings$`User-ID`)
n_distinct(books$ISBN)


## ----------------------------------------------------------------------------------------------------------------------
dataset = merge(ratings, users, by.x = "User-ID", by.y = "User-ID")


## ----------------------------------------------------------------------------------------------------------------------
dataset$Country <- sub('.*,\\s*','', dataset$Location)
dataset <- dataset[(which(nchar(dataset$Country) >= 2)),]
head(dataset, n=5)


## ----------------------------------------------------------------------------------------------------------------------
dataset <- merge(dataset, books, by.x = "ISBN", by.y = "ISBN")
head(dataset, n=5)


## ----------------------------------------------------------------------------------------------------------------------
colnames(dataset)


## ----------------------------------------------------------------------------------------------------------------------
colnames(dataset)[which(colnames(dataset) %in%  c("User-ID", "Book-Rating", "Book-Title", "Book-Author", "Year-Of-Publication", "Image-URL-S", "Image-URL-M", "Image-URL-L"))] <- c("User.ID", "Book.Rating", "Book.Title", "Book.Author", "Year.Of.Publication", "Image.URL.S", "Image.URL.M", "Image.URL.L")
colnames(dataset)


## ----------------------------------------------------------------------------------------------------------------------
sapply(dataset, class)


## ---- warning=FALSE----------------------------------------------------------------------------------------------------
dataset <- transform(dataset, Age = as.numeric(Age))
head(dataset, n=5)


## ----------------------------------------------------------------------------------------------------------------------
sum(duplicated(dataset))


## ----------------------------------------------------------------------------------------------------------------------
dataset$User.ID <- as.factor(dataset$User.ID)
dataset$ISBN <- as.factor(dataset$ISBN)
summary(dataset)


## ----------------------------------------------------------------------------------------------------------------------
rating.count.users <- dataset %>% count(User.ID)
head(rating.count.users, n=5)


## ----------------------------------------------------------------------------------------------------------------------
summary(rating.count.users)


## ----------------------------------------------------------------------------------------------------------------------
dataset %>%
  group_by(Book.Rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(Book.Rating, cases)) + geom_col(color="gray") +
  theme_minimal() + scale_x_continuous(breaks = 0:10)


## ----------------------------------------------------------------------------------------------------------------------
dataset <- dataset[dataset$Book.Rating!= 0, ]


## ----------------------------------------------------------------------------------------------------------------------
dataset %>%
  group_by(Book.Rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(Book.Rating, cases)) + geom_col(fill="orange") +
  theme_minimal() + scale_x_continuous(breaks = 0:10)


## ----------------------------------------------------------------------------------------------------------------------
books.rating.mean <- aggregate(Book.Rating ~ ISBN, dataset, mean)
head(books.rating.mean, n=5)


## ----------------------------------------------------------------------------------------------------------------------
summary(books.rating.mean)


## ----------------------------------------------------------------------------------------------------------------------
nrow(books.rating.mean)
sum(books.rating.mean$Book.Rating > 7.529)


## ----------------------------------------------------------------------------------------------------------------------
book.rating.mean <-  books.rating.mean[order(-books.rating.mean$Book.Rating),]


## ----------------------------------------------------------------------------------------------------------------------
books.rating.mean <- books.rating.mean[books.rating.mean$Book.Rating > 7.692,]


## ----------------------------------------------------------------------------------------------------------------------
dataset$Rating.Count.Above.Mean <- ifelse(dataset$ISBN %in% books.rating.mean$ISBN, "Yes", "No")


## ----------------------------------------------------------------------------------------------------------------------
nrow(dataset[dataset$Rating.Count.Above.Mean == "Yes",])
nrow(dataset[dataset$Rating.Count.Above.Mean == "No",])


## ----------------------------------------------------------------------------------------------------------------------
countries <- dataset %>% count(Country)
countries <- countries[!(countries$Country=="n/a")]
countries <- countries[order(-n)][1:10]
head(countries, n=10)


## ----------------------------------------------------------------------------------------------------------------------
countries %>% 
ggplot(aes(Country, n)) +
  geom_col(fill="brown")


## ----------------------------------------------------------------------------------------------------------------------
ratings.book <- dataset %>% group_by(ISBN) %>% filter(n()>100)
ratings.mean <- setorder(setDT(ratings.book)[, .(Book.Rating = mean(Book.Rating)), by = Book.Title], -Book.Rating)[1:10]
ratings.mean


## ----------------------------------------------------------------------------------------------------------------------
ratings.mean %>% 
ggplot(aes(Book.Rating, Book.Title)) +
  geom_col(fill='pink')


## ----------------------------------------------------------------------------------------------------------------------
summary(dataset$Year.Of.Publication)


## ----------------------------------------------------------------------------------------------------------------------
dataset$Year.Of.Publication[dataset$Year.Of.Publication == 0] <- NA


## ----------------------------------------------------------------------------------------------------------------------
summary(dataset$Year.Of.Publication)


## ----------------------------------------------------------------------------------------------------------------------
year_hist <- dataset %>%
    ggplot(aes(Year.Of.Publication)) +
    geom_histogram(binwidth=1, fill='purple') +
    theme(text = element_text(size = 20))

year_hist


## ----------------------------------------------------------------------------------------------------------------------
length(dataset$Book.Author)
n_distinct(dataset$Book.Author)


## ----------------------------------------------------------------------------------------------------------------------
author.high.count <-  dataset %>% group_by(Book.Author) %>% filter(n()>100)
author.high.count.mean <- setorder(setDT(author.high.count)[, .(Book.Rating = mean(Book.Rating)), by = Book.Author], -Book.Rating)[1:10]
author.high.count.mean


## ----------------------------------------------------------------------------------------------------------------------
author.high.count.mean %>% 
ggplot(aes(Book.Rating, Book.Author)) +
  geom_col(fill='blue')


## ----------------------------------------------------------------------------------------------------------------------
table(dataset$Rating.Count.Above.Mean)


## ----------------------------------------------------------------------------------------------------------------------
prop.table(table(dataset$Rating.Count.Above.Mean))


## ----------------------------------------------------------------------------------------------------------------------
ggplot(dataset, aes(x=reorder(Rating.Count.Above.Mean, Rating.Count.Above.Mean, function(x)-length(x)))) +
geom_bar(fill='red') +  labs(x='Rating Count Above Mean')


## ----------------------------------------------------------------------------------------------------------------------
imbalanced.dataset <- dataset
missing.dataset <- dataset


## ----------------------------------------------------------------------------------------------------------------------
n_legit <- 36151
new_frac_legit <- 0.68
new_n_total <- n_legit/new_frac_legit


## ----------------------------------------------------------------------------------------------------------------------
oversampling_result <- ovun.sample(Rating.Count.Above.Mean ~ ., data = dataset, method = "over", 
                                   N = new_n_total, seed = 2018)
dataset <- oversampling_result$data
row.names(dataset) <- NULL
table(dataset$Rating.Count.Above.Mean)


## ----------------------------------------------------------------------------------------------------------------------
prop.table(table(dataset$Rating.Count.Above.Mean))


## ----------------------------------------------------------------------------------------------------------------------
ggplot(dataset, aes(x=reorder(Rating.Count.Above.Mean, Rating.Count.Above.Mean, function(x)-length(x)))) +
geom_bar(fill='red') +  labs(x='Oversampled Rating Count Above Mean')


## ----------------------------------------------------------------------------------------------------------------------
sum(is.na(dataset))
sum(is.na(imbalanced.dataset))
names(which(colSums(is.na(dataset)) > 0))
names(which(colSums(is.na(imbalanced.dataset)) > 0))


## ----------------------------------------------------------------------------------------------------------------------
dataset$Age <- impute(dataset$Age, mean)
dataset$Year.Of.Publication <- impute(dataset$Year.Of.Publication, mean)

imbalanced.dataset$Age <- impute(imbalanced.dataset$Age, mean)
imbalanced.dataset$Year.Of.Publication <- impute(imbalanced.dataset$Year.Of.Publication, mean)


## ----------------------------------------------------------------------------------------------------------------------
sum(is.na(dataset))
sum(is.na(imbalanced.dataset))


## ----------------------------------------------------------------------------------------------------------------------
set.seed(12345)
dataset <- dataset[sample(1:nrow(dataset),500),]
imbalanced.dataset <- imbalanced.dataset[sample(1:nrow(dataset),500),]
missing.dataset <- missing.dataset[sample(1:nrow(dataset),500),]
row.names(dataset) <- NULL
row.names(imbalanced.dataset) <- NULL
row.names(missing.dataset) <- NULL
nrow(dataset)


## ----------------------------------------------------------------------------------------------------------------------
dataset$Rating.Count.Above.Mean <- ifelse(dataset$Rating.Count.Above.Mean == "No", 1, 2)


## ----------------------------------------------------------------------------------------------------------------------
data <-dataset[, c('Book.Rating',"Age","Year.Of.Publication","Rating.Count.Above.Mean")]


## ----------------------------------------------------------------------------------------------------------------------
sapply(data, class)


## ----------------------------------------------------------------------------------------------------------------------
data <- data[!is.na(data$Age),]
data <- data[!is.na(data$Year.Of.Publication),]
data <- data[!is.na(data$Book.Rating),]
data <- data[!is.na(data$Rating.Count.Above.Mean),]


## ----------------------------------------------------------------------------------------------------------------------
correlations <- cor(data)
corrplot::corrplot(correlations,method = "square",tl.cex = 0.6, tl.col = "black")


## ----------------------------------------------------------------------------------------------------------------------
data$Rating.Count.Above.Mean <- as.factor(data$Rating.Count.Above.Mean)


## ----------------------------------------------------------------------------------------------------------------------
sample<- createDataPartition(y= data$Rating.Count.Above.Mean,p=0.8,list = FALSE)

train_data <- data[sample,]
test_data <- data[-sample,]


## ----------------------------------------------------------------------------------------------------------------------
logistic_model <- glm(Rating.Count.Above.Mean~.,data = train_data,family = "binomial")


## ----------------------------------------------------------------------------------------------------------------------
prob <- predict(logistic_model,newdata=test_data,type="response")
pred <- ifelse(prob > 0.5, 2, 1)


## ----------------------------------------------------------------------------------------------------------------------
conf.matrix <- confusionMatrix(test_data$Rating.Count.Above.Mean,as.factor(pred))
conf.matrix


## ----------------------------------------------------------------------------------------------------------------------
# Heatmap visualization of confusion matrix
table <- data.frame(conf.matrix$table)
plotTable <- table %>%
  group_by(Prediction) %>%
  mutate(prop = Freq/sum(Freq))
ggplot(data =  plotTable, mapping = aes(x = Reference, y = Prediction, alpha = prop)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1, color="white", size=10) +
  scale_fill_gradient(low = "blue", high = "navyblue") +
  theme_bw() + theme(legend.position = "none")


## ----------------------------------------------------------------------------------------------------------------------
train_data$Book.Rating <- as.numeric(train_data$Book.Rating)
train_data$Year.Of.Publication <- as.numeric(train_data$Year.Of.Publication)
train_data$Rating.Count.Above.Mean <- as.numeric(train_data$Rating.Count.Above.Mean)
test_data$Book.Rating <- as.numeric(test_data$Book.Rating)
test_data$Year.Of.Publication <- as.numeric(test_data$Year.Of.Publication)
test_data$Rating.Count.Above.Mean <- as.numeric(test_data$Rating.Count.Above.Mean)


## ----------------------------------------------------------------------------------------------------------------------
sapply(train_data,class)
levels(train_data$Rating.Count.Above.Mean)


## ----------------------------------------------------------------------------------------------------------------------
pca <- prcomp(train_data, center = TRUE, scale = TRUE)
pca_test<-prcomp(test_data,center = TRUE, scale=TRUE)
pca


## ----------------------------------------------------------------------------------------------------------------------
plot(pca, type='l', main="PCA - Principal Components Analysis Chart", col="red")


## ----------------------------------------------------------------------------------------------------------------------
cumpro <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="red", lty=5)
abline(h = 0.88759, col="red", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("red"), lty=5, cex=0.6)


## ----------------------------------------------------------------------------------------------------------------------
explained.variance <- pca$sdev^2
explained.variance


## ----------------------------------------------------------------------------------------------------------------------
pca.var <- get_pca_var(pca)

kmean <- kmeans(pca.var$coord, centers = 3, nstart=25)
group <- as.factor(kmean$cluster)

fviz_pca_var(pca, col.var=group, palette='jco', legend.title='Cluster')


## ----------------------------------------------------------------------------------------------------------------------
correlations <- cor(pca$x[,c(0:4)])

corrplot::corrplot(correlations,method = "square", tl.col = "black")


## ----------------------------------------------------------------------------------------------------------------------
train_data$Rating.Count.Above.Mean <- as.factor(train_data$Rating.Count.Above.Mean)


## ----------------------------------------------------------------------------------------------------------------------
set.seed(42)

data_pca <- data.frame(Rating.Count.Above.Mean=train_data[,"Rating.Count.Above.Mean"],pca$x[,0:4])
head(data_pca)


## ----------------------------------------------------------------------------------------------------------------------
set.seed(42)
model_pca <- glm(Rating.Count.Above.Mean ~ .,data= data_pca,family = binomial)

test_data_pca <- predict(pca,newdata = test_data)


## ----------------------------------------------------------------------------------------------------------------------
prob <- predict(model_pca , newdata = data.frame(test_data_pca[,0:4]),type = "response")

pred <- factor(ifelse(prob>0.5,2,1))

levels(as.factor(pred))
levels(test_data$Rating.Count.Above.Mean)

confusionMatrix(as.factor(test_data$Rating.Count.Above.Mean),as.factor(pred))


## ----------------------------------------------------------------------------------------------------------------------
X <- dataset[sample,] %>% select("User.ID", "Age", "Book.Rating")
head(X, n=5)


## ----------------------------------------------------------------------------------------------------------------------
X <- X[!is.na(X$Age), ]


## ----------------------------------------------------------------------------------------------------------------------
set.seed(6)
wcss <- vector() 
for (i in 1:10) wcss[i] <-  sum(kmeans(X, i)$withinss)
plot(1:10, wcss, type = "b", main = paste("Clusters of users"), xlab = "Number of clusters", ylab = "WCSS")


## ----------------------------------------------------------------------------------------------------------------------
set.seed(29)
kmeans.model <- kmeans(X, 3, iter.max = 300, nstart = 10)
kmeans.model


## ----------------------------------------------------------------------------------------------------------------------
clusplot(X, clus = kmeans.model$cluster, lines = 0, shade = TRUE, color = TRUE, labels = 2, plotchar = FALSE, span = TRUE, main = paste("Clusters of users"), xlab = "Age", ylab = "Book Rating")


## ----------------------------------------------------------------------------------------------------------------------
head(X, n=5)


## ----------------------------------------------------------------------------------------------------------------------
dendrogram <- hclust(dist(X, method = 'euclidean'), method = 'ward.D')
plot(dendrogram, main = 'Dendrogram', xlab = 'Users', ylab = 'Euclidean distances')


## ----------------------------------------------------------------------------------------------------------------------
hc <- hclust(dist(X, method = 'euclidean'), method = 'ward.D')


## ----------------------------------------------------------------------------------------------------------------------
y_hc <- cutree(hc, 2)
y_hc


## ----------------------------------------------------------------------------------------------------------------------
clusplot(X, clus = y_hc, lines = 0, shade = TRUE, color = TRUE, labels = 2, plotchar = FALSE, span = TRUE, 
         main = paste("Clusters of clients"), xlab = "Age", ylab = "Book Rating")


## ----------------------------------------------------------------------------------------------------------------------
X <- missing.dataset[sample,] %>% select("User.ID", "Age", "Book.Rating")
head(X, n=5)


## ----------------------------------------------------------------------------------------------------------------------
dendrogram <- hclust(dist(X, method = 'euclidean'), method = 'ward.D')
plot(dendrogram, main = 'Dendrogram', xlab = 'Users', ylab = 'Euclidean distances')


## ----------------------------------------------------------------------------------------------------------------------
hc <- hclust(dist(X, method = 'euclidean'), method = 'ward.D')


## ----------------------------------------------------------------------------------------------------------------------
y_hc <- cutree(hc, 2)
y_hc


## ----------------------------------------------------------------------------------------------------------------------
clusplot(X, clus = y_hc, lines = 0, shade = TRUE, color = TRUE, labels = 2, plotchar = FALSE, span = TRUE, 
         main = paste("Clusters of clients"), xlab = "Age", ylab = "Book Rating")


## ----------------------------------------------------------------------------------------------------------------------
Z <- dataset[sample,] %>% select("Book.Rating", "Year.Of.Publication", "Rating.Count.Above.Mean")
head(Z, n=5)


## ----------------------------------------------------------------------------------------------------------------------
Z$Rating.Count.Factor <- factor(Z$Rating.Count.Above.Mean, levels = c(1, 2))


## ----------------------------------------------------------------------------------------------------------------------
Z <- Z[!is.na(Z$Year.Of.Publication), ]
Z <- select(Z,-c(Rating.Count.Above.Mean))
head(Z, n=5)


## ----------------------------------------------------------------------------------------------------------------------
set.seed(123)
splitted <- sample.split(Z$Rating.Count.Factor, SplitRatio = 0.75)
train_Set <- subset(Z, splitted == TRUE)
test_Set <- subset(Z, splitted == FALSE)


## ----------------------------------------------------------------------------------------------------------------------
train_y = train_Set[,3]
test_y = test_Set[,3]

row.names(train_Set) <- NULL
row.names(test_Set) <- NULL

# Scaled test and train set
trainSet = data.frame(scale(train_Set[,-3]))
trainSet$Rating.Count.Factor = train_y

testSet = data.frame(scale(test_Set[,-3]))
testSet$Rating.Count.Factor = test_y


## ----------------------------------------------------------------------------------------------------------------------
model.decision <- rpart(formula = Rating.Count.Factor ~ ., data = trainSet)
model.decision


## ----------------------------------------------------------------------------------------------------------------------
probability.prediction <- predict(model.decision, newdata = testSet[-3,], type = 'class')
probability.prediction


## ----------------------------------------------------------------------------------------------------------------------
levels(as.factor(probability.prediction))
levels(test_Set$Rating.Count.Factor)
conf.matrix <- confusionMatrix(as.factor(testSet[2:100, 3]),as.factor(probability.prediction))
decision.accuracy.balanced <- conf.matrix$overall['Accuracy']
conf.matrix


## ----------------------------------------------------------------------------------------------------------------------
# Heatmap visualization of confusion matrix
table <- data.frame(conf.matrix$table)
plotTable <- table %>%
  group_by(Prediction) %>%
  mutate(prop = Freq/sum(Freq))
ggplot(data =  plotTable, mapping = aes(x = Reference, y = Prediction, alpha = prop)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1, color="white", size=10) +
  scale_fill_gradient(low = "blue", high = "navyblue") +
  theme_bw() + theme(legend.position = "none")


## ----------------------------------------------------------------------------------------------------------------------
set <- trainSet
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('Book.Rating', 'Year.Of.Publication')
y_grid <- predict(model.decision, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Decision Tree (Train Set)',
     xlab = 'Book.Rating', ylab = 'Year.Of.Publication',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'deepskyblue', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'blue3', 'red3'))


## ----------------------------------------------------------------------------------------------------------------------
set <- testSet
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('Book.Rating', 'Year.Of.Publication')
y_grid <- predict(model.decision, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Decision Tree (Test Set)',
     xlab = 'Book.Rating', ylab = 'Year.Of.Publication',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'deepskyblue', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'blue3', 'red3'))


## ----------------------------------------------------------------------------------------------------------------------
imbalanced.dataset$Rating.Count.Above.Mean <- ifelse(imbalanced.dataset$Rating.Count.Above.Mean == "No", 1, 2)


## ----------------------------------------------------------------------------------------------------------------------
K <- imbalanced.dataset[sample,] %>% select("Book.Rating", "Year.Of.Publication", "Rating.Count.Above.Mean")
row.names(K) <- NULL
head(K, n=5)


## ----------------------------------------------------------------------------------------------------------------------
K$Rating.Count.Factor <- factor(K$Rating.Count.Above.Mean, levels = c(1, 2))


## ----------------------------------------------------------------------------------------------------------------------
K <- K[!is.na(K$Year.Of.Publication), ]
K <- select(K,-c(Rating.Count.Above.Mean))
head(K, n=5)


## ----------------------------------------------------------------------------------------------------------------------
set.seed(123)
splitted <- sample.split(K$Rating.Count.Factor, SplitRatio = 0.75)
train_Set <- subset(K, splitted == TRUE)
test_Set <- subset(K, splitted == FALSE)


## ----------------------------------------------------------------------------------------------------------------------
train_y = train_Set[,3]
test_y = test_Set[,3]

row.names(train_Set) <- NULL
row.names(test_Set) <- NULL

# Scaled test and train set
trainSet = data.frame(scale(train_Set[,-3]))
trainSet[,3] = train_y

testSet = data.frame(scale(test_Set[,-3]))
testSet[,3] = test_y


## ----------------------------------------------------------------------------------------------------------------------
model.decision <- rpart(formula = Rating.Count.Factor ~ ., data = trainSet)
model.decision


## ----------------------------------------------------------------------------------------------------------------------
probability.prediction <- predict(model.decision, newdata = testSet[-3,], type = 'class')
probability.prediction


## ----------------------------------------------------------------------------------------------------------------------
levels(as.factor(probability.prediction))
levels(test_Set$Rating.Count.Factor)
conf.matrix <- confusionMatrix(as.factor(testSet[2:100, 3]),as.factor(probability.prediction))
decision.accuracy.imbalanced <- conf.matrix$overall['Accuracy']
conf.matrix


## ----------------------------------------------------------------------------------------------------------------------
# Heatmap visualization of confusion matrix
table <- data.frame(conf.matrix$table)
plotTable <- table %>%
  group_by(Prediction) %>%
  mutate(prop = Freq/sum(Freq))
ggplot(data =  plotTable, mapping = aes(x = Reference, y = Prediction, alpha = prop)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1, color="white", size=10) +
  scale_fill_gradient(low = "blue", high = "navyblue") +
  theme_bw() + theme(legend.position = "none")


## ----------------------------------------------------------------------------------------------------------------------
set <- trainSet
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('Book.Rating', 'Year.Of.Publication')
y_grid <- predict(model.decision, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Decision Tree with Imbalanced Data (Train Set)',
     xlab = 'Book.Rating', ylab = 'Year.Of.Publication',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'deepskyblue', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'blue3', 'red3'))


## ----------------------------------------------------------------------------------------------------------------------
set <- testSet
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('Book.Rating', 'Year.Of.Publication')
y_grid <- predict(model.decision, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Decision Tree with Imbalanced Data(Test Set)',
     xlab = 'Book.Rating', ylab = 'Year.Of.Publication',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'deepskyblue', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'blue3', 'red3'))


## ----------------------------------------------------------------------------------------------------------------------
print("Accuracy of decision tree with balanced data")
decision.accuracy.balanced

print("Accuracy of decision tree with imbalanced data")
decision.accuracy.imbalanced


## ----------------------------------------------------------------------------------------------------------------------
K <- dataset[sample,] %>% select("Book.Rating", "Year.Of.Publication", "Rating.Count.Above.Mean")
head(K, n=5)


## ----------------------------------------------------------------------------------------------------------------------
K$Rating.Count.Factor <- factor(K$Rating.Count.Above.Mean, levels = c(1, 2))


## ----------------------------------------------------------------------------------------------------------------------
K <- K[!is.na(K$Year.Of.Publication), ]
K <- select(K,-c(Rating.Count.Above.Mean))
head(K, n=5)


## ----------------------------------------------------------------------------------------------------------------------
set.seed(123)
splitted <- sample.split(K$Rating.Count.Factor, SplitRatio = 0.75)
train_Set <- subset(K, splitted == TRUE)
test_Set <- subset(K, splitted == FALSE)


## ----------------------------------------------------------------------------------------------------------------------
train_y <- train_Set[,3]
test_y <- test_Set[,3]

# Scaled test and train set
trainSet <- data.frame(scale(train_Set[,-3]))
trainSet[,3] <- train_y

testSet <- data.frame(scale(test_Set[,-3]))
testSet[,3] <- test_y


## ----------------------------------------------------------------------------------------------------------------------
y_pred <- knn(train = trainSet[, -3], test = testSet[, -3], cl = trainSet[, 3], k = 5, prob = TRUE)
y_pred


## ----------------------------------------------------------------------------------------------------------------------
conf.matrix <- confusionMatrix(as.factor(testSet[, 3]),as.factor(y_pred))
knn.accuracy<- conf.matrix$overall['Accuracy']
conf.matrix


## ----------------------------------------------------------------------------------------------------------------------
# Heatmap visualization of confusion matrix
table <- data.frame(conf.matrix$table)
plotTable <- table %>%
  group_by(Prediction) %>%
  mutate(prop = Freq/sum(Freq))
ggplot(data =  plotTable, mapping = aes(x = Reference, y = Prediction, alpha = prop)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1, color="white", size=10) +
  scale_fill_gradient(low = "blue", high = "navyblue") +
  theme_bw() + theme(legend.position = "none")


## ----------------------------------------------------------------------------------------------------------------------
set <- trainSet
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('Book.Rating', 'Year.Of.Publication')
y_grid <- knn(train = trainSet[, -3], test = grid_set, cl = trainSet[, 3], k = 5)
plot(set[, -3], main = 'K-NN (Scaled Train Set)',
     xlab = 'Book.Rating', ylab = 'Year.Of.Publication',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'deepskyblue', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'blue3', 'red3'))


## ----------------------------------------------------------------------------------------------------------------------
set = testSet
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Book.Rating', 'Year.Of.Publication')
y_grid = knn(train = trainSet[, -3], test = grid_set, cl = trainSet[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Scaled Test set)',
     xlab = 'Book.Rating', ylab = 'Year.Of.Publication',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


## ----------------------------------------------------------------------------------------------------------------------
print("Accuracy of decision tree")
decision.accuracy.balanced

print("Accuracy of knn")
knn.accuracy

