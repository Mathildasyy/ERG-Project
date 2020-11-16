# import packages
library(ggplot2)
library(graphics)
library(splines)

# read data files
train <- read.csv('train.csv')
test <- read.csv('test.csv')
summary(train)

# Now, let's observe the training data
## First, we make a histogram of the sale prices
## It is obvious that the distribution is skewed to right
ggplot(train, aes(x = SalePrice/1000)) + # Measure the house prices in $1,000
  geom_histogram(binwidth = 30)

#  We roughly abstract the class of each feature
feature.class <- c()
for (i in 2:80){
  feature.class <- append(feature.class,class(train[,i]))
}

## To observe qualitative features, we use boxplots to idnetify outliers.
## To observe quantitative features, we draw basis-spline plots.
bsplot <- function(index, title){
  x <- train[,index]
  y <- train$SalePrice/1000
  pr <- predict(lm(y ~ bs(x)), newdata = data.frame(x))
  plot(x,y, main = title)
  points(x, pr, col = "red", pch = 10, cex = 0.5)
}
## Set 2 by 2 canvas
par(mfrow = c(2,2))

# Then, we plot the numeric and non-numeric variables in different ways
feature.graphs <- function(subset.index){
for (i in subset.index){
  class <- feature.class[i]
  title <- names(train)[i+1]
  if(class == 'character'){
    boxplot(SalePrice/1000~., data = train[,c(i+1,81)], main = title)
    }else
  bsplot(i+1, title)}
}


# Before we start automatic generation, we have to manually re-classify the feature type
## Delete the hashtag before you name, and input a vector of the actual types
##sy<- c()
##hy<- c()
##hsq<- c()
##zqf<- c()
##feature.class[1:20] = sy
##feature.class[21:40] = hy
##feature.class[41:60] = hsq
##feature.class[61:79] = zqf

# change the index to the subset of your part (e.g. sy: 1:20)
feature.subset <- 1:20

# generate your part of graphs
feature.graphs(feature.subset)

