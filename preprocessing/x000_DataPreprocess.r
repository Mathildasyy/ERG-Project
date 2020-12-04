# import packages
library(ggplot2)
library(graphics)
library(splines)
library(caret)


# read data files
train <- read.csv('train.csv')[,-1]
test <- read.csv('test.csv')
summary(train)

# Now, let's observe the training data
## First, we make a histogram of the sale prices
## It is obvious that the distribution is skewed to right
ggplot(train, aes(x = SalePrice/1000)) + # Measure the house prices in $1,000
  geom_histogram(binwidth = 30)

train$SalePrice.log <- log(train$SalePrice/1000+1) # log transformation
ggplot(train, aes(x = SalePrice.log)) + 
  geom_histogram(binwidth = .1)

#  We roughly abstract the class of each feature
feature.class <- c()
for (i in 1:79){
  feature.class <- append(feature.class,class(train[,i]))
}

## To observe qualitative features, we use boxplots to idnetify outliers.
## To observe quantitative features, we draw basis-spline plots.
## Set 2 by 2 canvas
par(mfrow = c(2,2), mar = c(2,2,2,2))

# Then, we plot the numeric and non-numeric variables in different ways
feature.graphs <- function(subset.index){
for (i in subset.index){
  class <- feature.class[i]
  title <- names(train)[i]
  if(class == 'character'){
    boxplot(SalePrice.log~., data = train[,c(i,81)], main = title)
    }else
  data <- data.frame(x = train[,i],
                         y = train$SalePrice.log)
  ggplot(data, aes(x,y)) + 
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_point(col='grey60', size = 0.5) + 
    geom_smooth(method="lm", formula= y~poly(x,3, raw =T), colour = 'hotpink3')}
}


# Before we start automatic generation, we have to manually re-classify the feature type
## Delete the hashtags before you name, and input a vector of the actual types
sy<- c('character','character','integer','integer','character',
       'character','character','character','character','character',
       'character','character','character','character','character',
       'character',"integer","integer","integer","integer")
##hy<- c()
##hsq<- c()
##zqf<- c()
feature.class[1:20] = sy
##feature.class[21:40] = hy
##feature.class[41:60] = hsq
##feature.class[61:79] = zqf

# change the index to the subset
feature.subset <- c(5,6,8,11,12,14,9,36,39,41,52,60,61,68,69,70,
                    71,74,22,43,45,46,55,27,30,31,35,72,75)

# generate your part of graphs
feature.graphs(feature.subset)

