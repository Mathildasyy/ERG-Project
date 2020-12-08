
library(caret)
library(sjstats)
library(ggplot2)
library(magrittr)

preprocess <- function(x,...){
  UseMethod(preprocess)
}

preprocess.fixNA <- function(x,...){
  class(x)<- 'data.frame'
  
  x$Alley[is.na(x$Alley)] <- "None"
  x$BsmtQual[is.na(x$BsmtQual)] <- "None"
  x$BsmtCond[is.na(x$BsmtCond)] <- "None"
  x$BsmtExposure[is.na(x$BsmtExposure)] <- "None"
  x$BsmtFinType1[is.na(x$BsmtFinType1)] <- "None"
  x$BsmtFinType2[is.na(x$BsmtFinType2)] <- "None"
  x$FireplaceQu[is.na(x$FireplaceQu)] <- "None"
  x$GarageType[is.na(x$GarageType)] <- "None"
  x$GarageFinish[is.na(x$GarageFinish)] <- "None"
  x$GarageQual[is.na(x$GarageQual)] <- "None"
  x$GarageCond[is.na(x$GarageCond)] <- "None"
  x$PoolQC[is.na(x$PoolQC)] <- "None"
  x$Fence[is.na(x$Fence)] <- "None"
  x$MiscFeature[is.na(x$MiscFeature)] <- "None"
  x$GarageYrBlt[is.na(x$GarageYrBlt)] <- 0
  
  x$MasVnrType[is.na(x$MasVnrType)] <- "None"
  x$Electrical[is.na(x$Electrical)] <- "SBrkr"
  
  xmodel <- preProcess(x, "knnImpute", k=38) # set k to equal to the square root of number of variables 
  x <- predict(xmodel, x)
  
  return(x)
}

preprocess.myMode <- function(x,...){
  class(x)<- 'data.frame'
  unique_x <- unique(x)
  mode <- unique_x[which.max(tabulate(match(x, unique_x)))]
  return(mode)
}

preprocess.nzv <- function(x, show = 'all',...){
  class(x) <- 'data.frame' 
  print('Preprocess: Near-zero-variance features')
  print(paste('There are',dim(x)[2],'columns in the original dataframe.'))
  nzv <- nearZeroVar(x, saveMetrics = T)
  ifelse(show =='all', print(nzv[nzv$nzv,]),print(nzv[nzv$nzv,][c(1:show),]))
  nzv <- nearZeroVar(x)
  new <- x[, -nzv]
  print(paste('After removing the near-zero-variance features, the dataframe has',dim(new)[2],'columns.'))
  print(paste('Features removed:',names(x)[nzv]))
  return(new) # exclude the close-to-zero-var features
}

preprocess.getNum <- function(x, range = 'all', type = 'index',...){
  class(x)<- 'data.frame'
  num.feature <- c()
  index <- ifelse(range == 'all', 1:ncol(x), range)
  for (i in index){
    class <- class(x[,i])
    if (class == 'numeric') {
      if(type == 'index'){num.feature <- c(num.feature, i)}
      if(type == 'name'){num.feature <- c(num.feature, names(x)[i])}
      }
  }
  return(num.feature)
}

preprocess.getCat <- function(x, range = 'all', type = 'index',...){
  class(x)<- 'data.frame'
  cat.feature <- c()
  index <- ifelse(range == 'all', 1:ncol(x), range)
  for (i in index){
    class <- class(x[,i])
    if (class == 'character') {
      if(type == 'index'){cat.feature <- c(cat.feature, i)}
      if(type == 'name'){cat.feature <- c(cat.feature, names(x)[i])}
    }
  }
  return(cat.feature)
}

preprocess.fixSkews <- function(x,...){
  # Your show time!
}

preprocess.corNum2Num <- function(x,index, cut = .85, cormethod = 'spearman',...){
  class(x)<- 'data.frame'
  num <- x[,index]
  descrCor <-  cor(num, method = cormethod)
  highlyCorDescr <- findCorrelation(descrCor, cutoff = cut)
  remaining <- num[,-highlyCorDescr] 
  print('Preprocess: Correlation between numeric features')
  print(paste('Features removed:',names(x)[highlyCorDescr]))
  return(remaining) # return the dataframe
}

preprocess.corCat2Cat <-function(x, index, remove = 5,...){
  class(x)<- 'data.frame'
  cramer_ind = c()
  for (i in 1:length(index)){
    for (j in 1:length(index)){
      tab = table(x[,index[i]], x[,index[j]])
      cramer_ind = append(cramer_ind, cramer(tab))
    }
  }
  cramer_ma = matrix(cramer_ind, ncol = length(index))
  
  index_c = c()
  for (i in 1:length(index)){
    index_c = append(index_c, mean(cramer_ma[,i]))
  }
  sort_c = sort(index_c, index.return = T, decreasing = T)
  removedCat <- index[c(sort_c$ix[1:remove])]
  print('Preprocess: Correlation between categorical features')
  print(paste('Features removed:',names(x)[removedCat]))
  return(x[,-removedCat]) # return the dataframe
}

preprocess.corNum2Cat <- function(x,numIndex, catIndex, removeNum = 2, accuracy =.8,...){
  class(x)<- 'data.frame'
  removedIndex <- c()
  allResult <- matrix(rep(0,100))
  
  for (i in numIndex){
    num = x[,i]
    for (j in catIndex){
      cat = x[,j]
      pair = data.frame(cat = cat, num = num)
      fit <- train(cat~num, 
            data = pair, 
            method = 'multinom',
            trControl = trainControl(method = "cv", number = 5))
      result <- fit$results$RMSE > accuracy
      allResult[i] <- allResult[i]+ifelse(result,1,0)
      allResult[j] <- allResult[i]+ifelse(result,1,0)
    }
  }
  for (t in removeNum){
    removedIndex <- c(removedIndex, which.max(allResult))
  }
  print('Preprocess: Correlation between numeric and categorical features')
  
  return(x[,-removedIndex])
}


preprocess.PCA <- function(x,...){
  class(x) <- 'data.frame'
  
  # plot the original PCA (2 dimensions)
  pca <- prcomp(x, scale = T)
  U <- pca$x
  dist1 <- apply(U, 2, function(x) abs(x - median(x)) / mad(x)) %>%
    apply(1, max)
  qplot(U[, 1], U[, 2], color = dist1, size = I(3)) + coord_equal() + 
    scale_color_viridis_c(trans = "log", breaks = c(1, 3, 6))
  
  # identify outliers (robust way:|x - median|/MeanAverageDeviation)
  print('These are outliers:')
  ind.out <- apply(U, 2, function(x) which( (abs(x - median(x)) / mad(x)) > 6 )) %>%
    Reduce(union, .) %>%
    print()
  
  # delete them
  x2 <- x[-ind.out,]
  
  # plot the new PCA (2 dimensions)
  pca2 <- prcomp(x2, scale = T)
  U2 <- pca2$x
  dist2 <- apply(U2, 2, function(x) abs(x - median(x)) / mad(x)) %>%
    apply(1, max)
  qplot(U2[, 1], U2[, 2], color = dist2, size = I(3)) + coord_equal() + 
    scale_color_viridis_c(trans = "log", breaks = c(1, 3, 6))
}

preprocess.clustering <- function(x,...){
  # Your show time!
}


preprocess.default <- function(x,...){
  print('Class Errror!')
}

