library(mice)
library(RANN)
library(caret)
library(sjstats)
library(ggplot2)
library(magrittr)

preprocess <- function(x,...){
  UseMethod(preprocess)
}

preprocess.feaEngineer <- function(x,...){
  class(x) <- 'data.frame'
  
  x$GarageQual[x$GarageQual=='Ex'] = 5
  x$GarageQual[x$GarageQual=='Gd'] = 4
  x$GarageQual[x$GarageQual=='TA'] = 3
  x$GarageQual[x$GarageQual=='Fa'] = 2
  x$GarageQual[x$GarageQual=='Po'] = 1
  x$GarageQual[is.na(x$GarageQual)] = 0
  x$GarageQual <- as.numeric(x$GarageQual)
  
  x$GarageCond[x$GarageCond=='Ex'] = 5
  x$GarageCond[x$GarageCond=='Gd'] = 4
  x$GarageCond[x$GarageCond=='TA'] = 3
  x$GarageCond[x$GarageCond=='Fa'] = 2
  x$GarageCond[x$GarageCond=='Po'] = 1
  x$GarageCond[is.na(x$GarageCond)] = 0
  x$GarageCond <- as.numeric(x$GarageCond)
  
  x$GarageQual <- x$GarageQual + x$GarageCond
  x <- x[,-which(colnames(x)=="GarageCond")]
  
  # 房屋整体质量+状态
  x$OverallQual <-x$OverallQual + x$OverallCond
  x <- x[,-which(colnames(x)=="OverallCond")]
  
  # 外部材料的质量+状态
  x$ExterCond[x$ExterCond == 'Ex'] = 5
  x$ExterCond[x$ExterCond == 'Gd'] = 4
  x$ExterCond[x$ExterCond == 'TA'] = 3
  x$ExterCond[x$ExterCond == 'Fa'] = 2
  x$ExterCond[x$ExterCond == 'Po'] = 1
  x$ExterCond <- as.numeric(x$ExterCond)
  
  x$ExterQual[x$ExterQual == 'Ex'] = 5
  x$ExterQual[x$ExterQual == 'Gd'] = 4
  x$ExterQual[x$ExterQual == 'TA'] = 3
  x$ExterQual[x$ExterQual == 'Fa'] = 2
  x$ExterQual[x$ExterQual == 'Po'] = 1
  x$ExterQual <- as.numeric(x$ExterQual)
  
  x$ExterQual <- x$ExterQual+x$ExterCond
  x <- x[,-which(colnames(x)=="ExterCond")]
  
  # 地下室的质量+状态
  x$BsmtCond[x$BsmtCond == 'Ex'] = 5
  x$BsmtCond[x$BsmtCond == 'Gd'] = 4
  x$BsmtCond[x$BsmtCond == 'TA'] = 3
  x$BsmtCond[x$BsmtCond == 'Fa'] = 2
  x$BsmtCond[x$BsmtCond == 'Po'] = 1
  x$BsmtCond[is.na(x$BsmtCond) ] = 0
  x$BsmtCond <- as.numeric(x$BsmtCond)
  
  x$BsmtQual[x$BsmtQual == 'Ex'] = 5
  x$BsmtQual[x$BsmtQual == 'Gd'] = 4
  x$BsmtQual[x$BsmtQual == 'TA'] = 3
  x$BsmtQual[x$BsmtQual == 'Fa'] = 2
  x$BsmtQual[x$BsmtQual == 'Po'] = 1
  x$BsmtQual[is.na(x$BsmtQual) ] = 0
  x$BsmtQual <- as.numeric(x$BsmtQual)
  
  x$BsmtQual <- x$BsmtQual+x$BsmtCond
  x <- x[,-which(colnames(x)=="BsmtCond")]
  
  # 地下室的敞开部分
  x$BsmtExposure[x$BsmtExposure == 'Gd'] = 4
  x$BsmtExposure[x$BsmtExposure == 'Av'] = 3
  x$BsmtExposure[x$BsmtExposure == 'Mn'] = 2
  x$BsmtExposure[x$BsmtExposure == 'No'] = 1
  x$BsmtExposure[is.na(x$BsmtExposure) ] = 0
  x$BsmtExposure <- as.numeric(x$BsmtExposure)
  
  # 地下室完工面积等级
  x$BsmtFinType1[x$BsmtFinType1 == 'GLQ'] = 6
  x$BsmtFinType1[x$BsmtFinType1 == 'ALQ'] = 5
  x$BsmtFinType1[x$BsmtFinType1 == 'BLQ'] = 4
  x$BsmtFinType1[x$BsmtFinType1 == 'Rec'] = 3
  x$BsmtFinType1[x$BsmtFinType1 == 'LwQ'] = 2
  x$BsmtFinType1[x$BsmtFinType1 == 'Unf'] = 1
  x$BsmtFinType1[is.na(x$BsmtFinType1)] = 0
  x$BsmtFinType1 <- as.numeric(x$BsmtFinType1)
  
  x$BsmtFinType2[x$BsmtFinType2 == 'GLQ'] = 6
  x$BsmtFinType2[x$BsmtFinType2 == 'ALQ'] = 5
  x$BsmtFinType2[x$BsmtFinType2 == 'BLQ'] = 4
  x$BsmtFinType2[x$BsmtFinType2 == 'Rec'] = 3
  x$BsmtFinType2[x$BsmtFinType2 == 'LwQ'] = 2
  x$BsmtFinType2[x$BsmtFinType2 == 'Unf'] = 1
  x$BsmtFinType2[is.na(x$BsmtFinType2)] = 0
  x$BsmtFinType2 <- as.numeric(x$BsmtFinType2)
  
  x$BsmtFinType1 <- x$BsmtFinType1 + x$BsmtFinType2
  x <- x[,-which(colnames(x)=="BsmtFinType2")]
  
  # 游泳池质量
  x$PoolQC[x$PoolQC == 'Ex'] = 4
  x$PoolQC[x$PoolQC == 'Gd'] = 3
  x$PoolQC[x$PoolQC == 'TA'] = 2
  x$PoolQC[x$PoolQC == 'Fa'] = 1
  x$PoolQC[is.na(x$PoolQC)] = 0
  x$PoolQC <- as.numeric(x$PoolQC)
  
  # 栅栏质量
  x$Fence[x$Fence == 'GdPrv'] = 4
  x$Fence[x$Fence == 'MnPrv'] = 3
  x$Fence[x$Fence == 'GdWo'] = 2
  x$Fence[x$Fence == 'MnWw'] = 1
  x$Fence[is.na(x$Fence)] = 0
  x$Fence <- as.numeric(x$Fence)
  
  # 厨房质量
  x$KitchenQual[x$KitchenQual == 'Ex'] = 4
  x$KitchenQual[x$KitchenQual == 'Gd'] = 3
  x$KitchenQual[x$KitchenQual == 'TA'] = 2
  x$KitchenQual[x$KitchenQual == 'Fa'] = 1
  x$KitchenQual[x$KitchenQual == 'Po'] = 0
  x$KitchenQual <- as.numeric(x$KitchenQual)
  
  # 壁炉质量
  x$FireplaceQu[x$FireplaceQu == 'Ex'] = 5
  x$FireplaceQu[x$FireplaceQu == 'Gd'] = 4
  x$FireplaceQu[x$FireplaceQu == 'TA'] = 3
  x$FireplaceQu[x$FireplaceQu == 'Fa'] = 2
  x$FireplaceQu[x$FireplaceQu == 'Po'] = 1
  x$FireplaceQu[is.na(x$FireplaceQu)] = 0
  x$FireplaceQu <- as.numeric(x$FireplaceQu)
  
  x <- x[,-which(colnames(x)=="GrLivArea")]
  
  return(x)
}



preprocess.fixNA <- function(x,...){
  class(x)<- 'data.frame'
  
  x$Alley[is.na(x$Alley)] <- "None"
  x$GarageType[is.na(x$GarageType)] <- "None"
  x$GarageFinish[is.na(x$GarageFinish)] <- "None"
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

preprocess.getNum <- function(x, type = 'index',...){
  class(x)<- 'data.frame'
  num.feature <- c()
  for (i in 1:ncol(x)){
    class <- class(x[,i])
    if (class == 'numeric') {
      if(type == 'index'){num.feature <- c(num.feature, i)}
      if(type == 'name'){num.feature <- c(num.feature, names(x)[i])}
      }
  }
  return(num.feature)
}

preprocess.getCat <- function(x, type = 'index',...){
  class(x)<- 'data.frame'
  cat.feature <- c()
  for (i in 1:ncol(x)){
    class <- class(x[,i])
    if (class == 'character') {
      if(type == 'index'){cat.feature <- c(cat.feature, i)}
      if(type == 'name'){cat.feature <- c(cat.feature, names(x)[i])}
    }
  }
  return(cat.feature)
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
  remainCat <- index[-c(sort_c$ix[2:remove+1])]
  print('Preprocess: Correlation between categorical features')
  print(paste('Features removed:',names(x)[index[c(sort_c$ix[2:remove+1])]]))
  return(x[,remainCat]) # return the dataframe
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
      fit <- x(cat~num, 
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
  print(qplot(U[, 1], U[, 2], color = dist1, size = I(3)) + coord_equal() + 
    scale_color_viridis_c(trans = "log", breaks = c(1, 3, 6)))
  
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
  print(qplot(U2[, 1], U2[, 2], color = dist2, size = I(3)) + coord_equal() + 
    scale_color_viridis_c(trans = "log", breaks = c(1, 3, 6)))
  
  return(ind.out)
}

preprocess.fixY <- function(x,...){
  class(x) <- 'data.frame'
  x <- log1p(x)
}

preprocess.default <- function(x,...){
  print('Class Errror!')
}

