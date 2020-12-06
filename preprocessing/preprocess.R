
library(caret)
library(sjstats)

preprocess <- function(x,...){
  UseMethod(preprocess)
}

preprocess.myMode <- function(x,...){
  class(x)<- 'data.frame'
  unique_x <- unique(x)
  mode <- unique_x[which.max(tabulate(match(x, unique_x)))]
  return(mode)
}

preprocess.nzv <- function(x, show = 'all',...){
  class(x) <- 'data.frame' 
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
  removed <- num[,-highlyCorDescr] 
  print(paste('Features removed:',names(x)[highlyCorDescr]))
  return(removed)
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
  removed.cat <- index[c(sort_c$ix[1:remove])]
  print(paste('Features removed:',names(x)[removed.cat]))
  return(x[,-removed.cat])
}

preprocess.corNum2Cat <- function(x,numIndex, catIndex, accuracy =.8,...){
  
}

preprocess.default <- function(x,...){
  print('Class Errror!')
}

