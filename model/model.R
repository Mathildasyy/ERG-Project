library(caret)


model <-function(x,...){
  UseMethod(model)
}

model.lm <- function(x,...)