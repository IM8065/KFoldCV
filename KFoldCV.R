## install package if it is not already.
if(!require("data.table")){
  install.packages("data.table")
}

## attach all functions provided by these packages.
library(data.table)
library(ggplot2)

## download spam data set to local directory, if it is not present.
if(!file.exists("spam.data")){
  download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data", "spam.data")
}

## Read spam data set and conver to X matrix and y vector we need for
## gradient descent.
spam.dt <- data.table::fread("spam.data")
N.obs <- nrow(spam.dt)
X.raw <- as.matrix(spam.dt[, -ncol(spam.dt), with=FALSE]) 
y.vec <- spam.dt[[ncol(spam.dt)]]
X.sc <- scale(X.raw) #scaled X/feature/input matrix.


KFoldCV(X_mat, y_vec, ComputePredictions, fold_vec, k){
    error_vec <- rep(0, k)
    
    for (folds in 1:k){
    is.test <- fold.vec == folds
    is.train <- !is.test
    X.new <- X.sc[is.test, ]
    y.new <- y.vec[is.test]
    X.train <- X.sc[is.train, ]
    y.train <- y.vec[is.train]
    pred_new <- ComputePredictions(X_train,X_new,y_train)
    zero.loss <- pred.new != y.new
    mean.err <- mean(zero.loss)    
    error_vec[folds] = mean.err
  }

  return(error_vec)
}


NearestNeighborsCV <- function(X_mat, y_vec, X_new, num_folds, max_neighbors){
    validation_fold_vec <- sample(rep(1:num_folds, l = nrow(X_mat)))
    error_mat = matrix(0, num_folds, max_neighbors)
    mean_error_vec <- c(1:max_neighbors)
    for (num_neighbors in 1:max_neighbors){
	     #figure class::knn()
         error_mat[, num_neighbors] = KFoldCV(X_mat, 
                                              y_vec, 
                                              function(X_mat, X_new, y_vec){class::knn(X_mat, X_new, y_vec, k=num_neighbors )}, 
                                              validation_fold_vec)
         mean_error_vec[num_neighbors] <- colMeans(error_mat)
         best_neighbors <- which.min(mean_error_vec)
    }
    
    #(5 points) Your function should output (1) the predictions for X_new, 
    #using the entire X_mat,y_vec with best_neighbors; (2) the mean_error_mat for 
    #visualizing the validation error.
}
