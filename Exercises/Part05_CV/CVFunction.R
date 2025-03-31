# Date: 19-11-2021

#****************#
#   CVFunction   #
#****************#

### Create function:
KNN_crossVal <- function(X, y, k_fold=10, KNN_k=1){
  ### Check input:
  stopifnot(nrow(X)==length(y), is.factor(y), 
            1<k_fold, k_fold<=nrow(X), KNN_k<=nrow(X))
  ### Create k sub-selections:
  n <- nrow(X)
  inds <- sample(1:n)
  indL <- suppressWarnings(split(inds, f = 1:k_fold))
  ### Run KNN on each selection:
  preds <- list()   # Create empty list
  for(i in 1:k_fold){
    ### Extract fold data:
    ind_fold <- indL[[i]]
    testDat <- X[ind_fold,]
    trainDat <- X[-ind_fold,]
    test.solu <- y[ind_fold]
    train.solu <- y[-ind_fold]
    ### Make predictions:
    knnprd <- class::knn(train = trainDat, cl = train.solu,
               test = testDat, k = KNN_k)
    ### Combine true solution with predictions:
    preds[[i]] <- data.frame("TrueValue"=test.solu, "Prediction"=knnprd)
  }
  ### Merge into one data frame:
  ### In one command:
  # predmrg <- do.call(rbind, preds)
  ### Using a for loop instead:
  predmrg <- preds[[1]]
  for(i in 2:length(preds)){
    predmrg <- rbind(predmrg, preds[[i]])
  }
  ### Confusion matrix:
  confm <- table(predmrg)
  ### Error rate:
  missMat <- confm
  diag(missMat) <- 0
  missCount <- sum(missMat)
  testErr <- missCount/sum(confm)
  ### Return value:
  rval <- list("ConfusionMatrix"=confm, "MissclassRate"=testErr)
  return(rval)
}


### Try out function:
dat <- iris[,-5]
y <- iris$Species
KNN_crossVal(X = dat, y = y, k_fold = 10, KNN_k = 5)








