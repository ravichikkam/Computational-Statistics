get_cv_error_lm <- function(data, formula, dv, K) {
  # first step is to create empty variables and divide data based on folds by randomly diving data in folds
  set.seed(123)
  cv_RMSE <- vector("numeric", K)
  cv_MAE <- vector("numeric", K)
  cv_MdAPE <- vector("numeric", K)
  folds <- sample(1:K, nrow(data), replace = TRUE)
  # Performing CV
  for(i in seq_len(K)) {
    ls.fit <- lm(formula, data = data[folds != i, ])
    pred.ls <- predict(ls.fit, data[folds == i, ])
    
    pred <- data %>% select(dv)
    cv_RMSE[i] <- sqrt(mean((pred[folds == i, ] - pred.ls)[, 1])^2)
    cv_MAE[i] <- mean(abs((pred[folds == i, ] - pred.ls)[, 1]))
    cv_MdAPE[i] <- median((200 * (abs(pred[folds == i, ] - pred.ls))/(pred[folds == i, ] + pred.ls))[[1]])
  }
  return(list(rmse = cv_RMSE, mae = cv_MAE, mdape = cv_MdAPE))
}

get_cv_error_rq <- function(data, formula, dv, K) {
  # first step is to create empty variables and divide data based on folds by randomly diving data in folds
  set.seed(123)
  cv_RMSE <- vector("numeric", K)
  cv_MAE <- vector("numeric", K)
  cv_MdAPE <- vector("numeric", K)
  folds <- sample(1:K, nrow(data), replace = TRUE)
  # Performing CV
  for(i in seq_len(K)) {
    ls.fit <- rq(formula, data = data[folds != i, ])
    pred.ls <- predict(ls.fit, data[folds == i, ])
    
    pred <- data %>% select(dv)
    cv_RMSE[i] <- sqrt(mean((pred[folds == i, ] - pred.ls)[, 1])^2)
    cv_MAE[i] <- mean(abs((pred[folds == i, ] - pred.ls)[, 1]))
    cv_MdAPE[i] <- median((200 * (abs(pred[folds == i, ] - pred.ls))/(pred[folds == i, ] + pred.ls))[[1]])
  }
  return(list(rmse = cv_RMSE, mae = cv_MAE, mdape = cv_MdAPE))
}

get_cv_logistic <- function(data, formula, dv, K) {
  # first step is to create empty variables and divide data based on folds by randomly diving data in folds
  set.seed(123)
  cv_error <- vector("numeric", K)
  folds <- sample(1:K, nrow(data), replace = TRUE)
  # Performing CV
  for(i in seq_len(K)) {
    glm.fit <- glm(formula, data = data[folds != i, ], family = "binomial")
    pred.glm <- predict(glm.fit, data[folds == i, ], type = "response")
    
    pred <- data %>% select(dv)
    pred1 <- pred[folds == i, ]
    mat <- as.matrix(table(pred1[[1]], pred.glm >= 0.5))
    # if(ncol(mat) == 1 && nrow(mat) == 1) {
    #   cv_error[i] <- 0
    # }
    # else if(ncol(mat) == 1 && colnames(mat) == "FALSE") {
    #   cv_error[i] <- mat[2, 1]/length(pred1[[1]])
    # }
    # else if(ncol(mat) == 1 && colnames(mat) == "TRUE") {
    #   cv_error[i] <- mat[1, 1]/length(pred1[[1]])
    # }
    # else if(nrow(mat) == 1 && rownames(mat) == 0) {
    #   cv_error[i] <- mat[1, 2]/length(pred1[[1]])
    # }
    # else if(nrow(mat) == 1 && rownames(mat) == 1) {
    #   cv_error[i] <- mat[1, 1]/length(pred1[[1]])
    # }
    # else {
    #   cv_error[i] <- sum(mat[2, 1] + mat[1, 2])/ length(pred1[[1]])
    # }
    cv_error[i] <- mean(pred1[[1]] != (pred.glm >= 0.5))
  }
  return(mean(cv_error))
}
