#### Predicting discreete valued time series
# in this problem we will use the majority rule on previous K days weather to predict next days weather
# 1 is rainy, 0 is not rainy
# the objective here is to choose the best k

library(ggplot2)
library(plotly)
#### create training data ----
data <- rbinom(100, prob = 0.35, size = 1)

# the function below will give us error on the prediction based on k
#### pred fucntion ----

predict <- function(vec, k) {
  pred <- as.integer()
  for(i in (k+1): length(vec)) {
    if(sum(vec[(i-k):(i-1)]) >= k/2) {
      pred <- c(pred, 1)
    }
    else {
      pred <- c(pred, 0)
    }
  }
  error_rate <- 1 - (sum(pred == vec[(k+1):length(vec)] )/length(pred))
  return(round(error_rate, digits = 2))
}

#### find best k for k = 1:10 and plot the graph

error_lsit <- sapply(c(1:10), FUN = predict, vec = data)

error_df <- data.frame(k = 1:10, error = error_lsit)

plot <- ggplot(error_df, mapping = aes(x = error_df$k, y = error_df$error)) + 
  geom_point() + geom_smooth() + labs(x = "K", y = "error") + 
  scale_x_continuous()
                                                                                
#### interactive plot using plotly
ggplotly(plot)
