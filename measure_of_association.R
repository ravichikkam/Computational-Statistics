#### Measure of association
# this script will allow to find the measure of association between two vectors. It is the proportional times two vectors 
# either increased or decreased together

#### Function to determin increase or decrease for each tick ----
cal_change <- function(x) {
  value <- as.integer()
  for(i in 2:length(x)) {
    #if((x[i] - x[i-1]) >=0) value <- c(value, 1) else value <- c(value, -1)
    value[i-1] <- ifelse((x[i] - x[i-1]) >=0, 1, -1)
  }
  value
}

#### Association function ----
get_association <- function(x, y) {
  x_value <- cal_change(x)
  y_value <- cal_change(y)
  association <- sum(x_value == y_value)/length(x)
  association
}

#### Test Case ----
c1 <- sample(1:100, size = 25, replace = TRUE)
c2 <- sample(200:300, size = 25, replace = TRUE)

get_association(c1, c2)
