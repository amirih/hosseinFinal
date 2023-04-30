#' Generate random samples from a vector or dataframe-like object
#'
#' @param x an atomic vector or dataframe-like object
#' @param n number of samples to draw (default = 1)
#' @param replace whether to sample with replacement (default = TRUE)
#' @return a sample or a dataframe-like object
#' @examples
#' # Sampling from a vector
#' vec <- 1:10
#' rando(vec, n = 3, replace = FALSE)
#'
#' # Sampling from a data frame
#' df <- data.frame(a = 1:5, b = 6:10)
#' rando(df, n = 2, replace = TRUE)
#'
#' # Sampling from a matrix
#' mat <- matrix(1:9, nrow = 3, ncol = 3)
#' rando(mat, n = 2, replace = FALSE)
#'
#' @export
rando = function(x, n = 1, replace = T) {
  if (is.atomic(x) && !is.matrix(x)) {
    if (length(x) == 0) {
      stop("x must have at least one element")
    }
    return(sample(x, size = n, replace = replace))
  } else if (is.data.frame(x) ||
             is.matrix(x) ||
             (is.list(x) && all(sapply(x, is.atomic)))) {
    if (nrow(x) == 0) {
      stop("x must have at least one row")
    }
    rows = sample(nrow(x), n, replace)
    return(x[rows, ])
  } else {
    stop("x must be an atomic vector or dataframe-like object")
  }
}

#' Check if an element in a vector is the minimum
#'
#' @param x a numeric vector
#' @param na.rm remove NA values (default = TRUE)
#' @return a logical vector
#'
#' @examples
#' # Example 1
#' is_min(c(1, 2, 3, 4, 5))
#' TRUE FALSE FALSE FALSE FALSE
#' # Example 2
#' is_min(c(NA, 1, 2, 3, 4, 5))
#'TRUE FALSE FALSE FALSE FALSE
#'
#' @export
is_min = function(x, na.rm = T) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x == min(x, na.rm = na.rm)
}

#' Check if an element in a vector is the maximum
#'
#' @param x a numeric vector
#' @param na.rm remove NA values (default = TRUE)
#' @return a logical vector
#'
#' @examples
#' # Example 1
#' is_max(c(1, 2, 3, 4, 5))
#' # Example 2
#' is_max(c(NA, 1, 2, 3, 4, 5))
#'
#' @export
is_max = function(x, na.rm = T) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  x == max(x, na.rm = na.rm)
}

#' Replicate a matrix or dataframe M times vertically and N times horizontally
#'
#' @param x a dataframe or matrix
#' @param M number of times to replicate vertically (default = 1)
#' @param N number of times to replicate horizontally (default = 1)
#' @return a replicated matrix or dataframe
#'
#' @examples
#' > mat <- matrix(1:4, nrow = 2, ncol = 2)
#' > mat
#' [,1] [,2]
#' [1,]    1    3
#' [2,]    2    4
#' > ( mat <- matrix(1:4, nrow = 2, ncol = 2))
#' [,1] [,2]
#' [1,]    1    3
#' [2,]    2    4
#' > (replicated_mat <- rep_mat(mat, M = 2, N = 3))
#' [,1] [,2] [,3] [,4] [,5] [,6]
#' [1,]    1    3    1    3    1    3
#' [2,]    2    4    2    4    2    4
#' [3,]    1    3    1    3    1    3
#' [4,]    2    4    2    4    2    4
#'
#' @export
rep_mat = function(x, M = 1, N = 1) {
  if (!(is.atomic(x) || is.matrix(x) || is.data.frame(x))) {
    stop("x must be a dataframe or matrix")
  }
  dataFrame = FALSE
  if (is.data.frame(x)) {
    dataFrame = TRUE
    x = as.matrix(x)
  }
  if (is.atomic(x)) {
    x = as.matrix(x)
  }

  nrow_x = nrow(x)
  ncol_x = ncol(x)

  # Initialize the replicated matrix with the desired dimensions
  replicated_matrix =
    matrix(0, nrow = nrow_x * M, ncol = ncol_x * N)

  # Replicate the input matrix using nested loops
  for (i in 1:M) {
    for (j in 1:N) {
      replicated_matrix[((i - 1) * nrow_x + 1):(i * nrow_x), ((j - 1) * ncol_x + 1):(j * ncol_x)] =
        x
    }
  }
  output = replicated_matrix
  if (dataFrame == TRUE) {
    output = as.data.frame(output)
  }
  return(output)
}

#' Get the classes of columns in a dataframe or the class of an object
#'
#' @param x a dataframe or an object
#' @return a character vector of class names or a single class name
#' @examples
#' # Create a dataframe
#' df <- data.frame(a = 1:3, b = c("one", "two", "three"), stringsAsFactors = FALSE)
#'
#' # Get the classes of columns in the dataframe
#' column_classes <- classes(df)
#'
#'          a           b
#'       "integer" "character"

#' # Get the class of an object (a numeric vector)
#' num_class <- classes(1:3)
#' [1] "integer"
#'
#' @export
classes = function(x) {
  if (is.data.frame(x) || is_tibble(x)) {
    return(sapply(x, class))
  }
  else {
    return(class(x))
  }
}

#' Scale the numeric columns of a dataframe
#'
#' @param x a dataframe
#' @param center center the numeric columns (default = TRUE)
#' @param scale scale the numeric columns (default = TRUE)
#' @return a scaled dataframe
#' @examples
#' # Create a dataframe
#' df <- data.frame(a = 1:5, b = c("one", "two", "three", "four", "five"), c = 11:15)
#'
#' # Scale the numeric columns of the dataframe
#' scaled_df <- df_scale(df)
#'
#' @export
df_scale = function(x, center = T, scale = T) {
  classes_x = classes(x)
  scaled_x = x

  for (i in seq_along(classes_x)) {
    class = classes_x[i]
    if (class == 'numeric' || class == 'integer') {
      scaled_x[[i]] = as.numeric(scale(scaled_x[[i]], center = center, scale = scale))
    }
  }

  return(scaled_x)
}

#' Calculate the log-likelihood of normal distribution
#'
#' @param x a numeric vector
#' @param mean mean of the normal distribution
#' @param sd standard deviation of the normal distribution
#' @return log-likelihood value
#' @examples
#' x <- rnorm(100, mean = 5, sd = 2)
#' mean_est <- mean(x)
#' sd_est <- sd(x)
#' log_likelihood_norm(x, mean_est, sd_est)
#'
#' @export
log_likelihood_norm = function(x, mean, sd) {
  log_likelihood = sum(dnorm(x, mean = mean, sd = sd, log = TRUE))
  return(log_likelihood)
}

#' Calculate the log-likelihood of uniform distribution
#'
#' @param x a numeric vector
#' @param min minimum value of the uniform distribution
#' @param max maximum value of the uniform distribution
#' @return log-likelihood value
#' @examples
#' # Generate a sample from a uniform distribution
#' set.seed(42)
#' x <- runif(100, min = 2, max = 5)
#'
#' # Calculate the log-likelihood for the correct uniform distribution
#' log_likelihood_unif(x, min = 2, max = 5)
#'
#' # Calculate the log-likelihood for a different uniform distribution
#' log_likelihood_unif(x, min = 0, max = 10)
#'
#' @export
log_likelihood_unif = function(x, min, max) {
  log_likelihood = sum(dunif(x, min = min, max = max, log = TRUE))
  return(log_likelihood)
}

#' Calculate the log-likelihood of chi-squared distribution
#'
#' @param x a numeric vector
#' @param df degrees of freedom
#' @return log-likelihood value
#' @examples
#' # Generate a sample from a chi-squared distribution
#' set.seed(42)
#' x <- rchisq(100, df = 5)
#'
#' # Calculate the log-likelihood for the correct chi-squared distribution
#' log_likelihood_chisq(x, df = 5)
#'
#' # Calculate the log-likelihood for a different chi-squared distribution
#' log_likelihood_chisq(x, df = 10)
#' @export
log_likelihood_chisq = function(x, df) {
  log_likelihood = sum(dchisq(x, df = df, log = TRUE))
  return(log_likelihood)
}

#' Calculate the log-likelihood of F distribution
#'
#' @param x a numeric vector
#' @param df1 numerator degrees of freedom
#' @param df2 denominator degrees of freedom
#' @return log-likelihood value
#' @examples
#' # Generate a sample from an F distribution
#' set.seed(42)
#' x <- rf(100, df1 = 5, df2 = 10)
#'
#' # Calculate the log-likelihood for the correct F distribution
#' log_likelihood_f(x, df1 = 5, df2 = 10)
#'
#' # Calculate the log-likelihood for a different F distribution
#' log_likelihood_f(x, df1 = 10, df2 = 5)
#'
#' @export
log_likelihood_f = function(x, df1, df2) {
  log_likelihood = sum(df(x, df1 = df1, df2 = df2, log = TRUE))
  return(log_likelihood)
}

#' Calculate the log-likelihood of t distribution
#'
#' @param x a numeric vector
#' @param df degrees of freedom
#' @return log-likelihood value
#' @examples
#' # Generate a sample from a t distribution
#' set.seed(42)
#' x <- rt(100, df = 5)
#'
#' # Calculate the log-likelihood for the correct t distribution
#' log_likelihood_t(x, df = 5)
#'
#' # Calculate the log-likelihood for a different t distribution
#' log_likelihood_t(x, df = 10)
#'
#' @export
log_likelihood_t = function(x, df) {
  log_likelihood = sum(dt(x, df = df, log = TRUE))
  return(log_likelihood)
}

#' Calculate the sensitivity (true positive rate) of predictions
#'
#' @param pred predicted values
#' @param truth true values
#' @return sensitivity value
#' @examples
#' # Define predicted and true values
#' pred <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
#'
#' # Calculate sensitivity
#' sensitivity(pred, truth)
#'
#' @export
sensitivity = function(pred, truth) {
  if (length(pred) != length(truth)) {
    0
  }
  if (is.numeric(pred)) {
    pred = pred == 1
  }
  if (is.numeric(truth)) {
    truth = truth == 1
  }
  (sum(pred & truth) / (sum(pred & truth) + sum(!pred & truth)))
}

#' Calculate the specificity (true negative rate) of predictions
#'
#' @param pred predicted values
#' @param truth true values
#' @return specificity value
#' @examples
#' # Define predicted and true values
#' pred <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
#'
#' # Calculate specificity
#' specificity(pred, truth)
#'
#' @export
specificity = function(pred, truth) {
  if (length(pred) != length(truth)) {
    0
  }
  if (is.numeric(pred)) {
    pred = pred == 1
  }
  if (is.numeric(truth)) {
    truth = truth == 1
  }
  (sum(!pred & !truth) / (sum(!pred & !truth) + sum(pred & !truth)))
}

#' Calculate the precision (positive predictive value) of predictions
#'
#' @param pred predicted values
#' @param truth true values
#' @return precision value
#' @examples
#' # Define predicted and true values
#' pred <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
#'
#' # Calculate precision
#' precision(pred, truth)
#'
#' @export
precision = function(pred, truth) {
  if (length(pred) != length(truth)) {
    0
  }
  if (is.numeric(pred)) {
    pred = pred == 1
  }
  if (is.numeric(truth)) {
    truth = truth == 1
  }
  (sum(pred & truth) / (sum(pred & truth) + sum(pred & !truth)))
}

#' Calculate the recall (sensitivity) of predictions
#'
#' @param pred predicted values
#' @param truth true values
#' @return recall value
#' @examples
#' # Define predicted and true values
#' pred <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
#'
#' # Calculate recall
#' recall(pred, truth)
#'
#' @export
recall = function(pred, truth) {
  if (length(pred) != length(truth)) {
    0
  }
  if (is.numeric(pred)) {
    pred = pred == 1
  }
  if (is.numeric(truth)) {
    truth = truth == 1
  }
  (sum(pred & truth) / (sum(pred & truth) + sum(!pred & truth)))
}

#' Calculate the accuracy of predictions
#'
#' @param pred predicted values
#' @param truth true values
#' @return accuracy value
#' @examples
#' # Define predicted and true values
#' pred <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
#'
#' # Calculate accuracy
#' accuracy(pred, truth)
#'
#' @export
accuracy = function(pred, truth) {
  if (length(pred) != length(truth)) {
    0
  }
  if (is.numeric(pred)) {
    pred = pred == 1
  }
  if (is.numeric(truth)) {
    truth = truth == 1
  }
  return((sum(pred == truth)) / length(pred))
}

#' Calculate the F1 score of predictions
#'
#' @param pred predicted values
#' @param truth true values
#' @return F1 score value
#' @examples
#' # Define predicted and true values
#' pred <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
#'
#' # Calculate F1 score
#' f1(pred, truth)
#'
#' @export
f1 = function(pred, truth) {
  if (length(pred) != length(truth)) {
    return(0)
  }
  if (is.numeric(pred)) {
    pred = pred == 1
  }
  if (is.numeric(truth)) {
    truth = truth == 1
  }
  precision = precision(pred, truth)
  recall = recall(pred, truth)
  if (precision == 0 || recall == 0) {
    return(0)
  }
  return((2 * (precision * recall) / (precision + recall)))
}


#' Calculate the minimum number of samples per group for a t-test
#'
#' @param d effect size
#' @param power desired power (default = 0.8)
#' @return minimum number of samples per group
#' @examples
#' # Define effect size and desired power
#' d <- 0.5
#' power <- 0.9
#'
#' # Calculate minimum number of samples per group
#' minimum_n_per_group(d, power)
#'
#' @export
minimum_n_per_group = function(d, power = 0.8) {
  result =
    power.t.test(
      n = NULL,
      d = d,
      power = power,
      type = "two.sample"
    )
  return(ceiling(result$n))
}

#' Calculate the R-squared value of predictions
#'
#' @param pred predicted values
#' @param truth true values
#' @return R-squared value
#' @examples
#' # Define true and predicted values
#' truth <- c(3, -0.5, 2, 7)
#' pred <- c(2.5, 0.0, 2, 8)
#'
#' # Calculate R-squared value
#' r2(pred, truth)
#'
#' @export
r2 = function(pred, truth) {
  mean_truth = mean(truth)
  total_ss = sum((truth - mean_truth) ^ 2)
  residual_ss = sum((truth - pred) ^ 2)
  r_squared = 1 - (residual_ss / total_ss)

  return(r_squared)
}

#' Calculate the adjusted R-squared value of predictions
#'
#' @param pred predicted values
#' @param truth true values
#' @param n_p number of predictor variables
#' @return adjusted R-squared value
#' @examples
#' # Define true and predicted values
#' truth <- c(3, -0.5, 2, 7)
#' pred <- c(2.5, 0.0, 2, 8)
#' n_p <- 3
#'
#' # Calculate adjusted R-squared value
#' adj_R2(pred, truth, n_p)
#'
#' @export
adj_R2 = function(pred, truth, n_p) {
  r_squared = r2(pred, truth)
  n = length(pred)
  adj_r_squared = 1 - (1 - r_squared) * ((n - 1) / (n - n_p - 1))

  return(adj_r_squared)
}
