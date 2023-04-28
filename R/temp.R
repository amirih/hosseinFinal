library(tibble)
library(testthat)

x_vec = 1:10
y_hat_vec = 3*x_vec + 4
y_vec = y_hat_vec + 0.2*rnorm(length(x_vec))

x_df = tibble(a = x_vec, b = x_vec + 3, c = factor(a))

classes = function(x) {
  if (is.data.frame(x) || is_tibble(x)) {
    classes = c(sapply(x, class))
    return (classes)

  }
  else {
    return(class(x))
  }
}

x_df_classes = classes(x_df)

test_that("classes works",{
  x_df_classes = classes(x_df)
  expect_equal(length(x_df_classes), 3)
  expect_equivalent(x_df_classes[3],"factor")
})
