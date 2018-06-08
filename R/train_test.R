# create train and testing set

library(devtools)
library(roxygen2)

#' @param file_name name of file inside project folder.
#' @param tr_p percent of data to include in training set.
#' @param te_p percent of data to include in testing set.
#' @return training and testing sets
#' @examples
#' train_test("train_copy.csv", 0.7, 0.3)
#' train_test("train_copy.csv", 0.6, 0.2)
#' train_test("train_copy.csv")
#' train_test("train_copy.csv", -.1, 1.1)
#' train_test("train_copy.csv", 1.1, -.1)
#' @export
train_test <- function(file_name, tr_p = 0, te_p = 0) {
#  if file isn't in r project folder stop("Invalid file name!", call. = FALSE) / HOW DO WE TEST THIS
  if (tr_p == 0 && te_p == 0) {
    tr_p = 0.8
    te_p = 0.2
  }
  if (tr_p < te_p) stop("Invalid percentages: training percentage must be larger than testing percentage.",
                        call. = FALSE)
  if (tr_p + te_p != 1) stop("Invalid percentages: training percentage and testing percentage must add to one.",
                             call. = FALSE)
  if (tr_p < 0 || te_p < 0) stop("Invalid percentages: training percentage and testing percentage must be positive.",
                             call. = FALSE)
  else {
    data <- read.csv(file_name)
    index <- sample(c(T, F), nrow(data), prob = c(tr_p, te_p), replace = TRUE)

    train = data[index,]
    test = data[!index,]

    return(list(train, test))

  }
}
