library(dplyr)
library(MASS)
library(neuralnet)

sumo.read <- function(csv) {
  data <- tibble(read.csv(csv)) %>%
    mutate_all(as.numeric) %>%
    mutate(result = as.logical(result))
  return(data)
}

data <- sumo.read('sumo.csv')
history <- na.omit(data)
undecided <- filter(data, is.na(result))

nn <- neuralnet(result ~ ., history, hidden = 8)
bin <- glm(result ~ ., history, family = 'binomial')

bin.ans <- predict.glm(bin, undecided)
nn.ans <- predict(nn, undecided)

if (bin.ans > 0) {
  print('GLM says left.') 
} else {print('GLM says right.')}
bin.ans

if (nn.ans >= 0.5) {
  print('NN says left.') 
} else {print('NN says right.')}
nn.ans