source('load_data.R')
library(ridge)

ridge.models <- lapply(1:3, function(model.num) linearRidge(y~., data=get(paste0('working.data', model.num))))


