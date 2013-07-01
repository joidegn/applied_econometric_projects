library(doSNOW)  # uncomment for faster calculations
library(foreach)
cluster <- makeCluster(rep('localhost', 4), type="SOCK", outfile='')  # that wont work on windows, I guess
registerDoSNOW(cluster)
#stopCluster(cluster)

save.predictions <- function(prediction.list, model.name) {
    completed.data <- lapply(1:3, function(data.set.num) data.frame(y=prediction.list[[data.set.num]], get(paste0('prediction.data', data.set.num))[, 2:ncol(get(paste0('prediction.data', data.set.num)))]))
    lapply(1:3, function(data.set.num) write.csv(completed.data[[data.set.num]], file=paste0('data/predictions/', model.name, data.set.num, '.csv'), row.names=F))
    return(NULL)
}

source('load_data.R')  # loads data and prepares sampling functions
source('evaluation.R')  # for the evaluation methods


predictions.ols <- lapply(1:3, function(model.num) predict(lm(y~., data=get(paste0('working.data', model.num))), newdata=get(paste0('prediction.data', model.num))))
# plot.predictions(predictions.ols[[3]], working.data3$y)
# mse(lm(y~., data=working.data1), working.data1)
save.predictions(predictions.ols, 'ols')

#source('subset.R')  # WARNING: takes a while and uses threads using doSNOW, result will be in variables subset.models, subset.formulas and subset.candidate.models
#save(subset.models, subset.candidate.models, subset.formulas, file='data/bestSubset.RData')
load(file='data/bestSubset.RData')
predictions.subset <- lapply(1:3, function(model.num) predict(subset.models[[model.num]], newdata=get(paste0('prediction.data', model.num))))
# plot.predictions(predictions.subset[[3]], working.data3$y)
# mse(subset.models[[1]], working.data1)
save.predictions(predictions.subset, 'bestSubset')

source('pca.R')  # needs subset.R to have run (or vars loaded) but only for comparison of mse and list of variables
predictions.targeted <- lapply(1:3, function(model.num) predict(targeted.models[[model.num]], newdata=pr.comp.data(targeted.pr.comp[[model.num]], get(paste0('prediction.data', model.num)))))
# plot.predictions(predictions.targeted[[3]], working.data3$y) 
# mse(targeted.models[[1]], pr.comp.data(targeted.pr.comp[[1]], working.data1))
save.predictions(predictions.targeted, 'targetedPredictors')

source('lars.R')
predictions.lasso <- lapply(1:3, function(model.num) predict(lasso.models[[model.num]], newx=get(paste0('prediction.data', model.num))[2: ncol(get(paste0('prediction.data', model.num)))], s=lasso.optimal.s[[model.num]])$fit)
# plot.predictions(predictions.lasso[[2]], working.data2$y) 
save.predictions(predictions.lasso, 'lasso')

#predictions.ols <- k.fold(5, data.set1, function() 1)
source('ridge.R')
predictions.ridge <- lapply(1:3, function(model.num) predict(ridge.models[[model.num]], newdata=get(paste0('prediction.data', model.num))))
# plot.predictions(predictions.ridge[[3]], working.data3$y) 
# mse(ridge.models[[1]], working.data1)
save.predictions(predictions.ridge, 'ridge')

predictions.lar <- lapply(1:3, function(model.num) predict(lar.models[[model.num]], newdata=get(paste0('prediction.data', model.num))))
# plot.predictions(predictions.lar[[3]], working.data3$y) 
save.predictions(predictions.lar, 'lar')


source('randomForest.R')
predictions.random.forests <- lapply(1:3, function(model.num) predict(random.forest.models[[model.num]], newdata=get(paste0('prediction.data', model.num))))
# plot.predictions(predictions.random.forests[[1]], working.data1$y) 
# mse(random.forest.models[[1]], working.data1)
save.predictions(predictions.random.forests, 'randomForests')



stopCluster(cluster)
