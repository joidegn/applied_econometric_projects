library(doSNOW)  # uncomment for faster calculations
library(foreach)
cluster <- makeCluster(rep('localhost', 4), type="SOCK", outfile='')
registerDoSNOW(cluster)
#stopCluster(cluster)


source('load_data.R')


predictions.ols <- lapply(1:3, function(model.num) predict(lm(y~., data=get(paste0('working.data', model.num))), newdata=get(paste0('prediction.data', model.num))))
# plot.predictions(predictions.ols[[3]], working.data3$y)
# mse(lm(y~., data=working.data3), working.data3)

#source('subset.R')  # WARNING: takes a while and uses 4 threads using doSNOW, result will be in variables subset.models, subset.formulas and subset.candidate.models
#save(subset.models, subset.candidate.models, subset.formulas, file='data/bestSubset.RData')
load(file='data/bestSubset.RData')
predictions.subset <- lapply(1:3, function(model.num) predict(subset.models[[model.num]], newdata=get(paste0('prediction.data', model.num))))
# plot.predictions(predictions.subset[[3]], working.data3$y)
# mse(subset.models[[3]], working.data3)

source('pca.R')  # needs subset.R to have run (or vars loaded) but only for comparison of mse and list of variables
predictions.targeted <- lapply(1:3, function(model.num) predict(targeted.models[[model.num]], newdata=pr.comp.data(targeted.pr.comp[[model.num]], get(paste0('prediction.data', model.num)))))
# plot.predictions(predictions.targeted[[3]], working.data3$y) 
# mse(targeted.models[[3]], pr.comp.data(targeted.pr.comp[[3]], working.data3))

source('lars.R')
#predictions.lasso <- k.fold(5, data.set1, function() 1)
#predictions.ols <- k.fold(5, data.set1, function() 1)
source('ridge.R')
predictions.ridge <- lapply(1:3, function(model.num) predict(ridge.models[[model.num]], newdata=get(paste0('prediction.data', model.num))))


# targeted predictor: best performing model so far
#targeted.model1 <- targeted.predictors(training.data1, included.variables=attr(best.subset.model1$terms, 'term.labels'), r=1)
