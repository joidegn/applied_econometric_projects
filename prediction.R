source('load_data.R')
#source('exploration.R')  # expects some model to be specified already
#source('pca.R')


#source('subset.R')  # WARNING: takes a while and uses 4 threads using doSNOW, result will be in variables subset.models, subset.formulas and subset.candidate.models
#save(subset.models, subset.candidate.models, subset.formulas, file='bestSubset.RData')
load(file='bestSubset.RData')
ptargetedredictions.subset <- lapply(1:3, function(data.set.num) predict(subset.models[[data.set.num]], newdata=get(paste0('prediction.data', data.set.num))))

source('pca.R')  # needs subset.R to have run but only for comparison of mse
#prediction.results.targeted <- k.fold(working.data1, function(training.data, validation.data) {
#    targeted.predictors(training.data, included.variables=c(), r=2)
#
#})




#predictions.ridge <- k.fold(5, data.set1, function() 1)
#predictions.lasso <- k.fold(5, data.set1, function() 1)
#predictions.ols <- k.fold(5, data.set1, function() 1)


# targeted predictor: best performing model so far
#targeted.model1 <- targeted.predictors(training.data1, included.variables=attr(best.subset.model1$terms, 'term.labels'), r=1)
