library(doSNOW)  # uncomment for faster calculations
library(foreach)
cluster <- makeCluster(rep('localhost', 4), type="SOCK")  # that wont work on windows, I guess
registerDoSNOW(cluster)
#stopCluster(cluster)

# some convenience functions to reduce code duplication
save.predictions <- function(prediction.list, model.name) {
    completed.data <- lapply(1:3, function(data.set.num) data.frame(y=prediction.list[[data.set.num]], get(paste0('prediction.data', data.set.num))[, 2:ncol(get(paste0('prediction.data', data.set.num)))]))
    lapply(1:3, function(data.set.num) write.csv(completed.data[[data.set.num]], file=paste0('data/predictions/', model.name, data.set.num, '.csv'), row.names=F))
    return(NULL)
}
save.plot <- function(plot.object, filename, type='png', width=1366, height=768) {  # height and width is optimal for pictures, not pdfs
    # type should be a device name (e.g. pdf, png, tiff, etc.)
    get(type)(paste0('graphs/', filename, '.', type), height=height, width=width)
    print(plot.object)
    dev.off()
}

source('load_data.R')  # loads data and prepares sampling functions
source('evaluation.R')  # for the evaluation methods


######################################################## predictions ########################################################

predictions.ols <- lapply(1:3, function(model.num) predict(lm(y~., data=get(paste0('working.data', model.num))), newdata=get(paste0('prediction.data', model.num))))
# plot.predictions(predictions.ols[[3]], working.data3$y)
mse.ols <- mse(lm(y~., data=training.data2), validation.data2)
#save.predictions(predictions.ols, 'ols')
#lapply(1:3, function(model.num) save.plot(plot.predictions(predictions.ols[[model.num]], get(paste0('working.data', model.num))$y), paste0('ols', model.num, '_prediction_plot')))
#lapply(1:3, function(model.num) save.plot(mse.plot(lm(y~., data=get(paste0('training.data', model.num))), training.data=get(paste0('training.data', model.num)), validation.data=get(paste0('validation.data', model.num)), title="Predictions vs training sample and prediction sample"), paste0('ols', model.num, '_validation')))

#source('subset.R')  # WARNING: takes a while and uses threads using doSNOW, result will be in variables subset.models, subset.formulas and subset.candidate.models
#save(subset.models, subset.candidate.models, subset.formulas, file='data/bestSubset.RData')
load(file='data/bestSubset.RData')  # lengthy operations have been precomputed
predictions.subset <- lapply(1:3, function(model.num) predict(subset.models[[model.num]], newdata=get(paste0('prediction.data', model.num))))
# plot.predictions(predictions.subset[[3]], working.data3$y)
mse.subset <- mse(update(subset.models[[2]], .~., data=training.data2), validation.data2)
#save.predictions(predictions.subset, 'bestSubset')
#lapply(1:3, function(model.num) save.plot(plot.predictions(predictions.subset[[model.num]], get(paste0('working.data', model.num))$y), paste0('bestSubset', model.num, '_prediction_plot')))
#lapply(1:3, function(model.num) save.plot(mse.plot(subset.models[[model.num]], training.data=get(paste0('training.data', model.num)), validation.data=get(paste0('validation.data', model.num)), title="Predictions vs training sample and prediction sample"), paste0('bestSubset', model.num, '_validation')))

source('pca.R')  # needs subset.R to have run (or vars loaded) but only for comparison of mse and list of variables
predictions.targeted <- lapply(1:3, function(model.num) predict(targeted.models[[model.num]], newdata=pr.comp.data(targeted.pr.comp[[model.num]], get(paste0('prediction.data', model.num)))))
# plot.predictions(predictions.targeted[[3]], working.data3$y) 
# mse.targeted <- mse(update(targeted.models[[2]], .~., data=pr.comp.data(targeted.pr.comp[[2]], training.data2)), pr.comp.data(targeted.pr.comp[[2]], validation.data2))
save.predictions(predictions.targeted, 'targetedPredictors')
#lapply(1:3, function(model.num) save.plot(plot.predictions(predictions.targeted[[model.num]], get(paste0('working.data', model.num))$y), paste0('targeted', model.num, '_prediction_plot')))
#lapply(1:3, function(model.num) save.plot(mse.plot(targeted.models[[model.num]], training.data=pr.comp.data(targeted.pr.comp[[model.num]], get(paste0('training.data', model.num))), validation.data=pr.comp.data(targeted.pr.comp[[model.num]], get(paste0('validation.data', model.num))), title="Predictions vs training sample and prediction sample"), paste0('targeted', model.num, '_validation')))

source('lars.R')
predictions.lasso <- lapply(1:3, function(model.num) predict(lasso.models[[model.num]], newx=as.matrix(get(paste0('prediction.data', model.num))[,2: ncol(get(paste0('prediction.data', model.num)))]), s=lasso.optimal.s[[model.num]])$fit)
# plot.predictions(predictions.lasso[[2]], working.data2$y) 
save.predictions(predictions.lasso, 'lasso')
#lapply(1:3, function(model.num) save.plot(plot.predictions(predictions.lasso[[model.num]], get(paste0('working.data', model.num))$y), paste0('lasso', model.num, '_prediction_plot')))
#lapply(1:3, function(model.num) save.plot(mse.plot.lars(lasso.models[[model.num]], s=lasso.optimal.s[[model.num]], training.data=get(paste0('training.data', model.num)), validation.data=get(paste0('validation.data', model.num)), title="Predictions vs training sample and prediction sample"), paste0('lasso', model.num, '_validation')))

predictions.lar <- lapply(1:3, function(model.num) predict(lar.models[[model.num]], newx=as.matrix(get(paste0('prediction.data', model.num))[, 2: ncol(get(paste0('prediction.data', model.num)))]), s=lasso.optimal.s[[model.num]])$fit)
# plot.predictions(predictions.lar[[3]], working.data3$y) 
save.predictions(predictions.lar, 'lar')
#lapply(1:3, function(model.num) save.plot(plot.predictions(predictions.lar[[model.num]], get(paste0('working.data', model.num))$y), paste0('lasso', model.num, '_prediction_plot')))
#lapply(1:3, function(model.num) save.plot(mse.plot.lars(lar.models[[model.num]], s=lar.optimal.s[[model.num]], training.data=get(paste0('training.data', model.num)), validation.data=get(paste0('validation.data', model.num)), title="Predictions vs training sample and prediction sample"), paste0('lar', model.num, '_validation')))

#predictions.ols <- k.fold(5, data.set1, function() 1)
source('ridge.R')
predictions.ridge <- lapply(1:3, function(model.num) predict(ridge.models[[model.num]], newdata=get(paste0('prediction.data', model.num))))
# plot.predictions(predictions.ridge[[3]], working.data3$y) 
# mse(ridge.models[[1]], working.data1)
save.predictions(predictions.ridge, 'ridge')
#lapply(1:3, function(model.num) save.plot(plot.predictions(predictions.ridge[[model.num]], get(paste0('working.data', model.num))$y), paste0('ridge', model.num, '_prediction_plot')))
#lapply(1:3, function(model.num) save.plot(mse.plot(ridge.models[[model.num]], training.data=get(paste0('training.data', model.num)), validation.data=get(paste0('validation.data', model.num)), title="Predictions vs training sample and prediction sample"), paste0('ridge', model.num, '_validation')))


source('randomForest.R')
predictions.random.forests <- lapply(1:3, function(model.num) predict(random.forest.models[[model.num]], newdata=get(paste0('prediction.data', model.num))))
# plot.predictions(predictions.random.forests[[1]], working.data1$y) 
# mse(random.forest.models[[1]], working.data1)
save.predictions(predictions.random.forests, 'randomForests')
#lapply(1:3, function(model.num) save.plot(plot.predictions(predictions.random.forests[[model.num]], get(paste0('working.data', model.num))$y), paste0('randomForests', model.num, '_prediction_plot')))
#lapply(1:3, function(model.num) save.plot(mse.plot(random.forest.models[[model.num]], training.data=get(paste0('training.data', model.num)), validation.data=get(paste0('validation.data', model.num)), title="Predictions vs training sample and prediction sample"), paste0('randomForests', model.num, '_validation')))


######################################################## evaluations ########################################################

avg.mses.kfolded.repeated <- get.mses.kfolded.repeated(100)
#stopCluster(cluster)
