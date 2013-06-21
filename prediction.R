library(foreach)
library(doSNOW)
cluster <- makeCluster(rep('localhost', 4), type="SOCK", outfile='')  # this might have to be cancelled out or changed accordingly
registerDoSNOW(cluster)



source('load_data.R')
#source('exploration.R')  # expects some model to be specified already
#source('pca.R')


k.fold <- function(data.set, fun, validation.ratio=0.2, .export=c()) {  # fun is repeated k times with training.data and validation.data sets being changed every time
    num.folds <- 1/validation.ratio
    samples <- sample.k.folds(data.set, num.folds)
    foreach(validation.sample.index=1:num.folds, .export=.export) %dopar% {  # replace %dopar% with %do% if clusters cannot be used or if on a windows machine

        validation.set <- samples[[validation.sample.index]]
        training.set <- do.call(rbind, samples[-validation.sample.index])

        return(do.call(fun, list(training.set, validation.set)))

    }
}

#prediction.results.targeted <- k.fold(working.data1, function(training.data, validation.data) {
#    targeted.predictors(training.data, included.variables=c(), r=2)
#
#})

# best subset
library(leaps)
# 1) run best subset on all 200 datapoints on all 3 data sets WARNING!! VERY SLOW
subset.models <- lapply(list(working.data1, working.data2, working.data3), function(working.data) {
    return(list(
        model = regsubsets(x=working.data[, 2:ncol(working.data)], y=working.data[, 1], nbest=3, nvmax=ncol(working.data)-1, really.big=T),
        working.data = working.data
    ))
})
best.models <- lapply(subset.models, function(model.set) {
    subset.model <- model.set$model
    working.data <- model.set$working.data
    validation.ratio <- 0.2
    num.folds <- 1/validation.ratio
    models.subset <- lapply(1:nrow(summary(subset.model)$which), function(model.num) paste0('X', 1:((ncol(working.data)-1)))[summary(subset.model)$which[model.num, -1]])
    mse.best.subset <- k.fold(working.data, function(training.data, validation.data) {
        # 2) compare all resulting models in terms of out of sample predictions
        # --> pick model with lowest average mse over cross validation steps
        models <- lapply(models.subset, function(model) lm(as.formula(paste0('y~', paste0(model, collapse='+'))), data=training.data))
        mse <- sapply(models, function(model) mean((predict(model, newdata=validation.data) - validation.data$y)^2))
        return(mse)

    }, .export=c('models.subset1'))
    rmse <- sqrt(Reduce('+', mse.best.subset)/(num.folds))
    return(summary(subset.model)$which[which(min(rmse)==rmse),-1])
})




#prediction.results.ridge <- k.fold(5, data.set1, function() 1)
#prediction.results.lasso <- k.fold(5, data.set1, function() 1)
#prediction.results.ols <- k.fold(5, data.set1, function() 1)

stopCluster(cluster)

# targeted predictor: best performing model so far
#targeted.model1 <- targeted.predictors(training.data1, included.variables=attr(best.subset.model1$terms, 'term.labels'), r=1)
