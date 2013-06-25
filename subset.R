source('load_data.R')

# best subset
library(leaps)
# 1) run best subset on all 200 datapoints on first two data sets. Data set 3 does not finish because of dimensionality. --> we anneal it to get subsets
system.time(subset.candidate.models <- lapply(list(working.data1, working.data2), function(working.data) {
    return(list(
        model = regsubsets(x=working.data[, 2:ncol(working.data)], y=working.data[, 1], nbest=3, nvmax=ncol(working.data)-1, really.big=T),
        working.data = working.data
    ))
}))
# load(file='subsetModels.RData')
library(subselect)  # for anneal function
# data set 3 cannot be best subsetted. so we find a represenation of X matrix which is similar to original but smaller in dimension
annealed.candidate.models <- list(
    model = anneal(cor(working.data3[, 2:ncol(working.data3)]), 1, ncol(working.data3)-2),
    working.data = working.data3
)
subset.candidate.models.beta.selectors <- c(
    lapply(subset.candidate.models, function(subset.model) summary(subset.model$model)$which[,-1]),  # we cut off the intercept because lm inserts one by default
    list(t(apply(annealed.candidate.models$model$bestsets, 1, function(row) (1:(ncol(working.data3)-1))%in%row)))
)
subset.candidate.models[[3]] <- annealed.candidate.models
system.time({
    library(foreach)
    library(doSNOW)
    cluster <- makeCluster(rep('localhost', 4), type="SOCK", outfile='')  # this might have to be cancelled out or changed accordingly
    registerDoSNOW(cluster)
    best.models <- lapply(1:3, function(data.set.num) {
        subset.model <- subset.candidate.models[[data.set.num]]$model
        working.data <- subset.candidate.models[[data.set.num]]$working.data
        beta.selectors <- subset.candidate.models.beta.selectors[[data.set.num]]
        validation.ratio <- 0.2
        num.folds <- 1/validation.ratio
        mse.best.subset <- k.fold(working.data, function(training.data, validation.data, beta.selectors) {
            # 2) compare all resulting models in terms of out of sample predictions
            # --> pick model with lowest average mse over cross validation steps
            models <- apply(beta.selectors, 1, function(beta.selector) lm(as.formula(paste0('y~', paste0(paste0('X', 1:(ncol(training.data)-1))[beta.selector], collapse='+'))), data=training.data))
            mse <- sapply(models, function(model) mean((predict(model, newdata=validation.data) - validation.data$y)^2))
            return(mse)

        }, .export=c('beta.selectors'), beta.selectors=beta.selectors)
        rmse <- sqrt(Reduce('+', mse.best.subset)/(num.folds))
        return(list(model.num=which(rmse==min(rmse)), rmse=min(rmse), avg.rmse=mean(rmse), beta.selector=beta.selectors[which(rmse==min(rmse)), ]))
    })
    stopCluster(cluster)
})
subset.formulas <- lapply(1:3, function(data.set.num) paste0('y~', paste0('X', (1:(ncol(subset.candidate.models[[data.set.num]]$working.data)-1))[best.models[[data.set.num]]$beta.selector], collapse='+')))
subset.models <- lapply(1:3, function(data.set.num) lm(as.formula(subset.formulas[[data.set.num]]), data=subset.candidate.models[[data.set.num]]$working.data))
