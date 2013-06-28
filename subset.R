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
# load(file='data/subsetModels.RData')
library(subselect)  # for anneal function
# data set 3 cannot be best subsetted. so we find a represenation of X matrix which is similar to original but smaller in dimension
data.Hmat <- lmHmat(working.data3[, -1], working.data3$y)

annealed.candidate.models <- list(
    model = anneal(cor(working.data3[, 2:ncol(working.data3)]), 1, ncol(working.data3)-2, H=data.Hmat$H, r=data.Hmat$r, crit='tau2'), # tau2 is simply R^2
    working.data = working.data3
)
subset.candidate.models.beta.selectors <- c(
    lapply(subset.candidate.models, function(subset.model) summary(subset.model$model)$which[,-1]),  # we cut off the intercept because lm inserts one by default
    list(t(apply(annealed.candidate.models$model$bestsets, 1, function(row) (1:(ncol(working.data3)-1))%in%row)))
)
subset.candidate.models[[3]] <- annealed.candidate.models
system.time({   # warning! this is slow, especially if not parallel (put num.repetitions higher if you have time)
    best.models <- lapply(1:3, function(data.set.num) {
        subset.model <- subset.candidate.models[[data.set.num]]$model
        working.data <- subset.candidate.models[[data.set.num]]$working.data
        beta.selectors <- subset.candidate.models.beta.selectors[[data.set.num]]
        validation.ratio <- 0.2
        num.folds <- 1/validation.ratio
        num.repetitions <- 10
        mses.best.subset <- k.fold.repeat(num.repetitions, working.data, function(training.data, validation.data, beta.selectors) {
            # 2) compare all resulting models in terms of out of sample predictions
            # --> pick model with lowest average mse over cross validation steps
            models <- apply(beta.selectors, 1, function(beta.selector) lm(as.formula(paste0('y~', paste0(paste0('X', 1:(ncol(training.data)-1))[beta.selector], collapse='+'))), data=training.data))
            mse <- sapply(models, function(model) mean((predict(model, newdata=validation.data) - validation.data$y)^2))
            return(mse)

        }, parallel=T, .export=c('beta.selectors'), beta.selectors=beta.selectors)
        avg.mses.k.fold <- lapply(mses.best.subset, function(mses.one.rep) Reduce('+', mses.one.rep)/(num.folds))  # average mses over the k folds (still a list)
        avg.mse <- Reduce('+', avg.mses.k.fold)/(num.repetitions)  # average over folds and over repetitions, finally no list anymore
        return(avg.mse)
        #return(list(model.num=which(rmse==min(rmse)), rmse=min(rmse), avg.rmse=mean(rmse), beta.selector=beta.selectors[which(rmse==min(rmse)), ]))
    })
})
# plot(best.models[[1]], type='l')  # avg.mse per model num (model size increases by 1 every three steps on the x.axis
beta.selectors <- lapply(1:3, function(data.set.num) subset.candidate.models.beta.selectors[[data.set.num]][which(best.models[[data.set.num]]==min(best.models[[data.set.num]])), ])
chosen.model.nums <- lapply(best.models, function(model.res) which(model.res==min(model.res)))
subset.formulas <- lapply(1:3, function(data.set.num) paste0('y~', paste0(get(paste0('x.vars', data.set.num))[beta.selectors[[data.set.num]]], collapse='+')))
subset.models <- lapply(1:3, function(data.set.num) lm(as.formula(subset.formulas[[data.set.num]]), data=subset.candidate.models[[data.set.num]]$working.data))
