source('load_data.R')

### principal components:
#pr.comp1 <- prcomp(training.data1[, 2:ncol(training.data1)-1], center=T, scale=T)  # we center and scale the variables before pca
## --> it seems the first principal component captures a huge amount of the variation (0.5161) (Bai, Ng 2008) use targeted predictors on weakly dependent data
#
## this is in line with the correlation findings from above
#pr.comp2 <- prcomp(training.data2[, 2:ncol(training.data2)-1], center=T, scale=T)  # we center and scale the variables before pca
## --> again the first principal componen catches a lot but not as much as with first data set (0.1513)
## this may be in line with the correlation findings from above
##biplot(pr.comp2)
##plot(training.data2$y, pr.comp2$x[, 1])
#pr.comp3 <- prcomp(training.data3[, 2:ncol(training.data3)-1], center=T, scale=T)  # we center and scale the variables before pca
## --> pca does not yield very important principal components. correlation of y with all covariates is low as well





## targeted predictors
# (Bai, Ng 2008)
# we will use targeted predictors on the first data set because there we have 'weakly dependent data'
# first augment training data by squares of the elements of training data (Bai, Ng 2008) dont include interactions because it does not increase predictive power with their data
targeted.predictors <- function(training.data, included.variables=NULL, r=2, validation.data=NULL) {  # r is the number of principal components to include, included variables is a vector of columns to include into the regression (e.g. from best subsets)
    if (is.null(included.variables))
        warning('needs a set of variables to include into the model (take e.g. from best subset)')
    params <- paste0('X', 1:(ncol(training.data)-1))
    augmented.data <- cbind(training.data[, 2:ncol(training.data)], X_sq = training.data[, 2:ncol(training.data)]^2)
    pr.comp.targeted <- prcomp(augmented.data, center=T, scale=T)
    if (r==1)  # R changes the column naming scheme if one column is selected
        forecasting.data <- cbind(training.data[, c('y', included.variables)], PC1=pr.comp.targeted$x[, 1:r])
    else
        forecasting.data <- cbind(training.data[, c('y', included.variables)], pr.comp.targeted$x[, 1:r])
    targeted.model <- lm(y~., data=forecasting.data)
    # validation (should be moved to its own function but then we have to return the rotation matrix)
    if (!is.null(validation.data)) {
        augmented.validation.data <- cbind(validation.data[, 2:ncol(validation.data)], X.sq=validation.data[, 2:ncol(validation.data)]^2)
        augmented.validation.data <- scale(augmented.validation.data, center=pr.comp.targeted$center, scale=pr.comp.targeted$scale)
        augmented.validation.data <- cbind(augmented.validation.data, augmented.validation.data %*% pr.comp.targeted$rotation)
        augmented.validation.data <- as.data.frame(augmented.validation.data)
        # compare to simply using `included.variables` which should be e.g. the best subset coefficients
        comparison.model <- lm(as.formula(paste0('y~', paste(included.variables, collapse='+'))), data=training.data)
        comparison <- compare.mse(targeted.model, comparison.model, validation.data=cbind(y=validation.data$y, augmented.validation.data))
        library(gridExtra)
        do.call(grid.arrange, comparison$plots)
    }
    return(list(
        resulting.model = targeted.model,
        pr.comp = pr.comp.targeted
    ))
}


pr.comp.data <- function(pr.comp, data) {  # augments data matrix with principal component terms: centers, scales and rotates original matrix
    augmented.data <- cbind(data[2:ncol(data)], data[2:ncol(data)]^2)
    scaled.data <- scale(augmented.data, center=pr.comp$center, scale=pr.comp$scale)
    return(as.data.frame(cbind(data, scaled.data %*% pr.comp$rotation)))
}

# targeted predictor fits and evaluation
#system.time(targeted.predictor1 <- targeted.predictors(training.data1, included.variables=attr(subset.models[[1]]$terms, 'term.labels'), validation.data=validation.data1, r=2))
#targeted.model1 <- targeted.predictor1$resulting.model
#
#system.time(targeted.predictor2 <- targeted.predictors(training.data2, included.variables=attr(subset.models[[2]]$terms, 'term.labels'), validation.data=validation.data2, r=2))
#targeted.model2 <- targeted.predictor2$resulting.model
#
#system.time(targeted.predictor3 <- targeted.predictors(training.data3, included.variables=attr(subset.models[[3]]$terms, 'term.labels'), validation.data=validation.data3, r=2))
#targeted.model3 <- targeted.predictor3$resulting.model


# TODO: fix validation ratio and maybe repeat k.folds multiple times because results vary a lot. --> for 5 repetitions they still do
system.time( # do cross validation to find optimal r (number of principal components to include) in a less-depent-on-sampling-way
    targeted.predictor.results <- lapply(1:3, function(model.num) {
        working.data <- get(paste0('working.data', model.num))
        included.variables <- attr(subset.models[[model.num]]$terms, 'term.labels')
        repetitions <- 5  # only use higher value if you have a lot of time ((ncol(data)-1)*5*(1/validation.ratio)) iterations
        mse.values <- lapply(1:(ncol(working.data)-1), function(cur.r)  # get mse values for different r values. cross validation to rule out randomness
            k.fold.repeat(repetitions, data=working.data, function(training.data, validation.data, included.variables, targeted.predictors, pr.comp.data=pr.comp.data) {
                targeted <- targeted.predictors(training.data, included.variables=included.variables, r=cur.r)
                return(mean((predict(targeted$resulting.model, newdata=pr.comp.data(targeted$pr.comp, validation.data))-validation.data$y)^2))
            }, validation.ratio=0.1, parallel=T, .export=c('included.variables', 'targeted.predictors', 'pr.comp.data'), included.variables=included.variables, targeted.predictors=targeted.predictors, pr.comp.data=pr.comp.data)
        )
        # mse.values is a list of lists of lists
        avg.folds <- lapply(mse.values, function(model) lapply(model, function(repetition) Reduce('+', repetition)/(length(repetition))))
        avg.folds.repetitions <- sapply(avg.folds, function(model) Reduce('+', model)/repetitions)
        return(avg.folds.repetitions)  # report mean mse over k.folds for all models
    })
)
optimal.r <- lapply(targeted.predictor.results, function(targeted.pred) which(min(targeted.pred) == targeted.pred))


targeted.predictor.results <- lapply(1:3, function(model.num) return(targeted.predictors(get(paste0('working.data', model.num)), included.variables=attr(subset.models[[model.num]]$terms, 'term.labels'), r=optimal.r[[model.num]])))

# mses.targeted <- sapply(1:1000, function(num) do.call(mean, mses.kfold(targeted.predictor.results[[1]]$resulting.model, working.data1, pr.comp=targeted.predictor.results[[1]]$pr.comp)))
# mses.subset <- sapply(1:1000, function(num) do.call(mean, mses.kfold(subset.models[[1]], working.data1)))
targeted.models <- lapply(targeted.predictor.results, function(predictor.results) predictor.results$resulting.model)
targeted.pr.comp <- lapply(targeted.predictor.results, function(predictor.results) predictor.results$pr.comp)


#comparison <- compare.mse(subset.models[[3]], targeted.predictor.results[[3]]$resulting.model, validation.data=cbind(working.data3$y, pr.comp.data(targeted.predictor.results[[3]]$pr.comp, working.data3)))
#do.call(grid.arrange, comparison$plots)
