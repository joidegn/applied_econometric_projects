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





## targeted predictors TODO: results are really bad
# (Bai, Ng 2008)
# we will use targeted predictors on the first data set because there we have 'weakly dependent data'
# first augment training data by squares of the elements of training data (Bai, Ng 2008) dont include interactions because it does not increase predictive power with their data
targeted.predictors <- function(training.data, included.variables=NULL, r=2, validation.data=NULL) {  # r is the number of principal components to include, included variables is a vector of columns to include into the regression (e.g. from best subsets)
    if (is.null(included.variables))
        warning('needs a set of variables to include into the model (take e.g. from best subset)')
    params <- paste0('X', 1:(ncol(training.data)-1))
    augmented.data <- cbind(training.data[, 2:ncol(training.data)], X_sq = training.data[, 2:ncol(training.data)]^2)
    pr.comp.targeted <- prcomp(augmented.data, center=T, scale=T)
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
        do.call(grid.arrange, comparison$plots)
    }
    return(targeted.model)
}

# targeted predictor fits and evaluation
best.subset.model1 <- subset.models[[1]]  # models used for comparison and variable extraction
system.time(targeted.model1 <- targeted.predictors(training.data1, included.variables=attr(subset.models[[1]]$terms, 'term.labels'), validation.data=validation.data1, r=2))

best.subset.model2 <- subset.models[[2]]
targeted.model2 <- targeted.predictors(training.data2, included.variables=attr(subset.models[[2]]$terms, 'term.labels'), validation.data=validation.data2, r=40)  # TODO: choose proper r

best.subset.model3 <- subset.models[[3]]
targeted.model3 <- targeted.predictors(training.data3, included.variables=attr(subset.models[[3]]$terms, validation.data=validation.data3, r=2)

#comparison <- compare.mse(best.subset.model3, lm.model3, validation.data=validation.data3)
#do.call(grid.arrange, comparison$plots)
