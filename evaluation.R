library(ggplot2)
library(gridExtra)
compare.mse <- function(model1, model2, validation.data=NULL, training.data=NULL) {
    if (!is.null(training.data))
        data <- rbind(cbind(training.data, data.type='training data'), cbind(validation.data, data.type='validation data'))
    else
        data <- cbind(validation.data, data.type='validation.data')  # validation data is necessary
    data <- as.data.frame(data)  # just to be sure its not a matrix
    data$x.axis = 1:(nrow(data))
    data$predictions1 <- predict(model1, newdata=data)
    data$predictions2 <- predict(model2, newdata=data)
    mse1 <- mse(model1, data)
    mse2 <- mse(model2, data)
    return(list(       
        mse1 = mse1,
        mse2 = mse2,
        plots = list(
            ggplot(data=data, aes(x=x.axis)) +
                geom_line(aes(y=y, color=data.type), size=1) +
                geom_line(aes(y=predictions1, color='predictions model 1'), size=1) + 
                scale_colour_manual("", breaks = c('training data', 'validation data', 'predictions model 1'), values = c('darkred', 'purple', 'slateblue3')) +
                ggtitle(paste(paste(model1$call, collapse=' '), 'Mean squared error:', round(mse1, 2))),
            ggplot(data=data, aes(x=x.axis)) +
                geom_line(aes(y=y, color=data.type), size=1) +
                geom_line(aes(y=predictions2, color='predictions model 2'), size=1) + 
                scale_colour_manual("", breaks = c('training data', 'validation data', 'predictions model 2'), values = c('tomato', 'purple', 'slateblue3')) +
                ggtitle(paste(paste(model2$call, collapse= ' '), 'Mean squared error:', round(mse2, 2)))
        )
    ))
}

mse.plot <- function(model, validation.data = NULL, training.data=NULL, title='') {
    if (!is.null(training.data))
        data <- rbind(cbind(training.data, data.type='training data'), cbind(validation.data, data.type='validation data'))
    else
        data <- cbind(validation.data, data.type='validation.data')  # validation data is necessary
    data <- as.data.frame(data)  # just to be sure its not a matrix
    data$x.axis = 1:(nrow(data))
    data$predictions <- predict(model, newdata=data)
    return(ggplot(data=data, aes(x=x.axis)) +
        geom_line(aes(y=y, color=data.type), size=1) +
        geom_line(aes(y=predictions, color='predictions'), size=1) + 
        scale_colour_manual("", breaks = c('training data', 'validation data', 'predictions'), values = c('tomato', 'seagreen3', 'slateblue3')) +
        ggtitle(title) + xlab('observations (unordered)')
    )
}



mse.plot.lars <- function(model, s, validation.data = NULL, training.data=NULL, title='') { # stoopid lars package does not follow naming conventions
    data <- rbind(training.data, validation.data)
    data  <- data.frame(predictions = predict(model, newx=as.matrix(data[, 2:ncol(data)]), s=s)$fit)
    if (!is.null(training.data))
        data <- cbind(data, rbind(cbind(training.data, data.type='training data'), cbind(validation.data, data.type='validation data')))
    else
        data <- cbind(validation.data, data.type='validation.data')  # validation data is necessary
    data <- as.data.frame(data)  # just to be sure its not a matrix
    data$x.axis = 1:(nrow(data))
    return(ggplot(data=data, aes(x=x.axis)) +
        geom_line(aes(y=y, color=data.type), size=1) +
        geom_line(aes(y=predictions, color='predictions'), size=1) + 
        scale_colour_manual("", breaks = c('training data', 'validation data', 'predictions'), values = c('tomato', 'seagreen3', 'slateblue3')) +
        ggtitle(title) + xlab('observations (unordered)')
    )
}


plot.correlograms <- function(data.set, title="") {
    correlograms <- list()
    for (i in 1:(ncol(data.set)-2))  # we leave out last column
        correlograms[[i]] <- ggplot(data=data.set, aes(y=y)) + geom_point(aes_string(x=paste0('X', i+1)))
    correlograms$main <- title
    do.call(grid.arrange, correlograms)
}

plot.predictions <- function(predictions, true.values) {
    plot.data <- data.frame(y=c(true.values, predictions), type=c(rep('training', length(true.values)), rep('predicted', length(predictions))), x=1:(length(predictions) + length(true.values)))
    return(
        ggplot(data=plot.data, aes(x)) +
        geom_line(aes(y=y, color=type))
    )
}

mse <- function(model, validation.data, dependent.var='y', pr.comp=NULL) {
    if (!is.null(pr.comp))  # we have been given a principal components model --> get principal components data
        validation.data <- pr.comp.data(pr.comp, validation.data)
    return(mean((predict(model, newdata=validation.data)-validation.data[, dependent.var])^2))
}

mse.lars <- function(model, validation.data, dependent.var='y') {  # thrice damnation upon the creators of the lars package
    return(mean((predict(model, newx=as.matrix(validation.data[, 2:ncol(validation.data)]))-validation.data[, dependent.var])^2))
}

mses.kfold <- function(model, data, dependent.var='y', validation.ratio=0.2, pr.comp=NULL) { # k.fold cross validation, reports mses for each fold (needs load_data.R)
    if (!is.null(pr.comp))  # we have been given a principal components model --> get principal components data
        data <- pr.comp.data(pr.comp, data)
    k.fold(data, function(training.data, validation.data, model) {
        updated.model <- update(model, .~., data=training.data)  # retrain model
        return(mean((validation.data$y-predict(updated.model, newdata=validation.data))^2))
    }, validation.ratio, .export=c('model'), model=model)
}

mses.kfold.repeat <- function(repeat.num, ...) {
    other.args <- list(...)
    lapply(1:repeat.num, function(cur.repetition) do.call(mses.kfold, other.args))
}

get.mses.kfolded.repeated <- function(num.repetitions = 20, validation.ratio = 0.2) {
    ## TODO: also add the unwieldy models here
    num.folds <- 1/validation.ratio
      # becomes slow fast because it combines k.fold and repetitions
    mses.kfold.repeated <- lapply(1:3, function(model.num) mses.kfold.repeat(num.repetitions, lm(y~., data=training.data[[model.num]]), data=working.data[[model.num]], validation.ratio=validation.ratio))
    fold.averages <- lapply(1:3, function(model.num) lapply(1:num.repetitions, function(rep.num) Reduce('+', mses.kfold.repeated[[model.num]][[rep.num]]) / num.folds))
    avg.mse.ols <- lapply(1:3, function(model.num) Reduce('+', fold.averages[[model.num]]) / num.repetitions)

    mses.kfold.repeated <- lapply(1:3, function(model.num) mses.kfold.repeat(num.repetitions, subset.models[[model.num]], data=working.data[[model.num]], validation.ratio=validation.ratio))
    fold.averages <- lapply(1:3, function(model.num) lapply(1:num.repetitions, function(rep.num) Reduce('+', mses.kfold.repeated[[model.num]][[rep.num]]) / num.folds))
    avg.mse.subset <- lapply(1:3, function(model.num) Reduce('+', fold.averages[[model.num]]) / num.repetitions)

    mses.kfold.repeated <- lapply(1:3, function(model.num) mses.kfold.repeat(num.repetitions, targeted.models[[model.num]], pr.comp=targeted.pr.comp[[model.num]], data=working.data[[model.num]], validation.ratio=validation.ratio))
    fold.averages <- lapply(1:3, function(model.num) lapply(1:num.repetitions, function(rep.num) Reduce('+', mses.kfold.repeated[[model.num]][[rep.num]]) / num.folds))
    avg.mse.targeted <- lapply(1:3, function(model.num) Reduce('+', fold.averages[[model.num]]) / num.repetitions)
    return(lapply(1:3, function(model.num) list(ols=avg.mse.ols[[model.num]], subset=avg.mse.subset[[model.num]], targeted=avg.mse.targeted[[model.num]])))
}
