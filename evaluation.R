library(ggplot2)
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


plot.correlograms <- function(data.set, title="") {
    correlograms <- list()
    for (i in 1:(ncol(data.set)-2))  # we leave out last column
        correlograms[[i]] <- ggplot(data=data.set, aes(y=y)) + geom_point(aes_string(x=paste0('X', i+1)))
    correlograms$main <- title
    do.call(grid.arrange, correlograms)
}

mse <- function(model, data, dependent.var='y') mean((predict(model, newdata=data)-data[, dependent.var])^2)
