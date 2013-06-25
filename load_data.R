set.seed(20130620)

if (!exists('validation.size'))
    validation.size <- 40
data.set1 <- read.csv('data/ds1.csv')
data.set2 <- read.csv('data/ds2.csv')
data.set3 <- read.csv('data/ds3.csv')

working.data1 <- data.set1[!is.na(data.set1$y),]
working.data2 <- data.set2[!is.na(data.set2$y),]
working.data3 <- data.set3[!is.na(data.set3$y),]

validation.sample <- sample(1:200, validation.size)
validation.data1 <- working.data1[validation.sample, ]
validation.data2 <- working.data2[validation.sample, ]
validation.data3 <- working.data3[validation.sample, ]

training.data1 <- working.data1[-validation.sample, ]
training.data2 <- working.data2[-validation.sample, ]
training.data3 <- working.data3[-validation.sample, ]

prediction.data1 <- data.set1[is.na(data.set1$y),]
prediction.data2 <- data.set2[is.na(data.set2$y),]
prediction.data3 <- data.set3[is.na(data.set3$y),]


####################################### Convenience functions #######################################
sample.k.folds <- function(data.set, k=3) {
    n <- nrow(data.set)
    fold.size <- floor(n/k)  # we potentially loose an some obs here but at least folds are of same size
    return(lapply(1:k, function(sample.num) data.set[sample(n), ][(fold.size*(sample.num-1)+1):(fold.size*sample.num), ]))
}

k.fold <- function(data.set, fun, validation.ratio=0.2, .export=NULL, ...) {  # fun is repeated k times with training.data and validation.data sets being changed every time
    other.args <- list(...)
    for (name in names(other.args)) assign(name, other.args[name])  # dont do this at home, kids!!
    num.folds <- 1/validation.ratio
    samples <- sample.k.folds(data.set, num.folds)
    foreach(validation.sample.index=1:num.folds, .export=.export) %dopar% {  # replace %dopar% with %do% if clusters cannot be used or if on a windows machine

        validation.set <- samples[[validation.sample.index]]
        training.set <- do.call(rbind, samples[-validation.sample.index])

        return(do.call(fun, c(list(training.set, validation.set), other.args)))
    }
}

#k.fold.test <- function() {
#    k.fold(data.frame(c(1,2,3), c(2,3,4)), function(training.data, validation.data, a) {
#        print(a)
#    }, .export=c('a'), a=c('aaa'))
#
#}
