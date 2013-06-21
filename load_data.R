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


sample.k.folds <- function(data.set, k=3) {
    n <- nrow(data.set)
    fold.size <- floor(n/k)  # we potentially loose an some obs here but at least folds are of same size
    return(lapply(1:k, function(sample.num) data.set[sample(n), ][(fold.size*(sample.num-1)+1):(fold.size*sample.num), ]))
}
