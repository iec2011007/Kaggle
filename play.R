library(sqldf)
library(plyr)
setwd('/home/aman/Documents/learn/kaggle/Employee_Access_Challenge/')

train <- read.table('./train.csv', header = TRUE,  colClasses = c('integer'), sep = ",")
test <- read.csv('./test.csv', colClasses)

sum(train$ACTION)/length(train$ACTION) #turns out to be 94.21% hence data totally skewed

#Finding the number of unique levels in each column
uniqueCount = function(column){
  length(unique(column))
}

apply(train, 2, uniqueCount)


#making unique combinations
#i.e. grouping on multiple columns
feature.colnames <- (colnames(train))[-c(1:2)]
feature.colnames

id.num <- 0
train.pids <- ddply(train, feature.colnames, function(x) {
  id.num <<- id.num + 1
  cbind(PID=id.num, x)
})

plot(train$ROLE_TITLE, train$ROLE_CODE)

x <- train
for(i in 1:ncol(x)) {
  the_names <- names(sort(table(train[,i]), decreasing = T)[1:2])
  x[!x[,i] %in% the_names, i] <- 'other'
}
View(x)
