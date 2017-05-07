##Install packages
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)

##Set wd
setwd("C:\\Users\\Tiago\\Documents\\DataScienceSpec\\GetAndCleanData\\Project")
path <- getwd()

##Get the data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Data.zip"
if (!file.exists(path)) {
  dir.create(path)
}
download.file(url, file.path(path, f))

##Unzip the downloaded file
executable <- file.path("C:", "Program Files (x86)", "WinRAR", "WinRAR.exe")
parameters <- "x"
cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path,f), "\""))
system(cmd)

pathIn <- file.path(path, "UCI HAR Dataset")
files <- list.files(pathIn, recursive=TRUE)

## Read the datasets
dttrain.subject <- fread(file.path(pathIn, "train", "subject_train.txt"))
dttest.subject <- fread(file.path(pathIn, "test" , "subject_test.txt" ))

dttrain.activity <- fread(file.path(pathIn, "train", "Y_train.txt"))
dttest.activity <- fread(file.path(pathIn, "test" , "Y_test.txt" ))


fileToDataTable <- function (f) {
  df <- read.table(f)
  dt <- data.table(df)
}

dttrain.x <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dttest.x  <- fileToDataTable(file.path(pathIn, "test" , "X_test.txt" ))


##COncatenate data tables
dtSubject <- rbind(dttrain.subject, dttest.subject)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dttrain.activity, dttest.activity)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dttrain.x, dttest.x)

##Merge the columns
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)


setkey(dt, subject, activityNum)

##Extract mean and standard deviation
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]

select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]

##Use descriptive activity names

activitylabels <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(activitylabels, names(activitylabels), c("activityNum", "activityName"))


dt <- merge(dt, activitylabels, by="activityNum", all.x=TRUE)

setkey(dt, subject, activityNum, activityName)

dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)


##Seperate features from featureName using the helper function featname.

featname <- function (regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(featname("^t"), featname("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(featname("Acc"), featname("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(featname("BodyAcc"), featname("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(featname("mean()"), featname("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(featname("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(featname("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(featname("-X"), featname("-Y"), featname("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))



r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

##Create a tidy data set
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

f <- file.path(path, "DatasetHumanActivityRecognitionUsingSmartphones.txt")
write.table(dtTidy, f, quote = FALSE, sep = "\t", row.names = FALSE)
