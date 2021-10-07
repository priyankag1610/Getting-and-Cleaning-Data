library(data.table)
library(dplyr)

# create train dataset

a <- fread('train/X_train.txt')

c <- fread('train/y_train.txt')

d <- cbind(a, c)

e <- fread('train/subject_train.txt')

f <- cbind(e, d)

f <- cbind(e, d)

x <- read.table('features.txt', col.names = c('MeasureID', 'MeasureName'))

names(f)[1:2] <- c('subject', 'activity')
names(f)[3:563] <- as.character(x$MeasureName)

subset_x <- grep(".*mean\\(\\)|.*std\\(\\)", x$MeasureName)

h <- subset_x + 2

y <- f[, h]

y <- f[, c(3, 4, 5, 6, 7, 8, 43, 44, 45, 46, 47, 48, 83, 84, 85, 86, 87, 88, 123, 124, 125, 126, 127, 128, 163, 164, 165, 166, 167, 168, 203, 204, 216, 217, 229, 230, 242, 243, 255, 256, 268, 269, 270, 271, 272, 273, 347, 348, 349, 350, 351, 352, 426, 427, 428, 429, 430, 431, 505, 506, 518, 519, 531, 532, 544, 545)]

dat_train <- cbind(f[,1:2], y)

#create test dataset
b <- fread('test/X_test.txt')

k <- fread('test/y_test.txt')

l <- cbind(k, b)

m <- fread('test/subject_test.txt')

n <- cbind(m, l)

x <- read.table('features.txt', col.names = c('MeasureID', 'MeasureName'))

names(n)[1:2] <- c('subject', 'activity')
names(n)[3:563] <- as.character(x$MeasureName)

z <- n[, c(3, 4, 5, 6, 7, 8, 43, 44, 45, 46, 47, 48, 83, 84, 85, 86, 87, 88, 123, 124, 125, 126, 127, 128, 163, 164, 165, 166, 167, 168, 203, 204, 216, 217, 229, 230, 242, 243, 255, 256, 268, 269, 270, 271, 272, 273, 347, 348, 349, 350, 351, 352, 426, 427, 428, 429, 430, 431, 505, 506, 518, 519, 531, 532, 544, 545)]

dat_test <- cbind(n[,1:2], z)

## join train and test datasets

full <- rbind(dat_train, dat_test)

## add activity labels

lab <- read.table("activity_labels.txt", col.names = c('activityID', 'activityName'))

names(full)[2] <- c('activityID')

new_full <- merge(full, lab)

## tidy names of columns

cnames <- colnames(new_full)

cnames1 <- gsub("-mean.+-", "Mean", cnames)
cnames2 <- gsub("-std.+-", "Std", cnames1)
cnames3 <- gsub("-std.+", "Std", cnames2)
cnames4 <- gsub("-mean.+", "Mean", cnames3)
cnames5 <- gsub("f", "Frequency", cnames4)
cnames6 <- gsub("^t", "Time", cnames5)
cnames7 <- gsub("Mag", "Magnitude", cnames6)
cnames8 <- gsub("Acc", "Accelerator", cnames7)
cnames9 <- gsub("Gyro", "Gyroscope", cnames8)
colnames(new_full) <- cnames9

## calculate average for each subject and activity
full_short <- select(new_full, -activityID)

h <- full_short$activityName

datset <- cbind(h, full_short)

names(datset)[1] <- c('activity')

datset$subject <- as.factor(datset$subject)

m_data <- melt(datset, id.vars = c('activity', 'subject'))

m_data$value <- as.numeric(m_data$value)

average <- dcast(m_data, subject + activity ~ variable, mean)

## write new tidy file

write.table(average, "tidy dataset.txt", quote = FALSE, row.names = FALSE)
