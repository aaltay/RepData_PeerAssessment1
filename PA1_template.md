
# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
### Load data

```r
library('data.table')
csv <- read.csv('activity.csv')
```
### 2. Preprocessing

```r
csv_good <- csv[!is.na(csv$steps),]
dt <- data.table(csv_good)
sum_steps <- dt[,sum(steps), by='date']
sum_steps <- sum_steps[,steps:=V1]
```


## What is mean total number of steps taken per day?

### 1. Histogram of steps

```r
hist(sum_steps$steps, main="Histogram of Total number of steps", xlab="Steps", ylab="Days")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

### 2. Mean and median

```r
mean_steps <- mean(sum_steps$steps)
median_steps <- median(sum_steps$steps)
```
Mean is 1.0766 &times; 10<sup>4</sup> and median is 10765 steps.

## What is the average daily activity pattern?
### 1. Plot

```r
int_steps <- dt[,mean(steps),by='interval']
int_steps <- int_steps[, mean:=V1]
plot(int_steps$interval, int_steps$mean, type='l', main="Daily activity plot", xlab='5-minute intervals', ylab='Steps Taken')
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 

### 2. Interval with maximum number of steps averaged across all days

```r
max_steps <- int_steps[,max(mean)]
max_interval <- with(int_steps, interval[mean == max(mean)])
```

835 has maximum number of steps with 206.1698 steps among all other 5-minute intervals.


## Imputing missing values

### 1. Total number of NAs

```r
# NA count is the length difference for csv and csv_good, since csv_good does not have NA values.
na_count <- length(csv$steps) - length(csv_good$steps)
```
Data has a total of 2304 NA values.

### 2. Strategy
Replace NA with mean for that 5-minute interval

### 3. Fill NA values

```r
csv_fill <- csv
for(i in 1:length(csv$steps)) { 
    if (is.na(csv$steps[i])) {
        csv_fill$steps[i] <- with(int_steps, mean[interval == csv$interval[i]])
    }
}
# csv_fill is the new data set, generate corresponding data.table
dt_fill <- data.table(csv_fill)
```

### 4. Histogram, mean, median and difference

```r
sum_steps_fill <- dt_fill[,sum(steps), by='date']
sum_steps_fill <- sum_steps_fill[,steps:=V1]
```


```r
hist(sum_steps_fill$steps, main="Histogram of Total number of steps (Filled Data)", xlab="Steps", ylab="Days")
```

![plot of chunk unnamed-chunk-10](./PA1_template_files/figure-html/unnamed-chunk-10.png) 


```r
mean_steps_fill <- mean(sum_steps_fill$steps)
median_steps_fill <- median(sum_steps_fill$steps)
```
Mean is 1.0766 &times; 10<sup>4</sup> and median is 1.0766 &times; 10<sup>4</sup> steps for filled data.

Histogram has a similar shape as the previous one but the center value (median) has a much higher frequency(days) because we added many filled in data with mean values and mean value is already close to median value.

Mean and median values are slightly different but close to original mean since we filled in the missing data with daily mean values.

## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable

```r
# Function to check whether a given date is a weekend day.
isWeekend <- function(date) {
    weekday <- weekdays(as.Date(date))
    weekday == 'Saturday' || weekday == 'Sunday'
}

fw <- factor(c('weekday', 'weekend'))

dayToFactor <- function(date) {
    if (isWeekend(date)) {
        fw[2]
    }
    else {
        fw[1]
    }
}

for(i in 1:length(csv_fill$date)) {
    csv_fill$day[i] <- dayToFactor(csv_fill[i,]$date)
}
dt_fill <- data.table(csv_fill)

# here R console and knit differs. R console requires == fw[2] instead)
weekend_int_steps <- dt_fill[,mean(steps[day == 2]),by='interval']
weekday_int_steps <- dt_fill[,mean(steps[day == 1]),by='interval']
```
### 2. Plot the results

```r
split.screen(c(2,1))
screen(1)
plot(weekend_int_steps$interval, weekend_int_steps$V1, type='l', main='Weekend', xlab='interval', ylab='Number of steps')
screen(2)
plot(weekday_int_steps$interval, weekday_int_steps$V1, type='l', main='Weekday', xlab='interval', ylab='Number of steps')
```

![plot of chunk unnamed-chunk-13](./PA1_template_files/figure-html/unnamed-chunk-13.png) 
