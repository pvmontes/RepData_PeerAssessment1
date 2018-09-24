1. Code for reading in the dataset and/or processing the data
-------------------------------------------------------------

First of all we indicate where the file is, (we have decomprissed
previously that file).

    setwd("/Users/purificacionvelascomontes/Documents/Personal/Cursos/Coursera_Data_Science/Reproducible_Research_(5)/Week_2/Assignment")
    archivo_entrada <- read.csv("activity.csv")

We change the format of date field to date format, for next tasks.

    archivo_entrada_date <- archivo_entrada
    archivo_entrada_date$date <- as.Date(archivo_entrada_date$date, format = "%Y-%m-%d")

2. Histogram of the total number of steps taken each day
--------------------------------------------------------

For this task we are going to need the libraries ggplot and dplyr.

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)

We calculate the total number of steps, and we remove the NA values.

    steps_per_day <- 
      archivo_entrada %>%
      group_by(date) %>%
      summarize(number_steps = sum(steps, na.rm = TRUE))

We add a new column to "archivo\_entrada" data frame (which is a copy of
"date" column), in date format.

    steps_date <- as.Date(archivo_entrada$date, format = "%Y-%m-%d")
    archivo_entrada$date_date <- steps_date
    steps_per_day_date <- steps_per_day

We are going to draw the histogram; (with the next sentence, we do in
axis **X** we see only some dates with correct format).

    steps_per_day_date$date <- as.Date(steps_per_day_date$date, format = "%Y-%m-%d")

    ggplot(data = steps_per_day_date, fill = "blue") +
      geom_bar(mapping = aes(x = date, y = number_steps), stat = "identity", col = c("orange"), fill = c("navy")) +
      xlab("Date") + ylab("Number of steps") + labs(title = "Number of steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

3. Mean and median number of steps taken each day
-------------------------------------------------

We can see the mean and median for all the days.

    mean_overall <- mean(steps_per_day$number_steps)
    mean_overall

    ## [1] 9354.23

    median(steps_per_day$number_steps)

    ## [1] 10395

Or the mean and median for each day

    mean_per_day <- 
      archivo_entrada %>%
      group_by(date) %>%
      filter(!is.na(steps)) %>%  
      summarize(mean_steps = sum(steps)/mean_overall)

    median_per_day <- 
      archivo_entrada %>%
      group_by(date) %>%
      filter(!is.na(steps)) %>%
      summarize(median_steps = sum(steps)/2)

    mean_per_day

    ## # A tibble: 53 x 2
    ##    date       mean_steps
    ##    <fct>           <dbl>
    ##  1 2012-10-02     0.0135
    ##  2 2012-10-03     1.21  
    ##  3 2012-10-04     1.30  
    ##  4 2012-10-05     1.42  
    ##  5 2012-10-06     1.65  
    ##  6 2012-10-07     1.18  
    ##  7 2012-10-09     1.37  
    ##  8 2012-10-10     1.06  
    ##  9 2012-10-11     1.10  
    ## 10 2012-10-12     1.86  
    ## # ... with 43 more rows

    median_per_day

    ## # A tibble: 53 x 2
    ##    date       median_steps
    ##    <fct>             <dbl>
    ##  1 2012-10-02         63.0
    ##  2 2012-10-03       5676. 
    ##  3 2012-10-04       6058. 
    ##  4 2012-10-05       6647. 
    ##  5 2012-10-06       7710. 
    ##  6 2012-10-07       5508. 
    ##  7 2012-10-09       6406. 
    ##  8 2012-10-10       4950. 
    ##  9 2012-10-11       5152. 
    ## 10 2012-10-12       8691. 
    ## # ... with 43 more rows

4. Time series plot of the average number of steps taken
--------------------------------------------------------

    mean_per_day_interval <- 
      archivo_entrada %>%
      group_by(interval) %>%
      summarize(avrg_steps = mean(steps, na.rm = TRUE))

    ggplot(mean_per_day_interval, aes(interval, avrg_steps)) + 
      geom_line(color= "blue") +
      xlab("Interval") + ylab("Number of steps taken") + 
      labs(title = "Time series of the average number of steps taken per interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

5. The 5-minute interval that, on average, contains the maximum number of steps.
--------------------------------------------------------------------------------

    max_number_of_steps <- max(mean_per_day_interval$avrg_steps)
    max_row <- mean_per_day_interval[which(mean_per_day_interval$avrg_steps == max_number_of_steps),]
    max_interval <- max_row$interval
    max_interval

    ## [1] 835

6. Code to describe and show a strategy for imputing missing data.
------------------------------------------------------------------

1.  Total number of missing values in the dataset The steps is the
    unique field that has NA values.

<!-- -->

    sum(is.na(archivo_entrada$steps))

    ## [1] 2304

1.  Strategy for filling in all of the missing values in the dataset. My
    strategy is to use the mean steps per interval to filling the
    missing values.  
    First, we copy the original data frame.

<!-- -->

    archivo_entrada_filled <- archivo_entrada

We create a function to fill the NA values.

    mean_steps_per_interval <- function(p_interval) {
      tmp <- mean_per_day_interval[mean_per_day_interval$interval == p_interval,]
      return(tmp$avrg_steps)
    }

And we use a *for* loop to read the data frame and insert the values.

    for(i in 1:nrow(archivo_entrada_filled)) {
      if(is.na(archivo_entrada_filled[i,"steps"])) {
        m_interval <- archivo_entrada_filled[i,"interval"]
        tmp <- mean_steps_per_interval(m_interval)
        archivo_entrada_filled[i,"steps"] <- tmp
      }
    }

7. Histogram of the total number of steps taken each day after missing values are imputed.
------------------------------------------------------------------------------------------

    steps_per_day_filled <- 
      archivo_entrada_filled %>%
      group_by(date) %>%
      summarize(number_steps = sum(steps))

    steps_per_day_filled_date <- steps_per_day_filled

With this, we can see dates with correct format.

    steps_per_day_filled_date$date <- as.Date(steps_per_day_filled_date$date, format = "%Y-%m-%d")

    ggplot(data = steps_per_day_filled_date, fill = "blue") +
      geom_bar(mapping = aes(x = date, y = number_steps), stat = "identity", col = c("orange"), fill = c("navy")) +
      xlab("Date") + ylab("Number of steps") + labs(title = "Number of steps per day without NA values")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-19-1.png)

Mean and median
===============

    mean(steps_per_day_filled$number_steps) 

    ## [1] 10766.19

    median(steps_per_day_filled$number_steps)

    ## [1] 10766.19

8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.
-------------------------------------------------------------------------------------------------------------

We are going to create a function to indicate if days are weekdays or
weekend.  
(My system language is Spanish, because I am from Spain; for that the
days of week are in Spanish).

    type_of_day <- function(p_day) {
      day <- weekdays(p_day)
      if (day %in% c("lunes", "martes", "miércoles", "jueves", "viernes"))
        return("weekday")
      else if (day %in% c("sábado", "domingo"))
        return("weekend")
      else
        stop("invalid date")
    }

We filled the field with the type of the day.

    archivo_entrada_filled$day <- sapply(archivo_entrada_filled$date_date, FUN = type_of_day)

And we are going to draw the plot using Lattice system.

    library(lattice)
    transform_data <- transform(archivo_entrada_filled, day = factor(day))
    xyplot(steps ~ interval | day, data = transform_data, type = "l",
           main = "Activity patterns between weekdays and weekend", 
           xlab = "Interval",
           ylab = "Number of steps",
           layout = c(1,2))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-23-1.png)
