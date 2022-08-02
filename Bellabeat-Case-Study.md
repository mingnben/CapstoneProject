Bellabeat Case Study: Sleep Quality
================
Benjamin
2022-07-30

## Introduction

Welcome everyone. This capstone project will mark my very first
milestone of DA projects. The objective of the analysis is to provide
high-level recommendations to Bellabeat marketing team in order to guide
marketing strategy for the company. I will first analyze a open source
dataset **“FitBit Fitness Tracker Data”** and reveal some interesting
insights of how consumers use smart devices. After that, I will apply
these insights into one of the Bellabeat product and provide some
data-driven recommendations.

## Background of Bellabeat

Bellabeat is a high-tech company that manufactures health-focused smart
products. Collecting data on activity, sleep, stress, and reproductive
health has allowed Bellabeat to empower women with knowledge about their
own health and habits.

## Business Tasks

First, analyze the activity, intensity, sleep and weight data that smart
devices collected and apply the insights onto Bellabeat product.
Finally, give high-level recommendations for marketing strategy.

## Data Description - Prepare

In this analysis, I will use FitBit Fitness Tracker Data
[link](https://www.kaggle.com/datasets/arashnic/fitbit) to reveal the
insights of device users’ daily habits. This dataset provides a
comprehensive records of 30 fitbit users, with their daily activity,
steps, we could even explore users’ habits. However, this dataset is
inconsistent, and the sample size is limited, we may need to add another
data source in order to address these limitations to give a thorough
analysis.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(tidyr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(ggplot2)
library(dplyr)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(skimr)
library(readr)
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
weight <- read_csv("Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
```

    ## Rows: 67 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Date
    ## dbl (6): Id, WeightKg, WeightPounds, Fat, BMI, LogId
    ## lgl (1): IsManualReport
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
activity <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
```

    ## Rows: 940 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): ActivityDate
    ## dbl (14): Id, TotalSteps, TotalDistance, TrackerDistance, LoggedActivitiesDi...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
step <- read_csv("Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
```

    ## Rows: 940 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): ActivityDay
    ## dbl (2): Id, StepTotal
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
intensity <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
```

    ## Rows: 940 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): ActivityDay
    ## dbl (9): Id, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, Ve...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
intensity_hour <- read_csv("Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
```

    ## Rows: 22099 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): ActivityHour
    ## dbl (3): Id, TotalIntensity, AverageIntensity
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
calories <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
```

    ## Rows: 940 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): ActivityDay
    ## dbl (2): Id, Calories
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sleep <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
```

    ## Rows: 413 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): SleepDay
    ## dbl (4): Id, TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

After Loading our data, lets clean up the column names using
clean_names() and have a overview of our datasets.

``` r
weight %>% clean_names() %>% head()
```

    ## # A tibble: 6 × 8
    ##           id date                  weight_kg weigh…¹   fat   bmi is_ma…²  log_id
    ##        <dbl> <chr>                     <dbl>   <dbl> <dbl> <dbl> <lgl>     <dbl>
    ## 1 1503960366 5/2/2016 11:59:59 PM       52.6    116.    22  22.6 TRUE    1.46e12
    ## 2 1503960366 5/3/2016 11:59:59 PM       52.6    116.    NA  22.6 TRUE    1.46e12
    ## 3 1927972279 4/13/2016 1:08:52 AM      134.     294.    NA  47.5 FALSE   1.46e12
    ## 4 2873212765 4/21/2016 11:59:59 PM      56.7    125.    NA  21.5 TRUE    1.46e12
    ## 5 2873212765 5/12/2016 11:59:59 PM      57.3    126.    NA  21.7 TRUE    1.46e12
    ## 6 4319703577 4/17/2016 11:59:59 PM      72.4    160.    25  27.5 TRUE    1.46e12
    ## # … with abbreviated variable names ¹​weight_pounds, ²​is_manual_report

``` r
activity %>% clean_names() %>% head()
```

    ## # A tibble: 6 × 15
    ##       id activ…¹ total…² total…³ track…⁴ logge…⁵ very_…⁶ moder…⁷ light…⁸ seden…⁹
    ##    <dbl> <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 1.50e9 4/12/2…   13162    8.5     8.5        0    1.88   0.550    6.06       0
    ## 2 1.50e9 4/13/2…   10735    6.97    6.97       0    1.57   0.690    4.71       0
    ## 3 1.50e9 4/14/2…   10460    6.74    6.74       0    2.44   0.400    3.91       0
    ## 4 1.50e9 4/15/2…    9762    6.28    6.28       0    2.14   1.26     2.83       0
    ## 5 1.50e9 4/16/2…   12669    8.16    8.16       0    2.71   0.410    5.04       0
    ## 6 1.50e9 4/17/2…    9705    6.48    6.48       0    3.19   0.780    2.51       0
    ## # … with 5 more variables: very_active_minutes <dbl>,
    ## #   fairly_active_minutes <dbl>, lightly_active_minutes <dbl>,
    ## #   sedentary_minutes <dbl>, calories <dbl>, and abbreviated variable names
    ## #   ¹​activity_date, ²​total_steps, ³​total_distance, ⁴​tracker_distance,
    ## #   ⁵​logged_activities_distance, ⁶​very_active_distance,
    ## #   ⁷​moderately_active_distance, ⁸​light_active_distance,
    ## #   ⁹​sedentary_active_distance
    ## # ℹ Use `colnames()` to see all variable names

``` r
step %>% clean_names() %>% head()
```

    ## # A tibble: 6 × 3
    ##           id activity_day step_total
    ##        <dbl> <chr>             <dbl>
    ## 1 1503960366 4/12/2016         13162
    ## 2 1503960366 4/13/2016         10735
    ## 3 1503960366 4/14/2016         10460
    ## 4 1503960366 4/15/2016          9762
    ## 5 1503960366 4/16/2016         12669
    ## 6 1503960366 4/17/2016          9705

``` r
intensity %>% clean_names() %>% head()
```

    ## # A tibble: 6 × 10
    ##       id activ…¹ seden…² light…³ fairl…⁴ very_…⁵ seden…⁶ light…⁷ moder…⁸ very_…⁹
    ##    <dbl> <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 1.50e9 4/12/2…     728     328      13      25       0    6.06   0.550    1.88
    ## 2 1.50e9 4/13/2…     776     217      19      21       0    4.71   0.690    1.57
    ## 3 1.50e9 4/14/2…    1218     181      11      30       0    3.91   0.400    2.44
    ## 4 1.50e9 4/15/2…     726     209      34      29       0    2.83   1.26     2.14
    ## 5 1.50e9 4/16/2…     773     221      10      36       0    5.04   0.410    2.71
    ## 6 1.50e9 4/17/2…     539     164      20      38       0    2.51   0.780    3.19
    ## # … with abbreviated variable names ¹​activity_day, ²​sedentary_minutes,
    ## #   ³​lightly_active_minutes, ⁴​fairly_active_minutes, ⁵​very_active_minutes,
    ## #   ⁶​sedentary_active_distance, ⁷​light_active_distance,
    ## #   ⁸​moderately_active_distance, ⁹​very_active_distance

``` r
calories %>% clean_names() %>% head()
```

    ## # A tibble: 6 × 3
    ##           id activity_day calories
    ##        <dbl> <chr>           <dbl>
    ## 1 1503960366 4/12/2016        1985
    ## 2 1503960366 4/13/2016        1797
    ## 3 1503960366 4/14/2016        1776
    ## 4 1503960366 4/15/2016        1745
    ## 5 1503960366 4/16/2016        1863
    ## 6 1503960366 4/17/2016        1728

``` r
sleep %>% clean_names() %>% head()
```

    ## # A tibble: 6 × 5
    ##           id sleep_day             total_sleep_records total_minutes_a…¹ total…²
    ##        <dbl> <chr>                               <dbl>             <dbl>   <dbl>
    ## 1 1503960366 4/12/2016 12:00:00 AM                   1               327     346
    ## 2 1503960366 4/13/2016 12:00:00 AM                   2               384     407
    ## 3 1503960366 4/15/2016 12:00:00 AM                   1               412     442
    ## 4 1503960366 4/16/2016 12:00:00 AM                   2               340     367
    ## 5 1503960366 4/17/2016 12:00:00 AM                   1               700     712
    ## 6 1503960366 4/19/2016 12:00:00 AM                   1               304     320
    ## # … with abbreviated variable names ¹​total_minutes_asleep, ²​total_time_in_bed

Next, we will check the completeness of our datasets.

``` r
Datasets <- c("weight", "activity", "step", "intensity", "calories", "sleep", "intensity_hour")
Unique_ID <- c(n_distinct(weight$Id), n_distinct(activity$Id), n_distinct(step$Id),
               n_distinct(intensity$Id), n_distinct(calories$Id), n_distinct(sleep$Id), n_distinct(intensity_hour$Id))
Duplicates <- c(sum(duplicated(weight)), sum(duplicated(activity)), sum(duplicated(step)), 
                sum(duplicated(intensity)), sum(duplicated(calories)), sum(duplicated(sleep)), sum(duplicated(intensity_hour)))
Missing <- c(sum(is.na(weight)), sum(is.na(activity)), sum(is.na(step)),
             sum(is.na(intensity)), sum(is.na(calories)), sum(is.na(sleep)), sum(is.na(intensity_hour)))

Summary <- data.frame(Datasets, Unique_ID, Duplicates, Missing)

Summary
```

    ##         Datasets Unique_ID Duplicates Missing
    ## 1         weight         8          0      65
    ## 2       activity        33          0       0
    ## 3           step        33          0       0
    ## 4      intensity        33          0       0
    ## 5       calories        33          0       0
    ## 6          sleep        24          3       0
    ## 7 intensity_hour        33          0       0

As we can see there are only 8 distinct users in weight data which is
not complete. And there are only 24 distinct users recording their sleep
data. Although it is not complete, we can still get some interesting
insights.Also, there are 65 missing records in *weight* dataset and 3
duplicates in *sleep* dataset.

## Data Manipulation - Process

First, we will remove any duplicated records and missing values. We can
observe that those 65 missing records in weight dataset are actually
from variable *Fat*. Therefore, we decided to drop the Fat column to
prevent data bias. Furthermore, we will remove the 3 duplicates in
*sleep* dataset as well.

``` r
weight$Fat <- NULL
sleep <- sleep %>% distinct()
```

``` r
Duplicates <- c(sum(duplicated(weight)), sum(duplicated(activity)), sum(duplicated(step)), 
                sum(duplicated(intensity)), sum(duplicated(calories)), sum(duplicated(sleep)), sum(duplicated(intensity_hour)))
Missing <- c(sum(is.na(weight)), sum(is.na(activity)), sum(is.na(step)),
             sum(is.na(intensity)), sum(is.na(calories)), sum(is.na(sleep)), sum(is.na(intensity_hour)))

Summary <- data.frame(Datasets, Unique_ID, Duplicates, Missing)
Summary
```

    ##         Datasets Unique_ID Duplicates Missing
    ## 1         weight         8          0       0
    ## 2       activity        33          0       0
    ## 3           step        33          0       0
    ## 4      intensity        33          0       0
    ## 5       calories        33          0       0
    ## 6          sleep        24          0       0
    ## 7 intensity_hour        33          0       0

Next, we can observe that the *date* column is now stored as string in
R, we want to transform the date variable into date or datetime format
in R.

``` r
weight <- weight %>% mutate(Date=parse_date_time(Date, "%m/%d/%Y %I:%M:%S %p"))
activity <- activity %>% mutate(ActivityDate=as.Date(ActivityDate, format="%m/%d/%Y")) %>% rename(Date = ActivityDate)
step <- step %>% mutate(ActivityDay=as.Date(ActivityDay, format="%m/%d/%Y")) %>% rename(Date = ActivityDay)
intensity <- intensity %>% mutate(ActivityDay=as.Date(ActivityDay, format="%m/%d/%Y")) %>% rename(Date = ActivityDay)
calories <- calories %>% mutate(ActivityDay=as.Date(ActivityDay, format="%m/%d/%Y")) %>% rename(Date = ActivityDay)
sleep <- sleep %>% mutate(SleepDay=parse_date_time(SleepDay, "%m/%d/%Y %I:%M:%S %p")) %>% rename(Date = SleepDay)
intensity_hour <- intensity_hour %>% mutate(ActivityHour=parse_date_time(ActivityHour, "%m/%d/%Y %I:%M:%S %p"))
intensity_hour <- intensity_hour %>% mutate(Time=format(ActivityHour, format = "%H:%M"),
                                            Date=format(ActivityHour, format = "%m/%d/%Y"))
```

## Patterns and Insights - Analyse & Share

### Intensity vs. Sleep

``` r
intensity_sleep <- merge(intensity,sleep, by=c('Id','Date'))
intensity_sleep <- intensity_sleep %>% mutate(TotalActiveMinutes = VeryActiveMinutes + 
                                                FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes)
## Donut Chart Data
Total <- mean(intensity_sleep$TotalActiveMinutes)
VeryActive <- round(mean(intensity_sleep$VeryActiveMinutes) / Total * 100, digit = 1)
FairlyActive <- round(mean(intensity_sleep$FairlyActiveMinutes) / Total * 100, digit = 1)
LightlyActive <- round(mean(intensity_sleep$LightlyActiveMinutes) / Total * 100, digit = 1)
SedentaryActive <- round(mean(intensity_sleep$SedentaryMinutes) / Total * 100, digit = 1)

donut_cat <- c("Very Active", "Fairly Active", "Lightly Active", "Sedentary Active")
donut_data <- c(VeryActive, FairlyActive, LightlyActive, SedentaryActive)
hsize <- 2

donut <- data_frame(donut_cat, donut_data, hsize)
```

    ## Warning: `data_frame()` was deprecated in tibble 1.1.0.
    ## Please use `tibble()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

``` r
## Donut Chart of Intensity Data
ggplot(donut, aes(x = hsize, y = donut_data, fill = donut_cat)) +
  geom_col() +
  geom_text(aes(label = donut_data),
             position = position_stack(vjust = 0.5),
            size = 3, color="#352961", show.legend=FALSE) +
  xlab("") +
  ylab("") +
  coord_polar(theta='y', direction=-1) +
  xlim(c(0.2, hsize + 0.5)) +
  scale_fill_manual(values=c("#AD5C6C", "#FBB637", "#E76236", "#C00000")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Average Distribution of Activeness", subtitle = "73% of the users time are in sedentary active",
       fill = "Activeness Category")
```

![](Bellabeat-Case-Study_files/figure-gfm/Intensity-1.png)<!-- -->

The above donut chart shows that 73% of the users time are in sedentary
active.

``` r
ggplot(data = intensity_sleep, mapping=aes(x=SedentaryMinutes, y=TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Sedentary time versus sleeping time ", subtitle = "The longer the sedentary time, the shorter the sleeping time",
       x = "Sedentary Time", y = "Sleeping Time", caption = "in minutes")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Bellabeat-Case-Study_files/figure-gfm/Sedentary%20time%20vs.%20Sleep-1.png)<!-- -->

As we can see, there is a small negative correlation between the
variables. The data follows the smooth line slanting down from left to
right. The longer the people sit, the relatively shorter time they are
asleep.

``` r
avg_intensity <- intensity_hour %>% 
  group_by(Time) %>% 
  summarize(avg_TotalIntensities = mean(TotalIntensity))

ggplot(data = avg_intensity, aes(x=Time, y=avg_TotalIntensities, fill=avg_TotalIntensities)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Average intensity throughout the day", subtitle = "Most users are more active between 8AM to 8PM",
       x = "Time", y = "Average Intensity", fill="Average Intensity") + 
  annotate("text", x=20,y=23, label="5PM to 7PM Most active period", color="black",
           fontface="bold", size=3)
```

![](Bellabeat-Case-Study_files/figure-gfm/Instensity-1.png)<!-- -->

We can observed that most users are more active between 8AM to 8PM.
While 5PM to 8PM is the peak active period. The graph also shows the
average intensity at 3PM is relatively lower than other afternoon time.
People might be taking afternoon break or nap during that period of
time.

### Sleeping Quality

``` r
intensity_sleep_df <- intensity_sleep %>% mutate(TotalActiveMinutes=FairlyActiveMinutes + VeryActiveMinutes)

intensity_sleep_df <- intensity_sleep_df %>% group_by(Id) %>% summarize(AvgActiveMinutesPerWeek=mean(TotalActiveMinutes) * 7)

intensity_sleep_df <- intensity_sleep_df %>%
  mutate(Category=case_when(AvgActiveMinutesPerWeek < 150 ~ "Sedentary",
                            AvgActiveMinutesPerWeek >= 150 & AvgActiveMinutesPerWeek <= 300 ~ "Active",
                            AvgActiveMinutesPerWeek > 300 ~ "Highly Active")) %>%
  mutate(Category=factor(Category, levels=c("Sedentary", "Active", "Highly Active"))) %>%
  arrange(Category)

user_category <- intensity_sleep_df %>% select(Id,AvgActiveMinutesPerWeek,Category)

sum_user_category <- user_category %>%
  group_by(Category) %>%
  summarize(Count=n())

hole_size <- 2
ggplot(sum_user_category, aes(x=hole_size, y=Count, fill=Category)) +
  geom_col() +
  coord_polar(theta='y', direction=-1) +
  xlim(c(0.2, hole_size + 0.5)) +
  scale_fill_manual(values=c("#AD5C6C", "#FBB637", "#E76236")) +
  geom_text(aes(label=Count), size=5, position=position_stack(vjust=0.5), color="#352961", show.legend=FALSE) +
  xlab("") + ylab("") + labs(title="Number of Users by Amount of Physical Activity", fill="Category", caption = "There are in total 24 unique users") +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.title=element_text(color="#333333"),
        legend.text=element_text(color="#333333"),
        panel.background=element_rect(fill="white", color="white"))
```

![](Bellabeat-Case-Study_files/figure-gfm/Sleeping%20Quality%20among%20intensity-1.png)<!-- -->

``` r
intensity_sleep <- intensity_sleep %>% mutate(TimeNeededToSleep = TotalTimeInBed - TotalMinutesAsleep)

df <- intensity_sleep %>%
  group_by(Id) %>% 
  summarize(Avg_TimeNeededToSleep = mean(TimeNeededToSleep)) %>% 
  data_frame()

merged <- merge(df, user_category, by='Id')

ggplot(data = merged, aes(Category,Avg_TimeNeededToSleep)) +
  labs(title="Time Needed To Fall Asleep", subtitle="Sedentary users take longer time to fall asleep", y="Average minutes needed to fall asleep", x="User Category") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  stat_summary(fun=mean, geom="bar", fill=c("#AD5C6C", "#FBB637", "#E76236"))
```

![](Bellabeat-Case-Study_files/figure-gfm/Time%20needed%20to%20fall%20asleep-1.png)<!-- -->

The plot shows us that users who are classified as “Sedentary” take
longer time to fall asleep.

## Recommendation - Act

-   Since the longer the people sitting, the shorter time they are
    asleep or the longer time needed to fall asleep. We suggest using
    **Bellebeat App** to send reminder to the users, notifying them for
    sitting or lying for too long and suggesting them to stand or walk
    or stretch for a while to improve their sleeping qualities. Also, we
    can suggest users to workout or exercise to improve their sleeping
    qualities.

-   Most users are more active between 8AM to 8PM, peak during 5PM to
    7PM. We suggest to encourage users using **Bellebeat App** to
    schedule their daily rountine and starting the cycle from 8AM. We
    can also deploy **Spring** bottle to send out hydration reminders
    based on users intensity.

-   Besides, we also suggest Bellabeat to educate more users to record
    their weight and sleep data in order to to keep track of their
    physical health. Having more complete weight and sleep data, we can
    further analyse the users health and provide data-driven knowledge
    and suggestions about their own health and habits.
