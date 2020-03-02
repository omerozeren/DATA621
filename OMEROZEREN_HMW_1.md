---
title: "HMW 1- Data 621"
author: "OMER OZEREN"
output:
   html_document:
      self_contained: false
      keep_md: true
---


## Introduction



I have been given a dataset with 2276 records summarizing a major league baseball team's season.  All statistics have been adjusted to match the performance of a 162 game season.  The objective is to build a linear regression model to predict the number of wins for a team.
This report covers an attempt to build a model to predict number of wins of a baseball team in a season based on several offensive and deffensive statistics. Resulting model explained about 36% of variability in the target variable and included most of the provided explanatory variables. Some potentially  variables were not included in the data set due to missing values.I used KNN for variable missing values imputtion.

## DATA EXPLORATION

Each record in the data set represents the performance of the team for the given year adjusted to the current length of the season - 162 games. The data set includes 16 variables and the training set includes 2,276 records.


```r
sumtable = data.frame(Variable = character(),
                   Min = integer(),
                   Median = integer(),
                   Mean = double(),
                   SD = double(),
                   Max = integer(),
                   Num_NaN = integer())
for (i in 2:17) {
  sumtable <- rbind(sumtable, data.frame(Variable = colnames(training)[i],
                                   Min = min(training[,i], na.rm=TRUE),
                                   Median = median(training[,i], na.rm=TRUE),
                                   Mean = mean(training[,i], na.rm=TRUE),
                                   SD = sd(training[,i], na.rm=TRUE),
                                   Max = max(training[,i], na.rm=TRUE),
                                   Num_NaN = sum(is.na(training[,i])))
                 )
}
colnames(sumtable) <- c("", "Min", "Median", "Mean", "SD", "Max", "Num of NaN")
sumtable
```

```
                     Min Median       Mean         SD   Max Num of NaN
1       TARGET_WINS    0   82.0   80.79086   15.75215   146          0
2    TEAM_BATTING_H  891 1454.0 1469.26977  144.59120  2554          0
3   TEAM_BATTING_2B   69  238.0  241.24692   46.80141   458          0
4   TEAM_BATTING_3B    0   47.0   55.25000   27.93856   223          0
5   TEAM_BATTING_HR    0  102.0   99.61204   60.54687   264          0
6   TEAM_BATTING_BB    0  512.0  501.55888  122.67086   878          0
7   TEAM_BATTING_SO    0  750.0  735.60534  248.52642  1399        102
8   TEAM_BASERUN_SB    0  101.0  124.76177   87.79117   697        131
9   TEAM_BASERUN_CS    0   49.0   52.80386   22.95634   201        772
10 TEAM_BATTING_HBP   29   58.0   59.35602   12.96712    95       2085
11  TEAM_PITCHING_H 1137 1518.0 1779.21046 1406.84293 30132          0
12 TEAM_PITCHING_HR    0  107.0  105.69859   61.29875   343          0
13 TEAM_PITCHING_BB    0  536.5  553.00791  166.35736  3645          0
14 TEAM_PITCHING_SO    0  813.5  817.73045  553.08503 19278        102
15  TEAM_FIELDING_E   65  159.0  246.48067  227.77097  1898          0
16 TEAM_FIELDING_DP   52  149.0  146.38794   26.22639   228        286
```





```r
training %>%
  gather(variable, value, TARGET_WINS:TEAM_FIELDING_DP) %>%
  ggplot(., aes(value)) + 
  geom_density(fill = "indianred4", color="indianred4") + 
  facet_wrap(~variable, scales ="free", ncol = 4) +
  labs(x = element_blank(), y = element_blank())
```

![](OMEROZEREN_HMW_1_files/figure-html/small_multiples_density-1.png)<!-- -->


```r
quick_summary <- function(df){
  df %>%
    summary() %>%
    kable() %>%
    kable_styling()
}
quick_summary(training)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;">     INDEX </th>
   <th style="text-align:left;">  TARGET_WINS </th>
   <th style="text-align:left;"> TEAM_BATTING_H </th>
   <th style="text-align:left;"> TEAM_BATTING_2B </th>
   <th style="text-align:left;"> TEAM_BATTING_3B </th>
   <th style="text-align:left;"> TEAM_BATTING_HR </th>
   <th style="text-align:left;"> TEAM_BATTING_BB </th>
   <th style="text-align:left;"> TEAM_BATTING_SO </th>
   <th style="text-align:left;"> TEAM_BASERUN_SB </th>
   <th style="text-align:left;"> TEAM_BASERUN_CS </th>
   <th style="text-align:left;"> TEAM_BATTING_HBP </th>
   <th style="text-align:left;"> TEAM_PITCHING_H </th>
   <th style="text-align:left;"> TEAM_PITCHING_HR </th>
   <th style="text-align:left;"> TEAM_PITCHING_BB </th>
   <th style="text-align:left;"> TEAM_PITCHING_SO </th>
   <th style="text-align:left;"> TEAM_FIELDING_E </th>
   <th style="text-align:left;"> TEAM_FIELDING_DP </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Min.   :   1.0 </td>
   <td style="text-align:left;"> Min.   :  0.00 </td>
   <td style="text-align:left;"> Min.   : 891 </td>
   <td style="text-align:left;"> Min.   : 69.0 </td>
   <td style="text-align:left;"> Min.   :  0.00 </td>
   <td style="text-align:left;"> Min.   :  0.00 </td>
   <td style="text-align:left;"> Min.   :  0.0 </td>
   <td style="text-align:left;"> Min.   :   0.0 </td>
   <td style="text-align:left;"> Min.   :  0.0 </td>
   <td style="text-align:left;"> Min.   :  0.0 </td>
   <td style="text-align:left;"> Min.   :29.00 </td>
   <td style="text-align:left;"> Min.   : 1137 </td>
   <td style="text-align:left;"> Min.   :  0.0 </td>
   <td style="text-align:left;"> Min.   :   0.0 </td>
   <td style="text-align:left;"> Min.   :    0.0 </td>
   <td style="text-align:left;"> Min.   :  65.0 </td>
   <td style="text-align:left;"> Min.   : 52.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 1st Qu.: 630.8 </td>
   <td style="text-align:left;"> 1st Qu.: 71.00 </td>
   <td style="text-align:left;"> 1st Qu.:1383 </td>
   <td style="text-align:left;"> 1st Qu.:208.0 </td>
   <td style="text-align:left;"> 1st Qu.: 34.00 </td>
   <td style="text-align:left;"> 1st Qu.: 42.00 </td>
   <td style="text-align:left;"> 1st Qu.:451.0 </td>
   <td style="text-align:left;"> 1st Qu.: 548.0 </td>
   <td style="text-align:left;"> 1st Qu.: 66.0 </td>
   <td style="text-align:left;"> 1st Qu.: 38.0 </td>
   <td style="text-align:left;"> 1st Qu.:50.50 </td>
   <td style="text-align:left;"> 1st Qu.: 1419 </td>
   <td style="text-align:left;"> 1st Qu.: 50.0 </td>
   <td style="text-align:left;"> 1st Qu.: 476.0 </td>
   <td style="text-align:left;"> 1st Qu.:  615.0 </td>
   <td style="text-align:left;"> 1st Qu.: 127.0 </td>
   <td style="text-align:left;"> 1st Qu.:131.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Median :1270.5 </td>
   <td style="text-align:left;"> Median : 82.00 </td>
   <td style="text-align:left;"> Median :1454 </td>
   <td style="text-align:left;"> Median :238.0 </td>
   <td style="text-align:left;"> Median : 47.00 </td>
   <td style="text-align:left;"> Median :102.00 </td>
   <td style="text-align:left;"> Median :512.0 </td>
   <td style="text-align:left;"> Median : 750.0 </td>
   <td style="text-align:left;"> Median :101.0 </td>
   <td style="text-align:left;"> Median : 49.0 </td>
   <td style="text-align:left;"> Median :58.00 </td>
   <td style="text-align:left;"> Median : 1518 </td>
   <td style="text-align:left;"> Median :107.0 </td>
   <td style="text-align:left;"> Median : 536.5 </td>
   <td style="text-align:left;"> Median :  813.5 </td>
   <td style="text-align:left;"> Median : 159.0 </td>
   <td style="text-align:left;"> Median :149.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Mean   :1268.5 </td>
   <td style="text-align:left;"> Mean   : 80.79 </td>
   <td style="text-align:left;"> Mean   :1469 </td>
   <td style="text-align:left;"> Mean   :241.2 </td>
   <td style="text-align:left;"> Mean   : 55.25 </td>
   <td style="text-align:left;"> Mean   : 99.61 </td>
   <td style="text-align:left;"> Mean   :501.6 </td>
   <td style="text-align:left;"> Mean   : 735.6 </td>
   <td style="text-align:left;"> Mean   :124.8 </td>
   <td style="text-align:left;"> Mean   : 52.8 </td>
   <td style="text-align:left;"> Mean   :59.36 </td>
   <td style="text-align:left;"> Mean   : 1779 </td>
   <td style="text-align:left;"> Mean   :105.7 </td>
   <td style="text-align:left;"> Mean   : 553.0 </td>
   <td style="text-align:left;"> Mean   :  817.7 </td>
   <td style="text-align:left;"> Mean   : 246.5 </td>
   <td style="text-align:left;"> Mean   :146.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 3rd Qu.:1915.5 </td>
   <td style="text-align:left;"> 3rd Qu.: 92.00 </td>
   <td style="text-align:left;"> 3rd Qu.:1537 </td>
   <td style="text-align:left;"> 3rd Qu.:273.0 </td>
   <td style="text-align:left;"> 3rd Qu.: 72.00 </td>
   <td style="text-align:left;"> 3rd Qu.:147.00 </td>
   <td style="text-align:left;"> 3rd Qu.:580.0 </td>
   <td style="text-align:left;"> 3rd Qu.: 930.0 </td>
   <td style="text-align:left;"> 3rd Qu.:156.0 </td>
   <td style="text-align:left;"> 3rd Qu.: 62.0 </td>
   <td style="text-align:left;"> 3rd Qu.:67.00 </td>
   <td style="text-align:left;"> 3rd Qu.: 1682 </td>
   <td style="text-align:left;"> 3rd Qu.:150.0 </td>
   <td style="text-align:left;"> 3rd Qu.: 611.0 </td>
   <td style="text-align:left;"> 3rd Qu.:  968.0 </td>
   <td style="text-align:left;"> 3rd Qu.: 249.2 </td>
   <td style="text-align:left;"> 3rd Qu.:164.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Max.   :2535.0 </td>
   <td style="text-align:left;"> Max.   :146.00 </td>
   <td style="text-align:left;"> Max.   :2554 </td>
   <td style="text-align:left;"> Max.   :458.0 </td>
   <td style="text-align:left;"> Max.   :223.00 </td>
   <td style="text-align:left;"> Max.   :264.00 </td>
   <td style="text-align:left;"> Max.   :878.0 </td>
   <td style="text-align:left;"> Max.   :1399.0 </td>
   <td style="text-align:left;"> Max.   :697.0 </td>
   <td style="text-align:left;"> Max.   :201.0 </td>
   <td style="text-align:left;"> Max.   :95.00 </td>
   <td style="text-align:left;"> Max.   :30132 </td>
   <td style="text-align:left;"> Max.   :343.0 </td>
   <td style="text-align:left;"> Max.   :3645.0 </td>
   <td style="text-align:left;"> Max.   :19278.0 </td>
   <td style="text-align:left;"> Max.   :1898.0 </td>
   <td style="text-align:left;"> Max.   :228.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA's   :102 </td>
   <td style="text-align:left;"> NA's   :131 </td>
   <td style="text-align:left;"> NA's   :772 </td>
   <td style="text-align:left;"> NA's   :2085 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA's   :102 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA's   :286 </td>
  </tr>
</tbody>
</table>

Some initial observations:  

* The response variable (`TARGET_WINS`) looks to be normally distributed.  This supports the working theory that there are good teams and bad teams.  There are also a lot of average teams.
* There are also quite a few variables with missing values.  We may need to deal with these in order to have the largest data set possible for modeling.
* A couple variables are bimodal (`TEAM_BATTING_HR`, `TEAM_BATTING_SO` `TEAM_PITCHING_HR`).  This may be a challenge as some of them are missing values and that may be a challenge in filling in missing values.
* Some variables are right skewed (`TEAM_BASERUN_CS`, `TEAM_BASERUN_SB`, etc.).  This might support the good team theory.  It may also introduce non-normally distributed residuals in the model.  We shall see.  

#### Zero Values

There are also variables that have verly low values.  Let's see how big of a problem this is:


```r
training %>% 
  gather(variable, value) %>%
  filter(value == 0) %>%
  group_by(variable) %>%
  tally() %>%
  mutate(percent = n / nrow(training) * 100) %>%
  mutate(percent = paste0(round(percent, ifelse(percent < 10, 1, 0)), "%")) %>%
  arrange(desc(n)) %>%
  rename(`Variable With Zeros` = variable,
         `Number of Records` = n,
         `Share of Total` = percent) %>%
  kable() %>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Variable With Zeros </th>
   <th style="text-align:right;"> Number of Records </th>
   <th style="text-align:left;"> Share of Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TEAM_BATTING_SO </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> 0.9% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_PITCHING_SO </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> 0.9% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_BATTING_HR </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> 0.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_PITCHING_HR </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> 0.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_BASERUN_SB </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 0.1% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_BATTING_3B </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 0.1% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TARGET_WINS </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_BASERUN_CS </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_BATTING_BB </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_PITCHING_BB </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 0% </td>
  </tr>
</tbody>
</table>

The report shows that missing values are nearly low.

#### Missing Values (NaN)

During our first look at the data it was noted that there were variables that are missing data.  Here's a look at what variables are missing data and how big of a problem it is:


```r
training %>% 
  gather(variable, value) %>%
  filter(is.na(value)) %>%
  group_by(variable) %>%
  tally() %>%
  mutate(percent = n / nrow(training) * 100) %>%
  mutate(percent = paste0(round(percent, ifelse(percent < 10, 1, 0)), "%")) %>%
  arrange(desc(n)) %>%
  rename(`Variable Missing Data` = variable,
         `Number of Records` = n,
         `Share of Total` = percent) %>%
  kable() %>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Variable Missing Data </th>
   <th style="text-align:right;"> Number of Records </th>
   <th style="text-align:left;"> Share of Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TEAM_BATTING_HBP </td>
   <td style="text-align:right;"> 2085 </td>
   <td style="text-align:left;"> 92% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_BASERUN_CS </td>
   <td style="text-align:right;"> 772 </td>
   <td style="text-align:left;"> 34% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_FIELDING_DP </td>
   <td style="text-align:right;"> 286 </td>
   <td style="text-align:left;"> 13% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_BASERUN_SB </td>
   <td style="text-align:right;"> 131 </td>
   <td style="text-align:left;"> 5.8% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_BATTING_SO </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:left;"> 4.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEAM_PITCHING_SO </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:left;"> 4.5% </td>
  </tr>
</tbody>
</table>

The "TEAM_BATTING_HBP"" varriable has "NaN" nearly 92%.  We will exclude this variable from consideration in our model.

#### Correlations Matrix

Let's take a look at the correlations.  The following is the correlations from the complete cases only:


```r
training %>% 
  cor(., use = "complete.obs") %>%
  corrplot(., method = "color", type = "upper", tl.col = "black", diag = FALSE)
```

![](OMEROZEREN_HMW_1_files/figure-html/correlation plot-1.png)<!-- -->
Anything over 0.5 or under -0.5 is highlighted in blue. The matrix was created using complete pairwise observations.

A few conclusions: 

- Not surprisingly there is a very strong correlation between home runs batted in and home runs given up by pitching.
- There is a negative correlation between number of triples and home runs. A less powerful team may not have enough power to hit home runs, but they get a lot of triples. 
- THere is a strong positive correlation between number of strikeouts and home runs. More swings of the bat results in more home runs. 

#### Correlations: Endogenous and  Exogenous Variables

Let's take a look at how the Exogenous(Model Inputs) are correlated with the response variable(Endogenous):


```r
training %>%
  gather(variable, value, -TARGET_WINS) %>%
  ggplot(., aes(value, TARGET_WINS)) + 
  geom_point(fill = "indianred4", color="grey") + 
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  facet_wrap(~variable, scales ="free", ncol = 4) +
  labs(x = element_blank(), y = "Wins")
```

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Variable chacteristics
Each variable is presented below with corresponding basic statistics (minimum, median and maximum values, mean and standard deviation, number of records with missing values), boxplot, density plot with highlighted mean value, and scatterplot against outcome variable (`TARGET_WINS`) with best fit line. This information is used to check general validity of data and adjust as necessary. 


#### TEAM_BATTING_H: 

This variable represents number of team base hits:

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 891 </td>
   <td style="text-align:right;"> 1454 </td>
   <td style="text-align:right;"> 1469.27 </td>
   <td style="text-align:right;"> 144.5912 </td>
   <td style="text-align:right;"> 2554 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-5-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

**Data Overview:** There are no missing values. The range and distribution are reasonable.


#### TEAM_BATTING_2B:

This variable represents number of team doubles:

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 238 </td>
   <td style="text-align:right;"> 241.2469 </td>
   <td style="text-align:right;"> 46.80141 </td>
   <td style="text-align:right;"> 458 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-6-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

**Data Overview:** There are no missing values. The range and distribution are reasonable.


#### TEAM_BATTING_3B: 

This variable represents  number of team triples:

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 55.25 </td>
   <td style="text-align:right;"> 27.93856 </td>
   <td style="text-align:right;"> 223 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-7-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

**Data Overview:**The range and distribution are reasonable. There are 2 records with zero values which is unrealistic for a team in a season. One record (index 1347) has 12 variables with missing values, including the outcome variable. This record will be deleted from the data set. Second record (index 1494) has 7 missing variables, but it does have some recorded values in all categories - batting, pitching and fielding. Zero value for `TEAM_BATTING_3B` can be replaced with the median (because the distribution is right-skewed, median value will provide more realistic estimate).


#### TEAM_BATTING_HR: 

This variable represents  number of team triples:

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 99.61204 </td>
   <td style="text-align:right;"> 60.54687 </td>
   <td style="text-align:right;"> 264 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-8-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-8-3.png)<!-- -->

**Analysis:**There are some low values in the data. So zero doesn't seem too unusual here either.


#### TEAM_BATTING_BB: 

This variable represents Number of team walks

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 512 </td>
   <td style="text-align:right;"> 501.5589 </td>
   <td style="text-align:right;"> 122.6709 </td>
   <td style="text-align:right;"> 878 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-9-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-9-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-9-3.png)<!-- -->

#### TEAM_BATTING_HBP: 

This variable represents Number of team batters hit by pitch

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:right;"> 59.35602 </td>
   <td style="text-align:right;"> 12.96712 </td>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:right;"> 2085 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-10-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-10-3.png)<!-- -->

**Analysis:** There are 2,085 records - 91.6% of data set - that are missing value. Because this variable is missing for majority of records, I wont consider this variable as input for regression model.

#### TEAM_BATTING_SO: 

This variable represents Number of team strikeouts by batters

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 750 </td>
   <td style="text-align:right;"> 735.6053 </td>
   <td style="text-align:right;"> 248.5264 </td>
   <td style="text-align:right;"> 1399 </td>
   <td style="text-align:right;"> 102 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-11-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-11-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

**Analysis:** There are 122 records with missing or zero value (as wtih other variables a zero value is unrealistic). These values can be imputed. Similarly to homeruns, the distribution is multimodal, which is interesting enough for additional analysis. Another area of concern is a noticeable left tail. It is highly unlikely to have games without any strikeouts, so anything lower than 162 (average of 1 strikeout per game) is definitely suspect.

#### TEAM_BASERUN_SB: 

This variable represents Number of team stolen bases

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 101 </td>
   <td style="text-align:right;"> 124.7618 </td>
   <td style="text-align:right;"> 87.79117 </td>
   <td style="text-align:right;"> 697 </td>
   <td style="text-align:right;"> 131 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-12-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-12-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-12-3.png)<!-- -->

#### TEAM_BASERUN_CS: 

This variable represents Number of team runners caught stealing

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 52.80386 </td>
   <td style="text-align:right;"> 22.95634 </td>
   <td style="text-align:right;"> 201 </td>
   <td style="text-align:right;"> 772 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-13-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-13-3.png)<!-- -->



#### TEAM_FIELDING_E: 

This variable represents  Number of team fielding errors

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 65 </td>
   <td style="text-align:right;"> 159 </td>
   <td style="text-align:right;"> 246.4807 </td>
   <td style="text-align:right;"> 227.771 </td>
   <td style="text-align:right;"> 1898 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-14-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-14-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-14-3.png)<!-- -->

#### TEAM_FIELDING_DP: 

This variable represents Number of team fielding double plays

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 149 </td>
   <td style="text-align:right;"> 146.3879 </td>
   <td style="text-align:right;"> 26.22639 </td>
   <td style="text-align:right;"> 228 </td>
   <td style="text-align:right;"> 286 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-15-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-15-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-15-3.png)<!-- -->

#### TEAM_PITCHING_BB: 

This variable represents Number of walks given up by pitchers

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 536.5 </td>
   <td style="text-align:right;"> 553.0079 </td>
   <td style="text-align:right;"> 166.3574 </td>
   <td style="text-align:right;"> 3645 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-16-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-16-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-16-3.png)<!-- -->

**Analysis:** There are no missing values with the exception of record 1347 which will be deleted from model building. There are some unrealistic outliers.


#### TEAM_PITCHING_H: 

This variable represents Number of base hits given up by pitchers

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1137 </td>
   <td style="text-align:right;"> 1518 </td>
   <td style="text-align:right;"> 1779.21 </td>
   <td style="text-align:right;"> 1406.843 </td>
   <td style="text-align:right;"> 30132 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-17-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-17-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-17-3.png)<!-- -->

**Analysis:** Similar to `TEAM_PITCHING_BB` above, there are no missing value, but there issues with outliers. Based on visualizations, this variable will be capped at 13,000 and any value over this will be set to this cap.

#### TEAM_PITCHING_SO:

This variable represents Number of strikeouts by pitchers

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 813.5 </td>
   <td style="text-align:right;"> 817.7305 </td>
   <td style="text-align:right;"> 553.085 </td>
   <td style="text-align:right;"> 19278 </td>
   <td style="text-align:right;"> 102 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-18-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-18-2.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-18-3.png)<!-- -->

**Analysis:** This variable has 122 missing or zero values. They can be imputed as needed. There is also an outlier issue as graph shows.

#### TARGET_WINS: 

This variable represents Number of wins **(Outcome)**

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> Num of NaN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 80.79086 </td>
   <td style="text-align:right;"> 15.75215 </td>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-19-1.png)<!-- -->![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-19-2.png)<!-- -->
**Analysis:** The range and distribution are reasonable. There are no missing values with the exception of record 1347.

## DATA PREPARATION

### Fixing Missing/Zero Values

First we will remove the invalid data and prep it for imputation.  We will drop the hit by pitcher variable from the dataset.


```r
clean_data <- function(df){
  # Change 0's to NA so they too can be imputed
  df <- df %>%
  mutate(TEAM_BATTING_SO = ifelse(TEAM_BATTING_SO == 0, NA, TEAM_BATTING_SO))
  # Remove the high pitching strikeout values
  df[which(df$TEAM_PITCHING_SO > 5346),"TEAM_PITCHING_SO"] <- NA
  # Drop the hit by pitcher variable
  df %>%
    select(-TEAM_BATTING_HBP)
}
training <- clean_data(training)
evaluation <- clean_data(evaluation)
```

### KNN imputation 


```r
set.seed(42)
knn <- training %>% knnImputation()
apply_func <- function(df, knn){
  impute_me <- is.na(df$TEAM_BATTING_SO)
  df[impute_me,"TEAM_BATTING_SO"] <- knn[impute_me,"TEAM_BATTING_SO"] 
  impute_me <- is.na(df$TEAM_BASERUN_SB)
  df[impute_me,"TEAM_BASERUN_SB"] <- knn[impute_me,"TEAM_BASERUN_SB"] 
  impute_me <- is.na(df$TEAM_BASERUN_CS)
  df[impute_me,"TEAM_BASERUN_CS"] <- knn[impute_me,"TEAM_BASERUN_CS"] 
  impute_me <- is.na(df$TEAM_PITCHING_SO)
  df[impute_me,"TEAM_PITCHING_SO"] <- knn[impute_me,"TEAM_PITCHING_SO"]
  impute_me <- is.na(df$TEAM_FIELDING_DP)
  df[impute_me,"TEAM_FIELDING_DP"] <- knn[impute_me,"TEAM_FIELDING_DP"]
  return(df)
}
training <- apply_func(training, knn)
```
### Feature Engineering

The batting singles is not included but we can back it out of the hits.  We will do this.


```r
add_features <- function(df){
  df %>%
    mutate(TEAM_BATTING_1B = TEAM_BATTING_H - TEAM_BATTING_2B - TEAM_BATTING_3B - TEAM_BATTING_HR)
}
training <- add_features(training)
evaluation <- add_features(evaluation)
```
### Model Data Look

Here's what the data look like after imputation and correction:


```r
training %>%
  gather(variable, value) %>%
  ggplot(., aes(value)) + 
  geom_density(fill = "indianred4", color="indianred4") + 
  facet_wrap(~variable, scales ="free", ncol = 4) +
  labs(x = element_blank(), y = element_blank())
```

![](OMEROZEREN_HMW_1_files/figure-html/unnamed-chunk-23-1.png)<!-- -->


```r
quick_summary <- function(df){
  df %>%
    summary() %>%
    kable() %>%
    kable_styling()
}
quick_summary(training)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;">     INDEX </th>
   <th style="text-align:left;">  TARGET_WINS </th>
   <th style="text-align:left;"> TEAM_BATTING_H </th>
   <th style="text-align:left;"> TEAM_BATTING_2B </th>
   <th style="text-align:left;"> TEAM_BATTING_3B </th>
   <th style="text-align:left;"> TEAM_BATTING_HR </th>
   <th style="text-align:left;"> TEAM_BATTING_BB </th>
   <th style="text-align:left;"> TEAM_BATTING_SO </th>
   <th style="text-align:left;"> TEAM_BASERUN_SB </th>
   <th style="text-align:left;"> TEAM_BASERUN_CS </th>
   <th style="text-align:left;"> TEAM_PITCHING_H </th>
   <th style="text-align:left;"> TEAM_PITCHING_HR </th>
   <th style="text-align:left;"> TEAM_PITCHING_BB </th>
   <th style="text-align:left;"> TEAM_PITCHING_SO </th>
   <th style="text-align:left;"> TEAM_FIELDING_E </th>
   <th style="text-align:left;"> TEAM_FIELDING_DP </th>
   <th style="text-align:left;"> TEAM_BATTING_1B </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Min.   :   1.0 </td>
   <td style="text-align:left;"> Min.   :  0.00 </td>
   <td style="text-align:left;"> Min.   : 891 </td>
   <td style="text-align:left;"> Min.   : 69.0 </td>
   <td style="text-align:left;"> Min.   :  0.00 </td>
   <td style="text-align:left;"> Min.   :  0.00 </td>
   <td style="text-align:left;"> Min.   :  0.0 </td>
   <td style="text-align:left;"> Min.   :  66 </td>
   <td style="text-align:left;"> Min.   :  0.0 </td>
   <td style="text-align:left;"> Min.   :  0.0 </td>
   <td style="text-align:left;"> Min.   : 1137 </td>
   <td style="text-align:left;"> Min.   :  0.0 </td>
   <td style="text-align:left;"> Min.   :   0.0 </td>
   <td style="text-align:left;"> Min.   :   0.0 </td>
   <td style="text-align:left;"> Min.   :  65.0 </td>
   <td style="text-align:left;"> Min.   : 52.0 </td>
   <td style="text-align:left;"> Min.   : 709.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 1st Qu.: 630.8 </td>
   <td style="text-align:left;"> 1st Qu.: 71.00 </td>
   <td style="text-align:left;"> 1st Qu.:1383 </td>
   <td style="text-align:left;"> 1st Qu.:208.0 </td>
   <td style="text-align:left;"> 1st Qu.: 34.00 </td>
   <td style="text-align:left;"> 1st Qu.: 42.00 </td>
   <td style="text-align:left;"> 1st Qu.:451.0 </td>
   <td style="text-align:left;"> 1st Qu.: 554 </td>
   <td style="text-align:left;"> 1st Qu.: 67.0 </td>
   <td style="text-align:left;"> 1st Qu.: 43.0 </td>
   <td style="text-align:left;"> 1st Qu.: 1419 </td>
   <td style="text-align:left;"> 1st Qu.: 50.0 </td>
   <td style="text-align:left;"> 1st Qu.: 476.0 </td>
   <td style="text-align:left;"> 1st Qu.: 618.5 </td>
   <td style="text-align:left;"> 1st Qu.: 127.0 </td>
   <td style="text-align:left;"> 1st Qu.:130.0 </td>
   <td style="text-align:left;"> 1st Qu.: 990.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Median :1270.5 </td>
   <td style="text-align:left;"> Median : 82.00 </td>
   <td style="text-align:left;"> Median :1454 </td>
   <td style="text-align:left;"> Median :238.0 </td>
   <td style="text-align:left;"> Median : 47.00 </td>
   <td style="text-align:left;"> Median :102.00 </td>
   <td style="text-align:left;"> Median :512.0 </td>
   <td style="text-align:left;"> Median : 733 </td>
   <td style="text-align:left;"> Median :104.0 </td>
   <td style="text-align:left;"> Median : 58.0 </td>
   <td style="text-align:left;"> Median : 1518 </td>
   <td style="text-align:left;"> Median :107.0 </td>
   <td style="text-align:left;"> Median : 536.5 </td>
   <td style="text-align:left;"> Median : 797.0 </td>
   <td style="text-align:left;"> Median : 159.0 </td>
   <td style="text-align:left;"> Median :147.0 </td>
   <td style="text-align:left;"> Median :1050.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Mean   :1268.5 </td>
   <td style="text-align:left;"> Mean   : 80.79 </td>
   <td style="text-align:left;"> Mean   :1469 </td>
   <td style="text-align:left;"> Mean   :241.2 </td>
   <td style="text-align:left;"> Mean   : 55.25 </td>
   <td style="text-align:left;"> Mean   : 99.61 </td>
   <td style="text-align:left;"> Mean   :501.6 </td>
   <td style="text-align:left;"> Mean   : 735 </td>
   <td style="text-align:left;"> Mean   :124.7 </td>
   <td style="text-align:left;"> Mean   : 69.7 </td>
   <td style="text-align:left;"> Mean   : 1779 </td>
   <td style="text-align:left;"> Mean   :105.7 </td>
   <td style="text-align:left;"> Mean   : 553.0 </td>
   <td style="text-align:left;"> Mean   : 795.8 </td>
   <td style="text-align:left;"> Mean   : 246.5 </td>
   <td style="text-align:left;"> Mean   :145.4 </td>
   <td style="text-align:left;"> Mean   :1073.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 3rd Qu.:1915.5 </td>
   <td style="text-align:left;"> 3rd Qu.: 92.00 </td>
   <td style="text-align:left;"> 3rd Qu.:1537 </td>
   <td style="text-align:left;"> 3rd Qu.:273.0 </td>
   <td style="text-align:left;"> 3rd Qu.: 72.00 </td>
   <td style="text-align:left;"> 3rd Qu.:147.00 </td>
   <td style="text-align:left;"> 3rd Qu.:580.0 </td>
   <td style="text-align:left;"> 3rd Qu.: 925 </td>
   <td style="text-align:left;"> 3rd Qu.:153.2 </td>
   <td style="text-align:left;"> 3rd Qu.: 89.0 </td>
   <td style="text-align:left;"> 3rd Qu.: 1682 </td>
   <td style="text-align:left;"> 3rd Qu.:150.0 </td>
   <td style="text-align:left;"> 3rd Qu.: 611.0 </td>
   <td style="text-align:left;"> 3rd Qu.: 957.0 </td>
   <td style="text-align:left;"> 3rd Qu.: 249.2 </td>
   <td style="text-align:left;"> 3rd Qu.:162.0 </td>
   <td style="text-align:left;"> 3rd Qu.:1129.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Max.   :2535.0 </td>
   <td style="text-align:left;"> Max.   :146.00 </td>
   <td style="text-align:left;"> Max.   :2554 </td>
   <td style="text-align:left;"> Max.   :458.0 </td>
   <td style="text-align:left;"> Max.   :223.00 </td>
   <td style="text-align:left;"> Max.   :264.00 </td>
   <td style="text-align:left;"> Max.   :878.0 </td>
   <td style="text-align:left;"> Max.   :1399 </td>
   <td style="text-align:left;"> Max.   :697.0 </td>
   <td style="text-align:left;"> Max.   :201.0 </td>
   <td style="text-align:left;"> Max.   :30132 </td>
   <td style="text-align:left;"> Max.   :343.0 </td>
   <td style="text-align:left;"> Max.   :3645.0 </td>
   <td style="text-align:left;"> Max.   :4224.0 </td>
   <td style="text-align:left;"> Max.   :1898.0 </td>
   <td style="text-align:left;"> Max.   :228.0 </td>
   <td style="text-align:left;"> Max.   :2112.0 </td>
  </tr>
</tbody>
</table>

## BUILD MODELS

### Model 1

The first model includes several variables, selected manually, that have higher than average correlation to the target variable. They cover hitting, walking and fielding errors.


```

Call:
lm(formula = TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_BB + 
    TEAM_FIELDING_E, data = training)

Residuals:
    Min      1Q  Median      3Q     Max 
-50.901  -9.079   0.080   9.089  51.837 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      3.662987   3.299955   1.110    0.267    
TEAM_BATTING_H   0.049554   0.002081  23.807  < 2e-16 ***
TEAM_BATTING_BB  0.015938   0.003134   5.085 3.98e-07 ***
TEAM_FIELDING_E -0.014908   0.001746  -8.538  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 13.71 on 2272 degrees of freedom
Multiple R-squared:  0.2437,	Adjusted R-squared:  0.2427 
F-statistic: 244.1 on 3 and 2272 DF,  p-value: < 2.2e-16
```

All variables are significant, but the $R^2$ value is relatively small at 0.2356.

### Model 2

The second model expand the base hit variable, `TEAM_BATTING_H`, into its components - singles, doubles, triples and home runs. 


```

Call:
lm(formula = TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + 
    TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_FIELDING_E, 
    data = training)

Residuals:
    Min      1Q  Median      3Q     Max 
-52.307  -8.825   0.088   8.699  63.698 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      7.866838   3.433496   2.291 0.022043 *  
TEAM_BATTING_1B  0.045773   0.003141  14.572  < 2e-16 ***
TEAM_BATTING_2B  0.021963   0.007412   2.963 0.003075 ** 
TEAM_BATTING_3B  0.160496   0.014912  10.763  < 2e-16 ***
TEAM_BATTING_HR  0.080311   0.007755  10.356  < 2e-16 ***
TEAM_BATTING_BB  0.012342   0.003200   3.857 0.000118 ***
TEAM_FIELDING_E -0.018474   0.001946  -9.495  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 13.52 on 2269 degrees of freedom
Multiple R-squared:  0.2648,	Adjusted R-squared:  0.2628 
F-statistic: 136.2 on 6 and 2269 DF,  p-value: < 2.2e-16
```

All variables are still significant and $R^2$ is slightly improved at 0.2574.


### Higher Order Stepwise Regression

For the third model we will use a stepwise regression method using a backwards elimination process.  We also introduce some higher order polynomial variables.


```r
full_formula <- "TARGET_WINS ~ TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + TEAM_BATTING_1B + I(TEAM_BATTING_2B^2) + I(TEAM_BATTING_3B^2) + I(TEAM_BATTING_HR^2) + I(TEAM_BATTING_BB^2) + I(TEAM_BATTING_SO^2) + I(TEAM_BASERUN_SB^2) + I(TEAM_BASERUN_CS^2) + I(TEAM_PITCHING_H^2) + I(TEAM_PITCHING_HR^2) + I(TEAM_PITCHING_BB^2) + I(TEAM_PITCHING_SO^2) + I(TEAM_FIELDING_E^2) + I(TEAM_FIELDING_DP^2) + I(TEAM_BATTING_1B^2) "
full_model <- lm(full_formula, training)
step_back <- MASS::stepAIC(full_model, direction="backward", trace = F)
poly_call <- summary(step_back)$call
step_back <- lm(poly_call[2], training)
summary(step_back)
```

```

Call:
lm(formula = poly_call[2], data = training)

Residuals:
    Min      1Q  Median      3Q     Max 
-42.356  -8.122  -0.046   7.607  62.986 

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
(Intercept)            8.045e+01  1.448e+01   5.555 3.10e-08 ***
TEAM_BATTING_2B        1.587e-01  4.641e-02   3.420 0.000636 ***
TEAM_BATTING_3B        1.223e-01  1.639e-02   7.462 1.21e-13 ***
TEAM_BATTING_BB       -2.121e-01  1.842e-02 -11.513  < 2e-16 ***
TEAM_BATTING_SO        4.911e-02  8.382e-03   5.859 5.34e-09 ***
TEAM_BASERUN_SB        2.511e-02  5.260e-03   4.773 1.93e-06 ***
TEAM_PITCHING_H       -4.660e-03  1.181e-03  -3.946 8.17e-05 ***
TEAM_PITCHING_HR       1.451e-01  2.173e-02   6.675 3.09e-11 ***
TEAM_PITCHING_BB       2.400e-02  3.617e-03   6.636 4.03e-11 ***
TEAM_FIELDING_E       -5.477e-02  5.779e-03  -9.477  < 2e-16 ***
TEAM_FIELDING_DP      -9.404e-02  1.358e-02  -6.926 5.62e-12 ***
TEAM_BATTING_1B       -3.278e-02  1.900e-02  -1.725 0.084587 .  
I(TEAM_BATTING_2B^2)  -2.176e-04  9.132e-05  -2.383 0.017273 *  
I(TEAM_BATTING_HR^2)   3.910e-04  7.362e-05   5.311 1.19e-07 ***
I(TEAM_BATTING_BB^2)   1.930e-04  1.586e-05  12.168  < 2e-16 ***
I(TEAM_BATTING_SO^2)  -3.677e-05  5.228e-06  -7.032 2.68e-12 ***
I(TEAM_BASERUN_CS^2)   5.579e-04  8.535e-05   6.537 7.75e-11 ***
I(TEAM_PITCHING_H^2)   9.878e-08  3.674e-08   2.689 0.007230 ** 
I(TEAM_PITCHING_HR^2) -4.589e-04  8.618e-05  -5.325 1.11e-07 ***
I(TEAM_FIELDING_E^2)   1.141e-05  4.147e-06   2.751 0.005988 ** 
I(TEAM_BATTING_1B^2)   3.795e-05  7.795e-06   4.868 1.20e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 12.32 on 2255 degrees of freedom
Multiple R-squared:  0.3941,	Adjusted R-squared:  0.3887 
F-statistic: 73.33 on 20 and 2255 DF,  p-value: < 2.2e-16
```
This model has the highest adjusted R-squared value .Some variables p-values are not in 95 % siginificant level but they are in 90 % significant level which is acceptable.

## SELECT MODELS

In order to select which model is the "best" we will test it against a validation(evaluation) set.  We will examine the difference between the predicted and actual values.
