Statistical assignment 3
================
Alicia Rey-Herme (014989)
21 February 2019

In this assignment we will explore political interest (*vote6*) and how it changes over time.

Read data
---------

First we want to read and join the data for the first 7 waves of the Understanding Society. (Wave 8 does not have a variable for political interest). We only want five variables: personal identifier, sample origin, sex, age and political interest. It is tedious to join all the seven waves manually, and it makes sense to use a loop in this case. Since you don't yet know about iteration I'll provide the code for you; please see the explanation of the code here: <http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is to provide a path to the directory where the data are stored on your computer.

``` r
# Install tidyverse and data.table packages
library(tidyverse)
library(data.table)

# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.


# Create a vector with the file names and paths
files <- dir(
             # Select the folder where the files are stored.
             "C:/Users/alici/Documents/DA3/data/SN6614/tab/",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)
# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "C:/Users/alici/Documents/DA3/data/SN6614/tab//ukhls_w1/a_indresp.tab"
    ## [2] "C:/Users/alici/Documents/DA3/data/SN6614/tab//ukhls_w2/b_indresp.tab"
    ## [3] "C:/Users/alici/Documents/DA3/data/SN6614/tab//ukhls_w3/c_indresp.tab"
    ## [4] "C:/Users/alici/Documents/DA3/data/SN6614/tab//ukhls_w4/d_indresp.tab"
    ## [5] "C:/Users/alici/Documents/DA3/data/SN6614/tab//ukhls_w5/e_indresp.tab"
    ## [6] "C:/Users/alici/Documents/DA3/data/SN6614/tab//ukhls_w6/f_indresp.tab"
    ## [7] "C:/Users/alici/Documents/DA3/data/SN6614/tab//ukhls_w7/g_indresp.tab"
    ## [8] "C:/Users/alici/Documents/DA3/data/SN6614/tab//ukhls_w8/h_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")
for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

Reshape data (20 points)
------------------------

Now we have got the data from all 7 waves in the same data frame **all7** in the wide format. Note that the panel is unbalanced, i.e. we included all people who participated in at least one wave of the survey. Reshape the data to the long format. The resulting data frame should have six columns for six variables.

``` r
# Reshaping data from wide to long format
Long <- all7 %>%
  melt(id = "pidp") %>%
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  dcast(pidp + wave ~ variable)
head(Long)
```

    ##    pidp wave age_dv memorig sex_dv vote6
    ## 1 22445    a     NA      NA     NA    NA
    ## 2 22445    b     NA      NA     NA    NA
    ## 3 22445    c     NA      NA     NA    NA
    ## 4 22445    d     27       3      2     2
    ## 5 22445    e     28       3      2     2
    ## 6 22445    f     29       3      2     2

Filter and recode (20 points)
-----------------------------

Now we want to filter the data keeping only respondents from the original UKHLS sample for Great Britain (memorig == 1). We also want to clean the variables for sex (recoding it to "male" or "female") and political interest (keeping the values from 1 to 4 and coding all negative values as missing). Tabulate *sex* and *vote6* to make sure your recodings were correct.

``` r
# Filtering data and Recoding sex and political interest variables
Long <- Long %>%
  filter(memorig == 1) %>%
   mutate(sex_dv = ifelse(sex_dv == 1, "male",
                           ifelse(sex_dv == 2, "female", NA))
          ) %>%
  mutate(vote6 = ifelse(vote6 < 0, NA_real_, vote6))

# Table for sex variable
Long %>%
        count(sex_dv)
```

    ## # A tibble: 3 x 2
    ##   sex_dv      n
    ##   <chr>   <int>
    ## 1 female 117665
    ## 2 male   100341
    ## 3 <NA>        9

``` r
# Table for vote6 variable
Long %>%
        count(vote6)
```

    ## # A tibble: 5 x 2
    ##   vote6     n
    ##   <dbl> <int>
    ## 1     1 21660
    ## 2     2 70952
    ## 3     3 56134
    ## 4     4 52145
    ## 5    NA 17124

Calculate mean political interest by sex and wave (10 points)
-------------------------------------------------------------

Political interest is an ordinal variable, but we will treat it as interval and calculate mean political interest for men and women in each wave.

``` r
# Create table of mean vote6 given wave and sex
meanVote6 <- Long %>%
    # Filter out missing values
        filter(complete.cases(.)) %>%
    # Group by Sex to calculate sums
        group_by(wave, sex_dv) %>%
    # Creating a new variable with mean
        summarise(MeanPI = mean(vote6, na.rm = TRUE))
meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   wave [?]
    ##    wave  sex_dv MeanPI
    ##    <chr> <chr>   <dbl>
    ##  1 a     female   2.84
    ##  2 a     male     2.53
    ##  3 b     female   2.82
    ##  4 b     male     2.51
    ##  5 c     female   2.87
    ##  6 c     male     2.54
    ##  7 d     female   2.89
    ##  8 d     male     2.55
    ##  9 e     female   2.87
    ## 10 e     male     2.51
    ## 11 f     female   2.81
    ## 12 f     male     2.47
    ## 13 g     female   2.73
    ## 14 g     male     2.42

Reshape the data frame with summary statistics (20 points)
----------------------------------------------------------

Your resulting data frame with the means is in the long format. Reshape it to the wide format. It should look like this:

| sex\_dv | a   | b   | c   | d   | e   | f   | g   |
|---------|-----|-----|-----|-----|-----|-----|-----|
| female  |     |     |     |     |     |     |     |
| male    |     |     |     |     |     |     |     |

In the cells of this table you should have mean political interest by sex and wave.

Write a short interpretation of your findings.

``` r
# Reshaping from long to wide format
meanVote6 %>%
      melt(id = c("wave", "sex_dv")) %>%
      unite("variable", c("wave", "variable"), sep = "_") %>%
      dcast(sex_dv ~ variable)
```

    ##   sex_dv a_MeanPI b_MeanPI c_MeanPI d_MeanPI e_MeanPI f_MeanPI g_MeanPI
    ## 1 female 2.839437 2.816370 2.874985 2.887006 2.865092 2.807873 2.728400
    ## 2   male 2.527112 2.512143 2.544448 2.551704 2.507875 2.472188 2.416039

The data appears to suggest that females are consistently more likely on average to have less political interest than males. From Waves a-e, both men and women are more likely to be on the "lower" end of the scale (ie. Responding as 3 or 4), however men become more interested (More likely to respond 1 or 2) in Waves f and q. Interestingly, the fluctuations in interest in sexes between waves seem to be rise and fall in tandem (The exception to this being Wave C to D).

Estimate stability of political interest (30 points)
----------------------------------------------------

Political scientists have been arguing how stable the level of political interest is over the life course. Imagine someone who is not interested in politics at all so that their value of *vote6* is always 4. Their level of political interest is very stable over time, as stable as the level of political interest of someone who is always very interested in politics (*vote6* = 1). On the other hand, imagine someone who changes their value of *votes6* from 1 to 4 and back every other wave. Their level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is going to be equal to the sum of the absolute values of changes in political interest from wave to wave. Let us call this measure Delta. It is difficult for me to typeset a mathematical formula in Markdown, but I'll explain this informally.

Imagine a person with the level of political interest that is constant over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from "very interested in politics" to "fairly interested in politics": {1, 1, 1, 1, 2, 2, 2}. For them, Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from "very interested in politics" to "not at all interested" every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta = (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3 \* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with non-missing values for political interest in all seven waves.
2.  Calculate Delta for each person in the data set.
3.  Calculate mean Delta for men and women.
4.  Calculate mean Delta by age (at wave 1) and plot the local polynomial curve showing the association between age at wave 1 and mean Delta. You can use either **ggplot2** or the *scatter.smooth()* function from base R.
5.  Write a short interpretation of your findings.

``` r
# Q1: Keep only respondents with non-missing values in all 7 waves
Base <- Long %>%
  # Filter out all missing values
  filter(complete.cases(.)) %>%
  # Keep only respondents who have answered all 7 waves through creating a variable to count, and only including those with 7 waves.
  group_by(pidp) %>%
  mutate(NumberWave = length(unique(wave))) %>%
  filter(NumberWave == 7)

# Q2: Calculate Deltas for each person
Deltas <- Base %>%
  group_by(pidp) %>%
  # Sum of absolute change: (n2 - n1) + (n3 - n1) ... etc.
  mutate(Delta = sum(abs(vote6 - lag(vote6)), na.rm = TRUE)) 

# For the next questions, the table was simplified to only show Wave 1 ("a") rows. This further answers Q2 more neatly.
DAnalysis <- Deltas %>%
  filter(wave == "a")
DAnalysis
```

    ## # A tibble: 14,820 x 8
    ## # Groups:   pidp [14,820]
    ##        pidp wave  age_dv memorig sex_dv vote6 NumberWave Delta
    ##       <int> <chr>  <int>   <int> <chr>  <dbl>      <int> <dbl>
    ##  1 68004087 a         59       1 male       2          7     2
    ##  2 68006127 a         39       1 female     4          7     0
    ##  3 68006807 a         72       1 female     4          7     1
    ##  4 68008847 a         51       1 female     2          7     6
    ##  5 68009527 a         31       1 male       2          7     2
    ##  6 68010887 a         45       1 female     2          7     6
    ##  7 68020407 a         72       1 female     3          7     4
    ##  8 68025847 a         73       1 female     4          7     8
    ##  9 68029927 a         36       1 female     2          7     5
    ## 10 68031967 a         61       1 female     4          7     4
    ## # ... with 14,810 more rows

``` r
# Calculate Mean by Sex
DSexMean <- DAnalysis %>%
      group_by(sex_dv) %>%
      summarise(mean = mean(Delta))
DSexMean
```

    ## # A tibble: 2 x 2
    ##   sex_dv  mean
    ##   <chr>  <dbl>
    ## 1 female  2.49
    ## 2 male    2.53

On average, both men and women have a similar amount of political volitility, with men being slightly more volatile than women. On average, respondents over the course of 7 waves will change their political interest by a net absolute sum of approximately 2.5.

``` r
# Calculate Mean by age (at wave 1)
DAgeMean <- DAnalysis %>%
      group_by(age_dv) %>%
      summarise(mean = mean(Delta))
DAgeMean
```

    ## # A tibble: 78 x 2
    ##    age_dv  mean
    ##     <int> <dbl>
    ##  1     15  2   
    ##  2     16  2.78
    ##  3     17  2.55
    ##  4     18  2.83
    ##  5     19  2.82
    ##  6     20  2.39
    ##  7     21  3.08
    ##  8     22  2.59
    ##  9     23  2.42
    ## 10     24  2.15
    ## # ... with 68 more rows

``` r
# Plotting DAgeMean
DAgeMean %>%
  # Create scatterplot with local polynomial curve
      ggplot(aes(x = age_dv, y = mean)) +
      geom_smooth() +
  # Add axis labels
      xlab("Age at Wave 1") +
      ylab("Volitility of Political Interest")
```

![](Assignment_3_files/figure-markdown_github/unnamed-chunk-7-1.png) Throughout the life course, it seems that respondents who were young adults in Wave 1 tend to fluctuate slightly in their political opinion, whilst middle-aged respondents changed the least, and seniors (Exponentially past the age of around 55) are most volatile. This could suggest that people will be likely to vary their opinions during their young adult years whilst increasingly stabilising through their adult and middle-aged years. This trend then dramatically changes as people enter their senior years, where they become exponentially more likely to fluctuate in their political opinions.
