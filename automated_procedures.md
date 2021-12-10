Automated Model Building
================
Louis Sharp
12/3/2021

``` r
library(tidyverse)
library(corrplot)
```

## Non-transformed variables

``` r
cdi_df = read_csv("./data/cdi.csv") %>% 
  mutate(crime_rate_1k = (crimes/pop)*1000) %>% 
  relocate(crime_rate_1k, .before = id)
         
cdi_regression_df = 
  cdi_df %>% 
  select(!(id:state))
```

Removed id, city, and state variables from the data set as those are not
numerical variables and as such we cannot do regression on them. id is
numerical but not actual data.

``` r
#pairs(cdi_regression_df) not super informative

corrplot(cor(cdi_regression_df), type = "upper", diag = FALSE)
```

![](automated_procedures_files/figure-gfm/correlation%20plot1-1.png)<!-- -->

``` r
par(mfrow = c(3, 5))
boxplot(pull(cdi_regression_df, crime_rate_1k), main = "crime_rate_1k")
boxplot(pull(cdi_regression_df, area), main = "area")
boxplot(pull(cdi_regression_df, pop), main = "pop")
boxplot(pull(cdi_regression_df, pop18), main = "pop18")
boxplot(pull(cdi_regression_df, pop65), main = "pop65")
boxplot(pull(cdi_regression_df, docs), main = "docs")
boxplot(pull(cdi_regression_df, beds), main = "beds")
boxplot(pull(cdi_regression_df, crimes), main = "crimes")
boxplot(pull(cdi_regression_df, hsgrad), main = "hsgrad")
boxplot(pull(cdi_regression_df, bagrad), main = "bagrad")
boxplot(pull(cdi_regression_df, poverty), main = "poverty")
boxplot(pull(cdi_regression_df, unemp), main = "unemp")
boxplot(pull(cdi_regression_df, pcincome), main = "pcincome")
boxplot(pull(cdi_regression_df, totalinc), main = "totalinc")
boxplot(pull(cdi_regression_df, region), main = "region")
```

![](automated_procedures_files/figure-gfm/correlation%20plot1-2.png)<!-- -->

Check for correlation, normal distribution.

``` r
all_vars_model = lm(crime_rate_1k ~ ., data = cdi_regression_df)
summary(all_vars_model)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ ., data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -46.412  -9.609  -0.327   9.518  54.351 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -7.043e+01  2.349e+01  -2.999  0.00287 ** 
    ## area        -9.168e-04  5.711e-04  -1.605  0.10914    
    ## pop         -2.460e-05  1.540e-05  -1.597  0.11106    
    ## pop18        1.465e+00  2.816e-01   5.200 3.10e-07 ***
    ## pop65        5.163e-01  2.524e-01   2.046  0.04140 *  
    ## docs        -1.348e-03  2.095e-03  -0.643  0.52033    
    ## beds         4.978e-03  1.516e-03   3.284  0.00111 ** 
    ## crimes       4.780e-04  3.212e-05  14.881  < 2e-16 ***
    ## hsgrad       1.371e-02  2.135e-01   0.064  0.94885    
    ## bagrad      -4.184e-01  2.477e-01  -1.690  0.09185 .  
    ## poverty      2.189e+00  3.257e-01   6.721 5.83e-11 ***
    ## unemp       -5.202e-01  4.363e-01  -1.192  0.23379    
    ## pcincome     2.755e-03  4.697e-04   5.866 8.99e-09 ***
    ## totalinc    -1.085e-03  6.670e-04  -1.627  0.10445    
    ## region       8.464e+00  8.760e-01   9.663  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.49 on 425 degrees of freedom
    ## Multiple R-squared:  0.6889, Adjusted R-squared:  0.6786 
    ## F-statistic: 67.22 on 14 and 425 DF,  p-value: < 2.2e-16

``` r
back_elim = update(all_vars_model, . ~ . -hsgrad)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + docs + 
    ##     beds + crimes + bagrad + poverty + unemp + pcincome + totalinc + 
    ##     region, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -46.403  -9.625  -0.336   9.518  54.374 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6.921e+01  1.379e+01  -5.021 7.59e-07 ***
    ## area        -9.141e-04  5.688e-04  -1.607 0.108804    
    ## pop         -2.461e-05  1.538e-05  -1.600 0.110382    
    ## pop18        1.462e+00  2.786e-01   5.247 2.44e-07 ***
    ## pop65        5.142e-01  2.501e-01   2.056 0.040359 *  
    ## docs        -1.360e-03  2.084e-03  -0.652 0.514463    
    ## beds         4.989e-03  1.505e-03   3.315 0.000996 ***
    ## crimes       4.779e-04  3.206e-05  14.908  < 2e-16 ***
    ## bagrad      -4.100e-01  2.099e-01  -1.953 0.051436 .  
    ## poverty      2.177e+00  2.666e-01   8.166 3.64e-15 ***
    ## unemp       -5.240e-01  4.317e-01  -1.214 0.225514    
    ## pcincome     2.748e-03  4.573e-04   6.009 4.00e-09 ***
    ## totalinc    -1.085e-03  6.661e-04  -1.628 0.104206    
    ## region       8.469e+00  8.713e-01   9.720  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.47 on 426 degrees of freedom
    ## Multiple R-squared:  0.6889, Adjusted R-squared:  0.6794 
    ## F-statistic: 72.56 on 13 and 426 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -docs)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + beds + 
    ##     crimes + bagrad + poverty + unemp + pcincome + totalinc + 
    ##     region, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -46.422  -9.735  -0.348   9.490  54.143 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6.846e+01  1.373e+01  -4.987 8.95e-07 ***
    ## area        -9.270e-04  5.681e-04  -1.632   0.1035    
    ## pop         -2.204e-05  1.486e-05  -1.483   0.1388    
    ## pop18        1.450e+00  2.778e-01   5.219 2.81e-07 ***
    ## pop65        5.095e-01  2.498e-01   2.040   0.0420 *  
    ## beds         4.313e-03  1.091e-03   3.954 9.00e-05 ***
    ## crimes       4.795e-04  3.194e-05  15.011  < 2e-16 ***
    ## bagrad      -4.350e-01  2.063e-01  -2.109   0.0355 *  
    ## poverty      2.167e+00  2.660e-01   8.148 4.13e-15 ***
    ## unemp       -5.311e-01  4.313e-01  -1.231   0.2189    
    ## pcincome     2.777e-03  4.550e-04   6.103 2.33e-09 ***
    ## totalinc    -1.279e-03  5.951e-04  -2.150   0.0321 *  
    ## region       8.432e+00  8.689e-01   9.705  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.46 on 427 degrees of freedom
    ## Multiple R-squared:  0.6886, Adjusted R-squared:  0.6798 
    ## F-statistic: 78.67 on 12 and 427 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -unemp)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + beds + 
    ##     crimes + bagrad + poverty + pcincome + totalinc + region, 
    ##     data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -45.721  -9.891  -0.325   9.465  54.848 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -7.003e+01  1.368e+01  -5.121 4.61e-07 ***
    ## area        -1.012e-03  5.642e-04  -1.793   0.0737 .  
    ## pop         -2.391e-05  1.479e-05  -1.616   0.1068    
    ## pop18        1.436e+00  2.778e-01   5.170 3.59e-07 ***
    ## pop65        4.850e-01  2.492e-01   1.947   0.0522 .  
    ## beds         4.666e-03  1.053e-03   4.431 1.19e-05 ***
    ## crimes       4.798e-04  3.196e-05  15.013  < 2e-16 ***
    ## bagrad      -3.408e-01  1.917e-01  -1.778   0.0761 .  
    ## poverty      2.020e+00  2.379e-01   8.491 3.39e-16 ***
    ## pcincome     2.647e-03  4.428e-04   5.977 4.79e-09 ***
    ## totalinc    -1.247e-03  5.949e-04  -2.096   0.0366 *  
    ## region       8.643e+00  8.524e-01  10.140  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.47 on 428 degrees of freedom
    ## Multiple R-squared:  0.6875, Adjusted R-squared:  0.6794 
    ## F-statistic: 85.58 on 11 and 428 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -pop)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop18 + pop65 + beds + crimes + 
    ##     bagrad + poverty + pcincome + totalinc + region, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -47.946  -9.628  -0.356   9.382  55.398 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -7.928e+01  1.245e+01  -6.370 4.86e-10 ***
    ## area        -1.314e-03  5.334e-04  -2.463   0.0142 *  
    ## pop18        1.477e+00  2.772e-01   5.328 1.61e-07 ***
    ## pop65        5.379e-01  2.475e-01   2.174   0.0303 *  
    ## beds         3.971e-03  9.631e-04   4.123 4.49e-05 ***
    ## crimes       4.525e-04  2.715e-05  16.666  < 2e-16 ***
    ## bagrad      -3.569e-01  1.918e-01  -1.861   0.0634 .  
    ## poverty      2.118e+00  2.305e-01   9.188  < 2e-16 ***
    ## pcincome     3.016e-03  3.802e-04   7.933 1.88e-14 ***
    ## totalinc    -2.167e-03  1.745e-04 -12.416  < 2e-16 ***
    ## region       8.640e+00  8.540e-01  10.118  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.5 on 429 degrees of freedom
    ## Multiple R-squared:  0.6856, Adjusted R-squared:  0.6782 
    ## F-statistic: 93.53 on 10 and 429 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -bagrad)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop18 + pop65 + beds + crimes + 
    ##     poverty + pcincome + totalinc + region, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -46.685  -9.736  -0.681   9.792  56.491 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6.836e+01  1.101e+01  -6.211 1.24e-09 ***
    ## area        -1.279e-03  5.346e-04  -2.393   0.0171 *  
    ## pop18        1.186e+00  2.295e-01   5.166 3.66e-07 ***
    ## pop65        5.713e-01  2.475e-01   2.308   0.0215 *  
    ## beds         4.002e-03  9.657e-04   4.145 4.10e-05 ***
    ## crimes       4.517e-04  2.722e-05  16.594  < 2e-16 ***
    ## poverty      2.120e+00  2.312e-01   9.168  < 2e-16 ***
    ## pcincome     2.497e-03  2.591e-04   9.637  < 2e-16 ***
    ## totalinc    -2.152e-03  1.748e-04 -12.307  < 2e-16 ***
    ## region       8.194e+00  8.219e-01   9.969  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.55 on 430 degrees of freedom
    ## Multiple R-squared:  0.683,  Adjusted R-squared:  0.6764 
    ## F-statistic: 102.9 on 9 and 430 DF,  p-value: < 2.2e-16

``` r
step(all_vars_model, direction = "backward")
```

    ## Start:  AIC=2426.22
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + crimes + 
    ##     hsgrad + bagrad + poverty + unemp + pcincome + totalinc + 
    ##     region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - hsgrad    1         1 102001 2424.2
    ## - docs      1        99 102099 2424.7
    ## - unemp     1       341 102341 2425.7
    ## <none>                  102000 2426.2
    ## - pop       1       612 102611 2426.8
    ## - area      1       619 102618 2426.9
    ## - totalinc  1       635 102635 2426.9
    ## - bagrad    1       685 102685 2427.2
    ## - pop65     1      1004 103004 2428.5
    ## - beds      1      2589 104588 2435.2
    ## - pop18     1      6490 108489 2451.4
    ## - pcincome  1      8258 110258 2458.5
    ## - poverty   1     10840 112840 2468.7
    ## - region    1     22408 124407 2511.6
    ## - crimes    1     53146 155146 2608.8
    ## 
    ## Step:  AIC=2424.22
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + crimes + 
    ##     bagrad + poverty + unemp + pcincome + totalinc + region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - docs      1       102 102102 2422.7
    ## - unemp     1       353 102353 2423.7
    ## <none>                  102001 2424.2
    ## - pop       1       613 102613 2424.9
    ## - area      1       618 102619 2424.9
    ## - totalinc  1       635 102635 2424.9
    ## - bagrad    1       914 102914 2426.1
    ## - pop65     1      1012 103013 2426.6
    ## - beds      1      2631 104631 2433.4
    ## - pop18     1      6593 108594 2449.8
    ## - pcincome  1      8646 110647 2458.0
    ## - poverty   1     15968 117968 2486.2
    ## - region    1     22622 124622 2510.4
    ## - crimes    1     53213 155213 2606.9
    ## 
    ## Step:  AIC=2422.66
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + beds + crimes + 
    ##     bagrad + poverty + unemp + pcincome + totalinc + region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - unemp     1       363 102465 2422.2
    ## <none>                  102102 2422.7
    ## - pop       1       526 102628 2422.9
    ## - area      1       637 102739 2423.4
    ## - pop65     1       995 103097 2424.9
    ## - bagrad    1      1063 103166 2425.2
    ## - totalinc  1      1105 103207 2425.4
    ## - beds      1      3738 105841 2436.5
    ## - pop18     1      6514 108616 2447.9
    ## - pcincome  1      8907 111010 2457.5
    ## - poverty   1     15875 117977 2484.2
    ## - region    1     22520 124622 2508.4
    ## - crimes    1     53883 155986 2607.1
    ## 
    ## Step:  AIC=2422.22
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + beds + crimes + 
    ##     bagrad + poverty + pcincome + totalinc + region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  102465 2422.2
    ## - pop       1       625 103090 2422.9
    ## - bagrad    1       757 103222 2423.5
    ## - area      1       770 103235 2423.5
    ## - pop65     1       907 103372 2424.1
    ## - totalinc  1      1052 103517 2424.7
    ## - beds      1      4700 107165 2439.9
    ## - pop18     1      6400 108865 2446.9
    ## - pcincome  1      8553 111018 2455.5
    ## - poverty   1     17262 119727 2488.7
    ## - region    1     24614 127079 2514.9
    ## - crimes    1     53961 156426 2606.4

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + beds + 
    ##     crimes + bagrad + poverty + pcincome + totalinc + region, 
    ##     data = cdi_regression_df)
    ## 
    ## Coefficients:
    ## (Intercept)         area          pop        pop18        pop65         beds  
    ##  -7.003e+01   -1.012e-03   -2.391e-05    1.436e+00    4.850e-01    4.666e-03  
    ##      crimes       bagrad      poverty     pcincome     totalinc       region  
    ##   4.798e-04   -3.408e-01    2.020e+00    2.647e-03   -1.247e-03    8.643e+00

``` r
step(all_vars_model, direction = "forward")
```

    ## Start:  AIC=2426.22
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + crimes + 
    ##     hsgrad + bagrad + poverty + unemp + pcincome + totalinc + 
    ##     region

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + docs + 
    ##     beds + crimes + hsgrad + bagrad + poverty + unemp + pcincome + 
    ##     totalinc + region, data = cdi_regression_df)
    ## 
    ## Coefficients:
    ## (Intercept)         area          pop        pop18        pop65         docs  
    ##  -7.043e+01   -9.168e-04   -2.460e-05    1.465e+00    5.163e-01   -1.348e-03  
    ##        beds       crimes       hsgrad       bagrad      poverty        unemp  
    ##   4.978e-03    4.780e-04    1.371e-02   -4.184e-01    2.189e+00   -5.202e-01  
    ##    pcincome     totalinc       region  
    ##   2.755e-03   -1.085e-03    8.464e+00

Automatic forward selection: 12 variables.

``` r
step(all_vars_model, direction = "both")
```

    ## Start:  AIC=2426.22
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + crimes + 
    ##     hsgrad + bagrad + poverty + unemp + pcincome + totalinc + 
    ##     region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - hsgrad    1         1 102001 2424.2
    ## - docs      1        99 102099 2424.7
    ## - unemp     1       341 102341 2425.7
    ## <none>                  102000 2426.2
    ## - pop       1       612 102611 2426.8
    ## - area      1       619 102618 2426.9
    ## - totalinc  1       635 102635 2426.9
    ## - bagrad    1       685 102685 2427.2
    ## - pop65     1      1004 103004 2428.5
    ## - beds      1      2589 104588 2435.2
    ## - pop18     1      6490 108489 2451.4
    ## - pcincome  1      8258 110258 2458.5
    ## - poverty   1     10840 112840 2468.7
    ## - region    1     22408 124407 2511.6
    ## - crimes    1     53146 155146 2608.8
    ## 
    ## Step:  AIC=2424.22
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + crimes + 
    ##     bagrad + poverty + unemp + pcincome + totalinc + region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - docs      1       102 102102 2422.7
    ## - unemp     1       353 102353 2423.7
    ## <none>                  102001 2424.2
    ## - pop       1       613 102613 2424.9
    ## - area      1       618 102619 2424.9
    ## - totalinc  1       635 102635 2424.9
    ## - bagrad    1       914 102914 2426.1
    ## + hsgrad    1         1 102000 2426.2
    ## - pop65     1      1012 103013 2426.6
    ## - beds      1      2631 104631 2433.4
    ## - pop18     1      6593 108594 2449.8
    ## - pcincome  1      8646 110647 2458.0
    ## - poverty   1     15968 117968 2486.2
    ## - region    1     22622 124622 2510.4
    ## - crimes    1     53213 155213 2606.9
    ## 
    ## Step:  AIC=2422.66
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + beds + crimes + 
    ##     bagrad + poverty + unemp + pcincome + totalinc + region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - unemp     1       363 102465 2422.2
    ## <none>                  102102 2422.7
    ## - pop       1       526 102628 2422.9
    ## - area      1       637 102739 2423.4
    ## + docs      1       102 102001 2424.2
    ## + hsgrad    1         4 102099 2424.7
    ## - pop65     1       995 103097 2424.9
    ## - bagrad    1      1063 103166 2425.2
    ## - totalinc  1      1105 103207 2425.4
    ## - beds      1      3738 105841 2436.5
    ## - pop18     1      6514 108616 2447.9
    ## - pcincome  1      8907 111010 2457.5
    ## - poverty   1     15875 117977 2484.2
    ## - region    1     22520 124622 2508.4
    ## - crimes    1     53883 155986 2607.1
    ## 
    ## Step:  AIC=2422.22
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + beds + crimes + 
    ##     bagrad + poverty + pcincome + totalinc + region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  102465 2422.2
    ## + unemp     1       363 102102 2422.7
    ## - pop       1       625 103090 2422.9
    ## - bagrad    1       757 103222 2423.5
    ## - area      1       770 103235 2423.5
    ## + docs      1       112 102353 2423.7
    ## - pop65     1       907 103372 2424.1
    ## + hsgrad    1        20 102445 2424.1
    ## - totalinc  1      1052 103517 2424.7
    ## - beds      1      4700 107165 2439.9
    ## - pop18     1      6400 108865 2446.9
    ## - pcincome  1      8553 111018 2455.5
    ## - poverty   1     17262 119727 2488.7
    ## - region    1     24614 127079 2514.9
    ## - crimes    1     53961 156426 2606.4

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + beds + 
    ##     crimes + bagrad + poverty + pcincome + totalinc + region, 
    ##     data = cdi_regression_df)
    ## 
    ## Coefficients:
    ## (Intercept)         area          pop        pop18        pop65         beds  
    ##  -7.003e+01   -1.012e-03   -2.391e-05    1.436e+00    4.850e-01    4.666e-03  
    ##      crimes       bagrad      poverty     pcincome     totalinc       region  
    ##   4.798e-04   -3.408e-01    2.020e+00    2.647e-03   -1.247e-03    8.643e+00

Automatic: 7 variables.

Using manual backwards elimination, we have a model with 9 variables.
Using the automatic step function, we have the same model but with
bagrad as well (cutoff differences?).  
Removing crimes (since it is obviously correlated with crime\_rate\_1k),
we get a different model.

``` r
cdi_regression_df = 
  cdi_df %>% 
  select(!(id:state), -crimes)

all_vars_model = lm(crime_rate_1k ~ ., data = cdi_regression_df)
summary(all_vars_model)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ ., data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -54.497 -10.176  -0.849   9.575 162.105 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -9.478e+01  2.887e+01  -3.283 0.001110 ** 
    ## area        -2.377e-03  6.930e-04  -3.429 0.000664 ***
    ## pop          8.741e-05  1.656e-05   5.280 2.06e-07 ***
    ## pop18        1.570e+00  3.468e-01   4.527 7.76e-06 ***
    ## pop65        5.316e-01  3.109e-01   1.710 0.087978 .  
    ## docs        -3.828e-03  2.573e-03  -1.488 0.137467    
    ## beds         6.605e-03  1.862e-03   3.547 0.000434 ***
    ## hsgrad      -1.132e-01  2.628e-01  -0.431 0.666791    
    ## bagrad      -3.511e-01  3.050e-01  -1.151 0.250407    
    ## poverty      2.735e+00  3.987e-01   6.861 2.42e-11 ***
    ## unemp       -5.965e-01  5.374e-01  -1.110 0.267625    
    ## pcincome     3.871e-03  5.711e-04   6.778 4.06e-11 ***
    ## totalinc    -4.465e-03  7.725e-04  -5.780 1.45e-08 ***
    ## region       9.161e+00  1.078e+00   8.501 3.19e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.08 on 426 degrees of freedom
    ## Multiple R-squared:  0.5268, Adjusted R-squared:  0.5123 
    ## F-statistic: 36.48 on 13 and 426 DF,  p-value: < 2.2e-16

``` r
back_elim = update(all_vars_model, . ~ . -hsgrad)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + docs + 
    ##     beds + bagrad + poverty + unemp + pcincome + totalinc + region, 
    ##     data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -54.148 -10.190  -0.727   9.612 162.247 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.049e+02  1.673e+01  -6.271 8.78e-10 ***
    ## area        -2.401e-03  6.900e-04  -3.480 0.000553 ***
    ## pop          8.770e-05  1.653e-05   5.306 1.80e-07 ***
    ## pop18        1.591e+00  3.431e-01   4.637 4.71e-06 ***
    ## pop65        5.485e-01  3.081e-01   1.780 0.075764 .  
    ## docs        -3.733e-03  2.561e-03  -1.458 0.145648    
    ## beds         6.520e-03  1.850e-03   3.524 0.000471 ***
    ## bagrad      -4.206e-01  2.586e-01  -1.626 0.104646    
    ## poverty      2.835e+00  3.239e-01   8.752  < 2e-16 ***
    ## unemp       -5.651e-01  5.319e-01  -1.062 0.288664    
    ## pcincome     3.928e-03  5.550e-04   7.078 6.03e-12 ***
    ## totalinc    -4.475e-03  7.714e-04  -5.801 1.29e-08 ***
    ## region       9.119e+00  1.072e+00   8.505 3.09e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.07 on 427 degrees of freedom
    ## Multiple R-squared:  0.5266, Adjusted R-squared:  0.5133 
    ## F-statistic: 39.58 on 12 and 427 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -unemp)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + docs + 
    ##     beds + bagrad + poverty + pcincome + totalinc + region, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -54.770 -10.436  -0.807   9.722 162.556 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.066e+02  1.665e+01  -6.404 3.98e-10 ***
    ## area        -2.491e-03  6.848e-04  -3.638 0.000308 ***
    ## pop          8.565e-05  1.642e-05   5.217 2.84e-07 ***
    ## pop18        1.577e+00  3.429e-01   4.598 5.62e-06 ***
    ## pop65        5.227e-01  3.072e-01   1.701 0.089588 .  
    ## docs        -3.802e-03  2.560e-03  -1.485 0.138235    
    ## beds         6.930e-03  1.810e-03   3.830 0.000148 ***
    ## bagrad      -3.192e-01  2.404e-01  -1.328 0.184957    
    ## poverty      2.680e+00  2.892e-01   9.268  < 2e-16 ***
    ## pcincome     3.790e-03  5.395e-04   7.025 8.50e-12 ***
    ## totalinc    -4.433e-03  7.705e-04  -5.753 1.67e-08 ***
    ## region       9.346e+00  1.051e+00   8.892  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.07 on 428 degrees of freedom
    ## Multiple R-squared:  0.5253, Adjusted R-squared:  0.5131 
    ## F-statistic: 43.06 on 11 and 428 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -bagrad)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + docs + 
    ##     beds + poverty + pcincome + totalinc + region, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -53.985 -10.562  -0.847  10.116 162.158 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -9.711e+01  1.504e+01  -6.458 2.88e-10 ***
    ## area        -2.440e-03  6.843e-04  -3.565 0.000404 ***
    ## pop          8.324e-05  1.633e-05   5.097 5.18e-07 ***
    ## pop18        1.330e+00  2.884e-01   4.612 5.27e-06 ***
    ## pop65        5.510e-01  3.067e-01   1.797 0.073112 .  
    ## docs        -4.437e-03  2.517e-03  -1.762 0.078711 .  
    ## beds         7.307e-03  1.789e-03   4.084 5.27e-05 ***
    ## poverty      2.682e+00  2.894e-01   9.265  < 2e-16 ***
    ## pcincome     3.311e-03  4.019e-04   8.239 2.13e-15 ***
    ## totalinc    -4.282e-03  7.628e-04  -5.614 3.56e-08 ***
    ## region       8.977e+00  1.015e+00   8.847  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.09 on 429 degrees of freedom
    ## Multiple R-squared:  0.5234, Adjusted R-squared:  0.5123 
    ## F-statistic: 47.11 on 10 and 429 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -docs)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + beds + 
    ##     poverty + pcincome + totalinc + region, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -58.299 -10.547  -0.788  10.038 163.736 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -9.261e+01  1.485e+01  -6.234 1.08e-09 ***
    ## area        -2.491e-03  6.854e-04  -3.635 0.000312 ***
    ## pop          9.256e-05  1.549e-05   5.976 4.79e-09 ***
    ## pop18        1.228e+00  2.832e-01   4.336 1.81e-05 ***
    ## pop65        5.414e-01  3.074e-01   1.761 0.078941 .  
    ## beds         5.136e-03  1.301e-03   3.949 9.18e-05 ***
    ## poverty      2.650e+00  2.896e-01   9.150  < 2e-16 ***
    ## pcincome     3.295e-03  4.028e-04   8.180 3.23e-15 ***
    ## totalinc    -4.943e-03  6.658e-04  -7.425 6.11e-13 ***
    ## region       8.775e+00  1.011e+00   8.683  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.13 on 430 degrees of freedom
    ## Multiple R-squared:  0.5199, Adjusted R-squared:  0.5099 
    ## F-statistic: 51.74 on 9 and 430 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -pop65)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + beds + poverty + 
    ##     pcincome + totalinc + region, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -59.027 -10.574  -0.786  10.041 163.996 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -7.512e+01  1.107e+01  -6.783 3.89e-11 ***
    ## area        -2.380e-03  6.841e-04  -3.479 0.000554 ***
    ## pop          8.833e-05  1.534e-05   5.759 1.61e-08 ***
    ## pop18        9.142e-01  2.207e-01   4.142 4.14e-05 ***
    ## beds         5.734e-03  1.259e-03   4.555 6.81e-06 ***
    ## poverty      2.618e+00  2.897e-01   9.037  < 2e-16 ***
    ## pcincome     3.229e-03  4.020e-04   8.032 9.27e-15 ***
    ## totalinc    -4.835e-03  6.646e-04  -7.276 1.64e-12 ***
    ## region       8.524e+00  1.003e+00   8.499 3.16e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.18 on 431 degrees of freedom
    ## Multiple R-squared:  0.5164, Adjusted R-squared:  0.5075 
    ## F-statistic: 57.54 on 8 and 431 DF,  p-value: < 2.2e-16

``` r
step(all_vars_model, direction = "backward")
```

    ## Start:  AIC=2608.75
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + hsgrad + 
    ##     bagrad + poverty + unemp + pcincome + totalinc + region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - hsgrad    1      67.6 155213 2606.9
    ## - unemp     1     448.7 155595 2608.0
    ## - bagrad    1     482.4 155628 2608.1
    ## <none>                  155146 2608.8
    ## - docs      1     806.5 155952 2609.0
    ## - pop65     1    1065.0 156211 2609.8
    ## - area      1    4283.4 159429 2618.7
    ## - beds      1    4580.8 159727 2619.6
    ## - pop18     1    7464.9 162611 2627.4
    ## - pop       1   10152.8 165299 2634.6
    ## - totalinc  1   12165.7 167312 2640.0
    ## - pcincome  1   16733.1 171879 2651.8
    ## - poverty   1   17143.2 172289 2652.9
    ## - region    1   26321.0 181467 2675.7
    ## 
    ## Step:  AIC=2606.94
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + bagrad + 
    ##     poverty + unemp + pcincome + totalinc + region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - unemp     1     410.3 155624 2606.1
    ## <none>                  155213 2606.9
    ## - docs      1     772.4 155986 2607.1
    ## - bagrad    1     961.3 156175 2607.7
    ## - pop65     1    1151.9 156365 2608.2
    ## - area      1    4401.5 159615 2617.2
    ## - beds      1    4514.3 159728 2617.6
    ## - pop18     1    7814.9 163028 2626.6
    ## - pop       1   10235.5 165449 2633.0
    ## - totalinc  1   12232.3 167446 2638.3
    ## - pcincome  1   18212.2 173426 2653.8
    ## - region    1   26292.3 181506 2673.8
    ## - poverty   1   27845.5 183059 2677.6
    ## 
    ## Step:  AIC=2606.11
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + bagrad + 
    ##     poverty + pcincome + totalinc + region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - bagrad    1     641.0 156265 2605.9
    ## <none>                  155624 2606.1
    ## - docs      1     802.0 156426 2606.4
    ## - pop65     1    1052.6 156676 2607.1
    ## - area      1    4812.3 160436 2617.5
    ## - beds      1    5332.4 160956 2618.9
    ## - pop18     1    7688.1 163312 2625.3
    ## - pop       1    9897.3 165521 2631.2
    ## - totalinc  1   12035.3 167659 2636.9
    ## - pcincome  1   17942.2 173566 2652.1
    ## - region    1   28748.1 184372 2678.7
    ## - poverty   1   31229.4 186853 2684.6
    ## 
    ## Step:  AIC=2605.91
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + poverty + 
    ##     pcincome + totalinc + region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  156265 2605.9
    ## - docs      1    1131.4 157396 2607.1
    ## - pop65     1    1175.6 157440 2607.2
    ## - area      1    4629.8 160895 2616.8
    ## - beds      1    6076.4 162341 2620.7
    ## - pop18     1    7747.6 164012 2625.2
    ## - pop       1    9464.2 165729 2629.8
    ## - totalinc  1   11480.0 167745 2635.1
    ## - pcincome  1   24725.6 180990 2668.6
    ## - region    1   28512.7 184777 2677.7
    ## - poverty   1   31269.3 187534 2684.2

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + docs + 
    ##     beds + poverty + pcincome + totalinc + region, data = cdi_regression_df)
    ## 
    ## Coefficients:
    ## (Intercept)         area          pop        pop18        pop65         docs  
    ##  -9.711e+01   -2.440e-03    8.324e-05    1.330e+00    5.510e-01   -4.437e-03  
    ##        beds      poverty     pcincome     totalinc       region  
    ##   7.307e-03    2.682e+00    3.311e-03   -4.282e-03    8.977e+00

Manual: 8 variables, automatic: 10 (pop65 + docs added). Likely due to
cutoff differences.

``` r
cdi_regression_df = 
  cdi_df %>% 
  select(!(id:state), -crimes, -region)

all_vars_model = lm(crime_rate_1k ~ ., data = cdi_regression_df)
summary(all_vars_model)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ ., data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -58.717 -11.496  -1.736  11.967 147.322 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -7.049e+01  3.103e+01  -2.272  0.02359 *  
    ## area        -7.933e-04  7.210e-04  -1.100  0.27185    
    ## pop          9.793e-05  1.783e-05   5.491 6.86e-08 ***
    ## pop18        1.079e+00  3.694e-01   2.921  0.00367 ** 
    ## pop65        2.998e-01  3.345e-01   0.896  0.37062    
    ## docs        -2.315e-03  2.772e-03  -0.835  0.40412    
    ## beds         3.579e-03  1.975e-03   1.812  0.07061 .  
    ## hsgrad       8.554e-02  2.828e-01   0.303  0.76241    
    ## bagrad      -1.067e-01  3.281e-01  -0.325  0.74511    
    ## poverty      3.650e+00  4.146e-01   8.803  < 2e-16 ***
    ## unemp       -1.437e+00  5.706e-01  -2.518  0.01215 *  
    ## pcincome     3.359e-03  6.135e-04   5.475 7.46e-08 ***
    ## totalinc    -4.624e-03  8.343e-04  -5.543 5.22e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.62 on 427 degrees of freedom
    ## Multiple R-squared:  0.4465, Adjusted R-squared:  0.4309 
    ## F-statistic:  28.7 on 12 and 427 DF,  p-value: < 2.2e-16

``` r
back_elim = update(all_vars_model, . ~ . -hsgrad)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + docs + 
    ##     beds + bagrad + poverty + unemp + pcincome + totalinc, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -58.997 -11.383  -1.635  11.934 147.163 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6.270e+01  1.725e+01  -3.633 0.000314 ***
    ## area        -7.693e-04  7.159e-04  -1.075 0.283178    
    ## pop          9.775e-05  1.781e-05   5.490 6.89e-08 ***
    ## pop18        1.062e+00  3.645e-01   2.913 0.003768 ** 
    ## pop65        2.862e-01  3.311e-01   0.864 0.387911    
    ## docs        -2.383e-03  2.761e-03  -0.863 0.388489    
    ## beds         3.634e-03  1.965e-03   1.850 0.065051 .  
    ## bagrad      -5.294e-02  2.754e-01  -0.192 0.847659    
    ## poverty      3.577e+00  3.369e-01  10.617  < 2e-16 ***
    ## unemp       -1.464e+00  5.631e-01  -2.600 0.009654 ** 
    ## pcincome     3.314e-03  5.944e-04   5.576 4.37e-08 ***
    ## totalinc    -4.617e-03  8.330e-04  -5.542 5.22e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.59 on 428 degrees of freedom
    ## Multiple R-squared:  0.4464, Adjusted R-squared:  0.4321 
    ## F-statistic: 31.37 on 11 and 428 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -bagrad)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + docs + 
    ##     beds + poverty + unemp + pcincome + totalinc, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -58.906 -11.482  -1.638  11.941 147.189 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6.159e+01  1.625e+01  -3.790 0.000172 ***
    ## area        -7.750e-04  7.145e-04  -1.085 0.278637    
    ## pop          9.721e-05  1.756e-05   5.537 5.38e-08 ***
    ## pop18        1.026e+00  3.147e-01   3.262 0.001193 ** 
    ## pop65        2.896e-01  3.303e-01   0.877 0.381127    
    ## docs        -2.487e-03  2.704e-03  -0.920 0.358140    
    ## beds         3.731e-03  1.896e-03   1.968 0.049713 *  
    ## poverty      3.563e+00  3.284e-01  10.850  < 2e-16 ***
    ## unemp       -1.419e+00  5.115e-01  -2.774 0.005778 ** 
    ## pcincome     3.236e-03  4.332e-04   7.469 4.56e-13 ***
    ## totalinc    -4.591e-03  8.212e-04  -5.591 4.02e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.57 on 429 degrees of freedom
    ## Multiple R-squared:  0.4463, Adjusted R-squared:  0.4334 
    ## F-statistic: 34.58 on 10 and 429 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -pop65)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + docs + beds + 
    ##     poverty + unemp + pcincome + totalinc, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -59.344 -11.674  -1.683  12.076 147.561 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -5.298e+01  1.295e+01  -4.092 5.11e-05 ***
    ## area        -7.445e-04  7.134e-04  -1.044 0.297276    
    ## pop          9.476e-05  1.733e-05   5.468 7.72e-08 ***
    ## pop18        8.662e-01  2.560e-01   3.384 0.000781 ***
    ## docs        -2.458e-03  2.703e-03  -0.910 0.363555    
    ## beds         4.097e-03  1.849e-03   2.216 0.027233 *  
    ## poverty      3.523e+00  3.251e-01  10.836  < 2e-16 ***
    ## unemp       -1.358e+00  5.066e-01  -2.681 0.007634 ** 
    ## pcincome     3.202e-03  4.314e-04   7.422 6.21e-13 ***
    ## totalinc    -4.536e-03  8.185e-04  -5.542 5.23e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.56 on 430 degrees of freedom
    ## Multiple R-squared:  0.4453, Adjusted R-squared:  0.4337 
    ## F-statistic: 38.36 on 9 and 430 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -docs)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + beds + poverty + 
    ##     unemp + pcincome + totalinc, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -61.721 -11.970  -1.719  12.049 148.621 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -5.115e+01  1.279e+01  -4.000 7.46e-05 ***
    ## area        -7.962e-04  7.110e-04  -1.120  0.26344    
    ## pop          9.979e-05  1.642e-05   6.078 2.69e-09 ***
    ## pop18        8.168e-01  2.501e-01   3.266  0.00118 ** 
    ## beds         2.931e-03  1.333e-03   2.200  0.02836 *  
    ## poverty      3.490e+00  3.231e-01  10.804  < 2e-16 ***
    ## unemp       -1.323e+00  5.050e-01  -2.620  0.00911 ** 
    ## pcincome     3.195e-03  4.313e-04   7.408 6.82e-13 ***
    ## totalinc    -4.903e-03  7.124e-04  -6.882 2.10e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.56 on 431 degrees of freedom
    ## Multiple R-squared:  0.4443, Adjusted R-squared:  0.4339 
    ## F-statistic: 43.07 on 8 and 431 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -area)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ pop + pop18 + beds + poverty + unemp + 
    ##     pcincome + totalinc, data = cdi_regression_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -61.438 -11.945  -1.722  11.784 151.757 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -5.126e+01  1.279e+01  -4.007 7.23e-05 ***
    ## pop          9.437e-05  1.569e-05   6.013 3.88e-09 ***
    ## pop18        8.347e-01  2.497e-01   3.343 0.000902 ***
    ## beds         3.390e-03  1.268e-03   2.673 0.007796 ** 
    ## poverty      3.445e+00  3.206e-01  10.745  < 2e-16 ***
    ## unemp       -1.358e+00  5.042e-01  -2.694 0.007339 ** 
    ## pcincome     3.171e-03  4.309e-04   7.359 9.40e-13 ***
    ## totalinc    -4.737e-03  6.971e-04  -6.795 3.60e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.57 on 432 degrees of freedom
    ## Multiple R-squared:  0.4426, Adjusted R-squared:  0.4336 
    ## F-statistic: 49.01 on 7 and 432 DF,  p-value: < 2.2e-16

``` r
step(all_vars_model, direction = "backward")
```

    ## Start:  AIC=2675.7
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + hsgrad + 
    ##     bagrad + poverty + unemp + pcincome + totalinc
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - hsgrad    1        39 181506 2673.8
    ## - bagrad    1        45 181512 2673.8
    ## - docs      1       296 181763 2674.4
    ## - pop65     1       341 181808 2674.5
    ## - area      1       514 181981 2674.9
    ## <none>                  181467 2675.7
    ## - beds      1      1396 182863 2677.1
    ## - unemp     1      2695 184162 2680.2
    ## - pop18     1      3627 185094 2682.4
    ## - pcincome  1     12741 194208 2703.6
    ## - pop       1     12816 194282 2703.7
    ## - totalinc  1     13057 194524 2704.3
    ## - poverty   1     32937 214403 2747.1
    ## 
    ## Step:  AIC=2673.8
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + bagrad + 
    ##     poverty + unemp + pcincome + totalinc
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - bagrad    1        16 181521 2671.8
    ## - docs      1       316 181822 2672.6
    ## - pop65     1       317 181823 2672.6
    ## - area      1       490 181995 2673.0
    ## <none>                  181506 2673.8
    ## - beds      1      1451 182957 2675.3
    ## - unemp     1      2866 184372 2678.7
    ## - pop18     1      3598 185104 2680.4
    ## - pop       1     12783 194288 2701.7
    ## - totalinc  1     13027 194533 2702.3
    ## - pcincome  1     13184 194690 2702.7
    ## - poverty   1     47806 229312 2774.7
    ## 
    ## Step:  AIC=2671.84
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + poverty + 
    ##     unemp + pcincome + totalinc
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - pop65     1       325 181847 2670.6
    ## - docs      1       358 181879 2670.7
    ## - area      1       498 182019 2671.0
    ## <none>                  181521 2671.8
    ## - beds      1      1639 183160 2673.8
    ## - unemp     1      3256 184777 2677.7
    ## - pop18     1      4503 186025 2680.6
    ## - pop       1     12971 194492 2700.2
    ## - totalinc  1     13227 194749 2700.8
    ## - pcincome  1     23604 205125 2723.6
    ## - poverty   1     49812 231333 2776.5
    ## 
    ## Step:  AIC=2670.62
    ## crime_rate_1k ~ area + pop + pop18 + docs + beds + poverty + 
    ##     unemp + pcincome + totalinc
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - docs      1       350 182197 2669.5
    ## - area      1       461 182307 2669.7
    ## <none>                  181847 2670.6
    ## - beds      1      2076 183923 2673.6
    ## - unemp     1      3039 184885 2675.9
    ## - pop18     1      4842 186688 2680.2
    ## - pop       1     12646 194492 2698.2
    ## - totalinc  1     12988 194834 2699.0
    ## - pcincome  1     23299 205145 2721.7
    ## - poverty   1     49659 231505 2774.9
    ## 
    ## Step:  AIC=2669.47
    ## crime_rate_1k ~ area + pop + pop18 + beds + poverty + unemp + 
    ##     pcincome + totalinc
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - area      1       530 182727 2668.8
    ## <none>                  182197 2669.5
    ## - beds      1      2045 184242 2672.4
    ## - unemp     1      2901 185098 2674.4
    ## - pop18     1      4508 186705 2678.2
    ## - pop       1     15614 197811 2703.7
    ## - totalinc  1     20019 202215 2713.3
    ## - pcincome  1     23198 205395 2720.2
    ## - poverty   1     49344 231541 2772.9
    ## 
    ## Step:  AIC=2668.75
    ## crime_rate_1k ~ pop + pop18 + beds + poverty + unemp + pcincome + 
    ##     totalinc
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  182727 2668.8
    ## - beds      1      3023 185749 2674.0
    ## - unemp     1      3069 185796 2674.1
    ## - pop18     1      4726 187452 2678.0
    ## - pop       1     15294 198021 2702.1
    ## - totalinc  1     19531 202258 2711.4
    ## - pcincome  1     22909 205635 2718.7
    ## - poverty   1     48838 231565 2771.0

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ pop + pop18 + beds + poverty + unemp + 
    ##     pcincome + totalinc, data = cdi_regression_df)
    ## 
    ## Coefficients:
    ## (Intercept)          pop        pop18         beds      poverty        unemp  
    ##  -5.126e+01    9.437e-05    8.347e-01    3.390e-03    3.445e+00   -1.358e+00  
    ##    pcincome     totalinc  
    ##   3.171e-03   -4.737e-03

Manual: 7 variables, automatic: 7 variables.

``` r
step(all_vars_model, direction = "forward")
```

    ## Start:  AIC=2675.7
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + hsgrad + 
    ##     bagrad + poverty + unemp + pcincome + totalinc

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ area + pop + pop18 + pop65 + docs + 
    ##     beds + hsgrad + bagrad + poverty + unemp + pcincome + totalinc, 
    ##     data = cdi_regression_df)
    ## 
    ## Coefficients:
    ## (Intercept)         area          pop        pop18        pop65         docs  
    ##  -7.049e+01   -7.933e-04    9.793e-05    1.079e+00    2.998e-01   -2.315e-03  
    ##        beds       hsgrad       bagrad      poverty        unemp     pcincome  
    ##   3.579e-03    8.554e-02   -1.067e-01    3.650e+00   -1.437e+00    3.359e-03  
    ##    totalinc  
    ##  -4.624e-03

Automatic forward selection: 12 variables.

``` r
step(all_vars_model, direction = "both")
```

    ## Start:  AIC=2675.7
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + hsgrad + 
    ##     bagrad + poverty + unemp + pcincome + totalinc
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - hsgrad    1        39 181506 2673.8
    ## - bagrad    1        45 181512 2673.8
    ## - docs      1       296 181763 2674.4
    ## - pop65     1       341 181808 2674.5
    ## - area      1       514 181981 2674.9
    ## <none>                  181467 2675.7
    ## - beds      1      1396 182863 2677.1
    ## - unemp     1      2695 184162 2680.2
    ## - pop18     1      3627 185094 2682.4
    ## - pcincome  1     12741 194208 2703.6
    ## - pop       1     12816 194282 2703.7
    ## - totalinc  1     13057 194524 2704.3
    ## - poverty   1     32937 214403 2747.1
    ## 
    ## Step:  AIC=2673.8
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + bagrad + 
    ##     poverty + unemp + pcincome + totalinc
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - bagrad    1        16 181521 2671.8
    ## - docs      1       316 181822 2672.6
    ## - pop65     1       317 181823 2672.6
    ## - area      1       490 181995 2673.0
    ## <none>                  181506 2673.8
    ## - beds      1      1451 182957 2675.3
    ## + hsgrad    1        39 181467 2675.7
    ## - unemp     1      2866 184372 2678.7
    ## - pop18     1      3598 185104 2680.4
    ## - pop       1     12783 194288 2701.7
    ## - totalinc  1     13027 194533 2702.3
    ## - pcincome  1     13184 194690 2702.7
    ## - poverty   1     47806 229312 2774.7
    ## 
    ## Step:  AIC=2671.84
    ## crime_rate_1k ~ area + pop + pop18 + pop65 + docs + beds + poverty + 
    ##     unemp + pcincome + totalinc
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - pop65     1       325 181847 2670.6
    ## - docs      1       358 181879 2670.7
    ## - area      1       498 182019 2671.0
    ## <none>                  181521 2671.8
    ## - beds      1      1639 183160 2673.8
    ## + bagrad    1        16 181506 2673.8
    ## + hsgrad    1        10 181512 2673.8
    ## - unemp     1      3256 184777 2677.7
    ## - pop18     1      4503 186025 2680.6
    ## - pop       1     12971 194492 2700.2
    ## - totalinc  1     13227 194749 2700.8
    ## - pcincome  1     23604 205125 2723.6
    ## - poverty   1     49812 231333 2776.5
    ## 
    ## Step:  AIC=2670.62
    ## crime_rate_1k ~ area + pop + pop18 + docs + beds + poverty + 
    ##     unemp + pcincome + totalinc
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - docs      1       350 182197 2669.5
    ## - area      1       461 182307 2669.7
    ## <none>                  181847 2670.6
    ## + pop65     1       325 181521 2671.8
    ## + bagrad    1        24 181823 2672.6
    ## + hsgrad    1         0 181846 2672.6
    ## - beds      1      2076 183923 2673.6
    ## - unemp     1      3039 184885 2675.9
    ## - pop18     1      4842 186688 2680.2
    ## - pop       1     12646 194492 2698.2
    ## - totalinc  1     12988 194834 2699.0
    ## - pcincome  1     23299 205145 2721.7
    ## - poverty   1     49659 231505 2774.9
    ## 
    ## Step:  AIC=2669.47
    ## crime_rate_1k ~ area + pop + pop18 + beds + poverty + unemp + 
    ##     pcincome + totalinc
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - area      1       530 182727 2668.8
    ## <none>                  182197 2669.5
    ## + docs      1       350 181847 2670.6
    ## + pop65     1       317 181879 2670.7
    ## + bagrad    1        72 182125 2671.3
    ## + hsgrad    1         0 182196 2671.5
    ## - beds      1      2045 184242 2672.4
    ## - unemp     1      2901 185098 2674.4
    ## - pop18     1      4508 186705 2678.2
    ## - pop       1     15614 197811 2703.7
    ## - totalinc  1     20019 202215 2713.3
    ## - pcincome  1     23198 205395 2720.2
    ## - poverty   1     49344 231541 2772.9
    ## 
    ## Step:  AIC=2668.75
    ## crime_rate_1k ~ pop + pop18 + beds + poverty + unemp + pcincome + 
    ##     totalinc
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  182727 2668.8
    ## + area      1       530 182197 2669.5
    ## + docs      1       419 182307 2669.7
    ## + pop65     1       277 182450 2670.1
    ## + bagrad    1        94 182632 2670.5
    ## + hsgrad    1         7 182719 2670.7
    ## - beds      1      3023 185749 2674.0
    ## - unemp     1      3069 185796 2674.1
    ## - pop18     1      4726 187452 2678.0
    ## - pop       1     15294 198021 2702.1
    ## - totalinc  1     19531 202258 2711.4
    ## - pcincome  1     22909 205635 2718.7
    ## - poverty   1     48838 231565 2771.0

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ pop + pop18 + beds + poverty + unemp + 
    ##     pcincome + totalinc, data = cdi_regression_df)
    ## 
    ## Coefficients:
    ## (Intercept)          pop        pop18         beds      poverty        unemp  
    ##  -5.126e+01    9.437e-05    8.347e-01    3.390e-03    3.445e+00   -1.358e+00  
    ##    pcincome     totalinc  
    ##   3.171e-03   -4.737e-03

Automatic: 7 variables.

## With transformed variables

Population density = Population / Area  
Percent doctors = Doctors / Population  
Percent beds = Beds / Population  
Region as dummy/indicator variable

``` r
cdi_df2 = read_csv("./data/cdi.csv") %>% 
  mutate(crime_rate_1k = (crimes/pop)*1000,
         popdensity = pop/area,
         pctdocs = docs/pop,
         pctbeds = beds/pop,
         region = as.factor(region),
         region = fct_recode(region, "northeast" = "1", "north_central" = "2",
                             "south" = "3", "west" = "4")) %>% 
  relocate(crime_rate_1k, .before = id) %>% 
  relocate(popdensity, .after = crime_rate_1k)

cdi_regression_df2 = 
  cdi_df2 %>% 
  select(!(id:state), -crimes, -area, -pop, -docs, -beds)
```

Removed id, city, and state variables from the data set as those are not
numerical variables and as such we cannot do regression on them. id is
numerical but not actual data.  
Created population density, percent docs, percent beds, and region
dummy/indicator variable. As such, removed crimes as crime\_rate\_1k is
a function of crimes, removed population and area as density is a
function of those variables, and same rationale for docs and beds.
Instead of absolute numbers, using percentages and ratios for
regression.

``` r
#pairs(cdi_regression_df) not super informative

cdi_regression_df_no_reg =
  cdi_regression_df2 %>% 
  select(-region)
  
corrplot(cor(cdi_regression_df_no_reg), type = "upper", diag = FALSE)
```

![](automated_procedures_files/figure-gfm/correlation%20plot2-1.png)<!-- -->

``` r
par(mfrow = c(3, 5))
boxplot(pull(cdi_regression_df2, crime_rate_1k), main = "crime_rate_1k")
boxplot(pull(cdi_regression_df2, popdensity), main = "popdensity")
#boxplot(pull(cdi_regression_df, area), main = "area")
#boxplot(pull(cdi_regression_df, pop), main = "pop")
boxplot(pull(cdi_regression_df2, pop18), main = "pop18")
boxplot(pull(cdi_regression_df2, pop65), main = "pop65")
boxplot(pull(cdi_regression_df2, pctdocs), main = "pctdocs")
boxplot(pull(cdi_regression_df2, pctbeds), main = "pctbeds")
#boxplot(pull(cdi_regression_df, crimes), main = "crimes")
boxplot(pull(cdi_regression_df2, hsgrad), main = "hsgrad")
boxplot(pull(cdi_regression_df2, bagrad), main = "bagrad")
boxplot(pull(cdi_regression_df2, poverty), main = "poverty")
boxplot(pull(cdi_regression_df2, unemp), main = "unemp")
boxplot(pull(cdi_regression_df2, pcincome), main = "pcincome")
boxplot(pull(cdi_regression_df2, totalinc), main = "totalinc")
#boxplot(pull(cdi_regression_df2, region), main = "region")
```

![](automated_procedures_files/figure-gfm/correlation%20plot2-2.png)<!-- -->

Check for correlation, normal distribution.

``` r
all_vars_model = lm(crime_rate_1k ~ ., data = cdi_regression_df2)
summary(all_vars_model)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ ., data = cdi_regression_df2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -48.570 -11.549  -0.911  10.417  75.670 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -6.862e+01  2.757e+01  -2.489  0.01319 *  
    ## popdensity           5.006e-03  4.536e-04  11.037  < 2e-16 ***
    ## pop18                7.160e-01  3.327e-01   2.152  0.03198 *  
    ## pop65               -1.993e-01  3.072e-01  -0.649  0.51695    
    ## hsgrad               6.074e-01  2.706e-01   2.245  0.02529 *  
    ## bagrad              -4.964e-01  2.988e-01  -1.662  0.09733 .  
    ## poverty              1.879e+00  3.887e-01   4.836 1.86e-06 ***
    ## unemp                6.015e-01  5.345e-01   1.125  0.26109    
    ## pcincome             1.029e-03  4.846e-04   2.123  0.03432 *  
    ## totalinc             2.072e-04  7.652e-05   2.707  0.00705 ** 
    ## regionnorth_central  9.098e+00  2.747e+00   3.312  0.00101 ** 
    ## regionsouth          2.785e+01  2.673e+00  10.419  < 2e-16 ***
    ## regionwest           2.160e+01  3.140e+00   6.879 2.16e-11 ***
    ## pctdocs             -6.554e+02  1.025e+03  -0.639  0.52298    
    ## pctbeds              3.138e+03  7.988e+02   3.928  0.00010 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.9 on 425 degrees of freedom
    ## Multiple R-squared:  0.5845, Adjusted R-squared:  0.5708 
    ## F-statistic: 42.71 on 14 and 425 DF,  p-value: < 2.2e-16

``` r
back_elim = update(all_vars_model, . ~ . -pctdocs)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ popdensity + pop18 + pop65 + hsgrad + 
    ##     bagrad + poverty + unemp + pcincome + totalinc + region + 
    ##     pctbeds, data = cdi_regression_df2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -49.205 -11.544  -0.733  10.323  76.530 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -6.762e+01  2.750e+01  -2.459 0.014341 *  
    ## popdensity           4.977e-03  4.509e-04  11.037  < 2e-16 ***
    ## pop18                6.983e-01  3.314e-01   2.107 0.035666 *  
    ## pop65               -1.953e-01  3.070e-01  -0.636 0.524882    
    ## hsgrad               6.215e-01  2.695e-01   2.306 0.021584 *  
    ## bagrad              -5.437e-01  2.892e-01  -1.880 0.060812 .  
    ## poverty              1.890e+00  3.880e-01   4.872 1.56e-06 ***
    ## unemp                5.947e-01  5.340e-01   1.114 0.266068    
    ## pcincome             9.910e-04  4.806e-04   2.062 0.039831 *  
    ## totalinc             2.056e-04  7.643e-05   2.690 0.007425 ** 
    ## regionnorth_central  9.181e+00  2.742e+00   3.348 0.000887 ***
    ## regionsouth          2.788e+01  2.671e+00  10.437  < 2e-16 ***
    ## regionwest           2.130e+01  3.103e+00   6.865 2.36e-11 ***
    ## pctbeds              2.780e+03  5.697e+02   4.879 1.51e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.89 on 426 degrees of freedom
    ## Multiple R-squared:  0.5841, Adjusted R-squared:  0.5714 
    ## F-statistic: 46.03 on 13 and 426 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -pop65)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ popdensity + pop18 + hsgrad + bagrad + 
    ##     poverty + unemp + pcincome + totalinc + region + pctbeds, 
    ##     data = cdi_regression_df2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -49.911 -11.554  -0.787  10.082  77.123 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -7.397e+01  2.562e+01  -2.888 0.004079 ** 
    ## popdensity           4.957e-03  4.496e-04  11.026  < 2e-16 ***
    ## pop18                8.045e-01  2.861e-01   2.812 0.005156 ** 
    ## hsgrad               6.313e-01  2.689e-01   2.348 0.019324 *  
    ## bagrad              -5.436e-01  2.890e-01  -1.881 0.060690 .  
    ## poverty              1.940e+00  3.798e-01   5.108 4.92e-07 ***
    ## unemp                5.521e-01  5.294e-01   1.043 0.297589    
    ## pcincome             1.013e-03  4.791e-04   2.115 0.035046 *  
    ## totalinc             2.035e-04  7.631e-05   2.667 0.007934 ** 
    ## regionnorth_central  9.512e+00  2.691e+00   3.535 0.000452 ***
    ## regionsouth          2.797e+01  2.665e+00  10.492  < 2e-16 ***
    ## regionwest           2.149e+01  3.087e+00   6.962 1.27e-11 ***
    ## pctbeds              2.642e+03  5.263e+02   5.019 7.65e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.88 on 427 degrees of freedom
    ## Multiple R-squared:  0.5837, Adjusted R-squared:  0.572 
    ## F-statistic:  49.9 on 12 and 427 DF,  p-value: < 2.2e-16

``` r
back_elim = update(back_elim, . ~ . -unemp)
summary(back_elim)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ popdensity + pop18 + hsgrad + bagrad + 
    ##     poverty + pcincome + totalinc + region + pctbeds, data = cdi_regression_df2)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -50.29 -11.31  -0.85  10.43  76.83 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -6.658e+01  2.462e+01  -2.704 0.007114 ** 
    ## popdensity           4.941e-03  4.494e-04  10.995  < 2e-16 ***
    ## pop18                8.036e-01  2.861e-01   2.808 0.005208 ** 
    ## hsgrad               5.786e-01  2.641e-01   2.191 0.028998 *  
    ## bagrad              -6.045e-01  2.831e-01  -2.135 0.033329 *  
    ## poverty              2.061e+00  3.618e-01   5.696 2.28e-08 ***
    ## pcincome             1.102e-03  4.714e-04   2.338 0.019866 *  
    ## totalinc             1.996e-04  7.622e-05   2.619 0.009136 ** 
    ## regionnorth_central  9.145e+00  2.668e+00   3.428 0.000668 ***
    ## regionsouth          2.710e+01  2.533e+00  10.699  < 2e-16 ***
    ## regionwest           2.127e+01  3.080e+00   6.906 1.81e-11 ***
    ## pctbeds              2.484e+03  5.042e+02   4.926 1.20e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.88 on 428 degrees of freedom
    ## Multiple R-squared:  0.5827, Adjusted R-squared:  0.572 
    ## F-statistic: 54.33 on 11 and 428 DF,  p-value: < 2.2e-16

``` r
step(all_vars_model, direction = "backward")
```

    ## Start:  AIC=2553.48
    ## crime_rate_1k ~ popdensity + pop18 + pop65 + hsgrad + bagrad + 
    ##     poverty + unemp + pcincome + totalinc + region + pctdocs + 
    ##     pctbeds
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - pctdocs     1       131 136340 2551.9
    ## - pop65       1       135 136344 2551.9
    ## - unemp       1       406 136615 2552.8
    ## <none>                    136209 2553.5
    ## - bagrad      1       885 137094 2554.3
    ## - pcincome    1      1445 137654 2556.1
    ## - pop18       1      1484 137693 2556.2
    ## - hsgrad      1      1615 137824 2556.7
    ## - totalinc    1      2349 138558 2559.0
    ## - pctbeds     1      4944 141153 2567.2
    ## - poverty     1      7495 143704 2575.0
    ## - region      3     39425 175634 2659.3
    ## - popdensity  1     39039 175248 2662.4
    ## 
    ## Step:  AIC=2551.9
    ## crime_rate_1k ~ popdensity + pop18 + pop65 + hsgrad + bagrad + 
    ##     poverty + unemp + pcincome + totalinc + region + pctbeds
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - pop65       1       130 136470 2550.3
    ## - unemp       1       397 136737 2551.2
    ## <none>                    136340 2551.9
    ## - bagrad      1      1131 137471 2553.5
    ## - pcincome    1      1361 137701 2554.3
    ## - pop18       1      1421 137761 2554.5
    ## - hsgrad      1      1702 138042 2555.4
    ## - totalinc    1      2316 138656 2557.3
    ## - poverty     1      7598 143938 2573.8
    ## - pctbeds     1      7620 143960 2573.8
    ## - region      3     39300 175640 2657.3
    ## - popdensity  1     38984 175324 2660.6
    ## 
    ## Step:  AIC=2550.32
    ## crime_rate_1k ~ popdensity + pop18 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + totalinc + region + pctbeds
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - unemp       1       348 136817 2549.4
    ## <none>                    136470 2550.3
    ## - bagrad      1      1130 137600 2551.9
    ## - pcincome    1      1429 137899 2552.9
    ## - hsgrad      1      1762 138232 2554.0
    ## - totalinc    1      2274 138744 2555.6
    ## - pop18       1      2527 138996 2556.4
    ## - pctbeds     1      8050 144520 2573.5
    ## - poverty     1      8338 144808 2574.4
    ## - region      3     39345 175815 2655.8
    ## - popdensity  1     38858 175327 2658.6
    ## 
    ## Step:  AIC=2549.44
    ## crime_rate_1k ~ popdensity + pop18 + hsgrad + bagrad + poverty + 
    ##     pcincome + totalinc + region + pctbeds
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## <none>                    136817 2549.4
    ## - bagrad      1      1457 138274 2552.1
    ## - hsgrad      1      1534 138352 2552.3
    ## - pcincome    1      1747 138564 2553.0
    ## - totalinc    1      2192 139010 2554.4
    ## - pop18       1      2521 139338 2555.5
    ## - pctbeds     1      7758 144576 2571.7
    ## - poverty     1     10373 147190 2579.6
    ## - popdensity  1     38646 175463 2656.9
    ## - region      3     40549 177366 2657.7

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ popdensity + pop18 + hsgrad + bagrad + 
    ##     poverty + pcincome + totalinc + region + pctbeds, data = cdi_regression_df2)
    ## 
    ## Coefficients:
    ##         (Intercept)           popdensity                pop18  
    ##          -6.658e+01            4.941e-03            8.036e-01  
    ##              hsgrad               bagrad              poverty  
    ##           5.786e-01           -6.045e-01            2.061e+00  
    ##            pcincome             totalinc  regionnorth_central  
    ##           1.102e-03            1.996e-04            9.145e+00  
    ##         regionsouth           regionwest              pctbeds  
    ##           2.710e+01            2.127e+01            2.484e+03

Using manual backwards elimination, we have a model with 9 variables.
Using the automatic step function, we have the same model.

``` r
step(all_vars_model, direction = "forward")
```

    ## Start:  AIC=2553.48
    ## crime_rate_1k ~ popdensity + pop18 + pop65 + hsgrad + bagrad + 
    ##     poverty + unemp + pcincome + totalinc + region + pctdocs + 
    ##     pctbeds

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ popdensity + pop18 + pop65 + hsgrad + 
    ##     bagrad + poverty + unemp + pcincome + totalinc + region + 
    ##     pctdocs + pctbeds, data = cdi_regression_df2)
    ## 
    ## Coefficients:
    ##         (Intercept)           popdensity                pop18  
    ##          -6.862e+01            5.006e-03            7.160e-01  
    ##               pop65               hsgrad               bagrad  
    ##          -1.993e-01            6.074e-01           -4.964e-01  
    ##             poverty                unemp             pcincome  
    ##           1.879e+00            6.015e-01            1.029e-03  
    ##            totalinc  regionnorth_central          regionsouth  
    ##           2.072e-04            9.098e+00            2.785e+01  
    ##          regionwest              pctdocs              pctbeds  
    ##           2.160e+01           -6.554e+02            3.138e+03

The automated forward selection process includes all variable from
automated backwards elimination plus pop65, unemp, and pctdocs.

``` r
step(all_vars_model, direction = "both")
```

    ## Start:  AIC=2553.48
    ## crime_rate_1k ~ popdensity + pop18 + pop65 + hsgrad + bagrad + 
    ##     poverty + unemp + pcincome + totalinc + region + pctdocs + 
    ##     pctbeds
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - pctdocs     1       131 136340 2551.9
    ## - pop65       1       135 136344 2551.9
    ## - unemp       1       406 136615 2552.8
    ## <none>                    136209 2553.5
    ## - bagrad      1       885 137094 2554.3
    ## - pcincome    1      1445 137654 2556.1
    ## - pop18       1      1484 137693 2556.2
    ## - hsgrad      1      1615 137824 2556.7
    ## - totalinc    1      2349 138558 2559.0
    ## - pctbeds     1      4944 141153 2567.2
    ## - poverty     1      7495 143704 2575.0
    ## - region      3     39425 175634 2659.3
    ## - popdensity  1     39039 175248 2662.4
    ## 
    ## Step:  AIC=2551.9
    ## crime_rate_1k ~ popdensity + pop18 + pop65 + hsgrad + bagrad + 
    ##     poverty + unemp + pcincome + totalinc + region + pctbeds
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - pop65       1       130 136470 2550.3
    ## - unemp       1       397 136737 2551.2
    ## <none>                    136340 2551.9
    ## + pctdocs     1       131 136209 2553.5
    ## - bagrad      1      1131 137471 2553.5
    ## - pcincome    1      1361 137701 2554.3
    ## - pop18       1      1421 137761 2554.5
    ## - hsgrad      1      1702 138042 2555.4
    ## - totalinc    1      2316 138656 2557.3
    ## - poverty     1      7598 143938 2573.8
    ## - pctbeds     1      7620 143960 2573.8
    ## - region      3     39300 175640 2657.3
    ## - popdensity  1     38984 175324 2660.6
    ## 
    ## Step:  AIC=2550.32
    ## crime_rate_1k ~ popdensity + pop18 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + totalinc + region + pctbeds
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - unemp       1       348 136817 2549.4
    ## <none>                    136470 2550.3
    ## + pop65       1       130 136340 2551.9
    ## + pctdocs     1       126 136344 2551.9
    ## - bagrad      1      1130 137600 2551.9
    ## - pcincome    1      1429 137899 2552.9
    ## - hsgrad      1      1762 138232 2554.0
    ## - totalinc    1      2274 138744 2555.6
    ## - pop18       1      2527 138996 2556.4
    ## - pctbeds     1      8050 144520 2573.5
    ## - poverty     1      8338 144808 2574.4
    ## - region      3     39345 175815 2655.8
    ## - popdensity  1     38858 175327 2658.6
    ## 
    ## Step:  AIC=2549.44
    ## crime_rate_1k ~ popdensity + pop18 + hsgrad + bagrad + poverty + 
    ##     pcincome + totalinc + region + pctbeds
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## <none>                    136817 2549.4
    ## + unemp       1       348 136470 2550.3
    ## + pctdocs     1       119 136699 2551.1
    ## + pop65       1        80 136737 2551.2
    ## - bagrad      1      1457 138274 2552.1
    ## - hsgrad      1      1534 138352 2552.3
    ## - pcincome    1      1747 138564 2553.0
    ## - totalinc    1      2192 139010 2554.4
    ## - pop18       1      2521 139338 2555.5
    ## - pctbeds     1      7758 144576 2571.7
    ## - poverty     1     10373 147190 2579.6
    ## - popdensity  1     38646 175463 2656.9
    ## - region      3     40549 177366 2657.7

    ## 
    ## Call:
    ## lm(formula = crime_rate_1k ~ popdensity + pop18 + hsgrad + bagrad + 
    ##     poverty + pcincome + totalinc + region + pctbeds, data = cdi_regression_df2)
    ## 
    ## Coefficients:
    ##         (Intercept)           popdensity                pop18  
    ##          -6.658e+01            4.941e-03            8.036e-01  
    ##              hsgrad               bagrad              poverty  
    ##           5.786e-01           -6.045e-01            2.061e+00  
    ##            pcincome             totalinc  regionnorth_central  
    ##           1.102e-03            1.996e-04            9.145e+00  
    ##         regionsouth           regionwest              pctbeds  
    ##           2.710e+01            2.127e+01            2.484e+03

The automated stepwise regression function includes the same variables
as the automated backwards selection function.
