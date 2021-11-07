Writing Functions
================
Wanxin Qi
11/7/2021

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -1.3467556 -0.1143800 -0.2614241 -0.7135144  0.9188993 -0.2727016
    ##  [7]  0.9304105  0.1273839  0.1658759 -0.5110863  0.2584837  1.3347905
    ## [13] -1.6249941  1.4305208 -0.3285908 -0.2517131  0.7565689  0.9797130
    ## [19] -0.7368539  2.3228091  0.1664198 -0.7557425 -1.9725745 -0.8479584
    ## [25]  0.3464138

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1] -1.3467556 -0.1143800 -0.2614241 -0.7135144  0.9188993 -0.2727016
    ##  [7]  0.9304105  0.1273839  0.1658759 -0.5110863  0.2584837  1.3347905
    ## [13] -1.6249941  1.4305208 -0.3285908 -0.2517131  0.7565689  0.9797130
    ## [19] -0.7368539  2.3228091  0.1664198 -0.7557425 -1.9725745 -0.8479584
    ## [25]  0.3464138

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1]  0.29555388  0.06899859  0.16224013 -0.60478013 -0.01771334  0.20490980
    ##  [7]  0.22359548 -0.34287552  0.07000491 -0.07582068  0.44606434  0.21134113
    ## [13]  3.26498321  0.67813032  1.23843380  1.04036898 -0.32281686 -0.39384388
    ## [19] -0.47962315 -2.65824503 -0.74259370  1.74510257  0.32712825  0.76415522
    ## [25] -0.72416049 -0.06786702  0.57829713 -1.07641383 -0.74079143 -0.65903293
    ## [31] -1.35149760 -1.98210287  0.11475099 -0.15180686 -0.68766976 -1.06272880
    ## [37]  0.46716820  0.95262967  0.88645593  0.40207135

How great is this?

Only kinda great.

Let’s try again.

``` r
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(c("my", "name", "is", "jeff")): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

``` r
z_scores(y_vec)
```

    ##  [1]  0.29555388  0.06899859  0.16224013 -0.60478013 -0.01771334  0.20490980
    ##  [7]  0.22359548 -0.34287552  0.07000491 -0.07582068  0.44606434  0.21134113
    ## [13]  3.26498321  0.67813032  1.23843380  1.04036898 -0.32281686 -0.39384388
    ## [19] -0.47962315 -2.65824503 -0.74259370  1.74510257  0.32712825  0.76415522
    ## [25] -0.72416049 -0.06786702  0.57829713 -1.07641383 -0.74079143 -0.65903293
    ## [31] -1.35149760 -1.98210287  0.11475099 -0.15180686 -0.68766976 -1.06272880
    ## [37]  0.46716820  0.95262967  0.88645593  0.40207135

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df =
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.50  4.54

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.265

## Different sample sizes, means, and sds

``` r
sim_data =
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>%
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.56  2.45

Let’s write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu, sigma) {
  
  # do checks on inputs
  
  sim_data =
  tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )
  
  sim_data %>%
    summarize(
      mean = mean(x),
      sd = sd(x)
  )
  
}

sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.55  2.77

``` r
sim_mean_sd(mu = 4, n = 30, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.15  2.93
