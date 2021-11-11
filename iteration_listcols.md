Iteration and Listcols
================
Wanxin Qi
11/7/2021

## Define functions

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
```

## Lists

``` r
l = 
  list(
    vec_numeric = 5:8,
    vec_logical = c(TRUE, FALSE),
    summary = summary(rnorm(1000, mean = 5, sd = 3))
  )

l[[3]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -3.501   2.944   5.098   4.994   7.090  14.723

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -3.501   2.944   5.098   4.994   7.090  14.723

``` r
l$summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -3.501   2.944   5.098   4.994   7.090  14.723

## list of normals

``` r
list_norms = 
  list(
    a = rnorm(50, mean = 2, sd = 1),
    b = rnorm(50, mean = 5, sd = 3),
    c = rnorm(50, mean = 20, sd = 1.2),
    d = rnorm(50, mean = -12, sd = 0.5)
  )

mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.96 0.776

``` r
mean_and_sd(list_norms[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.92  3.16

``` r
mean_and_sd(list_norms[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  20.0  1.07

``` r
mean_and_sd(list_norms[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.0 0.499

``` r
mean_and_sd(list_norms[["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.96 0.776

## for loop

Let’s use a for loop to iterate over my list of normals.

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norms[[i]])
  
}
```

let’s use map instead …

``` r
output = map(list_norms, mean_and_sd)

output = map(list_norms, summary)

output = map_dbl(list_norms, median)
```

## LIST COLUMNS!!!!

``` r
listcol_df =
  tibble(
    name = c("a", "b", "c", "d"),
    norms = list_norms
  )

listcol_df %>% filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  norms       
    ##   <chr> <named list>
    ## 1 a     <dbl [50]>

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(norms)
```

    ## $a
    ##  [1]  2.7370784  1.6813422  1.6297065  2.7305329  3.5230085  1.9879093
    ##  [7]  2.6799672  1.2634836  1.0939337  1.5642087  2.5104055 -0.1025768
    ## [13]  1.4856719  1.7906277  1.8331499  1.3226755  0.8229925  2.9818226
    ## [19]  1.3665644  1.5227670  1.6993765  0.6044455  2.9635541  2.6462868
    ## [25]  2.7399555  2.9707817  2.9578694  1.5743653  1.3929799  3.3781708
    ## [31]  1.8054778  2.0692972  2.0902391  1.2244113  2.3049917  1.5860826
    ## [37]  3.2142432  1.9715707  1.8411013  0.9156505  1.1227542  2.4429905
    ## [43]  1.7584747  1.0712295  2.2977760  2.4885978  1.2398723  2.7742812
    ## [49]  2.4545313  1.8312809
    ## 
    ## $b
    ##  [1]  8.6477832  1.2665777  5.1285789 -0.2511058  5.0313799  6.7661589
    ##  [7]  7.3923598  2.1248127  1.3179840  1.4834143 10.8816012  5.8646196
    ## [13]  3.2014757  5.2763401  6.9207892  5.4246262  1.4228627  0.5084956
    ## [19]  2.8287051  5.5482161 -1.2421052  4.7605932  9.6449843 12.4796463
    ## [25]  6.9689118  4.6160705  4.1318247  2.6851750  6.0082331  5.0340557
    ## [31]  5.2443513  6.6754494 10.8049698  1.6165603  6.4722444  3.3186375
    ## [37]  3.9187319  8.2284296  8.2355848  8.7269754  7.3553538  3.4431580
    ## [43]  3.6716598  1.9112795  6.7884434  4.0422881  3.8462336  7.1745791
    ## [49]  5.6691547 -3.1274960
    ## 
    ## $c
    ##  [1] 20.15589 23.09360 20.23578 18.24785 19.31550 18.95503 21.39557 19.02021
    ##  [9] 20.51011 19.97107 18.77632 20.15070 20.14065 20.95878 19.18962 18.59667
    ## [17] 18.34886 20.64362 19.80616 19.50534 20.38237 19.64897 19.53368 18.52259
    ## [25] 20.11510 19.57970 20.78841 17.62905 21.72415 20.98340 21.54668 19.98624
    ## [33] 21.66238 20.93891 19.60740 19.90113 19.44828 21.30055 19.51616 20.13728
    ## [41] 20.52105 21.47947 18.56008 20.59693 20.24654 20.09971 19.97653 18.26655
    ## [49] 20.92938 20.51988
    ## 
    ## $d
    ##  [1] -11.87361 -11.76292 -11.88349 -12.64733 -11.44379 -11.25300 -10.84866
    ##  [8] -12.13559 -12.65853 -11.18330 -11.78126 -12.16285 -11.96768 -11.49341
    ## [15] -12.03611 -11.19112 -12.22530 -12.17040 -11.46389 -12.14434 -11.90251
    ## [22] -12.72437 -11.89082 -12.16388 -12.67609 -11.77267 -12.65226 -11.41464
    ## [29] -12.51759 -12.45565 -11.73901 -11.15153 -11.81598 -11.29553 -12.24656
    ## [36] -12.00730 -12.10953 -11.84354 -12.80946 -12.70733 -11.16157 -11.39684
    ## [43] -11.93924 -12.27245 -11.82895 -11.46493 -12.30419 -12.40261 -12.70552
    ## [50] -11.96912

``` r
listcol_df$norms[[1]]
```

    ##  [1]  2.7370784  1.6813422  1.6297065  2.7305329  3.5230085  1.9879093
    ##  [7]  2.6799672  1.2634836  1.0939337  1.5642087  2.5104055 -0.1025768
    ## [13]  1.4856719  1.7906277  1.8331499  1.3226755  0.8229925  2.9818226
    ## [19]  1.3665644  1.5227670  1.6993765  0.6044455  2.9635541  2.6462868
    ## [25]  2.7399555  2.9707817  2.9578694  1.5743653  1.3929799  3.3781708
    ## [31]  1.8054778  2.0692972  2.0902391  1.2244113  2.3049917  1.5860826
    ## [37]  3.2142432  1.9715707  1.8411013  0.9156505  1.1227542  2.4429905
    ## [43]  1.7584747  1.0712295  2.2977760  2.4885978  1.2398723  2.7742812
    ## [49]  2.4545313  1.8312809

``` r
listcol_df %>%
  mutate(summaries = map(norms, mean_and_sd))
```

    ## # A tibble: 4 × 3
    ##   name  norms        summaries       
    ##   <chr> <named list> <named list>    
    ## 1 a     <dbl [50]>   <tibble [1 × 2]>
    ## 2 b     <dbl [50]>   <tibble [1 × 2]>
    ## 3 c     <dbl [50]>   <tibble [1 × 2]>
    ## 4 d     <dbl [50]>   <tibble [1 × 2]>

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-09-18 22:18:30 (7.599)

    ## file min/max dates: 1869-01-01 / 2021-09-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-09-18 22:18:38 (1.697)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-09-18 22:18:41 (0.909)

    ## file min/max dates: 1999-09-01 / 2021-09-30

Nest data within location

``` r
weather_nested = nest(weather_df, data = date:tmin)

weather_nested =
  weather_df %>%
  nest(data = date:tmin)

weather_nested %>%
  unnest(data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

``` r
unnest(weather_nested, data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
  
}

weather_lm(weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nested$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nested %>%
  mutate(lm_results = map(data, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

``` r
lm(tmax ~ tmin, data = weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nested$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
weather_nested %>%
  filter(name == "CentralPark_NY")
```

    ## # A tibble: 1 × 3
    ##   name           id          data              
    ##   <chr>          <chr>       <list>            
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]>

``` r
weather_nested %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
weather_nested$data[[1]]
```

    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows

## Napoleon!!

Function to get reviews / stars.

``` r
get_page_reviews = function(page_url) {
  
  page_html = read_html(page_url)

  review_titles = 
    page_html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    page_html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
  
  return(reviews)
  
}

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

map(urls, get_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 it was                                                    5 "mad good yo"    
    ##  2 Fun!                                                      4 "Fun and enterta…
    ##  3 Vintage                                                   5 "Easy to order. …
    ##  4 too many commercials                                      1 "5 minutes into …
    ##  5 this film is so good!                                     5 "VOTE FOR PEDRO!"
    ##  6 Good movie                                                5 "Weird story, go…
    ##  7 I Just everyone to know this....                          5 "VOTE FOR PEDRO …
    ##  8 the cobweb in his hair during the bike ramp scene lol     5 "5 stars for bei…
    ##  9 Best quirky movie ever                                    5 "You all know th…
    ## 10 Classic Film                                              5 "Had to order th…
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                             stars text                                 
    ##    <chr>                             <dbl> <chr>                                
    ##  1 hehehehe                              5 goodjobboys                          
    ##  2 Painful                               1 I think I sneezed during the movie a…
    ##  3 GRAND                                 5 GRAND                                
    ##  4 Hello, 90s                            5 So nostalgic movie                   
    ##  5 Cult Classic                          5 Watched it with my older grandchilde…
    ##  6 Format was inaccurate                 4 There was an option to choose blue R…
    ##  7 Good funny                            3 Would recommend                      
    ##  8 Not available w/in 48 hour window     1 I couldn't watch it and there is no …
    ##  9 Your mom went to college.             5 Classic funny movie. It has some of …
    ## 10 Very funny movie                      5 I watch this movie with my family. V…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                       stars text                       
    ##    <chr>                                       <dbl> <chr>                      
    ##  1 Watch it twice! Trust me!                       5 Nothing to dislike!  Cult …
    ##  2 A classic                                       5 If you don’t enjoy this mo…
    ##  3 Can't say how many times I've seen              5 Such a great movie. Will n…
    ##  4 I pity the fool who doesn’t own this movie.     5 I love technology but not …
    ##  5 I don’t know why it’s so popular!               2 My girlfriend loves it!    
    ##  6 Okay                                            3 Okay                       
    ##  7 A WHOLESOME comedic journey                     5 Not a moment of this movie…
    ##  8 Hilarious                                       5 Funny                      
    ##  9 Love it                                         5 What of the funniest movies
    ## 10 WORTH IT!                                       5 It's the dry humor for me.…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                         stars text                     
    ##    <chr>                                         <dbl> <chr>                    
    ##  1 Funny movie.                                      5 Great comedy             
    ##  2 Best movie ever!                                  5 Got this for my sister w…
    ##  3 I was stuck in the oil patch back in the day.     5 I watched this serially.…
    ##  4 Funny Dork humor                                  5 Humor that is funnier wh…
    ##  5 Still funny!                                      5 Still funny!             
    ##  6 Love it!! 💜                                      5 Love it!! 💜             
    ##  7 LOVE it                                           5 cult classic. So ugly it…
    ##  8 Perfect                                           5 Exactly what I asked for 
    ##  9 Love this movie!                                  5 Great movie and sent in …
    ## 10 Love it                                           5 Love this movie. However…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                             stars text                                 
    ##    <chr>                             <dbl> <chr>                                
    ##  1 As described                          3 Book is as described                 
    ##  2 GOSH!!!                               5 Just watch the movie GOSH!!!!        
    ##  3 Watch it right now                    5 You need to watch this movie today. …
    ##  4 At this point it’s an addiction       5 I watch this movie way too much. Hav…
    ##  5 💕                                    5 Hands down, one of my favorite movie…
    ##  6 Good dumb movie                       5 I really wanted to show my spouse a …
    ##  7 funny                                 5 so funny and inventive, if you know …
    ##  8 Best Movie- Try to prove me wrong     5 Best movie ever                      
    ##  9 Vote For Pedro!!                      5 What is NOT to like about this movie…
    ## 10 So Funny                              5 This is such a good movie, so unders…

``` r
napoleon_df =
  tibble(
    urls = urls
  )

napoleon_df %>%
  mutate(
    reviews = map(urls, get_page_reviews)
  ) %>%
  select(reviews) %>%
  unnest()
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(reviews)`

    ## # A tibble: 50 × 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 it was                                                    5 "mad good yo"    
    ##  2 Fun!                                                      4 "Fun and enterta…
    ##  3 Vintage                                                   5 "Easy to order. …
    ##  4 too many commercials                                      1 "5 minutes into …
    ##  5 this film is so good!                                     5 "VOTE FOR PEDRO!"
    ##  6 Good movie                                                5 "Weird story, go…
    ##  7 I Just everyone to know this....                          5 "VOTE FOR PEDRO …
    ##  8 the cobweb in his hair during the bike ramp scene lol     5 "5 stars for bei…
    ##  9 Best quirky movie ever                                    5 "You all know th…
    ## 10 Classic Film                                              5 "Had to order th…
    ## # … with 40 more rows
