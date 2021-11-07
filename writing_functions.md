Writing Functions
================
Wanxin Qi
11/7/2021

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.33297426 -0.65629703  1.03322845 -0.42099703 -0.49102391  0.96859969
    ##  [7] -1.52330566  0.18370072 -1.25844910  1.16292343 -0.67892489  2.49859293
    ## [13]  0.83259601 -0.78369457  0.66776192  0.91605100  1.00865725 -0.50423176
    ## [19] -1.71941067 -0.86087712  0.76094003 -0.24324646  0.02103518 -0.44596595
    ## [25] -0.80063675

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

    ##  [1]  0.33297426 -0.65629703  1.03322845 -0.42099703 -0.49102391  0.96859969
    ##  [7] -1.52330566  0.18370072 -1.25844910  1.16292343 -0.67892489  2.49859293
    ## [13]  0.83259601 -0.78369457  0.66776192  0.91605100  1.00865725 -0.50423176
    ## [19] -1.71941067 -0.86087712  0.76094003 -0.24324646  0.02103518 -0.44596595
    ## [25] -0.80063675

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1] -0.7804532 -0.9286272  1.1482435 -0.5761879  1.8048558  1.4133295
    ##  [7]  0.9413582  0.2839532 -1.0801489  0.8556809  0.1590670  0.8906967
    ## [13]  0.1368292  0.1118533  0.6359908  0.3734474 -1.5811202  1.0104652
    ## [19] -1.4840425 -0.4804773  0.7526970  1.0808997  0.8750104 -0.2678249
    ## [25] -1.2084325  0.6643525 -0.9855730 -0.4030620 -0.3285957  1.5981494
    ## [31] -0.6386416 -0.9001139  1.1469233 -0.6623676 -0.9941205 -1.0405567
    ## [37]  0.3158130 -0.1730731  0.8219748 -2.5081724

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

    ##  [1] -0.7804532 -0.9286272  1.1482435 -0.5761879  1.8048558  1.4133295
    ##  [7]  0.9413582  0.2839532 -1.0801489  0.8556809  0.1590670  0.8906967
    ## [13]  0.1368292  0.1118533  0.6359908  0.3734474 -1.5811202  1.0104652
    ## [19] -1.4840425 -0.4804773  0.7526970  1.0808997  0.8750104 -0.2678249
    ## [25] -1.2084325  0.6643525 -0.9855730 -0.4030620 -0.3285957  1.5981494
    ## [31] -0.6386416 -0.9001139  1.1469233 -0.6623676 -0.9941205 -1.0405567
    ## [37]  0.3158130 -0.1730731  0.8219748 -2.5081724

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
    ## 1  5.48  4.78

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.303

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
    ## 1  2.59  2.59

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
    ## 1  4.45  3.56

``` r
sim_mean_sd(mu = 4, n = 30, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.02  2.43

## Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
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
```

Okay but there are a lot of pages of reviews.

Write a function that gets reviews based on page url

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

bind_rows(
    get_page_reviews(urls[1]),
    get_page_reviews(urls[2]),
    get_page_reviews(urls[3]),
    get_page_reviews(urls[4]),
    get_page_reviews(urls[5]))
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 this film is so good!                                     5 VOTE FOR PEDRO!  
    ##  2 Good movie                                                5 Weird story, goo…
    ##  3 I Just everyone to know this....                          5 VOTE FOR PEDRO !…
    ##  4 the cobweb in his hair during the bike ramp scene lol     5 5 stars for bein…
    ##  5 Best quirky movie ever                                    5 You all know the…
    ##  6 Classic Film                                              5 Had to order thi…
    ##  7 hehehehe                                                  5 goodjobboys      
    ##  8 Painful                                                   1 I think I sneeze…
    ##  9 GRAND                                                     5 GRAND            
    ## 10 Hello, 90s                                                5 So nostalgic mov…
    ## # … with 40 more rows

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4
