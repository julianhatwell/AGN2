stats <- data.frame(supp = character(), mn = numeric(), se = numeric(), uci = numeric(), lci = numeric())
for (supp in c("VC", "OJ")) {
  smpl <- ToothGrowth$len[ToothGrowth$supp == supp]
  mn <- mean(smpl)
  se <- sd(smpl)/sqrt(length(smpl))
  uci <- mn + qnorm(0.975) * se
  lci <- mn - qnorm(0.975) * se
  stats <- rbind(stats
                 , data.frame(supp = supp
                              , mn = mn
                              , se = se
                              , uci = uci
                              , lci = lci
                 )
  )
}

The following code producse a densityplot of the distribution of the standard deviation of the sample mean (standard error). Superimposed on this is a PDF of the Standard Normal distribution, shifted to centre on the theoretical SE value:
  
  ```{r histogram_sample_stdevs}
densityplot(~sim_var, stats, bw = .1, lty = 2
            , panel = function(x,...) {
              panel.densityplot(x,..., col = "blue")
              x <- seq(-1, 3, 0.05)
              y <- dnorm(x, mean = stats$theo_var, sd = 1)
              panel.xyplot(x,y,..., type = "l", lwd = 1.5, col = "magenta")   
            }
            , main = "Density plot of st.devs from the simulation"
            , xlab = "sample st.dev"
            , ylab = "Probability"
            , key = list(text = list(c("density plot of sample st.devs", "standard normal curve")), columns = 2, lines = list(col = c("blue", "magenta"), lty = c(1,2))))
```


```{r mean_CI_plots_by_supplement}
g <- ggplot(stats, aes(x = supp, y = mn
                       , ymax = uci, ymin = lci
                       , colour = supp))
g <- g + labs(x = "Supplement", y = "Mean with confidence interval")
g <- g + geom_errorbar(size = 2)
g <- g + scale_color_discrete("Supplement")
g
```

```{r lib_gg}
library(ggplot2)
``
```{r hist_tooth_length}
densityplot(~len, bw = 2, data = ToothGrowth)
```

There are 3 peaks to the density plot. The question immediately arises whether this is simply due to the small data set (60 observations) or some other factor.

The next plot will see the how the density varies with dose for each supplement:
  
  ```{r hist_tooth_length_by_supp}
densityplot(~len | supp, bw = 2, data = ToothGrowth, layout = c(1,2))
```
`
Both density plots appear to have very similar ranges but distribution/density looks quite different. 

There is a pronounced striation particularly visible in the VC panel. The OJ panel may follow a similar pattern, but it is overwhelmed by a large right skewed peak. Is this a feature of the population (how all guinea pigs respond to the use of supplements)? Or is it simple an artefact of the small sample?






```{r bootstrapping_simulation}
# here are all the standard parameters. Can be tweaked for more further investigations
set.seed(100) # reprodicibility
sim_size <- 100 # simulation size
smpl_size <- 10 # sample size, each simulation

# set up an empty frame
sim = data.frame(supp = character(), mu = numeric(), se = numeric())
# run bootstrapping
for (supp in c("VC", "OJ")) {
  sample_means <- 
    apply(matrix(sample(ToothGrowth$len[ToothGrowth$supp == supp]
                        , smpl_size, replace = TRUE)
                 , nrow = sim_size), 1
          , mean)
  # capture the statistics
  sim <- rbind(sim
               , data.frame(supp = supp
                            , mu = mean(sample_means)
                            , se =  sd(sample_means)/sqrt(smpl_size)))
}


# In order to take advantage of the CLT, it is necessary to generate a number of sample means, and determine the confidence limits for the theoretical population mean derived from these. The bootstrapping principle is used, although the small starting sample size could introduce bias and other problems.

```{r basic_calculation_dose_statistics}
d_stats <- data.frame(d = factor(), mn = numeric(), se = numeric(), uci = numeric(), lci = numeric())
for (d in ToothGrowth$dose) {
  smpl <- ToothGrowth$len[ToothGrowth$dose == d]
  mn <- mean(smpl)
  se <- sd(smpl)/sqrt(length(smpl))
  uci <- mn + qnorm(0.975) * se
  lci <- mn - qnorm(0.975) * se
  d_stats <- rbind(d_stats
                   , data.frame(d = as.factor(d)
                                , mn = mn
                                , se = se
                                , uci = uci
                                , lci = lci
                   )
  )
}
```


The preference for this type of plotting is ggplot due to its intuitive customisation options:
  
  ```{r lib_gg2}
library(ggplot2)
```

```{r mean_CI_plots_by_dose}
g <- ggplot(d_stats, aes(x = d, y = mn
                         , ymax = uci, ymin = lci
                         , colour = d))
g <- g + labs(x = "Dose", y = "Mean with confidence interval")
g <- g + geom_errorbar(size = 2)
g <- g + scale_color_discrete("Dose")
g
```

The confidence intervals do not overlap. The null hypothesis is rejected. We can say that increasing the dose increase the tooth growth with >95% confidence.
```{r corrgram_mtcars}
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.pie,
         upper.panel=panel.ellipse
         , text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="Correlogram of mtcars dataset")
```


The following code plots the scatterplot matrix for the variables of interest conditioned by Transmission. Scales have been removed to focus only on trend relationships at this time:
  
  ```{r}
splom(mtcars[c(1, 5:7, 11)]
      , groups = mtcars$trans
      , cex = 0.75
      , pscales = 0
      , varnames = c("Miles\nper\ngallon"
                     ,"Rear\naxle\nratio"
                     , "Weight"
                     , "1/4 mile\ntime"
                     , "Carburetors")
      , auto.key = list(columns = 2
                        , title = "Transmission", cex = 0.6)
      , main = "Scatter Plot Matrix mpg vs variable of interest"
      , sub = "The reader is suggested to focus on the bottom row"
      , aspect = 0.6)
```

The graph shows the 4 selected variables, each with a different characteristic relationship to mpg.

The trends appear to be positive for rear axle ratio and 1/4 mile time although the separation between transmission type differs between these two.

The trends appear to be negative for weight and carburetors although the separation between transmission type differs for these two also.



This is useful as it helps to visualise the relationship of variables to mpg and also to each other.
xyplot(mpg~wt, groups = dgn, data = mtcars
       , main = "Scatter plot of mpg by weight separating the outliers"
       , xlab = "Weight (1000lbs"
       , ylab = "miles per gallon")


densityplot(~mpg, mtcars, groups = dgn, bw = 2
            , lwd = c(2,0.2), col = c("blue","steel blue")
            , lty = c(1,2), alpha = c(1,0.5)
            , main = "Density plot of mpg from mtcars, outliers separated"
            , xlab = "miles per gallon"
            , panel = panel.superpose
            , panel.groups = function(x, group.number,...) {
              if (group.number == 1) {
                panel.densityplot(x,...,)
                x <- seq(min(x), max(x), 1)
                y <- dnorm(x, mean = mean(x), sd = sd(x))
                panel.xyplot(x,y, type = "l", lwd = 2, lty = 2, col = "magenta")   
              }
              if (group.number == 2) {
                panel.densityplot(x,...)}
            }
            , key = list(text = list(
              c("mpg density plot, outliers removed"
                , "normal curve, mean(mpg), sd(mpg)"
                , "density outliers only")), columns = 2
              , lines = list(
                col = c("blue", "magenta", "steel blue")
                , lty = c(1,2, 2)))
)



#### Understanding the theoretical Exponential distribution:

To begin with, it is useful to understand the characteristics of the theoretical Exponential function. Without getting too deeply into the maths, this can be quite easily seen visually by plotting the Probability Density Function.

The following code plots the PDF of the Exponential Distribution:
  
  ```{r explore_exp_dist}
# a little trial and error to determine the correct range of x
x <- seq(-1, 25, 0.1)
y <- dexp(x, 0.2)
xyplot(y~x, type = "l", lwd = 2, col = "blue"
       , main = "PDF of the Exponential Distribution"
       , xlab = "Value", ylab = "Probability")
```

A density plot was created to examine the distribution of mpg data. A reference curve of the normal distribution with identical mean and sd is superimposed.

```{r mpg_density}
densityplot(~mpg, mtcars, bw = 2, lwd = 2
            , main = "Density plot of mpg from mtcars"
            , xlab = "miles per gallon"
            , panel = function(x,...) {
              panel.densityplot(x,..., col = "blue")
              x <- seq(min(x), max(x), 1)
              y <- dnorm(x, mean = mean(x), sd = sd(x))
              panel.xyplot(x,y,..., type = "l", lty = 2, col = "magenta")   
            }
            
            , key = list(text = list(c("mpg density", "N-Dist, mean(mpg), sd(mpg)")), columns = 2, lines = list(col = c("blue", "magenta"), lty = c(1,2))))
```

The mpg data is approximately normally distributed. Some deviation may be a result of the small data set, or there may be some systemic reason common to the car manufacturing process.

This investigation is primarily interested in which transmission type gives the best mpg value. The first thing to do is see what a simple comparison reveals.

A t-test was run of the mpg of automatic vs manual transmission to confirm and quantify the difference:
  
  ```{r ttest_mpg_transmission}
tt <- t.test(mpg ~ trans, paired = FALSE, data = mtcars)
tt$conf.int
```

Zero is not contained in the confidence interval and the null hypothesis is rejected. Cars with different transmission types have significantly different mpg ratings. 

It can be inferred with >95% confidence that the difference in mean mpg of automatic vehicles in the sample is between the confidence intervals of the t-test. 

Also a dot plot is generated to show how the weights are distributed. A reference line is added to show the marginal difference (difference in averages between the two groups).



A t.test is run to quantifies the correlation between wt and trans. 

```{r compare_wt_trans}
tt <- t.test(wt~trans, paired = FALSE, var.equal = FALSE, data = mtcars)
tt$conf.int
```

Diagnostic plots have been rendered 1/4 size for brevity. The reader is encouraged to run them full size from the [R Mark Down document](https://github.com/julianhatwell/Agn/blob/master/Regression_Modeling_of_mtcars_data.Rmd).
