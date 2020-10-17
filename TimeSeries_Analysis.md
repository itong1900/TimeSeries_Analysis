    library(ggplot2)
    library(forecast)

    ## Warning: package 'forecast' was built under R version 3.6.2

    library(fpp)
    library(gridExtra)
    library(astsa)
    library(forecast)
    library(TSA)
    library(cowplot)

    ## Warning: package 'cowplot' was built under R version 3.6.2

    library(dplyr)

### Introduction:

This Data Set contains 180 data points with column one representing time
(in months) and column two representing some unknown observations. <br/>

load the dataset

    data1 <- read.csv("./data1.csv")

### 1 Exploratory Data Analysis

#### 1.1 Stationarity & Variance

Let’s define the our initial Time Series as *A*<sub>*t*</sub>.
<br/><br/> We began our EDA by first observing its general trend and
shape (Figure 1.1a). Through its obvious upward trend,
heteroscedasticity, and potential seasonality, we understood that it was
clearly not stationary time series. Thus, to address the issue, we began
our journey to make predictions by conducting a variance stabilizing
transformation followed by differencing in order to achieve stationary
data for the purpose of using an appropriate (M)(S)ARIMA
model.<br/><br/>

To remove the unstationarities, we do the following transformations:
<br/> 1) Take Logarithm (*A*<sub>*t*</sub> −  &gt; *L*<sub>*t*</sub>) :
*L*<sub>*t*</sub> = *l**o**g**A*<sub>*t*</sub> <br/> 2) Take First
Difference (*L*<sub>*t*</sub> −  &gt; *X*<sub>*t*</sub>) :
*X*<sub>*t*</sub> = *L*<sub>*t*</sub> − *L*<sub>*t* − 1</sub>

    # figure 1.1a
    plot1 <- ggplot() +
      geom_line(data = data1, aes(X, x), color = "black")  + labs(title="Plot of Orginal Time Series, At",
            x ="Time", y = "X")

    # figure 1.1b  take logarithm
    data1 <- data1 %>% mutate(Lt = log(data1$x))
    plot2 <- ggplot() +
      geom_line(data = data1, aes(X, Lt), color = "black")  + labs(title="Plot of Log Transformed Data, Lt",
            x ="Time", y = "Nt")

    # figure 1.1c  take 1st difference
    data1 <- data1 %>% mutate(Xt = c(NA, diff(data1$x, lag = 1)))
    plot3 <- ggplot() +
      geom_line(data = data1, aes(X, Xt), color = "black") + geom_hline(yintercept = 0, 
                    color = "blue", size=0.7) + labs(title="Plot of 1st Difference Time Series, Xt",
            x ="Time", y = "Xt")

    grid.arrange(plot1, plot2, plot3, ncol = 2)

![](TimeSeries_Analysis_files/figure-markdown_strict/unnamed-chunk-3-1.png)

1.2 ACF & PACF of Xt Looking at Figure 1.2a, we observe that the ACF and
PACF of *X*<sub>*t*</sub> are significant at around lag 6, 12, 18, 24,
a.k.a implying a seasonal pattern of 6 periods. This is suggesting us to
remove the seasonalities by differencing.

1.3 Seasonal differencing<br/> Take Sixth Difference on
*X*<sub>*t*</sub> (*X*<sub>*t*</sub> −  &gt; *Y*<sub>*t*</sub>) :
*Y*<sub>*t*</sub> = *X*<sub>*t*</sub> − *X*<sub>*t* − 12</sub>

    p1 <- ggAcf(data1$Xt, lag.max = 120)+ ggtitle("Xt ACF")
    p2 <- ggPacf(data1$Xt, lag.max = 120) + ggtitle("Xt PACF")
    #grid.arrange(p1, p2, nrow = 2)

    data1 <- data1 %>% mutate(Yt = c(NA, NA,NA,NA,NA,NA,NA, NA,NA,NA,NA,NA, diff(data1$Xt, lag = 12)))
    plot4 <- ggplot() +
      geom_line(data = data1, aes(X, Yt), color = "black") + geom_hline(yintercept = 0, 
                    color = "blue", size=0.7) + labs(title="Plot of Yt, a.k.a Xt with Seasonality Removed",
            x ="Time", y = "Yt")

    grid.arrange(p1, p2,plot4, nrow = 2)

![](TimeSeries_Analysis_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    adf.test(na.remove(data1$Yt))

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  na.remove(data1$Yt)
    ## Dickey-Fuller = -4.3427, Lag order = 5, p-value = 0.01
    ## alternative hypothesis: stationary

1.4 select a model<br/> Augmented Dicky-Fuller test indicates if the
data correponds to a stationary process, and the above test shows
*Y*<sub>*t*</sub> is significant at 0.01 confidence level. Therefore, we
are quite convinced that *Y*<sub>*t*</sub> is a stationary process.
We’ll fit an ARMA model from here.<br/> From PACF, we see significant
lags at lag4, lag5, lag7, lag9, lag14; from the ACF, we still see a
seasonal pattern at period of 12. Since a seansonal differencing has
applied here, it’s reasonable to propose a GARCH model here(we’ll
discuss this in Second Section). But we’ll stick to SARIMA within this
section.

    p3 <- ggAcf(data1$Yt, lag.max = 60)+ ggtitle("Yt ACF")
    p4 <- ggPacf(data1$Yt, lag.max = 60) + ggtitle("Yt PACF")
    grid.arrange(p3, p4, nrow = 2)

![](TimeSeries_Analysis_files/figure-markdown_strict/unnamed-chunk-6-1.png)

1.5 Demo Fit an ARMA model on *Y*<sub>*t*</sub> Based on the ACF and
PACF, we’ll fit ARMA data on *Y*<sub>*t*</sub>, A.K.A a
*S**A**R**I**M**A*(*p*, 1, *q*) \* (*P*, 1, *Q*)<sub>12</sub> Model on
the log transformed of original data(*L*<sub>*t*</sub>). Since there’s
not an explicit obvious ARMA pattern given in *Y*<sub>*t*</sub>, a few
options are proposed here and we will later on make a model selection
based on the Cross Validation results.<br/> Option1 : ARMA(4,1) |||
Reason: We have two very relevant AR factor ar1 ar4, as well as an MA
factor ma1

    ## Model 1 summary
    model1 <- arma(na.remove(data1$Yt), order = c(4,2), include.intercept = FALSE)
    summary(model1)

    ## 
    ## Call:
    ## arma(x = na.remove(data1$Yt), order = c(4, 2), include.intercept = FALSE)
    ## 
    ## Model:
    ## ARMA(4,2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.28422 -0.05903 -0.00145  0.05779  0.29225 
    ## 
    ## Coefficient(s):
    ##      Estimate  Std. Error  t value Pr(>|t|)    
    ## ar1  -0.90974     0.08309  -10.949   <2e-16 ***
    ## ar2  -0.93102     0.10915   -8.530   <2e-16 ***
    ## ar3  -0.08272     0.11405   -0.725   0.4683    
    ## ar4   0.14985     0.07877    1.902   0.0571 .  
    ## ma1   0.81883     0.04521   18.112   <2e-16 ***
    ## ma2   0.95530     0.02482   38.489   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Fit:
    ## sigma^2 estimated as 0.01,  Conditional Sum-of-Squares = 1.62,  AIC = -283.09

    ## plot the Yt_hat and Yt
    model1_perf <- data.frame(c(1:180), c(NA, NA,NA,NA,NA,NA,NA,NA, NA,NA,NA,NA,NA, model1$fitted.values))
    x <- c("X", "Yt_fitted")
    colnames(model1_perf) <- x

    plot5 <- ggplot() +
      geom_line(data = model1_perf, aes(X, Yt_fitted), color = "black") +
      geom_line(data = data1, aes(X, Yt), color = "red") + ggtitle("model1, Yt red, Yt_fitted black")+
      geom_hline(yintercept = 0, color = "blue", size=0.7) 
      
    plot5

![](TimeSeries_Analysis_files/figure-markdown_strict/unnamed-chunk-7-1.png)

1.6 From here we’ll officially comparing a few potential models, to
avoid the redundant procedures above, we won’t repeat the above process
for tidyness, but they are essentially similar. Instead, to give better
interpretation, we’ll leverage the SARIMA and SARIMA.for functions for
our model validation process. For consistency purpose, we would also
apply the SARIMA function on model1 again.<br/><br/>

Model1: *S**A**R**I**M**A*(4, 1, 2)(0, 1, 0)<sub>12</sub>

    ## Predictions
    sarima.for(data1$Lt, p=4, d=1, q=2, P=0, D=1, Q=0, S = 12,n.ahead = 12)

    ## Warning in log(s2): NaNs produced

    ## Warning in log(s2): NaNs produced

![](TimeSeries_Analysis_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    ## $pred
    ## Time Series:
    ## Start = 181 
    ## End = 192 
    ## Frequency = 1 
    ##  [1] 0.6689945 0.7018705 0.6854186 0.6775948 0.6445388 0.6374632 0.6282993
    ##  [8] 0.6355440 0.6663835 0.6769867 0.6747376 0.7274834
    ## 
    ## $se
    ## Time Series:
    ## Start = 181 
    ## End = 192 
    ## Frequency = 1 
    ##  [1] 0.06835722 0.08183900 0.10111095 0.12251906 0.14516423 0.16219509
    ##  [7] 0.17430588 0.18482541 0.19654125 0.20942864 0.22146368 0.23150976

    ## validation

    ## a helper function to avoid sarima functions generating tons of graph during validation
    sarima.noplot = function(x0, x1, x2, x3, x4, x5, x6, x7, x8) {
      png(tf<-tempfile())
      out <- sarima.for(x0, p=x1, d=x2, q=x3, P=x4, D=x5, Q=x6, S = x7,n.ahead = x8)
      dev.off()
      file.remove(tf)
      return(out)
    }

    pred_value = matrix(0, nrow = 180, ncol = 1)

    i = 1
    while(i <= 156){
      data1sub = data1$Lt[data1$X < i + 24]
      # model1
      cvmodel1 <- sarima.noplot(x0 =data1sub, x1=4, x2=1, x3=2, x4=0, x5=1, x6=0, x7 = 12, x8= 12)
      pred_value[(24+i):(35+i),] <- exp(cvmodel1$pred)
      i = i + 12
    }

    ## plot the cross validation result against the true value
    pred_value <- data.frame(c(1:180),pred_value)
    ggplot() + 
      geom_line(data = data1, aes(X, x), color = "black")  +labs(title="Plot of At(dark) versas At_hat(red), by Model1 SARIMA(4,1,2)(0,1,0)12", x ="Time", y = "X") + 
      geom_line(data = pred_value, aes(c.1.180., pred_value), color = "red")

![](TimeSeries_Analysis_files/figure-markdown_strict/unnamed-chunk-10-1.png)

Model2: *S**A**R**I**M**A*(1, 1, 0)(0, 1, 1)<sub>6</sub>

    ## Predictions
    sarima.for(data1$Lt, p=1, d=1, q=0, P=0, D=1, Q=1, S = 6,n.ahead = 12)

![](TimeSeries_Analysis_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    ## $pred
    ## Time Series:
    ## Start = 181 
    ## End = 192 
    ## Frequency = 1 
    ##  [1] 0.7079299 0.7335497 0.7331298 0.7211477 0.6960043 0.7248602 0.7303324
    ##  [8] 0.7553687 0.7550433 0.7430459 0.7179049 0.7467605
    ## 
    ## $se
    ## Time Series:
    ## Start = 181 
    ## End = 192 
    ## Frequency = 1 
    ##  [1] 0.06053351 0.07896733 0.09471306 0.10806727 0.11996188 0.13077575
    ##  [7] 0.14152461 0.15136421 0.16061921 0.16936659 0.17768441 0.18562889

    pred_value = matrix(0, nrow = 180, ncol = 1)

    i = 1
    while(i <= 156){
      data1sub = data1$Lt[data1$X < i + 24]
      # model1
      cvmodel1 <- sarima.noplot(x0 =data1sub, x1=1, x2=1, x3=0, x4=0, x5=1, x6=1, x7 = 12, x8= 12)
      pred_value[(24+i):(35+i),] <- exp(cvmodel1$pred)
      i = i + 12
    }

    ## plot the cross validation result against the true value
    pred_value <- data.frame(c(1:180),pred_value)
    ggplot() + 
      geom_line(data = data1, aes(X, x), color = "black")  +labs(title="Plot of At(dark) versas At_hat(red), by Model1 SARIMA(1,1,0)(0,1,1)12", x ="Time", y = "X") + 
      geom_line(data = pred_value, aes(c.1.180., pred_value), color = "red")

![](TimeSeries_Analysis_files/figure-markdown_strict/unnamed-chunk-12-1.png)
