    library(ggplot2)
    library(forecast)
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
      geom_line(data = data1, aes(X, Lt), color = "black")  + labs(title="Plot of Log Transformed Time Series, Lt",
            x ="Time", y = "Nt")

    # figure 1.1c  take 1st difference
    data1 <- data1 %>% mutate(Xt = c(NA, diff(data1$x, lag = 1)))
    plot3 <- ggplot() +
      geom_line(data = data1, aes(X, Xt), color = "black") + geom_hline(yintercept = 0, 
                    color = "blue", size=0.7) + labs(title="Plot of 1st Difference Time Series",
            x ="Time", y = "Xt")

    grid.arrange(plot1, plot2, plot3, ncol = 2)

![](TimeSeries_Analysis_files/figure-markdown_strict/unnamed-chunk-3-1.png)

1.2 ACF & PACF of Xt Looking at the ACF and PACF of the Xt(Figure 1.2a),
we observe that both have a slight tapering off behavior in small lags,
implying a non-seasonal ARMA for Xt (we’ll discuss this in greater
detail in our model selection process from Section 3); at big lags, both
ACF and PACF also show a tapering off behavior in a seasonal pattern.
Such a behavior directs us to utilizing Frequency Domain Analysis to
detect the exact seasonality (this will be done in Section 2).

    p1 <- ggAcf(data1$Xt, lag.max = 120)+ ggtitle("1st diff of sqrt original")
    p2 <- ggPacf(data1$Xt, lag.max = 120) + ggtitle("1st diff of sqrt original")
    grid.arrange(p1, p2, nrow = 2)

![](TimeSeries_Analysis_files/figure-markdown_strict/unnamed-chunk-4-1.png)
