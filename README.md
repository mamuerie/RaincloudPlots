## Create Raincloud Plots

Plots and functions are based on the following scripts:
<https://github.com/RainCloudPlots/RainCloudPlots/tree/master/tutorial_R>

### 1. Load specific package versions

``` r
renv::restore()
```

    ## * The library is already synchronized with the lockfile.

### 2. Load Raincloud File

``` r
source(here("R-Function", "R_rainclouds.R"))
source(here("R-Function", "summarySE.R"))
```

### 3. Load and prepare data

``` r
data <-
  read.table(here("data", "PMF03_performance.csv"),
             sep = ";",
             header  = T) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    timepoint = fct_recode(timepoint, "Pre" = "pre", "Post" = "post"),
    timepoint = fct_relevel(timepoint, c("Pre", "Post")),
    mapping = fct_recode(mapping, "compatible" = "comp", "incompatible" = "incomp")
  )
head(data)
```

    ##   timepoint subjectID      mapping    condition     Sex AgeToday pHit.pFa_DT
    ## 1      Post       101   compatible   compInterv diverse       30      0.9500
    ## 2      Post       101 incompatible   compInterv diverse       30      0.8750
    ## 3      Post       102   compatible incompInterv    male       26      0.8275
    ## 4      Post       102 incompatible incompInterv    male       26      0.7875
    ## 5      Post       103   compatible   compInterv  female       26      0.7500
    ## 6      Post       103 incompatible   compInterv  female       26      0.3900
    ##   pHit.pFa_ST
    ## 1      1.0000
    ## 2      0.9750
    ## 3      0.9625
    ## 4      0.8500
    ## 5      0.9125
    ## 6      0.9250

``` r
str(data)
```

    ## 'data.frame':    180 obs. of  8 variables:
    ##  $ timepoint  : Factor w/ 2 levels "Pre","Post": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ subjectID  : int  101 101 102 102 103 103 104 104 105 105 ...
    ##  $ mapping    : Factor w/ 2 levels "compatible","incompatible": 1 2 1 2 1 2 1 2 1 2 ...
    ##  $ condition  : Factor w/ 3 levels "compInterv","incompInterv",..: 1 1 2 2 1 1 2 2 1 1 ...
    ##  $ Sex        : Factor w/ 3 levels "diverse","female",..: 1 1 3 3 2 2 3 3 3 3 ...
    ##  $ AgeToday   : int  30 30 26 26 26 26 22 22 22 22 ...
    ##  $ pHit.pFa_DT: num  0.95 0.875 0.828 0.787 0.75 ...
    ##  $ pHit.pFa_ST: num  1 0.975 0.963 0.85 0.912 ...

### 4. Calculate group means

``` r
summaryDT <- summarySE(data, groupvars = c("timepoint", "mapping", "condition"), measurevar = "pHit.pFa_DT", na.rm =T)
```

### 5. Define Raincloud theme

``` r
raincloud_theme <- theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 16),
  strip.text = element_text(size = 16),
  legend.position = "right",
  plot.title = element_text(
    lineheight = .8,
    face = "bold",
    size = 16
  ),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(
    colour = 'black',
    size = 0.5,
    linetype = 'solid'
  ),
  axis.line.y = element_line(
    colour = 'black',
    size = 0.5,
    linetype = 'solid'
  )
)
```

### 6. Create Raincloud plots

``` r
ggplot(data, aes(x = timepoint, y = pHit.pFa_DT, fill = mapping)) +
  geom_flat_violin(
    aes(fill = mapping),
    position = position_nudge(x = .15, y = 0),
    adjust = 1.5,
    trim = FALSE,
    alpha = .4,
    colour = NA,
    show.legend = FALSE
  ) +
  geom_point(
    aes(
      x = as.numeric(timepoint) + .25,
      y = pHit.pFa_DT,
      colour = mapping
    ),
    position = position_jitter(width = .05),
    size = 1.5,
    shape = 20,
    show.legend = FALSE
  ) +
  geom_boxplot(
    aes(x = timepoint, y = pHit.pFa_DT, fill = mapping),
    outlier.shape = NA,
    alpha = .7,
    width = .2,
    colour = "black"
  ) +
  facet_grid(. ~ condition) +
  scale_colour_manual(values = c("#005824", "#990000")) +
  scale_fill_manual(values = c("#005824", "#990000")) +
  labs(title = "Dual-task Performance for each Intervention Group and Mapping",
       subtitle = "Showing boxplot and distribution") +
  xlab("") +
  ylab("Performance p(hit)-p(fa)") +
  theme_classic() +
  raincloud_theme
```

<img src="C:/Users/Marie/Documents/IPU/Software/R-RaincloudPlots/R-RaincloudPlots/figure/figure-raincloudPlotBoxplot-1.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(data, aes(x = timepoint, y = pHit.pFa_DT, fill = mapping)) +
  geom_flat_violin(
    aes(fill = mapping),
    position = position_nudge(x = .1, y = 0),
    adjust = 1.5,
    trim = FALSE,
    alpha = .4,
    colour = NA,
    show.legend = FALSE
  ) +
  geom_point(
    aes(
      x = as.numeric(timepoint) + .2,
      y = pHit.pFa_DT,
      colour = mapping
    ),
    position = position_jitter(width = .05),
    size = 1.5,
    shape = 20,
    show.legend = FALSE
  ) +
  geom_line(
    data = summaryDT,
    aes(
      x = as.numeric(timepoint) - .1,
      y = pHit.pFa_DT_mean,
      group = mapping,
      colour = mapping
    ),
    linetype = 1,
    size = 1
  ) +
  geom_point(
    data = summaryDT,
    aes(
      x = as.numeric(timepoint) - .1,
      y = pHit.pFa_DT_mean,
      group = mapping,
      colour = mapping
    ),
    shape = 18,
    size = 2
  ) +
  geom_errorbar(
    data = summaryDT,
    aes(
      x = as.numeric(timepoint) - .1,
      y = pHit.pFa_DT_mean,
      group = mapping,
      colour = mapping,
      ymin = pHit.pFa_DT_mean - se,
      ymax = pHit.pFa_DT_mean + se
    ),
    width = .05
  ) +
  facet_grid(. ~ condition) +
  scale_colour_manual(values = c("#005824", "#990000")) +
  scale_fill_manual(values = c("#005824", "#990000")) +
  labs(title = "Dual-task Performance for each Intervention Group and Mapping",
       subtitle = "Showing means, standard error and distribution") +
  xlab("") +
  ylab("Performance p(hit)-p(fa)") +
  theme_classic() +
  raincloud_theme
```

<img src="C:/Users/Marie/Documents/IPU/Software/R-RaincloudPlots/R-RaincloudPlots/figure/figure-raincloudPlotPoints-1.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(data, aes(x = timepoint, y = pHit.pFa_DT, fill = mapping)) +
  geom_flat_violin(
    aes(fill = mapping),
    position = position_nudge(x = .1, y = 0),
    adjust = 1.5,
    trim = FALSE,
    alpha = .4,
    colour = NA
  ) +
  geom_point(
    aes(
      x = as.numeric(timepoint) - .2,
      y = pHit.pFa_DT,
      colour = mapping
    ),
    position = position_jitter(width = .05),
    size = 1,
    shape = 20,
    show.legend = FALSE,
    alpha = 0.4
  ) +
  geom_line(
    data = summaryDT,
    aes(
      x = as.numeric(timepoint),
      y = pHit.pFa_DT_mean,
      group = mapping,
      colour = mapping
    ),
    linetype = 3,
    size = 1
  ) +
  geom_point(
    data = summaryDT,
    aes(
      x = as.numeric(timepoint),
      y = pHit.pFa_DT_mean,
      group = mapping,
      colour = mapping
    ),
    shape = 18,
    size = 2
  ) +
  geom_errorbar(
    data = summaryDT,
    aes(
      x = as.numeric(timepoint),
      y = pHit.pFa_DT_mean,
      group = mapping,
      colour = mapping,
      ymin = pHit.pFa_DT_mean - se,
      ymax = pHit.pFa_DT_mean + se
    ),
    width = .05
  ) +
  facet_grid(. ~ condition) +
  scale_colour_manual(values = c("#005824", "#990000")) +
  scale_fill_manual(values = c("#005824", "#990000")) +
  labs(title = "Dual-task Performance for each Intervention Group and Mapping",
       subtitle = "Showing means, standard error and distribution") +
  xlab("") +
  ylab("Performance p(hit)-p(fa)") +
  theme_classic() +
  raincloud_theme
```

<img src="C:/Users/Marie/Documents/IPU/Software/R-RaincloudPlots/R-RaincloudPlots/figure/figure-raincloudPlotPosition-1.png" width="90%" style="display: block; margin: auto;" />
