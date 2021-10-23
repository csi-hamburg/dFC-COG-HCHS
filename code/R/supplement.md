---
title: "Supplement to dVC-WMH-COG in HCHS"
author: "ES"
date: "10/19/2021"
output: 
  html_document:
    keep_md: yes
---





```r
#source('./prepdata.r')
source('loadpackages.r')
load('prepdata.RData')
```



```r
na_patterns <- clindata %>% 
  dplyr::select(-c(ID, riskgroup_yn)) %>% 
  pivot_wider(names_from = 'cogmeas', values_from = 'cogvalue') %$% 
  Hmisc::naclus(.)
plot(na_patterns)
```

![](supplement_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
Hmisc::naplot(na_patterns, 'all', cex=1, cex.lab=.5, cex.axis = .5, col = 'red')
```

![](supplement_files/figure-html/unnamed-chunk-1-2.png)<!-- -->![](supplement_files/figure-html/unnamed-chunk-1-3.png)<!-- -->![](supplement_files/figure-html/unnamed-chunk-1-4.png)<!-- -->![](supplement_files/figure-html/unnamed-chunk-1-5.png)<!-- -->


```r
clindata %>% 
  dplyr::filter(cogmeas %in% c('MMST', 'TMTA', 'TMTB', 'Recall_Sumwords', 'Recall_percentsavings', 'Wortschatz', 'animaltest')) %>% 
  pivot_wider(id_cols = ID, names_from = cogmeas, values_from = cogvalue) %>% 
  dplyr::select(-ID) %>% 
  na.omit() %>% 
  ggplot(aes(x = .panel_x, y = .panel_y)) +
  geom_autopoint(alpha = 0.2, shape = 16, size = 0.5, position = 'auto') + 
  geom_autohistogram(alpha = 0.3, colour = NA, position = 'identity') + 
  facet_matrix(vars(everything()),  layer.diag = 2, grid.y.diag = FALSE) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_bw() +
  theme(legend.direction = 'horizontal'
        , legend.position = 'bottom'
        , legend.margin=margin(0,0,0,0)
        , legend.box.margin=margin(-15,0,0,0)
        , legend.text = element_text(size = 5)
        , legend.title = element_text(size = 6)
        , legend.key.height = unit(0.1, 'cm')
        , axis.title = element_text(size = 6)
        , axis.text = element_text(size = 6)
        , strip.background = element_blank()
        , strip.text = element_text(size = 8))
```

<img src="supplement_files/figure-html/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

```r
ggsave(filename = './../../derivatives/figures/scatterplotCOG.png'
                                   , device = 'png', dpi = 600, width = 18, height = 18, units = 'cm')
```


```r
x <- dd %>% filter(meas == 'dwell' & state == levels(dd$state)[1]) %>% pull('value')

fit.gamma <- fitdist(x, distr = "gamma", method = "mme",  lower = c(0.1, 0.1), upper = c(20, 20), start = list(scale = 1, shape = 1))
summary(fit.gamma)
```

```
## Fitting of the distribution ' gamma ' by matching moments 
## Parameters : 
##       estimate
## shape 21.26876
## rate  12.87754
## Loglikelihood:  -350.9407   AIC:  705.8815   BIC:  715.6545
```

```r
plot(fit.gamma)
```

![](supplement_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



```r
x <- dd %>% filter(meas == 'fracocc' & state == levels(dd$state)[1]) %>% pull('value')
fit.beta <- fitdist(x, distr = "beta", method = "mme")
summary(fit.beta)
```

```
## Fitting of the distribution ' beta ' by matching moments 
## Parameters : 
##         estimate
## shape1  5.577165
## shape2 26.058393
## Loglikelihood:  1278.214   AIC:  -2552.428   BIC:  -2542.655
```

```r
plot(fit.beta)
```

![](supplement_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
lapply(unique(dd$meas), function(m){dd %>% 
  merge(d.struct) %>%  
  filter(meas == m) %>%
  group_by(ID) %>%
  mutate(entropy = -sum(value*log(value, base = 2))) %>% 
  dplyr::select(ID, state, value, entropy) %>% 
  pivot_wider(names_from = state, values_from = value) %>% 
  ungroup() %>% dplyr::select(-ID) %>% 
  na.omit() %>% 
  ggplot(aes(x = .panel_x, y = .panel_y)) +
  geom_point(alpha = 0.2, shape = 16, size = 0.5, position = 'auto') + 
  geom_autodensity(alpha = 0.3, colour = NA, position = 'identity') + 
  facet_matrix(vars(everything(), -entropy),  layer.diag = 2, grid.y.diag = FALSE) +
  geom_point(x=.2, y=.2, color='red') +
  scale_color_gradient2(low = 'green', mid = 'blue', high = 'red', midpoint = 2.2)
})
```

```
## [[1]]
```

<img src="supplement_files/figure-html/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

```
## 
## [[2]]
```

<img src="supplement_files/figure-html/unnamed-chunk-5-2.png" style="display: block; margin: auto;" />
