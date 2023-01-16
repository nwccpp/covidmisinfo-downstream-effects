covid-misinfo-ds-effects-regression
================
adl
2023-01-16

### Supplementary Code for: Covid Misinfo Downstream Effects

#### Authors: Erik Nisbet, PhD; Ayse D. Lokmanoglu, PhD;

- Install the packages and libraries needed.

``` r
#devtools::install_github("jacob-long/dpm")
# install.packages(c("haven", "tidyverse", "panelr", 
#                    "survival", "lavaan", "sJplot",
#                    "ggplot2", "dpm", "modelsummary",
#                    "ggeffects", "wesanderson", "ggthemes"))

library(haven)
library(tidyverse)        
library(panelr)           
library(survival)
library(lavaan)
library(sjPlot)
library(ggplot2)
library(dpm)
library(modelsummary)
library(ggeffects)
library(wesanderson)
library(ggthemes)
```

### Run Fixed-Effect Regression Models

- Load the data

``` r
load("~/Covid_Misinfo/COVID_Misinfo/Rda/Covid_Misinfo_Panel_Data_011123.Rda")
```

- Model Risk Behavior

``` r
model_rb_wbm <- wbm(Rbehavw ~ misinfow + accinfow + mediaw +
                      nmediaw |
                      male + reduc + rage + relimp + evan + 
                      hisp + white + ideo, 
                    data = test, 
                    model = "within",
                    interaction.style = "demean",
                    use.wave = TRUE,
                    wave.factor = TRUE)
```

- Model Public Health Trust

``` r
model_pht_wbm <- wbm(phtrustw ~ misinfow + accinfow + mediaw +
                       nmediaw | 
                       male + reduc + rage + relimp + evan + 
                       hisp + white + ideo, 
                     data = test, 
                     model = "within",
                     interaction.style = "demean",
                     use.wave = TRUE,
                     wave.factor = TRUE)
```

- Model Vaccination Attitude

``` r
model_vxt_wbm <- wbm(vaxattw ~ misinfow + accinfow + mediaw +
                       nmediaw |
                       male + reduc + rage + relimp + evan + 
                       hisp + white + ideo , 
                     data = test, 
                     model = "within",
                     interaction.style = "demean",
                     use.wave = TRUE,
                     wave.factor = TRUE)
```

- Model Social Distancing Policy

``` r
model_sdp_wbm <- wbm(sdpolw ~ misinfow + accinfow + mediaw +
                       nmediaw |
                       male + reduc + rage + relimp + evan + 
                       hisp + white + ideo , 
                     data = test, 
                     model = "within",
                     interaction.style = "demean",
                     use.wave = TRUE,
                     wave.factor = TRUE)
```

- Model Face Mask Wearing

``` r
model_Hb1_wbm <- wbm(HB1_w ~ misinfow + accinfow + mediaw + 
                       nmediaw |
                       male + reduc + rage + relimp + evan + 
                       hisp + white + ideo , 
                     data = test, 
                     model = "within",
                     interaction.style = "demean",
                     use.wave = TRUE,
                     wave.factor = TRUE)
```

- Model Likelihood of Vaccination

``` r
model_lkv_wbm <- wbm(lkvacw ~ misinfow + accinfow + mediaw +
                       nmediaw |
                       male + reduc + rage + relimp + evan + 
                       hisp + white + ideo , 
                     data = test, 
                     model = "within",
                     interaction.style = "demean",
                     use.wave = TRUE,
                     wave.factor = TRUE)
```

### Table and Plot the Regression Models

- List all the models and coefficient names

``` r
models_wbm_sig <- list('Public Health\nTrust' = model_pht_wbm,
                       'Risky Social\nBehaviors' = model_rb_wbm, 
                       'Face Mask\nWearing' = model_Hb1_wbm,
                       'Social Distancing\nPolicy Support' = model_sdp_wbm,
                       'Safety/Efficacy\nCOVID-19 Vaccines' = model_vxt_wbm,
                       'COVID-19 Vaccine \nAcceptance' = model_lkv_wbm)

coef_fe_tb<-c("misinfow" = "Misinformation\nEndorsement", 
           "accinfow" = "Accurate Information \nEndorsement",
           "mediaw" = "Social Media Use",
           "nmediaw" = "News Media Use")
```

- Create a regression table

``` r
modelsummary(models_wbm_sig,
             coef_omit = 'Interc',
             coef_map = coef_fe_tb,
             estimate  = c( "{estimate}{stars}"),
             statistic = c( 
                           "p = {p.value}"),
             output = "flextable")
```

- List coefficient names for regression plot

``` r
cef_fe_plot<-c("nmediaw" = "News Media Usage",
               "mediaw" = "Social Media Use",
               "accinfow" = "Accurate Information\nEndorsement",
               "misinfow" = "Misinformation\nEndorsement")
```

- Plot the Fixed-Effect Regression Models

``` r
modelplot(models_wbm_sig,
          coef_omit = 'Interc',
          coef_map = cef_fe_plot,
          color = "black") + 
  geom_vline(xintercept = 0, 
             linetype = 2,
             color = "grey") +
  ggtitle(" ")+
  facet_grid(~model, scales="free") +
  theme_wsj(color="white")+
  theme(text=element_text(size=14,family="sans"),
        title=element_text(size=8,family="sans"),
        axis.text.x=element_text(angle=60, hjust=1, family="sans"),
        axis.text.y=element_text(size=12, family="sans"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="sans"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="sans"),
        legend.position="none", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=10, family="sans")) 
```

![](covid-misinfo-downstream-effects-feregression_files/figure-gfm/plot-models-1.png)<!-- -->
