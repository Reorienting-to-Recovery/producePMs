library(tidyverse)
library(producePMs)

### 15 ###
# Non-salmon-oriented recreation -----------------------------------------------
# Total weeks flooded for each year - take mean of annual totals, report mean and range of annual totals


### 16.1 ###
### Pull From Trend Report ###



### 16.2 ###
### Proportion Unimparied ###
#TODO review baseline proportion unimpared


### 17 ###
# Flood frequency and stage for each watershed ---------------------------------
# Mark/Erin develop simple calculation based on change in peak flow from one month to next.


### 18 ###
# Hydropower generation ability ------------------------------------------------
# TODO pull snow globe feather volumne to cost relationship in
# TODO pull storage nodes from calsim - ask Ashley
# Mark/Erin develop simple calculation based on storage (change in storage, change in annual volumne)
# Use work from SnowGlobe
#

lost_gen <- tibble(volume_af = c(49000,44000,42000,74000,
                                 23000,7000,10000,120000,137000,
                                 21000,18000,12000,70000,83000),
                   lost_gen = c(106656,91038,99412,165255,
                                48634,16578,22076,250968,316259,
                                42377,37589,24761,150909,193031))
ggplot(lost_gen, aes(x = volume_af, y = lost_gen)) +
  geom_point()

power_mod <- lm(lost_gen ~ volume_af, data = lost_gen)
summary(power_mod)
predict(power_mod, tibble(volume_af = 5000))
