library(tidyverse) 
library(readxl)
library(magrittr)
library(dplyr)

set.seed(2201)
data(mpg, package = "ggplot2")
mpg_sub <- mpg %>%
  select(cty, displ, year, cyl, drv, fl, hwy, class) %>%
  mutate(as.factor(year)) %>%
  sample_frac(size = 0.5)
m_full <- log(cty) ~ displ + year + cyl + drv + fl + hwy + class
m_null <- log(cty) ~ 1

forward_auto <- lm(m_null, data = mpg_sub)
forward_auto <- step(forward_auto, scope = m_full, direction = "forward")
summary(forward_auto)

set.seed(2202)
data(iris)
iris_sub <- iris %>%
  group_by(Species) %>%
  sample_frac(size = 0.45) %>%
  ungroup()

lm1 <- lm(Sepal.Width ~ Species, data = iris_sub)
anova(lm1)

qf(1-0.05, 2,63)
