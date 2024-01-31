###CE1
library(tidyverse) 
library(readxl)
library(magrittr)
library(dplyr)
pacman::p_load(tidyverse, magrittr)
getwd()
setwd("/path")
getwd()
CE1_data <- read_excel("excel.xlxs")
CE1_data
CE1_data[1,] #first row
CE1_data[,1] #first column
CE1_data[2,3] #row2,column3
CE1_data$name
table(CE1_data$gender)
CE1_data$gender[CE1_data$gender == "-"] <- NA
table(CE1_data$gender)
table(CE1_data$gender, useNA = "always")
CE1_data$gender <- fct_recode(CE1_data$gender, 
                              female = "F", 
                              female = "Female", 
                              male = "Male")
table(CE1_data$gender, useNA = "always")
CE1_data$weight <- as.numeric(CE1_data$weight)
summary(CE1_data)
mean(CE1_data$weight, na.rm = TRUE)
sd(CE1_data$weight, na.rm = TRUE)
median(CE1_data$weight, na.rm = TRUE)
ggplot(CE1_data, aes(weight)) + geom_histogram()
ggplot(CE1_data, aes(transport)) + geom_bar()

###CE2
data("mpg", package = "ggplot2") 
mpg
table(mpg$manufacturer)
table(mpg$model)
filter(mpg, manufacturer == "nissan")
filter(mpg, manufacturer == "nissan", year <= 2000)
filter(mpg, drv == "f", cyl == 6)
filter(mpg, manufacturer == "nissan" | manufacturer == "toyota")
select(mpg, model, trans) #subset
select(mpg, contains("dis"))
select(mpg, displ:cyl)
select(mpg, model:year)
mpg <- mutate(mpg, cty_kmpl = cty * 0.425144)#transform
mpg
mpg_trans2cty_km <- 
  mpg %>% 
  filter(manufacturer == "volkswagen") %>% 
  select(trans:cty) %>% 
  mutate(cty_kmpl = cty * 0.425144)
filter(mpg_trans2cty_km, cty_kmpl >=8.5)

mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean = mean(cty, na.rm = TRUE))
mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean = mean(cty, na.rm = TRUE), 
            n = n(), 
            sd = sd(cty, na.rm = TRUE))

get_lwr_uwr_ci <- function(x, level = 0.95){ 
  # calculate the mean 
  m <- mean(x, na.rm = TRUE) 
  # calculate the standard error 
  s <- sd(x, na.rm = TRUE) 
  n <- length(x) 
  se <- s/sqrt(n) 
  # calculate the critical value 
  a <- (1-level)/2 
  t <- qt(a, df = n-1, lower.tail = FALSE) 
  # calculate the lower bound 
  lwr <- m-t*se
  uwr <- m+t*se
  # return 
  return(c(lwr,uwr)) 
}
get_lwr_uwr_ci(mpg$cty)

###CE3
ggplot(mpg, aes(x = displ, y = cty)) + geom_point() 
#negative relationship #somewhat linear #moderately strong
ggplot(mpg, aes(x = displ, y = cty, col = drv)) + geom_point() + theme(legend.position = "bottom")
#drv = "f"  has the highest values of  cty 
ggplot(mpg, aes(x = displ, y = cty, col = class, shape = drv)) +  geom_point()
ggplot(mpg, aes(x = displ, y = cty)) +geom_point() + facet_grid(class ~ drv)
ggplot(mpg, aes(x = displ, y = cty)) + geom_point() + facet_wrap(~ class)

ggplot(mpg, aes(x = displ, y = cty)) + geom_point() +  geom_smooth(method = "lm")
ggplot(mpg, aes(x = displ, y = cty, col = drv)) + geom_point() + geom_smooth(method = "lm")
ggplot(mpg, aes(x = drv, y = cty)) + geom_boxplot()

###CE4
cricket <- read.csv("cricket.csv")
ggplot(cricket[cricket$player == "de villiers",], aes(x = runs)) +geom_histogram(color = "black", fill = "#007749") +ggtitle("Histogram of Runs for AB de Villiers")
ggplot(cricket[cricket$player == "kohli",], aes(x = runs)) + 
  geom_histogram(color = "black", fill = "#00bfff") + 
  ggtitle("Histogram of Runs for Virat Kohli")
cricket %>% 
  group_by(player) %>% 
  summarise(n = n(), 
            mean = mean(runs), 
            sd = sd(runs), 
            max = max(runs)) %>% 
  as.data.frame() # use this to get more than 1 decimal point where needed


###module6
# data
x <- c(-1,0,2,-2,5,6,8,11,12,-3)
y <- c(-5,-4,2,-7,6,9,13,21,20,-9)

# simple linear regression model
slr <- lm(y ~ x)

# model summary
summary(slr)

# 95% CIs for model parameters
confint(slr)

# predictions
x0 <- data.frame(x = 5)

# CI
predict(slr, newdata = x0, interval = "confidence", level = 0.95)

# PI
predict(slr, newdata = x0, interval = "prediction", level = 0.95)

###workshop6
cheddar <- read.csv("cheddar.csv")
cheddar_lm <- lm(taste ~ lactic, data = cheddar)
plot(cheddar_lm) 
#1. assumption of linearity #residuals vs fitted #Roughly random scatter about zero - no obvious non-linear trend.
#2. assumption of homoscedasticity #residuals vs fitted values plot #Approximately constant vertical spread.
#OR #Check the scale-location plot. #Constant vertical spread and a horizontal red line.

#3. assumption of normality. #normal QQ plot  #Points that lie along a straight line.


###module8 Multiple Regression Hypotheses and Model Selection
#Contrasts
data(chickwts)
summary(chickwts)
chickwts_lm <-lm(weight~feed, data=chickwts)
summary(chickwts_lm)
library(emmeans)
chickwts_em <- emmeans(chickwts_lm, "feed")
chickwts_em
chickwts_em_c1 <- contrast(chickwts_em, method = list("casein vs meatmeal"=c(1,0,0,-1,0,0)))
chickwts_em_c1 #p = 0.0456 < 0.05 reject H0 at 5%
# there is a difference in mean weight between chickens that receive casein and chickens that receive meatmeal

confint(chickwts_em_c1)
# Based the above confidence interval, we are 95% confident that the true mean weight of chickens that receive casein is between 0.948 and 92.4 units greater than the true mean weight of chickens that receive meatmeal.

chickwts_em_c11 <- contrast(chickwts_em, method = list("casein vs meatmeal"=c(-1,0,0,1,0,0)))
chickwts_em_c11

chickwts_em_c2 <- contrast(chickwts_em, method = list("other vs plant"=c(1/2,-1/4,-1/4,1/2,-1/4,-1/4)))
chickwts_em_c2 #https://myuni.adelaide.edu.au/courses/79142/pages/module-8-multiple-regression-hypotheses-and-model-selection?module_item_id=2882519
#Model Selection
#Forward Selection Algorithm with P-Values #pin=0.05
marks <- read.csv("marks.csv")
m0 <- lm(E ~ 1, data = marks)
scope <- E ~ OQ + A2 + A3 + A4 + A5 + A6
add1(m0, scope = scope, test = "F")

m1 <- update(m0, . ~ . + A6)
add1(m1, scope = scope, test = "F")

m2 <- update(m1, . ~ . + OQ)
add1(m2, scope = scope, test = "F")

m3 <- update(m2, . ~ . + A3)
add1(m3, scope = scope, test = "F")

summary(m3)
#Backward Elimination Algorithm with P-Values #pout=0.05
mb0 <- lm(E ~ ., data = marks)
drop1(mb0, test = "F")

mb1 <- update(mb0, . ~ . - A1)
drop1(mb1, test = "F")

mb2 <- update(mb1, . ~ . - A2)
drop1(mb2, test = "F")

mb3 <- update(mb2, . ~ . - A5)
drop1(mb3, test = "F")

mb4 <- update(mb3, . ~ . - A4)
drop1(mb4, test = "F")
summary(mb4)

#Stepwise Selection #pin = 0.15 #pout=0.05

ms0 <- lm(E ~ 1, data = marks)
scope <- E ~ OQ + A1 + A2 + A3 + A4 + A5 + A6
add1(ms0, scope = scope, test = "F") #look at F value

ms1 <- update(ms0, . ~ . + A6)
drop1(ms1, test = "F")
add1(m1, scope = scope, test = "F")

ms2 <- update(ms1, . ~ . + OQ)
drop1(ms2, test = "F")
add1(ms2, scope = scope, test = "F")


ms3 <- update(ms2, . ~ . + A3)
drop1(ms3, test = "F")
add1(ms3, scope = scope, test = "F")

ms4 <- update(ms4, . ~ . + A4)
drop1(ms4, test = "F")

summary(m3)
#Criteria for Model Selection

marks <- read.csv("marks.csv")
m0 <- lm(E ~ 1, data = marks)
scope <- E ~ OQ + A2 + A3 + A4 + A5 + A6
mAIC <- step(m0, scope = scope, direction = "both") 
summary(mAIC)

#workshop7
data(mpg, package = "ggplot2")
mpg


mpg %<>%
  mutate(trans =
           case_when(str_detect(trans, "auto") ~ "auto",
                     TRUE ~ "manual")) %>%
  mutate(trans = as.factor(trans))

table(mpg_trans2cty_km)

mpg_year <- mpg %>% mutate(year = as.factor(year))

mpg %>%
  select(cty, trans, drv, displ, year, cyl, hwy)

cty_lm <- lm(cty ~ trans + drv + displ + year
             + cyl + hwy, data = mpg)
summary(cty_lm)
#1 beta coefficient for the intercept
#category: trans(2), drv(3), cyl(2)
#quantitative: displ, year, hwy
#1+3+(2-1)+(3-1)+(2-1)=8 coefficient
#drvf        -0.73782    


#predict
new_car <- data.frame(trans = "auto",
                      drv = "r",
                      cyl = 4,
                      displ = 3.6,
                      hwy = 17.2,
                      year = 2008)
predict(cty_lm, newdata = new_car)
predict(cty_lm, newdata = new_car,
        interval = "confidence", level = 0.9)
predict(cty_lm, newdata = new_car,
        interval = "predict", level = 0.9)

###module9
#Polynomial Regression
x <- c(50,50,50,70,70,70,80,80,80,90,90,90,100,100,100)
y <- c(3.3,2.8,2.9,2.3,2.6,2.1,2.5,2.9,2.4,3,3.1,2.8,3.3,3.5,3)
m1 <- lm(y ~ x) #linear regression
summary(m1)
 vm2 <- lm(y ~ x + I(x^2)) # polynomial term 
summary(m2)

anova(m1, m2)#Should the quadratic term   be included in the final model?
#H0: beta2 = 0 against Ha beta2 =/= 0
#reject H0, conclude that we have evidence to include the quadratic term  in the model.

y <- c(66,58,65,-31,39,17,7,-35,43,-5,43,-26,49,-40,-22)
x1 <- c(-1,-1,0,0,1,1,0,0,-1,-1,0,0,1,1,0)
x2 <- c(-1,0,-1,0,-1,0,1,0,1,0,-1,0,1,0,1)
x3 <- c(0,-1,-1,0,0,-1,-1,0,0,1,1,0,0,1,1)
m0 <- lm(y ~ x1 + x2 + x3 + I(x1^2) + I(x2^2) + I(x3^2))
summary(m0)

m1 <- lm(y ~ x1 + x2 + x3 + I(x1^2) + I(x2^2))
summary(m1)

#Variable Transformations
x <- 1:15
y <- c(355,211,197,166,142,106,104,60,56,38,36,32,21,19,15)
m1 <- lm(y~x)
summary(m1)
plot(m1)


df %>%
  group_by(layer) %>%
  summarise(mean = mean(x),
            sd = sd(x)) %>%
  select(x2)
m2 <-lm(log(y)~x)
summary(m2)
#5.973160-0.218425x
#exp(5.973160)
#392.7448e^(0.218425x)
plot(m2) #Exponential Regression


x <- c(3.87,3.61,4.33,3.43,3.81,3.83,3.46,3.76,3.50,3.58,4.19,3.78,3.71,3.73,3.78)
y <- c(4.87,3.93,6.46,3.33,4.38,4.70,3.50,4.50,3.58,3.64,5.90,4.43,4.38,4.42,4.25)
gators <- lm(y ~ x)
summary(gators) #Power Regression

-8.4761 + 3.4311 x

exp(-8.4761)
0.0002083898l^ 3.4311

x0 <- data.frame(x = 4)
predict(gators, newdata = x0, interval = "prediction", level = .9)
exp(predict(gators, newdata = x0, interval = "prediction", level = .9))

###workshop8
data(trees)
summary(trees)
trees %<>%
  mutate(Radius = Girth/2) %>%
  select(Volume, Height, Radius)
head(trees)
trees_lm1 <- lm(Volume ~ Height + Radius,
                data = trees)
summary(trees_lm1)

par(mfrow=c(2,2))
plot(trees_lm1)
par(mfrow=c(1,1))


plot(trees$Height, residuals(trees_lm1), pch = 20,
     cex = 1.5, xlab = "Height", ylab = "Residual")
lines(lowess(residuals(trees_lm1) ~ trees$Height),
      col = "#ff5f00", lwd = 2)

plot(trees$Height, residuals(trees_lm1), pch = 20,
     cex = 1.5, xlab = "Radius", ylab = "Residual")
lines(lowess(residuals(trees_lm1) ~ trees$Height),
      col = "#ff5f00", lwd = 2)
par(mfrow=c(1,2)
    
    
trees_lm2 <- lm(log(Volume) ~ log(Height)
                    + log(Radius), data = trees)
summary(trees_lm2)
par(mfrow=c(2,2))
plot(trees_lm2)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
plot(log(trees$Height), residuals(trees_lm2), pch = 20,
     cex = 1.5, xlab = "log(Height)", ylab = "Residual")
lines(lowess(residuals(trees_lm2) ~ log(trees$Height)),
      col = "#ff5f00", lwd = 2)
plot(log(trees$Radius), residuals(trees_lm2), pch = 20,
     cex = 1.5, xlab = "log(Radius)", ylab = "Residual")
lines(lowess(residuals(trees_lm2) ~ log(trees$Radius)),
      col = "#007fff", lwd = 2)
par(mfrow=c(1,1))

###CE5
library(ggplot2)
mpg %>%
  ggplot(aes(x = displ, y = cty)) +
  geom_point()
m1 <- lm(cty ~ displ, data = mpg) #simple linear regression
summary(m1)
#reject. ; there is evidence of a significant linear relationship between cty and displ .
summary(m1)$coefficients
summary(m1)$coefficients[2,1]
library(broom)
tidy(m1)
glance(m1)

par(mfrow = c(2,2))
plot(m1)

plot(m1, which = 2)

residuals(m1)
fitted(m1)


predict(m1)[1:5]

new_cars <- data.frame(displ = 3:6)
new_cars

new_cars$pred <- predict(m1, newdata = new_cars)
new_cars


m2 <- lm(cty ~ drv, data = mpg)
summary(m2)
anova(m2) 
glance(m2)

m3 <- lm(cty ~ displ + drv, data = mpg)
summary(m3)
anova(m3) 
glance(m3)

m4 <- lm(cty ~ displ*drv, data = mpg)
summary(m4)
anova(m4)
glance(m4)

m5 <- lm(cty ~ displ:drv, data = mpg)
summary(m5)
anova(m5) 
glance(m5)

m6 <- lm(cty ~ displ * drv + cyl, data = mpg)
summary(m6)
anova(m6)
glance(m6)

m7 <- lm(cty ~ displ * drv * cyl, data = mpg)
summary(m7)
anova(m7)
glance(m7)

m8 <- lm(cty ~ (displ + drv + cyl)^2, data = mpg)
summary(m8) 
anova(m8)
glance(m8)

m9 <- lm(cty ~ displ * (drv + cyl), data = mpg)
summary(m9) 
anova(m9)

#model selection
m_full <- log(cty) ~ (displ + drv + cyl)^2
m_null <- log(cty) ~ 1
#Backward Elimination
back_lm <- lm(m_full, data = mpg)
drop1(back_lm, test = "F")
back_lm <- update(back_lm, . ~ . - drv:cyl)
drop1(back_lm, test = "F")
back_lm <- update(back_lm, . ~ . - displ:drv)
summary(back_lm)

#Forward Selection
forward_lm <- lm(m_null, data = mpg)
add1(forward_lm, scope = m_full, test ="F")
forward_lm <- update(forward_lm, . ~ . + cyl)
add1(forward_lm, scope = m_full, test ="F")
forward_lm <- update(forward_lm, . ~ . + drv)
add1(forward_lm, scope = m_full, test ="F")
forward_lm <- update(forward_lm, . ~ . + displ)
add1(forward_lm, scope = m_full, test ="F")
forward_lm <- update(forward_lm, . ~ . + displ:cyl)
add1(forward_lm, scope = m_full, test ="F")
summary(forward_lm)

#Stepwise Selection
step_lm <- lm(m_null, data = mpg)
add1(step_lm, scope = m_full, test = "F")
step_lm <- update(step_lm, . ~ . + cyl)
drop1(step_lm, test = "F")
add1(step_lm, scope = m_full, test = "F")
step_lm <- update(step_lm, . ~ . + drv)
drop1(step_lm, test = "F")
add1(step_lm, scope = m_full, test = "F")
step_lm <- update(step_lm, . ~ . + displ)
drop1(step_lm, test = "F")
add1(step_lm, scope = m_full, test = "F")
step_lm <- update(step_lm, . ~ . + displ:cyl)
drop1(step_lm, test = "F")
add1(step_lm, scope = m_full, test = "F")
summary(step_lm)
back_auto <- lm(m_full, data = mpg)
back_auto <- step(back_auto, direction = "backward")

summary(back_auto)

forward_auto <- lm(m_null, data = mpg)
forward_auto <- step(forward_auto, scope = m_full, direction = "forward")
summary(forward_auto)

step_auto <- lm(m_null, data = mpg)
step_auto <- step(step_auto, scope = m_full, direction = "both")
summary(step_auto)

#module10 ANOVA
#https://myuni.adelaide.edu.au/courses/79142/pages/module-10-regressio

#One-way ANOVA with Multiple Linear Regression
pet_data <- data.frame(owner = c("A","B","C","D","E"),
                       pet = c("rabbit","cat","cat","dog","dog"),
                       happiness = c(10,7,9,6,2))
pet_data$pet <- factor(pet_data$pet)
library(modelr)
model_matrix(pet_data, happiness ~ pet)

library(tidyverse, magrittr)
data(mpg, package = "ggplot2")
mpg %<>%
  mutate(across(where(is.character),as.factor)) %>%
  mutate(trans = case_when(str_detect(trans, "auto") ~ "auto",
                           TRUE ~ "manual")) %>%
  mutate(trans = as.factor(trans)) %>%
  select(cty, trans, drv)

mpg_add <- lm(cty ~ drv + trans, data = mpg)
mpg_inter <- lm(cty ~ drv * trans, data = mpg)
anova(mpg_inter)
anova(mpg_add)
anova(mpg_inter, mpg_add)
summary(mpg_add)
#Two-way ANOVA (with Replication) with Multiple Linear Regression
new_car <- data.frame(trans = "auto",
                      drv = "f")
predict(mpg_add, newdata = new_car)

# M: separate regressions
# H1: parallel regressions
# H0: identical regressions

library(tidyverse, magrittr, modelr)
data(mpg, package = "ggplot2")
mpg %>%
  model_matrix(hwy ~ displ) #identical

mpg %<>%
  mutate(across(where(is.character),as.factor)) %>%
  mutate(trans = case_when(str_detect(trans, "auto") ~ "auto",
                           TRUE ~ "manual")) %>%
  mutate(trans = as.factor(trans))
mpg %>%
  model_matrix(hwy ~ displ + trans) #parallel


mpg %>%
  model_matrix(hwy ~ displ * trans)#separate


mpg_sep <- lm(hwy ~ displ * trans, data = mpg)
summary(mpg_sep)

mpg_par <- lm(hwy ~ displ + trans, data = mpg)
summary(mpg_par)


mpg_ident <- lm(hwy ~ displ, data = mpg)
summary(mpg_ident)

anova(mpg_ident, mpg_par, mpg_sep)


AIC(mpg_ident, mpg_par, mpg_sep) #parallel
BIC(mpg_ident, mpg_par, mpg_sep) #identical


#workshop9
titanic <- read.csv("titanic.csv", header = TRUE)
class_tab <- table(titanic$pclass,
                   titanic$survived)
class_tab

chisq.test(class_tab)

#Produce a crosstabulation of sex vs survived.
sex_tab <- table(titanic$sex, titanic$survived)
sex_tab
chisq.test #Reject H0 and conclude that there is an association between sex andsurvived.


#workshop10

data(penguins, package = "palmerpenguins")
penguins %<>% na.omit()
penguins %>%
  count(species)
#histogram
ggplot(penguins, aes(body_mass_g)) + geom_histogram() 
#boxplot
ggplot(penguins, aes(x = species, y = body_mass_g,)) + geom_boxplot()

summary(penguins)

#one way ANOVA for penguins
penguins_lm1 <- lm(body_mass_g ~ species,
                   data = penguins)
summary(penguins_lm1)
anova(penguins_lm1)
#Adelie     
#Chinstrap   #Gentoo   

penguins %>%
  count(species,sex)
ggplot(penguins, aes(x = sex, y = body_mass_g)) + geom_boxplot()


#two way ANOVA
ggplot(penguins, aes(x = species, y = body_mass_g,  col = sex)) + geom_boxplot()

penguins_lm2 <- lm(body_mass_g ~ species * sex,
                   data = penguins)
summary(penguins_lm2)
anova(penguins_lm2)

penguins_lm3 <- lm(body_mass_g ~ species * sex
                   * flipper_length_mm,
                   data = penguins)
summary(penguins_lm3)
anova(penguins_lm3)
penguin_BIC <- step(penguins_lm3,
                    direction = "backward",
                    k = log(nrow(penguins)))
summary(penguin_BIC)
anova(penguin_BIC,penguins_lm3)


#CE6
mpg %>% 
  ggplot(aes(x = displ, y = cty)) + 
  geom_point()

mpg %>% 
  ggplot(aes(y = cty, x = drv)) + 
  geom_boxplot()


mpg %>% 
  ggplot(aes(x = displ, y = cty, colour = drv)) + 
  geom_point()


#ANOVA
lm1 <- lm(cty ~ drv, data = mpg) 
anova(lm1)
summary(lm1)
lm2 <- lm(cty ~ displ, data = mpg) #Identical
summary(lm2)
lm3 <- lm(cty~ displ + drv, data = mpg) #Parallel
summary(lm3)



#4: cty = 23.6796 - 2.3385 displ
#f: cty = 25.9548 - 2.3385 displ
#r: cty = 26.1841 - 2.3385 displ
lm4 <- lm(cty ~ displ * drv, data = mpg)
summary(lm4)

#model selection
AIC(lm2,lm3,lm4)
BIC(lm2,lm3,lm4)
new_car <- data.frame(displ = 4.7, drv = "4")
predict(lm4, newdata = new_car)

#AIC for the separate regressions model is 1051.857.
#BIC for the separate regressions model is 1076.045.
#The predicted value of cty is 12.8797

df %>%
  group_by(layer) %>%
  summarise(mean = mean(x1),
            sd = sd(x1)) %>%
  select(x2)

