install.packages(c("metafor","meta","dplyr","robumeta","devtools"))

library(metafor)
library(meta)
library(dplyr)
library(robumeta)


# Reading CSV File
file_path <-  "./data/within-subjects-data.csv"
dat <- read.csv(file_path, sep=",")
View(dat)


# Calculate The Freeman-Tukey Double Arcsine Transformation for Decimal Proportions 
# Calculate Effect Sizes
pi <- dat$classi_accuracy
ni <- dat$no_subjects
dat <- escalc(
  measure="PFT",
  xi = ni*pi,
  ni = ni, 
  data = dat,
  slab = paste(authors, year, sep=", "))
View(dat)


# Fit Random-Effects Meta-analysis Model
# tau^2,tau,I^2,H^2,Test for Heterogeneity, model results
sink(file="./results/console-random-effects-model.txt", split = TRUE)
res <- rma(yi,vi,data = dat)
res
sink()

# Calculation of Confidence Intervals
sink(file="./results/console-confidence-intervals.txt", split = TRUE)
conf <- confint(res)
conf


# Calculate Measures of Influence
sink(file="./results/console-measures-of-influence.txt", split = TRUE)
inf <- influence(res)
print(inf)
sink()

plot(inf)


# Forest Plot
forest(
  res,
  digits = c(2,1),
  cex = .8
  )
text(-.4, 12, "Author(s), Year", pos=2, cex= .8)
text( 3.1, 12, "Accuracy [95% CI]", pos=2, cex= .8)


# Publication Bias from Funnel Plot
metafor::funnel(res)


# Publication Bias from Egger's Regression Test
sink(file="./results/console-eggers-regression-test.txt", split = TRUE)
regtest(res)
sink()

# Moderator Analysis
res.modscnrrtr <- rma(yi, vi, mods = ~ scnnr_tr, data = dat)
res.modscnrrtr


#--------------------------------------------------------

#Alternative Way to Check Publication Bias
mes_ss <- robu(
  formula = yi ~ 1,
  data = dat, 
  studynum = study_id, 
  var.eff.size = vi,
  modelweights = "HIER", 
  small = TRUE
  )
sink(file="./results/console-heirarchical-effects-Model.txt", split = TRUE)
mes_ss
sink()

library(weightr)
wf <- weightfunct(dat$yi, dat$vi, table = TRUE)
sink(file="./results/console-weight-function-applied-model.txt", split = TRUE)
wf
sink()

library(devtools)

devtools::install_github("dsquintana/metameta")

library(metameta)

#Lower and Upper bounds Should be Added Manually for Now
yi <- dat$yi
df <- data.frame(
  yi
)
# write.csv(df, file = "data/ci.csv")

dat_ci <- read.csv("./data/ci.csv", sep = ",")
View(dat_ci)


power <- mapower_ul(
  dat = dat_ci,
  observed_es = 1.01,
  name = "Meta Analysis"
  )
power_dat <- power$dat
power_dat <- power_dat %>% select(1:8)
power_dat
power_med <- power$power_median_dat

list_power <- list(power_med)

fp <- firepower(list_power)
fp
