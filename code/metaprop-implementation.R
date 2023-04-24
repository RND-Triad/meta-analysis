
library(meta)
library(metafor)


# Reading CSV File
data_directory <- "./data/"
file_name <- "Meta Analysis Data Sheet - Within_Accuracy_Data.csv"
file_path <- paste(data_directory, file_name, sep = "")
dat <- read.csv(file_path, sep=",")
View(dat)

#
pi <- dat$classi_accuracy
ni <- dat$no_subjects
res <- metaprop(
  event = ni*pi,
  n = ni,
  data = dat,
  method = "GLMM",
  sm = "PLOGIT",
  method.bias = "Egger")
res

summary(res)
forest(res)
funnel(res)