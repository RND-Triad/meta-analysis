# Reading CSV File
data_directory <- "./data/"
file_name <- "Meta Analysis Data Sheet - Within_Accuracy_Data.csv"
file_path <- paste(data_directory, file_name, sep = "")
dat <- read.csv(file_path, sep=",")
View(dat)

log_reg_model <- glm(dat$classi_accuracy ~ dat$no_subjects, data = dat, family = binomial("logit"))
summary(log_reg_model)
