setwd('D:\\MDA\\Course\\Boot Camp\\SCMA 632\\Assignments\\A3')

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA","glue")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68.csv")
dim(data)

unique(data$Religion)

# Filtering for Bhr
bhr <- data %>%
  filter(state == "10")
# Display dataset info
cat("Dataset Information:\n")
print(names(bhr))
print(head(bhr))
print(dim(bhr))

# Finding missing values
missing_info <- colSums(is.na(bhr))
cat("Missing Values Information:\n")
print(missing_info)

# Sub-setting the data
bhrnew <- bhr %>%
  select(state_1,Religion, District, Region, Sector,emftt_q, emftt_v)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(bhrnew)))

dim(bhrnew)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
bhrnew$emftt_q <- impute_with_mean(bhrnew$emftt_q)
bhrnew$emftt_v <- impute_with_mean(bhrnew$emftt_v)

dim(bhrnew)

# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(bhrnew)))

bhr$Religion

bhrnew$emftt_v

bhr$Religion

unique(bhr$Religion)
str(bhr$Religion)

# Sub-setting the data
bhr_pr <- bhr %>%
  select(Religion, eggsno_q, fishprawn_q, goatmeat_q, beef_q, pork_q, chicken_q, othrbirds_q)

dim(bhr_pr)
bhr_pr$eggsno_q
data
names(bhr_pr)
str(bhr_pr)

# Fitting a probit regression to identify non-vegetarians. 

religion_mapping <- c("Hinduism", "Islam", "Christianity","Jainism","Others")
bhr_pr$Religion <- factor(bhr_pr$Religion, labels = religion_mapping)
table(bhr_pr$Religion)

columns <- c('eggsno_q','fishprawn_q', 'goatmeat_q', 'beef_q','pork_q', 'chicken_q', 'othrbirds_q')
data1 <- bhr[columns]
data1$target <- ifelse(data1$eggsno_q>0,1,0) 
probit_modet <- glm(target~., data = data1, family = binomial(link = "probit"))
summary(probit_modet)

# Performorming a Tobit regression analysis on "NSSO68.csv" 
df_bhr = data[data$state_1 == 'Bhr',]
vars <- c("state_1","Religion", "District", "Region", "Sector","emftt_q", "emftt_v")

df_bhr_p = df_bhr[vars]
names(df_bhr_p)

df_bhr_p$price = df_bhr_p$emftt_v / df_bhr_p$emftt_q
names(df_bhr_p)

summary(df_bhr_p)

head(table(df_bhr_p$emftt_q))

dim(df_bhr_p)

names(bhr)

#  dependent variable and independent variables
y <- bhr$foodtotal_v
X <- bhr[, c("sauce_jam_v", "Othrprocessed_v", "Beveragestotal_v", "fv_tot")]

#  data for Tobit regression
y_tobit <- pmin(pmax(y, 0), 1)  
X_tobit <- cbind(1, X) 

install.packages("censReg")
library(censReg)
# Fitting the Tobit model
X_tobit_df <- as.data.frame(X_tobit)
model <- censReg(y_tobit ~ ., data = X_tobit_df[, -1])

# Printing model summary
summary(model)
