#Simulating data for HMCMC in gender stereotyping project
#Code by: Dimitriy Leksanov
#Updated on 12/26/2023


##################
#SETUP
##################

#Remove everything from workspace 
rm(list=ls())
setwd("~")
base_wd <- getwd()
base_wd<-gsub("/Documents|/Dropbox|/PC","",base_wd)
# dataset <- "baseline_4000" # Placeholder
dataset <- "BottomlessA_4000" # Placeholder
analysis_wd <- ifelse(grepl("dominic", base_wd),"/Dropbox/whatisagoodlife/Analysis/Analysis_Dominic",ifelse(grepl("johl", base_wd),"/Dropbox/whatisagoodlife/Analysis/Analysis_Jeffrey",ifelse(grepl("dimitriyl|leksa", base_wd),"/Dropbox/whatisagoodlife/Analysis/Analysis_Dimitriy", "/Dropbox/whatisagoodlife/Analysis/Analysis_Tushar")))
# load the default packages
source(paste0(base_wd,"/Dropbox/whatisagoodlife/Analysis/Collaborative_Code/default_packages.R"))
packages <- default_packages
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))
install.packages("extraDistr")
library(extraDistr)

# load the default paths and functions
source(paste0(base_wd,"/Dropbox/whatisagoodlife/Analysis/Collaborative_Code/default_datasets_and_paths_new.R"))
source(paste0(base_wd,"/Dropbox/whatisagoodlife/Analysis/Collaborative_Code/default_all_functions.R"))

# Set sample size
N <- 1500

# Set everyone's marginal utilities
# Marginal utilities are between 0.1 and 4: following results in Tushar's presentation
M_math    <- runif(N, min = 0.1, max = 1)
M_english <- runif(N, min = 0.9, max = 1.8)
M_science <- runif(N, min = 1.5, max = 2.6)
M_social  <- runif(N, min = 2.3, max = 3.4)
M_art     <- runif(N, min = 3.1, max = 4)
sample_data <- as.data.frame(
  cbind(
    M_math,
    M_english,
    M_science,
    M_social,
    M_art
  )
)
sample_data %<>% mutate(
  m_math = log(M_math),
  m_english = log(M_english),
  m_science = log(M_science),
  m_social = log(M_social),
  m_art = log(M_art),
)

# For now, set lambda between -0.1 and 0.1
# To keep the effect small
sample_data$lambda <- runif(N, min = -0.1, max = 0.1)

# Set list of subject pairs
subject_pairs <- c(
  "math-english",
  "math-science",
  "math-social",
  "math-art",
  "english-science",
  "english-social",
  "english-art",
  "science-social",
  "science-art",
  "social-art"
)

# Create appropriate columns
for (i in c(1:10)) {
  subject_pair = str_split(subject_pairs[i], "-")
  index_left = sample(c(1, 2), N, replace = T)
  index_right = 3 - index_left
  subject_left = subject_pair[[1]][index_left]
  subject_right = subject_pair[[1]][index_right]
  increment_left = sample(c(1:8), N, replace = T)
  increment_right = sample(c(1:8), N, replace = T)
  epsilon = rnorm(N, mean=0, sd=1)
  
  sample_data[, paste0("subject_left_", i)] <- subject_left
  sample_data[, paste0("subject_right_", i)] <- subject_right
  sample_data[, paste0("increment_left_", i)] <- increment_left
  sample_data[, paste0("increment_right_", i)] <- increment_right
  sample_data[, paste0("epsilon_", i)] <- epsilon
}

# Now, create the 5 repeat pairs
# Might need to loop over all respondents
for (i in c(11:15)) {
  epsilon = rnorm(N, mean=0, sd=1)
  sample_data[, paste0("subject_left_", i)] <- NA
  sample_data[, paste0("subject_right_", i)] <- NA
  sample_data[, paste0("increment_left_", i)] <- NA
  sample_data[, paste0("increment_right_", i)] <- NA
  sample_data[, paste0("epsilon_", i)] <- epsilon
}
for (i in c(1:N)) {
  repeat_pairs = sample(c(1:10), 5, replace = F)
  for (j in c(11:15)) {
    repeat_pair = repeat_pairs[(j-10)]
    sample_data[i, paste0("subject_left_", j)] <- sample_data[i, paste0("subject_right_", repeat_pair)]
    sample_data[i, paste0("subject_right_", j)] <- sample_data[i, paste0("subject_left_", repeat_pair)]
    sample_data[i, paste0("increment_left_", j)] <- sample_data[i, paste0("increment_right_", repeat_pair)]
    sample_data[i, paste0("increment_right_", j)] <- sample_data[i, paste0("increment_left_", repeat_pair)]
  }
}

# Now, producing the actual results
for (j in c(1:15)) {
  sample_data[, paste0("probit_value_nolambda_", j)] <- NA
  sample_data[, paste0("probit_value_withlambda_", j)] <- NA
  sample_data[, paste0("probit_value_", j)] <- NA
  sample_data[, paste0("choose_left_", j)] <- NA
}
for (i in c(1:N)) {
  for (j in c(1:15)) {
    subject_left <- sample_data[i, paste0("subject_left_", j)]
    subject_right <- sample_data[i, paste0("subject_right_", j)]
    m_left <- sample_data[i, paste0("m_", subject_left)]
    m_right <- sample_data[i, paste0("m_", subject_right)]
    increment_left <- sample_data[i, paste0("increment_left_", j)]
    increment_right <- sample_data[i, paste0("increment_right_", j)]
    
    sample_data[i, paste0("probit_value_nolambda_", j)] <- m_left - m_right + log(increment_left / increment_right)
    sample_data[i, paste0("probit_value_withlambda_", j)] <- m_left - m_right + log(increment_left / increment_right) + sample_data[i, paste0("lambda")]
    sample_data[i, paste0("probit_value_", j)] <- m_left - m_right + log(increment_left / increment_right) + sample_data[i, paste0("lambda")] + sample_data[i, paste0("epsilon_", j)]
  }
}
sample_data %<>% mutate(
  choose_left_1 = ifelse(probit_value_1 > 0, 1, 0),
  choose_left_2 = ifelse(probit_value_2 > 0, 1, 0),
  choose_left_3 = ifelse(probit_value_3 > 0, 1, 0),
  choose_left_4 = ifelse(probit_value_4 > 0, 1, 0),
  choose_left_5 = ifelse(probit_value_5 > 0, 1, 0),
  choose_left_6 = ifelse(probit_value_6 > 0, 1, 0),
  choose_left_7 = ifelse(probit_value_7 > 0, 1, 0),
  choose_left_8 = ifelse(probit_value_8 > 0, 1, 0),
  choose_left_9 = ifelse(probit_value_9 > 0, 1, 0),
  choose_left_10 = ifelse(probit_value_10 > 0, 1, 0),
  choose_left_11 = ifelse(probit_value_11 > 0, 1, 0),
  choose_left_12 = ifelse(probit_value_12 > 0, 1, 0),
  choose_left_13 = ifelse(probit_value_13 > 0, 1, 0),
  choose_left_14 = ifelse(probit_value_14 > 0, 1, 0),
  choose_left_15 = ifelse(probit_value_15 > 0, 1, 0),
)
# Creating ID for each fake data respondent
sample_data$ID <- c(1:nrow(sample_data))

# Now, trimming and lengthening data to pass into hierarchical model
hierarchical_model_data <- sample_data %>%
  select(c(ID,
           starts_with("subject_left_"),
           starts_with("subject_right_"),
           starts_with("increment_left"),
           starts_with("increment_right_"),
           starts_with("choose_left_")))
hierarchical_model_data %<>% 
  mutate_all(as.character) %>%
  pivot_longer(cols = -ID) %>%
  separate(name, into = c("name", "number"), sep = "_(?=[0-9])") %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(increment_left = as.numeric(increment_left),
         increment_right = as.numeric(increment_right),
         choose_left = as.numeric(choose_left),
         ID = as.numeric(ID),
         number = as.numeric(number))
# Finishing touches
subjects_df <- as.data.frame(
  cbind(
    c("math", "english", "social", "science", "art"),
    c(1,2,3,4,5)
  )
)
colnames(subjects_df) <- c("subject", "subject_num")

hierarchical_model_data %<>%
  # select(-c("number")) %>%
  merge(subjects_df, by.x = "subject_left", by.y = "subject") %>%
  rename(subject_num_left = subject_num) %>%
  mutate(subject_num_left = as.numeric(subject_num_left)) %>% 
  merge(subjects_df, by.x = "subject_right", by.y = "subject") %>%
  rename(subject_num_right = subject_num) %>%
  mutate(subject_num_right = as.numeric(subject_num_right)) %>%
  arrange(ID, number)

# This makes the data convenient to work with in Stan, as we can define a 
# two-dimensional parameter array of marginal utilities
# where one dimension is fake respondents
# and the other dimension is the five subjects

# Now, actually compiling and running the model
library(rstan)
curr_directory <- paste0(base_wd, "/Dropbox/whatisagoodlife/Analysis/Analysis_Dimitriy/hebrewucollaboration/Code/")
# Check syntactic correctness before compiling
rstan:::rstudio_stanc(paste0(curr_directory, "HMCMC.stan"))
# Compile model
model <- stan_model(paste0(curr_directory, "HMCMC.stan"))
# How many cores?
parallel::detectCores()
options(mc.cores = 4)
fit <- sampling(model, data = list(
  np = nrow(sample_data),
  nt = nrow(hierarchical_model_data),
  ns = nrow(subjects_df),
  choice = hierarchical_model_data$choose_left,
  subjl = hierarchical_model_data$subject_num_left,
  subjr = hierarchical_model_data$subject_num_right,
  incl = hierarchical_model_data$increment_left,
  incr = hierarchical_model_data$increment_right,
  id = hierarchical_model_data$ID
),
iter = 400, chains = 1)

fit_summary <- summary(fit)
fit_summary_summary <- fit_summary$summary
