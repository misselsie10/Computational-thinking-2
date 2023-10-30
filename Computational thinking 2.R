first_last_chr <- function(s) {
  first_chr <- substr(s, 1, 1)
  last_chr <- substr(s, nchar(s), nchar(s))
  result <- paste(first_chr, last_chr, sep = "")
  return(result)
}

#we have defined a function bc it is now in the functions box

text <- "Amazing!"
first_last_chr(text)

#Q: What are the 4 parts?

#parameter = (s) because it falls within the function parenthesis
#keywords = function or argument
#body is everything in thefour lines of text
#name is first_last_chr

#I can still use s <- 10 and that is fine
s <- 10
#(s has no meaning besides what I put in here)
#arguments are actual thing

#substr doesn't change value of s

#use option or alt to move a line up or down
first_last_chr(text)
#[1] "A!"

#parameters are hypothetical and take on the values of the argument

first_last_chr("Money")
#output to be My

#we don't have to define the
#we called first and last character on that variable

foo <- 1:10
sum(foo)
#[1] 55

library(tidyverse)
df <- tibble(a = 1:3)
df
# # A tibble: 3 × 1
# a
# <int>
#   1     1
# 2     2
# 3     3

#command shift c

filter(mutate(df, a2 = a * 2), a2 < 5)
# # A tibble: 2 × 2
# a    a2
# <int> <dbl>
#   1     1     2
# 2     2     4

mean <- function(x) {
  result <- sum(x) / length(x)
  return(result)
}

mean(c(1, 2))
#[1] 1.5

mean(c(1, 2, NA))
#missing in and missing out
#[1] NA

#is.na 
#[] is for subsetting
#! is the not operator

foo <- c(1, 2, NA)
#what function does not call NA
is.na(foo)
#[1] FALSE FALSE  TRUE

#inverse is the NA
foo[is.na(foo)]

#subset of foo that is not NA
foo[!is.na(foo)]

mean <- function(x, na.rm) {
  if (na.rm) {
    x <- x[is.na(x)] #what tells where the NA's are (or aren't)
  }
  result <- sum(x) / length(x)
  return(result)
}


mean <- function(x, na.rm = FALSE) {
  if (na.rm == TRUE) {
    x <- x[is.na(x)] #what tells where the NA's are (or aren't)
  }
  result <- sum(x) / length(x)
  return(result)
}

mean(c(1, 2, NA), TRUE) #should be 1.5


na.omit(foo)
foo[c(1,2)]

foo <- c(3, 4, NA)
foo[na.omit(foo)]

foo[!NA]
#gives values where index is true

foo <- c("a", "a", "b")

foo["a"]

foo[foo == "a"]


mean(c(1, 2, NA))

repeat_chr <- function(s, n, separator = "_"){
  repeated <- rep(s, n)
  result <- paste(repeated, collapse = separator)
  return(result)
}

repeat_chr("A", 3, ":")

#to make separator argument to be colon

repeat_chr("A", separator = ":")

?rep


# keyword function
# name
# parameters
# body

#ASSESSMENT

#dir.create() creates directories

#file.create() creates files
# dir.create("computational")
# file.create("foo/bar.md")
# writeLines("The foo directory is pointless, except for demonstration", 
#            "foo/bar.md")


dir.create("R")
dir.create("data")
dir.create("figs")
dir.create("output")
dir.create("paper")
dir.create("docs")
dir.create("R/")


file.create("R/README.md")

writeLines("data file", 
           "Computational thinking 2/data.md")

#assessment

#going to analyze survey sata of young-of-the-year Black
#Oystercatchers in Santa Cruz to figure out where chicks hatched

library(tidyverse)

# Generate sample data
# Sightings of Black Oystercatcher chicks at Santa Cruz beaches
beaches <- c("Cowell's", "Steamer Lane", "Natural Bridges", "Mitchell's", "Main")
# blue, green, black, white, yellow
band_colors <- c("B", "G", "K", "W", "Y") 
# Surveys took place weekly in the summer of 2023
surveys <- seq(as.Date("2023-06-01"), as.Date("2023-08-31"), by = 7)

# Setting the "seed" forces randomized functions (like sample()) to generate
# the same output
set.seed(1538)
# 3 band colors identify a bird. We want 12 birds.
birds <- paste0(
  sample(band_colors, 25, replace = TRUE),
  sample(band_colors, 25, replace = TRUE),
  sample(band_colors, 25, replace = TRUE)
) %>% 
  unique() %>%
  head(12)
bloy_chicks <- tibble(
  # Randomly generate survey data
  beach = sample(beaches, size = 100, replace = TRUE),
  bird = sample(birds, size = 100, replace = TRUE),
  survey = sample(surveys, size = 100, replace = TRUE)
) %>% 
  # Remove duplicates (see ?distinct)
  distinct() %>% 
  # Sort by survey date and location
  arrange(survey, beach)

#Q1 We’re randomly generating data, but we’re all going to end up with the same data frames. How is that happening?
#Answer: the random seed is just that - random and different every time
#we run the set seed code

#Q2 Explain in plain language what this part does. Your answer should be one or two sentences
#Answer: we are assigning "birds" to give us the information (three band colors per bird)
#of 12 random birds

birds <- paste0(
  sample(band_colors, 25, replace = TRUE),
  sample(band_colors, 25, replace = TRUE),
  sample(band_colors, 25, replace = TRUE)
) %>%
  unique() %>%
  head(12)

#Q3 We generated 100 random survey observations. How many rows are in bloy_chicks? Why the difference?
#Answer: 95 rows are in bloy_chicks
nrow(bloy_chicks)

#going to estimate where chicks hatched using tidyverse

# 1. For each bird, where was it seen most often?
# 2. If multiple sites are tied, choose the one with the earliest observation
# 3. If still tied, randomly choose one

  #Find most frequent beach per bird
group_by(bird) %>%
  beach_freq <- bloy_chicks %>%
  count(bird, beach) %>%
  ungroup()
filter(n == max(n)) %>%
  # Find first date for each bird+beach
  summarize(earliest = min(survey),
            beach_early <- bloy_chicks %>%
              .groups = "drop")
group_by(bird, beach) %>%
  # Join the two conditions and retain most frequent beach, only earliest
  filter(earliest == min(earliest)) %>%
  ungroup()
sample_n(1) %>% # Randomly choose 1 row. See ?sample_n
  left_join(beach_early, by = c("bird", "beach")) %>%
  group_by(bird) %>%
  hatch_beach <- beach_freq %>%

#Q4 Sort the pipelines back into correct order.

# Put the logic for estimating the hatching beach in a single function.
# Group the data by bird
# Summarize each group using your custom function

find_hatching_beach <- function(site, date) {
  # Start with a data frame (or tibble) of site and date for *one* bird
  # Use pipes and dplyr functions to find the hatching beach
  bird_observations <- tibble(site, date)
  result <- bird_observations %>% 
    birds # use as many pipes and dplyr functions as necessary
  # result should end up as a data frame with one row for the hatching beach
  return(result$site) # return the hatching beach
}

# split-apply-combine
bloy_chicks %>% 
  group_by(birds) %>% 
  summarize(birds)

#Q5 The two parameters of find_hatching_beach() are named site and date.
#When this function is called, what columns in bloy_chicks will you use as arguments for these parameters?
head(bloy_chicks)
#I can use "beach", "bird", and "survey"

#Q6 What will be the value of site when find_hatching_beach() is called
#on the group for bird YWG? How about WYB?
band_colors
#The value of site for WYG will be a bird with white, yellow, green
#The value of WYB will be a bird with white, yellow, blue

