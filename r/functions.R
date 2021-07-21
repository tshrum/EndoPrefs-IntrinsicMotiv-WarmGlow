# Functions for this project

likelys <- function(x) {
  x <- ifelse(x == "Extremely likely", 7, x)
  x <- ifelse(x == "Moderately likely", 6, x)
  x <- ifelse(x == "Slightly likely", 5, x)
  x <- ifelse(x == "Neither likely nor unlikely", 4, x)
  x <- ifelse(x == "Slightly unlikely", 3, x)
  x <- ifelse(x == "Moderately unlikely", 2, x)
  x <- ifelse(x == "Extremely unlikely", 1, x)
  x <- ifelse(x == "I don't know", 0, x)
  x <- as.numeric(x)
}

likelys2 <- function(x) {
  x <- ifelse(x == "Extremely likely", 7, x)
  x <- ifelse(x == "Very likely", 6, x)
  x <- ifelse(x == "Likely", 5, x)
  x <- ifelse(x == "Neither likely nor unlikely", 4, x)
  x <- ifelse(x == "Unlikely", 3, x)
  x <- ifelse(x == "Very unlikely", 2, x)
  x <- ifelse(x == "Extremely unlikely", 1, x)
  x <- ifelse(x == "I don't know", 0, x)
  x <- ifelse(x == "I  don’t  know", 0, x)
  x <- ifelse(substr(x, 1, 3) == "Not", 0, x)  # For "Not applicable" answers
  x <- as.numeric(x)
}

sometimes <- function(x) {
  x <- ifelse(x == "Always", 5, x)
  x <- ifelse(x == "Most of the time", 4, x)
  x <- ifelse(x == "Sometimes", 3, x)
  x <- ifelse(x == "Rarely", 2, x)
  x <- ifelse(x == "Never", 1, x)
  x <- ifelse(x == "I don't know", 0, x)
  x <- ifelse(substr(x, 1, 3) == "Not", 0, x)  # For "Not applicable" answers
  x <- as.numeric(x)
}

true <- function(x) {
  x <- ifelse(x == "True", 1, x)
  x <- ifelse(x == "False", 0, x)
  x <- ifelse(x == "I don't know", 0, x)
  x <- ifelse(grepl("I have", x), 0, x)
  x <- ifelse(substr(x, 1, 3) == "Not", 0, x)
  x <- as.numeric(x)
}

onetosix <- function(x) {
  x <- ifelse(x == "6  (A great amount)", 6, x)
  x <- ifelse(x == "1 (Not at all)" | x == "1 (not at all)", 1, x)
  x <- ifelse(x == "", NA, x)
  x <- as.numeric(x)
}

onetoten <- function(x) {
  x <- ifelse(x == "10 (Very easily)" | x == "10 (Extremely concerned )" | x == "10 (Extremely likely)" | x == "10 (Extremely Confident)", 10, x)
  x <- ifelse(x == "1 (Not at all easily)" | x == "1 (Not at all concerned)" | x == "1 (Not at all likely)" | x == "1 (Not at all confident)", 1, x)
  x <- ifelse(x == "" | x == "N/A, I do not have or intend to have children", 0, x)
  x <- as.numeric(x)
}

agree <- function(x) {
  x <- ifelse(x == "Strongly agree", 7, x)
  x <- ifelse(x == "Agree", 6, x)
  x <- ifelse(x == "Somewhat agree", 5, x)
  x <- ifelse(x == "Somewhat disagree", 3, x)
  x <- ifelse(x == "Disagree", 2, x)
  x <- ifelse(x == "Strongly disagree", 1, x)
  x <- ifelse(x == "Neither agree nor disagree", 4, x)
  x <- ifelse(substr(x, 1, 3) == "N/A", 0, x)  # For "Not applicable" answers
  x <- as.numeric(x)
}

reverse_agree <- function(x) {
  x <- ifelse(x == "Strongly agree", 1, x)
  x <- ifelse(x == "Agree", 2, x)
  x <- ifelse(x == "Somewhat agree", 3, x)
  x <- ifelse(x == "Somewhat disagree", 5, x)
  x <- ifelse(x == "Disagree", 6, x)
  x <- ifelse(x == "Strongly disagree", 7, x)
  x <- ifelse(x == "Neither agree nor disagree", 4, x)
  x <- ifelse(substr(x, 1, 3) == "N/A", 0, x)  # For "Not applicable" answers
  x <- as.numeric(x)
}

actions <- function(x, y) {
  x <- ifelse(x == 0, NA, x)  # replacing 0's with NA's
  x <- scale(x)  # scaling x
  x1 <- ifelse(is.na(x), 0, x)  # changing NA's back to 0's so individuals with NA's can still have an index
  z <- x1 + y
  z
}


numtimes <- function(x) {
  x <- ifelse(x == "Daily", 5, x)
  x <- ifelse(x == "4-6 times", 4, x)
  x <- ifelse(x == "2-3 times", 3, x)
  x <- ifelse(x == "Once", 2, x)
  x <- ifelse(x == "Never", 1, x)
  x <- ifelse(substr(x, 1, 3) == "N/A", 0, x)  # For "Not applicable" answers
  x <- as.numeric(x)
}

times <- function(x) {
  x <- ifelse(x == "Always", 5, x)
  x <- ifelse(x == "Most of the time", 4, x)
  x <- ifelse(x == "About half the time", 3, x)
  x <- ifelse(x == "Less than half the time" | x == "Sometimes", 2, x)
  x <- ifelse(x == "Never", 1, x)
  x <- ifelse(substr(x, 1, 3) == "N/A", 0, x)  # For "Not applicable" answers
  x <- as.numeric(x)
}

#for negative PEBS
reverse_times <- function(x) {
  x <- ifelse(x == "Always", 1, x)
  x <- ifelse(x == "Most of the time", 2, x)
  x <- ifelse(x == "About half the time", 3, x)
  x <- ifelse(x == "Less than half the time" | x == "Sometimes", 4, x)
  x <- ifelse(x == "Never", 5, x)
  x <- as.numeric(x)
}



likely5 <- function(x) {
  x <- ifelse(x == "Extremely likely", 5, x)
  x <- ifelse(x == "Somewhat likely", 4, x)
  x <- ifelse(x == "Neither likely nor unlikely", 3, x)
  x <- ifelse(x == "Somewhat unlikely", 2, x)
  x <- ifelse(x == "Extremely unlikely", 1, x)
  x <- ifelse(substr(x, 1, 3) == "N/A", 0, x)  # For "Not applicable" answers
  x <- as.numeric(x)
}

#for negative PEBS
reverse_likely <- function(x) {
  x <- ifelse(x == "Extremely likely", 1, x)
  x <- ifelse(x == "Somewhat likely", 2, x)
  x <- ifelse(x == "Neither likely nor unlikely", 3, x)
  x <- ifelse(x == "Somewhat unlikely", 4, x)
  x <- ifelse(x == "Extremely unlikely", 5, x)
  x <- as.numeric(x)
}

serious <- function(x) {
  x <- ifelse(x == "Very serious", 4, x)
  x <- ifelse(x == "Somewhat serious", 3, x)
  x <- ifelse(x == "Not too serious", 2, x)
  x <- ifelse(x == "Not a problem", 1, x)
  x <- ifelse(x == "I don't know", 0, x)
  x <- as.numeric(x)
}

when <- function(x) {
  x <- ifelse(x == "Now", 4, x)
  x <- ifelse(x == "In the next few years", 3, x)
  x <- ifelse(x == "Not for many years", 2, x)
  x <- ifelse(x == "Never", 1, x)
  x <- ifelse(x == "I don't know", 0, x)
  x <- as.numeric(x)
}

concern <- function(x) {
  x <- ifelse(x == "Very concerned", 4, x)
  x <- ifelse(x == "Somewhat concerned", 3, x)
  x <- ifelse(x == "Not too concerned", 2, x)
  x <- ifelse(x == "Not at all concerned", 1, x)
  x <- ifelse(x == "I don't know", 0, x)
  x <- as.numeric(x)
}

select <- dplyr::select  # MASS package also uses a select function, this makes it so select is always dplyr function
coalesce <- dplyr::coalesce  #BBmisc package also uses

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

options(show.signif.stars = TRUE)
