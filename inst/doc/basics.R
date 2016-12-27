## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("sachsmc/rclinicaltrials")

## ------------------------------------------------------------------------
library(rclinicaltrials)
library(ggplot2)
library(dplyr)

z <- clinicaltrials_search(query = 'lime+disease')
str(z)

## ------------------------------------------------------------------------
clinicaltrials_count(query = "myeloma")
clinicaltrials_count(query = "29485tksrw@")

## ------------------------------------------------------------------------
clinicaltrials_count(query = c("type=Intr", "cond=cancer"))

## ------------------------------------------------------------------------
head(advanced_search_terms)

## ------------------------------------------------------------------------
y <- clinicaltrials_download(query = 'myeloma', count = 10, include_results = TRUE)
str(y)

## ------------------------------------------------------------------------
melanom <- clinicaltrials_search(query = c("cond=melanoma", "phase=2", 
                                           "type=Intr", "rslt=With"), 
                                 count = 1e6)
nrow(melanom)
table(melanom$status.text)

melanom2 <- clinicaltrials_search(query = list(cond = "melanoma", phase = "2", 
                                           type = "Intr", rslt = "With"), 
                                 count = 1e6)
nrow(melanom)


## ----check---------------------------------------------------------------
melanom_information <- clinicaltrials_download(query = c("cond=melanoma", "phase=2", 
                                                         "type=Intr", "rslt=With"), 
                                               count = 1e6, include_results = TRUE)

## ----fig, fig.width = 6.5, fig.height = 5--------------------------------

summary(melanom_information$study_results$baseline_data)

gend_data <- subset(melanom_information$study_results$baseline_data, 
                    title == "Gender" & arm != "Total")

gender_counts <- gend_data %>% group_by(nct_id, subtitle) %>% 
  do( data.frame(
    count = sum(as.numeric(paste(.$value)), na.rm = TRUE)
    ))

dates <- melanom_information$study_information$study_info[, c("nct_id", "start_date")]
dates$year <- sapply(strsplit(paste(dates$start_date), " "), function(d) as.numeric(d[2]))

counts <- merge(gender_counts, dates, by = "nct_id")

cts <- counts %>% group_by(year, subtitle) %>%
  summarize(count = sum(count))
colnames(cts)[2] <- "Gender"

ggplot(cts, aes(x = year, y = cumsum(count), color = Gender)) + 
  geom_line() + geom_point() + 
  labs(title = "Cumulative enrollment into Phase III, \n interventional trials in Melanoma, by gender") + 
  scale_y_continuous("Cumulative Enrollment") 


