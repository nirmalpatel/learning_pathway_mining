intify <- function(x) {
  y <- sort(unique(x))
  z <- setNames(1:length(y), y)
  z[x]
}

`%hascols%` <- function(x, y) {
  all(y %in% colnames(x))
}

read_eventlog_csv <- function(filename) {
  
  # read given file
  eventlog <- read_csv(filename)
  
  # stop if case and event columns are not found
  stopifnot(eventlog %hascols% c("caseid", "activity", "completeTime"))
  
  # return log if there are no errors
  eventlog
}

trace_distmatrix <- function(x, ...) {
  
  stopifnot(x %hascols% c("caseid", "activity", "completeTime"))
  
  x[["activityid"]] <- intify(x[["activity"]])
  
  # encoded trace list
  trace_list <- lapply(split(x, x[["caseid"]]), function(case_data) case_data[["activityid"]])
  
  trace_distances <- seq_distmatrix(trace_list, ...)
  
  trace_distances
}

summarise_cases <- function(x) {
  
  stopifnot(x %hascols% c("caseid", "activity", "completeTime"))
  
  case_summary <- x %>%
    group_by_(~ caseid) %>%
    summarise_(trace_length = ~ n(),
               unique_activities = ~ n_distinct(activity))
  
  case_summary
}

summarise_log <- function(x) {
  
  stopifnot(x %hascols% c("caseid", "activity", "completeTime"))
  
  case_summary <- summarise_cases(x)
  
  n_cases <- nrow(case_summary)
  
  avg_trace_length <- mean(case_summary[["trace_length"]])
  sd_trace_length <- sd(case_summary[["trace_length"]])
  
  avg_unique_activities <- mean(case_summary[["unique_activities"]])
  sd_unique_activities <- sd(case_summary[["unique_activities"]])
  
  c("Number of cases" = n_cases,
    "Average trace length" = avg_trace_length,
    "SD trace length" = sd_trace_length,
    "Average unique activities (per trace)" = avg_unique_activities,
    "SD unique activities (per trace)" = sd_unique_activities)
}

cluster_eventlog <- function(x, m, ...) {
  
  stopifnot(x %hascols% c("caseid", "activity", "completeTime"))
  
  stopifnot(class(m) == "hclust")
  
  # cut tree at a specific place to generate cluster assignments
  clusters <- cutree(m, ...)
  
  # make log with cluster information
  x %>%
    mutate_(cluster = ~ unname(clusters[as.character(caseid)]))
}

summarise_clusters <- function(x) {
  
  stopifnot(x %hascols% c("caseid", "activity", "completeTime", "cluster"))
  
  x %>%
    group_by_(~ cluster, ~ caseid) %>%
    summarise_(n = ~ n()) %>%
    ungroup() %>%
    group_by_(~ cluster) %>%
    summarise_(n_cases = ~ n_distinct(caseid),
               avg_trace_length = ~ mean(n),
               sd_trace_length = ~ sd(n)) %>%
    arrange_(~ desc(n_cases))
}