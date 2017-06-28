trace_adjmat <- function(x) {
  
  stopifnot(x %hascols% c("caseid", "activity", "completeTime"))
  
  activity_alphabet <- sort(unique(x[["activity"]]))  
  n_activities <- n_distinct(x[["activity"]])
  
  # Trace adjacency matrix
  adjmat <- matrix(rep(0, n_activities ^ 2), nrow = n_activities, ncol = n_activities)
  
  rownames(adjmat) <- activity_alphabet
  colnames(adjmat) <- activity_alphabet
  
  caseids <- unique(x[["caseid"]])
  
  for (curcaseid in caseids) {
    
    case_data <- x %>%
      filter_(~ caseid == curcaseid) %>%
      arrange_(~ completeTime)
    
    case_path <- case_data[["activity"]]
    
    path_length <- length(case_path)
    
    if (path_length > 1) {
      
      for (i in 2:path_length) {
        adjmat[case_path[i - 1], case_path[i]] <- adjmat[case_path[i - 1], case_path[i]] + 1
      }  
    }  
  }
  
  adjmat
}

vis_trace_adjmat <- function(x) {
  
  visIgraph(graph.adjacency(x, mode = "directed", weighted = TRUE))
}