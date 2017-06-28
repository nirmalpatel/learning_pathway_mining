library(tidyverse)
library(stringdist)
library(igraph)
library(visNetwork)

source("trace_clustering.R")
source("graph_mining.R")

df <- read_eventlog_csv("example.csv")

head(df)

summarise_log(df)

summarise_cases(df)

distmat <- trace_distmatrix(df)

distmat

hcmod <- hclust(distmat, method = "average")
plot(hcmod)

clustered_df <- cluster_eventlog(df, hcmod, k = 3)

head(clustered_df)

summarise_clusters(clustered_df)

mat1 <- trace_adjmat(filter(clustered_df, cluster == 1))
vis_trace_adjmat(mat1)

mat2 <- trace_adjmat(filter(clustered_df, cluster == 2))
vis_trace_adjmat(mat2)

mat3 <- trace_adjmat(filter(clustered_df, cluster == 3))
vis_trace_adjmat(mat3)
