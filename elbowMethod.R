setwd("c:\\Users\\malat\\OneDrive\\Bureau\\S6\\Stage\\StageL3MIDL")


get_distance_in_clust<-function(df, index, center){
  output<-c()
  cluster_subset <- get_cluster_i(df, index)
  matrix_dist<-stringdistmatrix(cluster_subset, cluster_subset, method = "lcs")
  clust_dist = rowSums(matrix_dist**2)
  index_center <- which(cluster_subset == center)
  return(clust_dist[index_center][1])
}

ncluster<-c()
distcluster<-c()

for (k in 1:10){
  cluster<-cutree(tree, k)
  df<-tibble(list_of_strings, cluster)
  nodes_data<-get_nodes_mat(df)
  nodes<-nodes_data
  sum_all_clust<-0
  for (i in 1:k){
    sum_all_clust <- sum_all_clust + get_distance_in_clust(df, i, nodes[i])
  }
  ncluster[k]<-k
  distcluster[k]<-sum_all_clust
}

df_plot <- tibble(ncluster, distcluster)
ggplot(data = df_plot, aes(x=ncluster, y=distcluster)) + geom_line() + geom_point()

