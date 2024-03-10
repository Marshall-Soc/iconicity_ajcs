
# Header ------------------------------------------------------------------

# Purpose: Take the link matrix and use it to create cluster assignments
# Authors: Michael Lee Wood and Marshall A. Taylor


# Load in Data ------------------------------------------------------------

# top <- top_link_mat_comb -- run this line if you created the link matrix
                              #yourself in the create_link_matrices script
#If it's not in memory, load in the .rds
top <- readRDS(here::here("Data", "link_mat_top_comb.rds"))


# Assign Clusters ---------------------------------------------------------
# We create multiple cluster variables for different numbers of partitions

#subset the comb file into just the link matrix
top.diss <- (1 - top[1:2733,1:2733]) %>% as.matrix()

sil.w <- data.frame(matrix(nrow = 9, ncol = 1))
colnames(sil.w) <- "sil_width"
rownames(sil.w) <- 2:10

#two clusters
sil <- NbClust(diss = as.dist(top.diss), distance = NULL,
               min.nc = 2, max.nc = 2, method = "ward.D2",
               index = "silhouette")
top$n2 <- sil$Best.partition
sil.w[1,1] <- sil$All.index


#three clusters
sil <- NbClust(diss = as.dist(top.diss), distance = NULL,
               min.nc = 3, max.nc = 3, method = "ward.D2",
               index = "silhouette")
top$n3 <- sil$Best.partition
sil.w[2,1] <- sil$All.index

#four clusters
sil <- NbClust(diss = as.dist(top.diss), distance = NULL,
               min.nc = 4, max.nc = 4, method = "ward.D2",
               index = "silhouette")
top$n4 <- sil$Best.partition
sil.w[3,1] <- sil$All.index

#five clusters
sil <- NbClust(diss = as.dist(top.diss), distance = NULL,
               min.nc = 5, max.nc = 5, method = "ward.D2",
               index = "silhouette")
top$n5 <- sil$Best.partition
sil.w[4,1] <- sil$All.index

#six clusters
sil <- NbClust(diss = as.dist(top.diss), distance = NULL,
               min.nc = 6, max.nc = 6, method = "ward.D2",
               index = "silhouette")
top$n6 <- sil$Best.partition
sil.w[5,1] <- sil$All.index

#seven clusters
sil <- NbClust(diss = as.dist(top.diss), distance = NULL,
               min.nc = 7, max.nc = 7, method = "ward.D2",
               index = "silhouette")
top$n7 <- sil$Best.partition
sil.w[6,1] <- sil$All.index

#eight clusters
sil <- NbClust(diss = as.dist(top.diss), distance = NULL,
               min.nc = 8, max.nc = 8, method = "ward.D2",
               index = "silhouette")
top$n8 <- sil$Best.partition
sil.w[7,1] <- sil$All.index

#nine clusters
sil <- NbClust(diss = as.dist(top.diss), distance = NULL,
               min.nc = 9, max.nc = 9, method = "ward.D2",
               index = "silhouette")
top$n9 <- sil$Best.partition
sil.w[8,1] <- sil$All.index

#ten clusters
sil <- NbClust(diss = as.dist(top.diss), distance = NULL,
               min.nc = 10, max.nc = 10, method = "ward.D2",
               index = "silhouette")
top$n10 <- sil$Best.partition
sil.w[9,1] <- sil$All.index

  #10-cluster fits the best according to sil.w

#Create a file with the ids and cluster assignments
ids_and_clusters <- top %>% 
  select(WorkerId,n2:n10)
ids_and_clusters <- unique(ids_and_clusters) #just want one record of a respondent
                                            #being in any given micro-community
