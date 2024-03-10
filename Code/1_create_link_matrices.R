
#NOTE: You don't necessarily need to run this script. See 0_start_here.R.

# Header ------------------------------------------------------------------

# Purpose: Uses Lizardo's method to create a link cluster matrix
# Author: Michael Lee Wood


# subset and create var list ----------------------------------------------
image.sub.top <- icon.data.sub %>% 
  select("WorkerId","Q194_0_1_RANK":"Q194_0_20_RANK")


top_var_list <- image.sub.top %>% 
  select("Q194_0_1_RANK":"Q194_0_20_RANK") %>% 
  colnames()



# create edgelist ---------------------------------------------------------
#creates an edgelist of all the person-image_rank ties
image.sub.top.long <- gather(data = image.sub.top,
                             key = "image",
                             value = "value",
                             all_of(top_var_list),
                             factor_key = TRUE)

#need to filter out of the edgelist all the rows that don't have an edge
image.sub.edges <- image.sub.top.long %>% 
  filter(value != 0)

#create an edge_id for each unique person-image_rank edge
image.sub.edges$edgeId <- paste0(image.sub.edges$WorkerId, "_", image.sub.edges$image)                             

image.sub.edges <- image.sub.edges[order(image.sub.edges$WorkerId),]
image.sub.edges.test <- image.sub.edges[1:12,]


#recode the edges so they are all binary. We only care if they were in the top, not their rank
image.sub.top.bin <- image.sub.top %>% 
  mutate_at(vars(all_of(top_var_list)), function(x)ifelse(x >= 1,1,0))


#make workerID the rowname. Not sure if this is necessary tbh
rownames(image.sub.top.bin) <- image.sub.top.bin$WorkerId
image.sub.top.bin <- image.sub.top.bin %>% 
  select (-"WorkerId")


# calculate jaccard similarity function -----------------------------------

#Jaccard works as long as the things you are comparing are two vectors of zeros and ones.
#So if the data are set up like that then you can use this function to compute J

Jaccard <- function (x, y) {
  M.11 <- sum(x == 1 & y == 1)
  M.10 <- sum(x == 1 & y == 0)
  M.01 <- sum(x == 0 & y == 1)
  return (M.11 / (M.11 + M.10 + M.01))
}


# Create link matrix v2 ---------------------------------------------------
n_iter <- nrow(image.sub.edges) # Number of iterations of the loop

# Initializes the progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

link_sim_mat <- matrix(0, nrow=nrow(image.sub.edges), ncol=nrow(image.sub.edges))

start.time <- Sys.time()
for (i in 1:nrow(image.sub.edges)){
  setTxtProgressBar(pb,i)
  for (j in 1:nrow(image.sub.edges)){
    id1 <- image.sub.edges[i,1]
    id2 <- image.sub.edges[j,1]
    image1 <- image.sub.edges[i,2]
    image2 <- image.sub.edges[j,2]
    if(image1 == image2 && id1 != id2){
      link_sim_mat[i, j] <- Jaccard(image.sub.top.bin[id1,], image.sub.top.bin[id2,])}
  }
}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
close(pb)


# Write to file -----------------------------------------------------------

saveRDS(image.sub.edges, file = "iconic_ranking_edges_top.rds")
saveRDS(link_sim_mat, file = "similarity_matrix.rds")

#Create a combined file that include the link-cluster matrix with the demographic variables
link_mat_2 <- link_sim_mat
link_mat_2 <- as.data.frame(link_mat_2)
rownames(link_mat_2) <-image.sub.edges$edgeId
colnames(link_mat_2) <-image.sub.edges$edgeId
link_mat_2$WorkerId <- image.sub.edges$WorkerId


demo_df <- icon_data_sub %>% 
  select(WorkerId,Q389_1:Q401)

top_link_mat_comb <- left_join(link_mat_2,demo_df)

top_link_mat_comb$edgeId <- image.sub.edges$edgeId

saveRDS(top_link_mat_comb, file = "link_mat_top_comb.rds")

