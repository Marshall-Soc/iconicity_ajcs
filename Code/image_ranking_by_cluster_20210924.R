
# Header ------------------------------------------------------------------

# Purpose: Create image rankings based on cluster assignment
# Author: Michael Lee Wood
# Last Updated: 20210924


# Merge files -------------------------------------------------------------

library(dplyr)
icon.data.nsub <- icon.data.sub

icon.data.nsub <- left_join(icon.data.nsub, ids_and_clusters, by = "WorkerId")

# icon.data.men <- icon.data.nsub %>% 
#   filter(Q396=="Male")
# 
# icon.data.women <- icon.data.nsub %>% 
#   filter(Q396=="Female")
# 
# icon.data.trump <- icon.data.nsub %>% 
#   filter(Q394=="Donald Trump")
# icon.data.clinton <- icon.data.nsub %>% 
#   filter(Q394=="Hillary Clinton")
# icon.data.novote <- icon.data.nsub %>% 
#   filter(Q394=="Did not vote")


# create subsets for each cluster in each set -----------------------------
icon.data.n2a <- icon.data.nsub %>% 
  filter(n2==1)
icon.data.n2b <- icon.data.nsub %>% 
  filter(n2==2)

icon.data.n3a <- icon.data.nsub %>% 
  filter(n3==1)
icon.data.n3b <- icon.data.nsub %>% 
  filter(n3==2)
icon.data.n3c <- icon.data.nsub %>% 
  filter(n3==3)


icon.data.n4a <- icon.data.nsub %>% 
  filter(n4==1)
icon.data.n4b <- icon.data.nsub %>% 
  filter(n4==2)
icon.data.n4c <- icon.data.nsub %>% 
  filter(n4==3)
icon.data.n4d <- icon.data.nsub %>% 
  filter(n4==4)

icon.data.n5a <- icon.data.nsub %>% 
  filter(n5==1)
icon.data.n5b <- icon.data.nsub %>% 
  filter(n5==2)
icon.data.n5c <- icon.data.nsub %>% 
  filter(n5==3)
icon.data.n5d <- icon.data.nsub %>% 
  filter(n5==4)
icon.data.n5e <- icon.data.nsub %>% 
  filter(n5==5)

icon.data.n6a <- icon.data.nsub %>% 
  filter(n6==1)
icon.data.n6b <- icon.data.nsub %>% 
  filter(n6==2)
icon.data.n6c <- icon.data.nsub %>% 
  filter(n6==3)
icon.data.n6d <- icon.data.nsub %>% 
  filter(n6==4)
icon.data.n6e <- icon.data.nsub %>% 
  filter(n6==5)
icon.data.n6f <- icon.data.nsub %>% 
  filter(n6==6)

icon.data.n7a <- icon.data.nsub %>% 
  filter(n7==1)
icon.data.n7b <- icon.data.nsub %>% 
  filter(n7==2)
icon.data.n7c <- icon.data.nsub %>% 
  filter(n7==3)
icon.data.n7d <- icon.data.nsub %>% 
  filter(n7==4)
icon.data.n7e <- icon.data.nsub %>% 
  filter(n7==5)
icon.data.n7f <- icon.data.nsub %>% 
  filter(n7==6)
icon.data.n7g <- icon.data.nsub %>% 
  filter(n7==7)

icon.data.n8a <- icon.data.nsub %>% 
  filter(n8==1)
icon.data.n8b <- icon.data.nsub %>% 
  filter(n8==2)
icon.data.n8c <- icon.data.nsub %>% 
  filter(n8==3)
icon.data.n8d <- icon.data.nsub %>% 
  filter(n8==4)
icon.data.n8e <- icon.data.nsub %>% 
  filter(n8==5)
icon.data.n8f <- icon.data.nsub %>% 
  filter(n8==6)
icon.data.n8g <- icon.data.nsub %>% 
  filter(n8==7)
icon.data.n8h <- icon.data.nsub %>% 
  filter(n8==8)

icon.data.n9a <- icon.data.nsub %>% 
  filter(n9==1)
icon.data.n9b <- icon.data.nsub %>% 
  filter(n9==2)
icon.data.n9c <- icon.data.nsub %>% 
  filter(n9==3)
icon.data.n9d <- icon.data.nsub %>% 
  filter(n9==4)
icon.data.n9e <- icon.data.nsub %>% 
  filter(n9==5)
icon.data.n9f <- icon.data.nsub %>% 
  filter(n9==6)
icon.data.n9g <- icon.data.nsub %>% 
  filter(n9==7)
icon.data.n9h <- icon.data.nsub %>% 
  filter(n9==8)
icon.data.n9i <- icon.data.nsub %>% 
  filter(n9==9)

icon.data.n10a <- icon.data.nsub %>% 
  filter(n10==1)
icon.data.n10b <- icon.data.nsub %>% 
  filter(n10==2)
icon.data.n10c <- icon.data.nsub %>% 
  filter(n10==3)
icon.data.n10d <- icon.data.nsub %>% 
  filter(n10==4)
icon.data.n10e <- icon.data.nsub %>% 
  filter(n10==5)
icon.data.n10f <- icon.data.nsub %>% 
  filter(n10==6)
icon.data.n10g <- icon.data.nsub %>% 
  filter(n10==7)
icon.data.n10h <- icon.data.nsub %>% 
  filter(n10==8)
icon.data.n10i <- icon.data.nsub %>% 
  filter(n10==9)
icon.data.n10j <- icon.data.nsub %>% 
  filter(n10==10)


# Rank Images function ----------------------------------------------------

rank_images <- function(icon.df){
  
  #initialize the vector that will contain the scores (percent won for each image)
  ranking.df <- matrix(ncol = 20, nrow = 1) %>% 
    as.data.frame()
  
  colnames(ranking.df) <- paste("image", 1:20, sep = "")
  
  #Image 1
  image1 <- icon.df[, paste("Q", 3:21, sep = "")]
  
  ranking.df[1,1] <- length(which(image1 == "IM_eKCQj2ODyqY0JzD"))/
    length(which(image1 != ""))
  
  #Image 2
  image2 <- icon.df[, paste("Q", c(3,22:39), sep = "")]
  
  ranking.df[1,2] <- length(which(image2 == "IM_blLJc7S42LAAnA1"))/
    length(which(image2 != ""))
  
  #Image 3
  image3 <- icon.df[, paste("Q", c(4,22,40:56), sep = "")]
  
  ranking.df[1,3] <- length(which(image3 == "IM_6oqXwfam2yUUKFL"))/
    length(which(image3 != ""))
  
  #Image 4
  image4 <- icon.df[, paste("Q", c(5,23,40,57:72), sep = "")]
  
  ranking.df[1,4] <- length(which(image4 == "IM_0MM4QRePd2oW4iV"))/
    length(which(image4 != ""))
  
  #Image 5
  image5 <- icon.df[, paste("Q", c(6,24,41,57,73:87), sep = "")]
  
  ranking.df[1,5] <- length(which(image5 == "IM_exqucKK1fNvfex7"))/
    length(which(image5 != ""))
  
  #Image 6
  image6 <- icon.df[, paste("Q", c(7,25,42,58,73,88:101), sep = "")]
  
  ranking.df[1,6] <- length(which(image6 == "IM_77zCBXoagELg71b"))/
    length(which(image6 != ""))
  
  #Image 7
  image7 <- icon.df[, paste("Q", c(8,26,43,59,74,88,102:114), sep = "")]
  
  ranking.df[1,7] <- length(which(image7 == "IM_cx7BXwQHCjv82XP"))/
    length(which(image7 != ""))
  
  #Image 8
  image8 <- icon.df[, paste("Q", c(9,27,44,60,75,89,102,115:126), sep = "")]
  
  ranking.df[1,8] <- length(which(image8 == "IM_0B4JRrW3BSgJVOZ"))/
    length(which(image8 != ""))
  
  #Image 9
  image9 <- icon.df[, paste("Q", c(10,28,45,61,76,90,103,115,127:137), sep = "")]
  
  ranking.df[1,9] <- length(which(image9 == "IM_0PAOKG4Ux1RjX8x"))/
    length(which(image9 != ""))
  
  #Image 10
  image10 <- icon.df[, paste("Q", c(11,29,46,62,77,91,104,116,127,138:147), sep = "")]
  
  ranking.df[1,10] <- length(which(image10 == "IM_29vv7lyHfmbmpc9"))/
    length(which(image10 != ""))
  
  #Image 11
  image11 <- icon.df[, paste("Q", c(12,30,47,63,78,92,105,117,128,138,148:156), sep = "")]
  
  ranking.df[1,11] <- length(which(image11 == "IM_efyxVfCLfcjpAl7"))/
    length(which(image11 != ""))
  
  #Image 12
  image12 <- icon.df[, paste("Q", c(13,31,48,64,79,93,106,118,129,139,148,157:164), sep = "")]
  
  ranking.df[1,12] <- length(which(image12 == "IM_6GwREh50Ir92SMZ"))/
    length(which(image12 != ""))
  
  #Image 13
  image13 <- icon.df[, paste("Q", c(14,32,49,65,80,94,107,119,130,140,149,157,
                                    165:171), sep = "")]
  
  ranking.df[1,13] <- length(which(image13 == "IM_9oSsJLZycoo2fBP"))/
    length(which(image13 != ""))
  
  #Image 14
  image14 <- icon.df[, paste("Q", c(15,33,50,66,81,95,108,120,131,141,150,158,
                                    165,172:177), sep = "")]
  
  ranking.df[1,14] <- length(which(image14 == "IM_2ou3Ggq4Z4Wof41"))/
    length(which(image14 != ""))
  
  #Image 15
  image15 <- icon.df[, paste("Q", c(16,34,51,67,82,96,109,121,132,142,151,159,
                                    166,172,178:182), sep = "")]
  
  ranking.df[1,15] <- length(which(image15 == "IM_cD5KCCjZpo3kB3T"))/
    length(which(image15 != ""))
  
  #Image 16
  image16 <- icon.df[, paste("Q", c(17,35,52,68,83,97,110,122,133,143,152,160,
                                    167,173,178,183:186), sep = "")]
  
  ranking.df[1,16] <- length(which(image16 == "IM_56Yr8Bl5cGqDALX"))/
    length(which(image16 != ""))
  
  #Image 17
  image17 <- icon.df[, paste("Q", c(18,36,53,69,84,98,111,123,134,144,153,161,
                                    168,174,179,183,187:189), sep = "")]
  
  ranking.df[1,17] <- length(which(image17 == "IM_5chM5XizEwbDgaN"))/
    length(which(image17 != ""))
  
  #Image 18
  image18 <- icon.df[, paste("Q", c(19,37,54,70,85,99,112,124,135,145,154,162,
                                    169,175,180,184,187,190:191), sep = "")]
  
  ranking.df[1,18] <- length(which(image18 == "IM_eh4pddLlR1uqasR"))/
    length(which(image18 != ""))
  
  #Image 19
  image19 <- icon.df[, paste("Q", c(20,38,55,71,86,100,113,125,136,146,155,163,
                                    170,176,181,185,188,190,192), sep = "")]
  
  ranking.df[1,19] <- length(which(image19 == "IM_8j4GoD5llnuBfx3"))/
    length(which(image19 != ""))
  
  #Image 20
  image20 <- icon.df[, paste("Q", c(21,39,56,72,87,101,114,126,137,147,156,164,
                                    171,177,182,186,189,191,192), sep = "")]
  
  ranking.df[1,20] <- length(which(image20 == "IM_d4CFFZnpKYPfS3b"))/
    length(which(image20 != ""))
  
  ranking.df <- t(ranking.df)
  colnames(ranking.df) <- "image_score"
  return(ranking.df)
  
  
}


# Calculate the image scores for each cluster of each set -----------------

n1.df1 <- rank_images(icon.data.nsub)

n2.df1 <- rank_images(icon.data.n2a)
n2.df2 <- rank_images(icon.data.n2b)


n3.df1 <- rank_images(icon.data.n3a)
n3.df2 <- rank_images(icon.data.n3b)
n3.df3 <- rank_images(icon.data.n3c)

n4.df1 <- rank_images(icon.data.n4a)
n4.df2 <- rank_images(icon.data.n4b)
n4.df3 <- rank_images(icon.data.n4c)
n4.df4 <- rank_images(icon.data.n4d)

n5.df1 <- rank_images(icon.data.n5a)
n5.df2 <- rank_images(icon.data.n5b)
n5.df3 <- rank_images(icon.data.n5c)
n5.df4 <- rank_images(icon.data.n5d)
n5.df5 <- rank_images(icon.data.n5e)

n6.df1 <- rank_images(icon.data.n6a)
n6.df2 <- rank_images(icon.data.n6b)
n6.df3 <- rank_images(icon.data.n6c)
n6.df4 <- rank_images(icon.data.n6d)
n6.df5 <- rank_images(icon.data.n6e)
n6.df6 <- rank_images(icon.data.n6f)

n7.df1 <- rank_images(icon.data.n7a)
n7.df2 <- rank_images(icon.data.n7b)
n7.df3 <- rank_images(icon.data.n7c)
n7.df4 <- rank_images(icon.data.n7d)
n7.df5 <- rank_images(icon.data.n7e)
n7.df6 <- rank_images(icon.data.n7f)
n7.df7 <- rank_images(icon.data.n7g)

n8.df1 <- rank_images(icon.data.n8a)
n8.df2 <- rank_images(icon.data.n8b)
n8.df3 <- rank_images(icon.data.n8c)
n8.df4 <- rank_images(icon.data.n8d)
n8.df5 <- rank_images(icon.data.n8e)
n8.df6 <- rank_images(icon.data.n8f)
n8.df7 <- rank_images(icon.data.n8g)
n8.df8 <- rank_images(icon.data.n8h)

n9.df1 <- rank_images(icon.data.n9a)
n9.df2 <- rank_images(icon.data.n9b)
n9.df3 <- rank_images(icon.data.n9c)
n9.df4 <- rank_images(icon.data.n9d)
n9.df5 <- rank_images(icon.data.n9e)
n9.df6 <- rank_images(icon.data.n9f)
n9.df7 <- rank_images(icon.data.n9g)
n9.df8 <- rank_images(icon.data.n9h)
n9.df9 <- rank_images(icon.data.n9i)

n10.df1 <- rank_images(icon.data.n10a)
n10.df2 <- rank_images(icon.data.n10b)
n10.df3 <- rank_images(icon.data.n10c)
n10.df4 <- rank_images(icon.data.n10d)
n10.df5 <- rank_images(icon.data.n10e)
n10.df6 <- rank_images(icon.data.n10f)
n10.df7 <- rank_images(icon.data.n10g)
n10.df8 <- rank_images(icon.data.n10h)
n10.df9 <- rank_images(icon.data.n10i)
n10.df10 <- rank_images(icon.data.n10j)


# img_rank_men <- rank_images(icon.data.men)
# img_rank_women <- rank_images(icon.data.women)
# 
# img_rank_trump <- rank_images(icon.data.trump)
# img_rank_clinton <- rank_images(icon.data.clinton)
# img_rank_novote <- rank_images(icon.data.novote)
# 
# 
# men_women_rankings <- cbind(img_rank_men,img_rank_women)


#create a character vector with all the object names
# x = list()
# for (i in 2:10){
#   for (j in 1:i){
#     x <- append(x, paste0("n",i,".df",j))
#   }
# }
# 
# var_names <- list()
# for (i in 2:10){
#   for (j in 1:i){
#     var_names <- append(var_names, paste0("k",i,"-",j))
#   }
# }





# n2.df1 <- set_col_names(n2.df1,var_names[1])
# 
# 
# 
# set_col_names <- function(df, var.name){
#   colnames(df) <- var.name
#   return(df)
#   }
# 
# df_list <- list()
# df_list <- lapply(x, function(i) append(df_list, get(i)))
# 
# 
# 
# for(k in length(x)){
#   df_list[[k]] <- as.data.frame(df_list[[k]])
#   df_list[[k]] <- set_col_names(df_list[[k]],var_names[[k]])
# }
# 
# View(df_list[[1]])
# 
# test <- as.data.frame(df_list[[1]])
# 
# 
# colnames(test.df) <- "k2-1"
# test.df$IMAGE_NUMBER <- rownames(test.df)
# colnames(test.df2) <- "k2-2"



# Give each df its own column name ----------------------------------------

colnames(n1.df1) <- "k1-1"

colnames(n2.df1) <- "k2-1"
colnames(n2.df2) <- "k2-2"


colnames(n3.df1) <- "k3-1"
colnames(n3.df2) <- "k3-2"
colnames(n3.df3) <- "k3-3"

colnames(n4.df1) <- "k4-1"
colnames(n4.df2) <- "k4-2"
colnames(n4.df3) <- "k4-3"
colnames(n4.df4) <- "k4-4"

colnames(n5.df1) <- "k5-1"
colnames(n5.df2) <- "k5-2"
colnames(n5.df3) <- "k5-3"
colnames(n5.df4) <- "k5-4"
colnames(n5.df5) <- "k5-5"

colnames(n6.df1) <- "k6-1"
colnames(n6.df2) <- "k6-2"
colnames(n6.df3) <- "k6-3"
colnames(n6.df4) <- "k6-4"
colnames(n6.df5) <- "k6-5"
colnames(n6.df6) <- "k6-6"

colnames(n7.df1) <- "k7-1"
colnames(n7.df2) <- "k7-2"
colnames(n7.df3) <- "k7-3"
colnames(n7.df4) <- "k7-4"
colnames(n7.df5) <- "k7-5"
colnames(n7.df6) <- "k7-6"
colnames(n7.df7) <- "k7-7"

colnames(n8.df1) <- "k8-1"
colnames(n8.df2) <- "k8-2"
colnames(n8.df3) <- "k8-3"
colnames(n8.df4) <- "k8-4"
colnames(n8.df5) <- "k8-5"
colnames(n8.df6) <- "k8-6"
colnames(n8.df7) <- "k8-7"
colnames(n8.df8) <- "k8-8"

colnames(n9.df1) <- "k9-1"
colnames(n9.df2) <- "k9-2"
colnames(n9.df3) <- "k9-3"
colnames(n9.df4) <- "k9-4"
colnames(n9.df5) <- "k9-5"
colnames(n9.df6) <- "k9-6"
colnames(n9.df7) <- "k9-7"
colnames(n9.df8) <- "k9-8"
colnames(n9.df9) <- "k9-9"

colnames(n10.df1) <- "k10-1"
colnames(n10.df2) <- "k10-2"
colnames(n10.df3) <- "k10-3"
colnames(n10.df4) <- "k10-4"
colnames(n10.df5) <- "k10-5"
colnames(n10.df6) <- "k10-6"
colnames(n10.df7) <- "k10-7"
colnames(n10.df8) <- "k10-8"
colnames(n10.df9) <- "k10-9"
colnames(n10.df10) <- "k10-10"



# Give each df an Image Number variable -----------------------------------

n1.df1 <- as.data.frame(n1.df1)
n1.df1$IMAGE_NUMBER <- row.names(n1.df1)

n2.df1 <- as.data.frame(n2.df1)
n2.df1$IMAGE_NUMBER <- row.names(n2.df1)


n2.df2 <- as.data.frame(n2.df2)
n2.df2 <- n2.df2 %>% 
  mutate(IMAGE_NUMBER = row.names(.))

n3.df1 <- as.data.frame(n3.df1)
n3.df1$IMAGE_NUMBER <- row.names(n3.df1)

n3.df2 <- as.data.frame(n3.df2)
n3.df2$IMAGE_NUMBER <- row.names(n3.df2)

n3.df3 <- as.data.frame(n3.df3)
n3.df3$IMAGE_NUMBER <- row.names(n3.df3)

n4.df1 <- as.data.frame(n4.df1)
n4.df1$IMAGE_NUMBER <- row.names(n4.df1)

n4.df2 <- as.data.frame(n4.df2)
n4.df2$IMAGE_NUMBER <- row.names(n4.df2)

n4.df3 <- as.data.frame(n4.df3)
n4.df3$IMAGE_NUMBER <- row.names(n4.df3)

n4.df4 <- as.data.frame(n4.df4)
n4.df4$IMAGE_NUMBER <- row.names(n4.df4)

n5.df1 <- as.data.frame(n5.df1)
n5.df1$IMAGE_NUMBER <- row.names(n5.df1)

n5.df2 <- as.data.frame(n5.df2)
n5.df2$IMAGE_NUMBER <- row.names(n5.df2)

n5.df3 <- as.data.frame(n5.df3)
n5.df3$IMAGE_NUMBER <- row.names(n5.df3)

n5.df4 <- as.data.frame(n5.df4)
n5.df4$IMAGE_NUMBER <- row.names(n5.df4)

n5.df5 <- as.data.frame(n5.df5)
n5.df5$IMAGE_NUMBER <- row.names(n5.df5)

n6.df1 <- as.data.frame(n6.df1)
n6.df1$IMAGE_NUMBER <- row.names(n6.df1)

n6.df2 <- as.data.frame(n6.df2)
n6.df2$IMAGE_NUMBER <- row.names(n6.df2)

n6.df3 <- as.data.frame(n6.df3)
n6.df3$IMAGE_NUMBER <- row.names(n6.df3)

n6.df4 <- as.data.frame(n6.df4)
n6.df4$IMAGE_NUMBER <- row.names(n6.df4)

n6.df5 <- as.data.frame(n6.df5)
n6.df5$IMAGE_NUMBER <- row.names(n6.df5)

n6.df6 <- as.data.frame(n6.df6)
n6.df6$IMAGE_NUMBER <- row.names(n6.df6)

n7.df1 <- as.data.frame(n7.df1)
n7.df1$IMAGE_NUMBER <- row.names(n7.df1)

n7.df2 <- as.data.frame(n7.df2)
n7.df2$IMAGE_NUMBER <- row.names(n7.df2)

n7.df3 <- as.data.frame(n7.df3)
n7.df3$IMAGE_NUMBER <- row.names(n7.df3)

n7.df4 <- as.data.frame(n7.df4)
n7.df4$IMAGE_NUMBER <- row.names(n7.df4)

n7.df5 <- as.data.frame(n7.df5)
n7.df5$IMAGE_NUMBER <- row.names(n7.df5)

n7.df6 <- as.data.frame(n7.df6)
n7.df6$IMAGE_NUMBER <- row.names(n7.df6)

n7.df7 <- as.data.frame(n7.df7)
n7.df7$IMAGE_NUMBER <- row.names(n7.df7)

n8.df1 <- as.data.frame(n8.df1)
n8.df1$IMAGE_NUMBER <- row.names(n8.df1)

n8.df2 <- as.data.frame(n8.df2)
n8.df2$IMAGE_NUMBER <- row.names(n8.df2)

n8.df3 <- as.data.frame(n8.df3)
n8.df3$IMAGE_NUMBER <- row.names(n8.df3)

n8.df4 <- as.data.frame(n8.df4)
n8.df4$IMAGE_NUMBER <- row.names(n8.df4)

n8.df5 <- as.data.frame(n8.df5)
n8.df5$IMAGE_NUMBER <- row.names(n8.df5)

n8.df6 <- as.data.frame(n8.df6)
n8.df6$IMAGE_NUMBER <- row.names(n8.df6)

n8.df7 <- as.data.frame(n8.df7)
n8.df7$IMAGE_NUMBER <- row.names(n8.df7)

n8.df8 <- as.data.frame(n8.df8)
n8.df8$IMAGE_NUMBER <- row.names(n8.df8)

n9.df1 <- as.data.frame(n9.df1)
n9.df1$IMAGE_NUMBER <- row.names(n9.df1)

n9.df2 <- as.data.frame(n9.df2)
n9.df2$IMAGE_NUMBER <- row.names(n9.df2)

n9.df3 <- as.data.frame(n9.df3)
n9.df3$IMAGE_NUMBER <- row.names(n9.df3)

n9.df4 <- as.data.frame(n9.df4)
n9.df4$IMAGE_NUMBER <- row.names(n9.df4)

n9.df5 <- as.data.frame(n9.df5)
n9.df5$IMAGE_NUMBER <- row.names(n9.df5)

n9.df6 <- as.data.frame(n9.df6)
n9.df6$IMAGE_NUMBER <- row.names(n9.df6)

n9.df7 <- as.data.frame(n9.df7)
n9.df7$IMAGE_NUMBER <- row.names(n9.df7)

n9.df8 <- as.data.frame(n9.df8)
n9.df8$IMAGE_NUMBER <- row.names(n9.df8)

n9.df9 <- as.data.frame(n9.df9)
n9.df9$IMAGE_NUMBER <- row.names(n9.df9)

n10.df1 <- as.data.frame(n10.df1)
n10.df1$IMAGE_NUMBER <- row.names(n10.df1)

n10.df2 <- as.data.frame(n10.df2)
n10.df2$IMAGE_NUMBER <- row.names(n10.df2)

n10.df3 <- as.data.frame(n10.df3)
n10.df3$IMAGE_NUMBER <- row.names(n10.df3)

n10.df4 <- as.data.frame(n10.df4)
n10.df4$IMAGE_NUMBER <- row.names(n10.df4)

n10.df5 <- as.data.frame(n10.df5)
n10.df5$IMAGE_NUMBER <- row.names(n10.df5)

n10.df6 <- as.data.frame(n10.df6)
n10.df6$IMAGE_NUMBER <- row.names(n10.df6)

n10.df7 <- as.data.frame(n10.df7)
n10.df7$IMAGE_NUMBER <- row.names(n10.df7)

n10.df8 <- as.data.frame(n10.df8)
n10.df8$IMAGE_NUMBER <- row.names(n10.df8)

n10.df9 <- as.data.frame(n10.df9)
n10.df9$IMAGE_NUMBER <- row.names(n10.df9)

n10.df10 <- as.data.frame(n10.df10)
n10.df10$IMAGE_NUMBER <- row.names(n10.df10)

# df_list <- vector(mode = "list", length = length(x))
# 
# 
# for(i in length(x)){
#  append(df_list, get(x[[i]]))
# }


# Aggregate all these columns ---------------------------------------------
df_list <- list(
            n1.df1,
            n2.df1,n2.df2,
            n3.df1,n3.df2,n3.df3,
            n4.df1,n4.df2,n4.df3,n4.df4,
            n5.df1,n5.df2,n5.df3,n5.df4,n5.df5,
            n6.df1,n6.df2,n6.df3,n6.df4,n6.df5,n6.df6,
            n7.df1,n7.df2,n7.df3,n7.df4,n7.df5,n7.df6,n7.df7,
            n8.df1,n8.df2,n8.df3,n8.df4,n8.df5,n8.df6,n8.df7,n8.df8,
            n9.df1,n9.df2,n9.df3,n9.df4,n9.df5,n9.df6,n9.df7,n9.df8,n9.df9,
            n10.df1,n10.df2,n10.df3,n10.df4,n10.df5,n10.df6,n10.df7,n10.df8,n10.df9,n10.df10)



#df_list <- lapply(x, function(i) append(df_list, get(i)))


library(purrr)
image_scores_cluster <- df_list %>% 
  reduce(left_join, by = "IMAGE_NUMBER")



# Create a combined image.data dataframe ----------------------------------
image.data.comb <- as.data.frame(left_join(image.data,image_scores_cluster))







# image_scores_clustered <- cbind(test.df,test.df2,
#                                 n3.df1,n3.df2,n3.df3,
#                                 n4.df1,n4.df2,n4.df3,n4.df4,
#                                 n5.df1,n5.df2,n5.df3,n5.df4,n5.df5,
#                                 n6.df1,n6.df2,n6.df3,n6.df4,n6.df5,n6.df6,
#                                 n7.df1,n7.df2,n7.df3,n7.df4,n7.df5,n7.df6,n7.df7,
#                                 n8.df1,n8.df2,n8.df3,n8.df4,n8.df5,n8.df6,n8.df7,n8.df8,
#                                 n9.df1,n9.df2,n9.df3,n9.df4,n9.df5,n9.df6,n9.df7,n9.df8,n9.df9,
#                                 n10.df1,n10.df2,n10.df3,n10.df4,n10.df5,n10.df6,n10.df7,n10.df8,n10.df9,n10.df10)
# 
# 
# image.data.comb <- as.data.frame(cbind(image.data,image_scores_clustered))
# getwd()


# Export to csv -----------------------------------------------------------
# 
# output.csv <- apply(image.data.comb,2,as.character)
# output.csv <- as.data.frame(output.csv)
# write.csv(output.csv,"image_data_comb_20210924.csv")




# Remove extra objects ------------------------------------------------------

# if desired, run this to remove most of the objects
rm(list=setdiff(ls(), c("image.data","icon.data","icon.data.sub",
                        "image.data.comb","top","icon.data.nsub",
                        "ids_and_clusters")))

# table(image.data.comb$image_score)
# table(image.data.comb$k1)
# cor(image.data.comb$`k10-2`,image.data.comb$k1)
