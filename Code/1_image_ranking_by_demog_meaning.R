
# Header ------------------------------------------------------------------

# Purpose: Create image rankings based on cluster assignment
# Author: Marshall A. Taylor


# create subsets for each demographic category -----------------------------
  #gender
icon.data.women <- icon.data.2 %>% 
  filter(gender2=="Female")
icon.data.men <- icon.data.2 %>% 
  filter(gender2=="Male")

  #race-ethnicity
icon.data.asian <- icon.data.2 %>% 
  filter(race2=="Asian")
icon.data.black <- icon.data.2 %>% 
  filter(race2=="Black")
icon.data.hisp <- icon.data.2 %>% 
  filter(race2=="Hispanic/Latinx")
icon.data.multi <- icon.data.2 %>% 
  filter(race2=="Multiracial")
icon.data.white <- icon.data.2 %>% 
  filter(race2=="White")

  #education
icon.data.less4 <- icon.data.2 %>% 
  filter(educ2=="Less than 4-year degree")
icon.data.4more <- icon.data.2 %>% 
  filter(educ2=="At least a 4-year degree")

  #partisanship
icon.data.democrat <- icon.data.2 %>% 
  filter(party2=="Democrat")
icon.data.repub <- icon.data.2 %>% 
  filter(party2=="Republican")
icon.data.ind <- icon.data.2 %>% 
  filter(party2=="Independent")
icon.data.other <- icon.data.2 %>% 
  filter(party2=="Other")


# create subsets for meanings of the presidency top ratings -----------------------------
icon.data.citiz <- icon.data.2 %>% 
  filter(Role_Chief_Citiz==1)
icon.data.leg <- icon.data.2 %>% 
  filter(Role_Chief_Leg==1)
icon.data.party <- icon.data.2 %>% 
  filter(Role_Chief_Party==1)
icon.data.exec <- icon.data.2 %>% 
  filter(Role_Chief_Exec==1)
icon.data.state <- icon.data.2 %>% 
  filter(Role_Chief_State==1)
icon.data.command <- icon.data.2 %>% 
  filter(Role_Command==1)
icon.data.diplo <- icon.data.2 %>% 
  filter(Role_Chief_Diplo==1)


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

n.women <- rank_images(icon.data.women)
n.men <- rank_images(icon.data.men)
n.asian <- rank_images(icon.data.asian)
n.black <- rank_images(icon.data.black)
n.hisp <- rank_images(icon.data.hisp)
n.multi <- rank_images(icon.data.multi)
n.white <- rank_images(icon.data.white)
n.less4 <- rank_images(icon.data.less4)
n.4more <- rank_images(icon.data.4more)
n.democrat <- rank_images(icon.data.democrat)
n.repub <- rank_images(icon.data.repub)
n.ind <- rank_images(icon.data.ind)
n.other <- rank_images(icon.data.other)

n.citiz <- rank_images(icon.data.citiz)
n.leg <- rank_images(icon.data.leg)
n.party <- rank_images(icon.data.party)
n.exec <- rank_images(icon.data.exec)
n.state <- rank_images(icon.data.state)
n.command <- rank_images(icon.data.command)
n.diplo <- rank_images(icon.data.diplo)


# Give each df its own column name ----------------------------------------

colnames(n.women) <- "women_score"
colnames(n.men) <- "men_score"
colnames(n.asian) <- "asian_score"
colnames(n.black) <- "black_score"
colnames(n.hisp) <- "hisp_score"
colnames(n.multi) <- "multi_score"
colnames(n.white) <- "white_score"
colnames(n.less4) <- "less4_score"
colnames(n.4more) <- "fourmore_score"
colnames(n.democrat) <- "dem_score"
colnames(n.repub) <- "rep_score"
colnames(n.ind) <- "ind_score"
colnames(n.other) <- "other_score"

colnames(n.citiz) <- "citiz_score"
colnames(n.leg) <- "leg_score"
colnames(n.party) <- "party_score"
colnames(n.exec) <- "exec_score"
colnames(n.state) <- "state_score"
colnames(n.command) <- "command_score"
colnames(n.diplo) <- "diplo_score"


# Give each df an Image Number variable -----------------------------------

n.women <- as.data.frame(n.women)
n.women$IMAGE_NUMBER <- row.names(n.women)

n.men <- as.data.frame(n.men)
n.men$IMAGE_NUMBER <- row.names(n.men)

n.asian <- as.data.frame(n.asian)
n.asian$IMAGE_NUMBER <- row.names(n.asian)

n.black <- as.data.frame(n.black)
n.black$IMAGE_NUMBER <- row.names(n.black)

n.hisp <- as.data.frame(n.hisp)
n.hisp$IMAGE_NUMBER <- row.names(n.hisp)

n.multi <- as.data.frame(n.multi)
n.multi$IMAGE_NUMBER <- row.names(n.multi)

n.white <- as.data.frame(n.white)
n.white$IMAGE_NUMBER <- row.names(n.white)

n.less4 <- as.data.frame(n.less4)
n.less4$IMAGE_NUMBER <- row.names(n.less4)

n.4more <- as.data.frame(n.4more)
n.4more$IMAGE_NUMBER <- row.names(n.4more)

n.democrat <- as.data.frame(n.democrat)
n.democrat$IMAGE_NUMBER <- row.names(n.democrat)

n.repub <- as.data.frame(n.repub)
n.repub$IMAGE_NUMBER <- row.names(n.repub)

n.ind <- as.data.frame(n.ind)
n.ind$IMAGE_NUMBER <- row.names(n.ind)

n.other <- as.data.frame(n.other)
n.other$IMAGE_NUMBER <- row.names(n.other)

n.citiz <- as.data.frame(n.citiz)
n.citiz$IMAGE_NUMBER <- row.names(n.citiz)

n.leg <- as.data.frame(n.leg)
n.leg$IMAGE_NUMBER <- row.names(n.leg)

n.party <- as.data.frame(n.party)
n.party$IMAGE_NUMBER <- row.names(n.party)

n.exec <- as.data.frame(n.exec)
n.exec$IMAGE_NUMBER <- row.names(n.exec)

n.state <- as.data.frame(n.state)
n.state$IMAGE_NUMBER <- row.names(n.state)

n.command <- as.data.frame(n.command)
n.command$IMAGE_NUMBER <- row.names(n.command)

n.diplo <- as.data.frame(n.diplo)
n.diplo$IMAGE_NUMBER <- row.names(n.diplo)



# Aggregate all these columns ---------------------------------------------
df_list <- list(
  n.women,n.men,
  n.asian,n.black,n.hisp,n.multi,n.white,
  n.less4,n.4more,
  n.democrat,n.repub,n.ind,n.other,
  n.citiz,n.leg,n.party,n.exec,n.state,n.command,n.diplo)


image_scores_demo <- df_list %>% 
  reduce(left_join, by = "IMAGE_NUMBER")


# Create a combined image.data dataframe ----------------------------------
image.data.comb <- as.data.frame(left_join(image.data.comb, image_scores_demo))
