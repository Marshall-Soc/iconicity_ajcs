

# Header ------------------------------------------------------------------

##  prep.R: Setup code for Iconicity image data
##  Note: The code produces three main dataframes: 
##             image.data (the image-level dataset)
##             icon.data (the individual-level dataset)
##             icon.data.sub (the individual-level dataset prepped for cluster analysis)
##  Author: Marshall A. Taylor & Michael Lee Wood
##  Last Updated: 20211005


### BEGIN ###


# Load required packages --------------------------------------------------
#install.packages("pacman")

pacman::p_load(tidyverse, install = T)
library(dplyr)
library(here)

# Load in data & remove rejected obs --------------------------------------

#Read in survey data
qual <- read.csv(here::here('Data', 'iconicity_qual.csv')) %>% 
  slice(., -(1:2)) #Remove metadata in first two rows

#Read in the mturk data, so we can remove cases we rejected 
mturk <- read.csv(here::here('Data', 'mturk.csv')) %>%
  filter(AssignmentStatus == "Approved") %>%
  select(WorkerId, Answer.surveycode)

codes <- read.csv(here::here('Data', 'image_codes.csv')) %>%
  select(-2)


# Merge and Remove Duplicates ---------------------------------------------
mturk$Answer.surveycode <- gsub(": ", "", mturk$Answer.surveycode)

icon.data <- left_join(mturk, qual, by = c("Answer.surveycode" = "Random.ID"))


#Remove Duplicates
#This is fairly comprehensive, in that it removes all cases where either the 
#workerid, the surveycode, or the ip.address were duplicated. This is to clear
#out any possibility of shenanigans. It doesn't just remove the "extra"--it
#removes ALL cases that were part of a duplicate pair. (n=53)
dupes1 <- icon.data[duplicated(icon.data$IPAddress),] %>% 
  select(IPAddress)

dupes2 <- icon.data[duplicated(icon.data$WorkerId),] %>% 
  select(IPAddress)

dupes3 <- icon.data[duplicated(icon.data$Answer.surveycode),] %>% 
  select(IPAddress)

dupes_comb <- bind_rows(dupes1,dupes2,dupes3)
dupes_comb <- unique(dupes_comb)
icon.data <- icon.data %>% 
  filter(!IPAddress %in% dupes_comb$IPAddress)

#here's this, if you want to inspect the data where one or more of the id vars was duplicated
#dupe.data <- icon.data %>% 
#  filter(IPAddress %in% dupes_comb$IPAddress)


#This uses IP addresses to locate people so we can drop non-US respondents
devtools::install_github("ironholds/rgeolocate")
library(rgeolocate)

icon.data$geo_location <- unlist(maxmind(ips = icon.data$IPAddress,
                        file = "Data/GeoLite2-City.mmdb",
                        fields = "country_name"))

#remove non-US responses
icon.data <- icon.data %>% 
  filter(geo_location == "United States")


# Make image-level DV (image ranking) -------------------------------------


  #Key:
    #"2014 2": IM_eKCQj2ODyqY0JzD
    #"2015 11": IM_blLJc7S42LAAnA1
    #"2012 5": IM_6oqXwfam2yUUKFL
    #""2012 08": IM_0MM4QRePd2oW4iV
    #"2013 09": IM_exqucKK1fNvfex7
    #"2014 10": IM_77zCBXoagELg71b
    #"2014 13": IM_cx7BXwQHCjv82XP
    #"2015 08": IM_0B4JRrW3BSgJVOZ
    #"2015 09": IM_0PAOKG4Ux1RjX8x
    #"2015 10": IM_29vv7lyHfmbmpc9
    #"obamaps03": IM_efyxVfCLfcjpAl7
    #"obamaps15": IM_6GwREh50Ir92SMZ
    #"obamaps19: IM_9oSsJLZycoo2fBP
    #"obamaps22": IM_2ou3Ggq4Z4Wof41
    #"obamawh07": IM_cD5KCCjZpo3kB3T
    #"obamawh30": IM_56Yr8Bl5cGqDALX
    #"obamawh31": IM_5chM5XizEwbDgaN
    #"obamawh35": IM_eh4pddLlR1uqasR
    #"obamawh63": IM_8j4GoD5llnuBfx3
    #"obamawh69": IM_d4CFFZnpKYPfS3b

#initialize the matrix
image.dv <- matrix(ncol = 20, nrow = 1) %>% 
  as.data.frame()

#name the columns
colnames(image.dv) <- paste("image", 1:20, sep = "")

#Image 1
image1 <- icon.data[, paste("Q", 3:21, sep = "")]

image.dv[1,1] <- length(which(image1 == "IM_eKCQj2ODyqY0JzD"))/
  length(which(image1 != ""))

#Image 2
image2 <- icon.data[, paste("Q", c(3,22:39), sep = "")]

image.dv[1,2] <- length(which(image2 == "IM_blLJc7S42LAAnA1"))/
  length(which(image2 != ""))

#Image 3
image3 <- icon.data[, paste("Q", c(4,22,40:56), sep = "")]

image.dv[1,3] <- length(which(image3 == "IM_6oqXwfam2yUUKFL"))/
  length(which(image3 != ""))

#Image 4
image4 <- icon.data[, paste("Q", c(5,23,40,57:72), sep = "")]

image.dv[1,4] <- length(which(image4 == "IM_0MM4QRePd2oW4iV"))/
  length(which(image4 != ""))

#Image 5
image5 <- icon.data[, paste("Q", c(6,24,41,57,73:87), sep = "")]

image.dv[1,5] <- length(which(image5 == "IM_exqucKK1fNvfex7"))/
  length(which(image5 != ""))

#Image 6
image6 <- icon.data[, paste("Q", c(7,25,42,58,73,88:101), sep = "")]

image.dv[1,6] <- length(which(image6 == "IM_77zCBXoagELg71b"))/
  length(which(image6 != ""))

#Image 7
image7 <- icon.data[, paste("Q", c(8,26,43,59,74,88,102:114), sep = "")]

image.dv[1,7] <- length(which(image7 == "IM_cx7BXwQHCjv82XP"))/
  length(which(image7 != ""))

#Image 8
image8 <- icon.data[, paste("Q", c(9,27,44,60,75,89,102,115:126), sep = "")]

image.dv[1,8] <- length(which(image8 == "IM_0B4JRrW3BSgJVOZ"))/
  length(which(image8 != ""))

#Image 9
image9 <- icon.data[, paste("Q", c(10,28,45,61,76,90,103,115,127:137), sep = "")]

image.dv[1,9] <- length(which(image9 == "IM_0PAOKG4Ux1RjX8x"))/
  length(which(image9 != ""))

#Image 10
image10 <- icon.data[, paste("Q", c(11,29,46,62,77,91,104,116,127,138:147), sep = "")]

image.dv[1,10] <- length(which(image10 == "IM_29vv7lyHfmbmpc9"))/
  length(which(image10 != ""))

#Image 11
image11 <- icon.data[, paste("Q", c(12,30,47,63,78,92,105,117,128,138,148:156), sep = "")]

image.dv[1,11] <- length(which(image11 == "IM_efyxVfCLfcjpAl7"))/
  length(which(image11 != ""))

#Image 12
image12 <- icon.data[, paste("Q", c(13,31,48,64,79,93,106,118,129,139,148,157:164), sep = "")]

image.dv[1,12] <- length(which(image12 == "IM_6GwREh50Ir92SMZ"))/
  length(which(image12 != ""))

#Image 13
image13 <- icon.data[, paste("Q", c(14,32,49,65,80,94,107,119,130,140,149,157,
                                    165:171), sep = "")]

image.dv[1,13] <- length(which(image13 == "IM_9oSsJLZycoo2fBP"))/
  length(which(image13 != ""))

#Image 14
image14 <- icon.data[, paste("Q", c(15,33,50,66,81,95,108,120,131,141,150,158,
                                    165,172:177), sep = "")]

image.dv[1,14] <- length(which(image14 == "IM_2ou3Ggq4Z4Wof41"))/
  length(which(image14 != ""))

#Image 15
image15 <- icon.data[, paste("Q", c(16,34,51,67,82,96,109,121,132,142,151,159,
                                    166,172,178:182), sep = "")]

image.dv[1,15] <- length(which(image15 == "IM_cD5KCCjZpo3kB3T"))/
  length(which(image15 != ""))

#Image 16
image16 <- icon.data[, paste("Q", c(17,35,52,68,83,97,110,122,133,143,152,160,
                                    167,173,178,183:186), sep = "")]

image.dv[1,16] <- length(which(image16 == "IM_56Yr8Bl5cGqDALX"))/
  length(which(image16 != ""))

#Image 17
image17 <- icon.data[, paste("Q", c(18,36,53,69,84,98,111,123,134,144,153,161,
                                    168,174,179,183,187:189), sep = "")]

image.dv[1,17] <- length(which(image17 == "IM_5chM5XizEwbDgaN"))/
  length(which(image17 != ""))

#Image 18
image18 <- icon.data[, paste("Q", c(19,37,54,70,85,99,112,124,135,145,154,162,
                                    169,175,180,184,187,190:191), sep = "")]

image.dv[1,18] <- length(which(image18 == "IM_eh4pddLlR1uqasR"))/
  length(which(image18 != ""))

#Image 19
image19 <- icon.data[, paste("Q", c(20,38,55,71,86,100,113,125,136,146,155,163,
                                    170,176,181,185,188,190,192), sep = "")]

image.dv[1,19] <- length(which(image19 == "IM_8j4GoD5llnuBfx3"))/
  length(which(image19 != ""))

#Image 20
image20 <- icon.data[, paste("Q", c(21,39,56,72,87,101,114,126,137,147,156,164,
                                    171,177,182,186,189,191,192), sep = "")]

image.dv[1,20] <- length(which(image20 == "IM_d4CFFZnpKYPfS3b"))/
  length(which(image20 != ""))

image.dv <- t(image.dv)
colnames(image.dv) <- "image_score"


# Make image-level dataset ------------------------------------------------

colnames(image.dv) <- "image_score_new"
image.dv <- as.data.frame(image.dv)
image.dv$IMAGE_NUMBER <- rownames(image.dv)
codes <- codes %>% select(-image_score)

image.data <- left_join(codes, image.dv, by = "IMAGE_NUMBER")
#saveRDS(image.data, "data/image_data.rds")


# Prep the data for cluster analysis based on top-3 rankings --------------

#create a subset
icon.data.sub <- icon.data

#create a list of column names
col_list <- icon.data.sub %>% 
  select("Q194_0_1_RANK":"Q194_1_20_RANK") %>% 
  colnames()

#make them all numeric (they are currently characters) 
icon.data.sub <- icon.data.sub %>% 
  mutate_at(vars(all_of(col_list)), as.numeric)

#remove rows that skipped the ranking question entirely (n=2)
icon.data.sub <- icon.data.sub %>% 
  filter_at(vars(all_of(col_list)), any_vars(!is.na(.)))


#replace the NAs with 0's 
icon.data.sub <- icon.data.sub %>% 
  mutate_at(vars(all_of(col_list)), tidyr::replace_na ,0)

#identify the people who messed up the ranking task by putting more than 3 in a box
mistake_list <- icon.data.sub %>% 
  filter_at(vars(all_of(col_list)), any_vars(.>3)) %>% 
  select(WorkerId)

#filter out these 81 mistake people
icon.data.sub <- icon.data.sub %>% 
  filter(!WorkerId %in% mistake_list$WorkerId)

#remove all the unneeded objects
rm(list=setdiff(ls(), c("image.data","icon.data","icon.data.sub")))

### END ###