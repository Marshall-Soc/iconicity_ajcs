
# Packages ----------------------------------------------------------------

# install.packages("pacman")
pacman::p_load(tidyverse, here, tidyr, devtools,
               factoextra, NbClust, purrr,
               png, grid, ggrepel, reshape2,
               ggimage, scales, Hmisc,
               corrplot, ggpubr, ggplotify,
               install = T)

install_github("ironholds/rgeolocate")
library(rgeolocate)



# Script Execution Order --------------------------------------------------

# 1. prep_data.R
# 2. create_link_matrices.R -- Note: This script
                        #takes a while to run. So we
                        #provided a premade link matrix
                        #that is read in in the script below.
                        #If you read in that RDS, then you
                        #don't need to run script #2.
# 3. link_mat_cluster_assignment.R
# 4. image_ranking_by_cluster.R
# 5. prep_demog_variables.R
# 6. image_ranking_by_demog_meaning.R
# 8. slope_graphs.R
# 9. ttests.R