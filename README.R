
# Script Execution Order --------------------------------------------------

# 1. prep_data_20230612.R
# 2. create_link_matrices_20230612.R -- Note: This script
                        #takes a while to run. So we
                        #provided a premade link matrix
                        #that is read in in the script below.
                        #If you read in that RDS, then you
                        #don't need to run script #2.
# 3. link_mat_cluster_assignment_20220224.R
# 4. image_ranking_by_cluster_20210924.R
# 5. prep_demog_variables_20230613.R
# 6. image_ranking_by_demog_meaning_20230731.R
# 8. slope_graphs_20210929.R
# 9. ttests_20210929.R