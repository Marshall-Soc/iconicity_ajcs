
# Header ------------------------------------------------------------------

# Purpose: t-tests for each code
# Author: Marshall A. Taylor


# Recode a variable -------------------------------------------------------

image.data.comb$contxt_inside <- recode_factor(image.data.comb$contxt_inside, 
                                          "1(inside or outside stadium?)" = "1") %>%
  fct_rev()


# t-tests for communities -----------------------------------------------------------------

for (k in c("bod_profile","sym_us_flag","oba_formal",
            "oba_informal","bod_sit","bod_stand",
            "cam_closeup","cam_blurry","bod_face_fwd",
            "obama_active_hands","oba_speech","oba_smile",
            "oba_grim","oba_talk","copres_kids",
            "copres_advisor","copres_citizens","contxt_inside",
            "contxt_alone","contxt_crowd","contxt_politic_bg",
            "contxt_event_legib","contxt_event_formal")) {
  
  temp <- lapply(image.data.comb[c(53,99:108)], function(x)
    t.test(x ~ get(k), alternative = "two.sided", 
           data = image.data.comb))
  
  temp.stat <- matrix(ncol = 2, nrow = length(temp)) %>% as.data.frame()
  
  for (i in 1:length(temp)) {
    temp.stat[i,1] <- temp[[i]]$statistic
    
    temp.stat[i,2] <- temp[[i]]$p.value
    
  }
  
  colnames(temp.stat) <- c("t","p")
  rownames(temp.stat) <- c("Full Sample", paste0("Cluster #", 1:10))
  
  temp.stat$color <- ifelse(temp.stat$p < .05, "sig", "not sig")
  
  assign(paste0("plot.", k), ggplot(temp.stat, aes(x = (t*-1), y = reorder(rownames(temp.stat), (t*-1)))) +
           geom_col(aes(fill = color), color = "black", width = .5) +
           labs(title = k) +
           xlab("") + ylab("") +
           xlim(-9.5,9.5) +
           theme_bw() +
           theme(axis.text.y = element_text(size = 8),
                 plot.title = element_text(face = "bold"),
                 legend.title = element_text(face = "bold"),
                 legend.position = "none") +
           scale_fill_manual(values = c("sig" = "#fdbf11", "not sig" = "#d2d2d2"),
                             labels = c("sig" = "Statistically Significant", "not sig" = "Not Statistically Significant"),
                             name = paste("Significance at", "\u03b1", "= 0.05")))
  
}

temp <- lapply(image.data.comb[c(53,99:108)], function(x)
  t.test(x ~ bod_profile, alternative = "two.sided", 
         data = image.data.comb))

temp.stat <- matrix(ncol = 2, nrow = length(temp)) %>% as.data.frame()

for (i in 1:length(temp)) {
  temp.stat[i,1] <- temp[[i]]$statistic
  
  temp.stat[i,2] <- temp[[i]]$p.value
  
}

colnames(temp.stat) <- c("t","p")
rownames(temp.stat) <- c("Full Sample", paste0("Cluster #", 1:10))

temp.stat$color <- ifelse(temp.stat$p < .05, "sig", "not sig")

legendplot <- ggplot(temp.stat, aes(x = (t*-1), y = reorder(rownames(temp.stat), (t*-1)))) +
  geom_col(aes(fill = color), color = "black", width = .5) +
  labs(title = k) +
  xlab("") + ylab("") +
  xlim(-5,5) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = c("sig" = "#fdbf11", "not sig" = "#d2d2d2"),
                    labels = c("sig" = "Statistically Significant", "not sig" = "Not Statistically Significant"),
                    name = paste("Significance at", "\u03b1", "= 0.05"))

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(legendplot) 
legend <- as.ggplot(legend)

list.all <- grep("plot.", names(.GlobalEnv), value = T)
list.all <- do.call("list", mget(list.all))
list.all$legend <- legend

ttest.plot <- ggarrange(plotlist = list.all, align = "hv")

ttest.plot <- annotate_figure(ttest.plot, bottom = text_grob(expression(bold("t-Statistics for Difference in Code Means (Not Iconic" %<->% "Iconic)")),
                                                             size = 20, face = "bold"))

png("Figures/ttest_plots_cluster.png", res = 750, height = 16, width = 16, units = "in")
  # ggpubr::ggarrange(plotlist = plots, common.legend = T, align = "hv")
  ttest.plot
dev.off()


# t-tests for demos -----------------------------------------------------------------

rm(list=ls(pattern="plot."))

for (k in c("bod_profile","sym_us_flag","oba_formal",
            "oba_informal","bod_sit","bod_stand",
            "cam_closeup","cam_blurry","bod_face_fwd",
            "obama_active_hands","oba_speech","oba_smile",
            "oba_grim","oba_talk","copres_kids",
            "copres_advisor","copres_citizens","contxt_inside",
            "contxt_alone","contxt_crowd","contxt_politic_bg",
            "contxt_event_legib","contxt_event_formal")) {
  
  temp <- lapply(image.data.comb[c(53,109:121)], function(x)
    t.test(x ~ get(k), alternative = "two.sided", 
           data = image.data.comb))
  
  temp.stat <- matrix(ncol = 2, nrow = length(temp)) %>% as.data.frame()
  
  for (i in 1:length(temp)) {
    temp.stat[i,1] <- temp[[i]]$statistic
    
    temp.stat[i,2] <- temp[[i]]$p.value
    
  }
  
  colnames(temp.stat) <- c("t","p")
  rownames(temp.stat) <- c("Full Sample", c("Women","Men","Asian","Black","Hispanic/Latinx","Multiracial",
                                            "White","Less than a 4-Year Degree","At Least a 4-Year Degree",
                                            "Democrat","Republican","Independent","Other Political Affiliation"))
  
  temp.stat$color <- ifelse(temp.stat$p < .05, "sig", "not sig")
  
  assign(paste0("plot.", k), ggplot(temp.stat, aes(x = (t*-1), y = reorder(rownames(temp.stat), (t*-1)))) +
           geom_col(aes(fill = color), color = "black", width = .5) +
           labs(title = k) +
           xlab("") + ylab("") +
           xlim(-9.5,9.5) +
           theme_bw() +
           theme(axis.text.y = element_text(size = 8),
                 plot.title = element_text(face = "bold"),
                 legend.title = element_text(face = "bold"),
                 legend.position = "none") +
           scale_fill_manual(values = c("sig" = "#fdbf11", "not sig" = "#d2d2d2"),
                             labels = c("sig" = "Statistically Significant", "not sig" = "Not Statistically Significant"),
                             name = paste("Significance at", "\u03b1", "= 0.05")))
  
}

temp <- lapply(image.data.comb[c(53,109:121)], function(x)
  t.test(x ~ bod_profile, alternative = "two.sided", 
         data = image.data.comb))

temp.stat <- matrix(ncol = 2, nrow = length(temp)) %>% as.data.frame()

for (i in 1:length(temp)) {
  temp.stat[i,1] <- temp[[i]]$statistic
  
  temp.stat[i,2] <- temp[[i]]$p.value
  
}

colnames(temp.stat) <- c("t","p")
rownames(temp.stat) <- c("Full Sample", c("Women","Men","Asian","Black","Hispanic/Latinx","Multiracial",
                                          "White","Less than a 4-Year Degree","At Least a 4-Year Degree",
                                          "Democrat","Republican","Independent","Other Political Affiliation"))

temp.stat$color <- ifelse(temp.stat$p < .05, "sig", "not sig")

legendplot <- ggplot(temp.stat, aes(x = (t*-1), y = reorder(rownames(temp.stat), (t*-1)))) +
  geom_col(aes(fill = color), color = "black", width = .5) +
  labs(title = k) +
  xlab("") + ylab("") +
  xlim(-5,5) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = c("sig" = "#fdbf11", "not sig" = "#d2d2d2"),
                    labels = c("sig" = "Statistically Significant", "not sig" = "Not Statistically Significant"),
                    name = paste("Significance at", "\u03b1", "= 0.05"))

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(legendplot) 
legend <- as.ggplot(legend)

list.all <- grep("plot.", names(.GlobalEnv), value = T)
list.all <- do.call("list", mget(list.all))
list.all$legend <- legend

ttest.plot <- ggarrange(plotlist = list.all, align = "hv")

ttest.plot <- annotate_figure(ttest.plot, bottom = text_grob(expression(bold("t-Statistics for Difference in Code Means (Not Iconic" %<->% "Iconic)")),
                                                             size = 20, face = "bold"))

png("Figures/ttest_plots_demos.png", res = 750, height = 16, width = 24, units = "in")
# ggpubr::ggarrange(plotlist = plots, common.legend = T, align = "hv")
  ttest.plot
dev.off()


# t-tests for meanings -----------------------------------------------------------------

rm(list=ls(pattern="plot."))

for (k in c("bod_profile","sym_us_flag","oba_formal",
            "oba_informal","bod_sit","bod_stand",
            "cam_closeup","cam_blurry","bod_face_fwd",
            "obama_active_hands","oba_speech","oba_smile",
            "oba_grim","oba_talk","copres_kids",
            "copres_advisor","copres_citizens","contxt_inside",
            "contxt_alone","contxt_crowd","contxt_politic_bg",
            "contxt_event_legib","contxt_event_formal")) {
  
  temp <- lapply(image.data.comb[c(53,122:128)], function(x)
    t.test(x ~ get(k), alternative = "two.sided", 
           data = image.data.comb))
  
  temp.stat <- matrix(ncol = 2, nrow = length(temp)) %>% as.data.frame()
  
  for (i in 1:length(temp)) {
    temp.stat[i,1] <- temp[[i]]$statistic
    
    temp.stat[i,2] <- temp[[i]]$p.value
    
  }
  
  colnames(temp.stat) <- c("t","p")
  rownames(temp.stat) <- c("Full Sample", c("Chief Citizen","Chief Legislator","Chief of Party",
                                            "Chief Executive","Chief of State","Commander in Chief",
                                            "Chief Diplomat"))
  
  temp.stat$color <- ifelse(temp.stat$p < .05, "sig", "not sig")
  
  assign(paste0("plot.", k), ggplot(temp.stat, aes(x = (t*-1), y = reorder(rownames(temp.stat), (t*-1)))) +
           geom_col(aes(fill = color), color = "black", width = .5) +
           labs(title = k) +
           xlab("") + ylab("") +
           xlim(-9.5,9.5) +
           theme_bw() +
           theme(axis.text.y = element_text(size = 8),
                 plot.title = element_text(face = "bold"),
                 legend.title = element_text(face = "bold"),
                 legend.position = "none") +
           scale_fill_manual(values = c("sig" = "#fdbf11", "not sig" = "#d2d2d2"),
                             labels = c("sig" = "Statistically Significant", "not sig" = "Not Statistically Significant"),
                             name = paste("Significance at", "\u03b1", "= 0.05")))
  
}

temp <- lapply(image.data.comb[c(53,122:128)], function(x)
  t.test(x ~ bod_profile, alternative = "two.sided", 
         data = image.data.comb))

temp.stat <- matrix(ncol = 2, nrow = length(temp)) %>% as.data.frame()

for (i in 1:length(temp)) {
  temp.stat[i,1] <- temp[[i]]$statistic
  
  temp.stat[i,2] <- temp[[i]]$p.value
  
}

colnames(temp.stat) <- c("t","p")
rownames(temp.stat) <- c("Full Sample", c("Chief Citizen","Chief Legislator","Chief of Party",
                                          "Chief Executive","Chief of State","Commander in Chief",
                                          "Chief Diplomat"))

temp.stat$color <- ifelse(temp.stat$p < .05, "sig", "not sig")

legendplot <- ggplot(temp.stat, aes(x = (t*-1), y = reorder(rownames(temp.stat), (t*-1)))) +
  geom_col(aes(fill = color), color = "black", width = .5) +
  labs(title = k) +
  xlab("") + ylab("") +
  xlim(-5,5) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = c("sig" = "#fdbf11", "not sig" = "#d2d2d2"),
                    labels = c("sig" = "Statistically Significant", "not sig" = "Not Statistically Significant"),
                    name = paste("Significance at", "\u03b1", "= 0.05"))

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(legendplot) 
legend <- as.ggplot(legend)

list.all <- grep("plot.", names(.GlobalEnv), value = T)
list.all <- do.call("list", mget(list.all))
list.all$legend <- legend

ttest.plot <- ggarrange(plotlist = list.all, align = "hv")

ttest.plot <- annotate_figure(ttest.plot, bottom = text_grob(expression(bold("t-Statistics for Difference in Code Means (Not Iconic" %<->% "Iconic)")),
                                                             size = 20, face = "bold"))

png("Figures/ttest_plots_meaning.png", res = 750, height = 16, width = 20, units = "in")
# ggpubr::ggarrange(plotlist = plots, common.legend = T, align = "hv")
  ttest.plot
dev.off()


# Correlation tests -------------------------------------------------------

cor.test(image.data.comb$image_score_new, 
         image.data.comb$img_symmetry_v, method = "pearson") #not sig

cor.test(image.data.comb$image_score_new, 
         image.data.comb$img_symmetry_h, method = "pearson") #not sig

cor.test(image.data.comb$image_score_new, 
         image.data.comb$img_complexity, method = "pearson") #not sig

cor.test(image.data.comb$image_score_new, 
         image.data.comb$img_typicality, method = "pearson") #not sig

cor.test(image.data.comb$image_score_new, 
         image.data.comb$img_contrast, method = "pearson") #not sig

cor.test(image.data.comb$image_score_new, 
         image.data.comb$img_self_sim, method = "pearson") #not sig

  #for the micro-communities
vars <- c("img_symmetry_v","img_symmetry_h","img_complexity",
          "img_typicality","img_contrast","img_self_sim")

for (i in paste0("k10-", 1:10)) {
  
  for(k in vars) {
  
  cor.test(image.data.comb[[i]], 
           image.data.comb[[k]], 
           method = "pearson") %>%
    print()
    
  }
} #not sig


#Are any of the mean micro-community win percentages significantly different from 
  #the mean full sample win percentage?

for (i in paste0("k10-", 1:10)) {
    
  t.test(image.data.comb[[i]], 
           image.data.comb[["image_score_new"]], 
         paired = T, #paired t-test
         alternative = "two.sided") %>%
    print()
  
} #not sig

