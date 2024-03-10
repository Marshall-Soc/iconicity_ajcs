
# Header ------------------------------------------------------------------

# Purpose: Create slope graphs
# Author: Marshall A. Taylor


# Load image data ---------------------------------------------------------------

images <- list.files("Images", full.names = T) %>% 
  as.data.frame() %>%
  slice(-21)


# Full slope graph for clusters --------------------------------------------------------

temp.list <- lapply(paste0("k10-", 1:10), function(i)
  
  image.data.comb %>%
    select(image_score_new, as.name(i), IMAGE_NUMBER) %>%
    arrange(-image_score_new) %>%
    add_column(images) %>%
    rename(., image = .) %>%
    mutate(color = ifelse(as.numeric(rownames(.)) <= 3, "top", "not"),
           color = ifelse(as.numeric(rownames(.)) >= 18, "bottom", color),
           order = 1:20) %>%
    arrange(get(i)) %>%
    mutate(order2 = 20:1,
           diff = image_score_new - get(i)) %>%
    arrange(-abs(diff)) %>%
    mutate(color = ifelse(as.numeric(rownames(.)) <= 3, "diff", color)) %>%
    melt(id.vars = c("IMAGE_NUMBER","color","image_score_new",i,"image","diff")) %>%
    ggplot(aes(x = variable, y = -value, group = IMAGE_NUMBER)) +
    geom_line(aes(color = color, alpha = color, size = color)) +
    geom_text_repel(data = . %>% filter(variable == "order"),
                    aes(label = scales::percent(round(image_score_new, 3))),
                    hjust = "left",
                    size = 3,
                    nudge_x = -.3) +
    # direction = "y") +
    geom_text_repel(data = . %>% filter(variable == "order2"),
                    aes(label = scales::percent(round(get(i), 3))),
                    hjust = "right",
                    size = 3,
                    nudge_x = .3) +
    # direction = "y") +
    geom_image(aes(image = image), size = .04, by = "height") +
    # scale_size_identity() +
    # geom_label(data = . %>% filter(variable == "order"),
    #            aes(label = round(image_score, 3)),
    #            size = 4,
    #            label.padding = unit(.05, "lines"),
    #            label.size = 0) + 
    # geom_label(data = . %>% filter(variable == "order2"),
    #            aes(label = round(k10.1, 3)),
    #            size = 4,
    #            label.padding = unit(.05, "lines"),
    #            label.size = 0) +
  labs(y = "", x = "") +
    scale_color_manual(values = c("top" = "#1696d2",
                                  "bottom" = "#fdbf11",
                                  "diff" = "#db2b27",
                                  "not" = "gray75")) +
    scale_alpha_manual(values = c("top" = 1,
                                  "bottom" = 1,
                                  "diff" = 1,
                                  "not" = .3)) +
    scale_size_manual(values = c("top" = 2,
                                 "bottom" = 2,
                                 "diff" = 2,
                                 "not" = 1)) +
    guides(alpha = "none", color = "none", size = "none") +
    theme_void() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white", color = "white"))
) 

png("Figures/full_rankings_cluster.png", res = 750, height = 12, width = 28, units = "in")
  ggpubr::ggarrange(plotlist = temp.list, align = "hv", ncol = 5, nrow = 2)
dev.off() 


# Durability slope graph for clusters ----------------------------------------------------

for(i in paste0("k10-", 1:10)) {
  
  new_col_name <- paste0("order.", i)
  
  assign(paste0("table.", i), image.data.comb %>%
           select(image_score_new, as.name(i), IMAGE_NUMBER) %>%
           arrange(-image_score_new) %>%
           add_column(images) %>%
           rename(., image = .) %>%
           mutate(color = ifelse(as.numeric(rownames(.)) <= 3, "top", "not"),
                  color = ifelse(as.numeric(rownames(.)) >= 18, "bottom", color),
                  color = ifelse(as.numeric(rownames(.)) == 1, "toptop", color),
                  order = 1:20) %>%
           arrange(get(i)) %>%
           mutate(!!sym(new_col_name) := 20:1)
         
  )
  
}

list.all <- grep("table", names(.GlobalEnv), value = T)
list.all <- do.call("list", mget(list.all))

multi_inner <- Reduce(
  function(x, y, ...) merge(x, y, ...), 
  list.all
)


full.df <- melt(multi_inner, id.vars = c("IMAGE_NUMBER","image_score_new","image","color",
                                         paste0("k10-", 1:10)))

image1.order <- full.df[full.df$IMAGE_NUMBER == "image1", c("variable","value")] %>%
  arrange(value) %>%
  pull(variable) %>%
  as.character()

durability.plot <- ggplot(full.df, aes(x = factor(variable, levels = image1.order), y = -value, group = IMAGE_NUMBER)) +
  geom_line(aes(color = color, alpha = color, size = color)) +
  geom_image(aes(image = image), size = .025, by = "height") +
  labs(y = "", x = "") +
  scale_color_manual(values = c("toptop" = "#c3cb71",
                                "top" = "#559e83",
                                "bottom" = "#ae5a41",
                                "not" = "gray75")) +
  scale_alpha_manual(values = c("toptop" = 1,
                                "top" = 1,
                                "bottom" = 1,
                                "not" = 0)) +
  scale_size_manual(values = c("toptop" = 2,
                               "top" = 2,
                               "bottom" = 2,
                               "not" = 0)) +
  guides(alpha = "none", color = "none", size = "none") +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) 

png("Figures/durability_cluster.png", height = 24, width = 24, units = "in", res = 750)
  durability.plot
dev.off()


# Full slope graph for demos --------------------------------------------------------

demo.labels <- c("Women","Men","Asian","Black","Hispanic/Latinx","Multiracial",
                 "White","Less than a 4-Year Degree","At Least a 4-Year Degree",
                 "Democrat","Republican","Independent","Other Political Affiliation")

names(demo.labels) <- c("women_score","men_score","asian_score","black_score",
                        "hisp_score","multi_score","white_score","less4_score",
                        "fourmore_score","dem_score","rep_score","ind_score",
                        "other_score")

temp.list <- lapply(c("women_score","men_score","asian_score","black_score",
                      "hisp_score","multi_score","white_score","less4_score",
                      "fourmore_score","dem_score","rep_score","ind_score",
                      "other_score"), function(i)
  
  image.data.comb %>%
    select(image_score_new, as.name(i), IMAGE_NUMBER) %>%
    arrange(-image_score_new) %>%
    add_column(images) %>%
    rename(., image = .) %>%
    mutate(color = ifelse(as.numeric(rownames(.)) <= 3, "top", "not"),
           color = ifelse(as.numeric(rownames(.)) >= 18, "bottom", color),
           order = 1:20) %>%
    arrange(get(i)) %>%
    mutate(order2 = 20:1,
           diff = image_score_new - get(i)) %>%
    arrange(-abs(diff)) %>%
    mutate(color = ifelse(as.numeric(rownames(.)) <= 3, "diff", color)) %>%
    melt(id.vars = c("IMAGE_NUMBER","color","image_score_new",i,"image","diff")) %>%
    ggplot(aes(x = variable, y = -value, group = IMAGE_NUMBER)) +
    geom_line(aes(color = color, alpha = color, size = color)) +
    geom_text_repel(data = . %>% filter(variable == "order"),
                    aes(label = scales::percent(round(image_score_new, 3))),
                    hjust = "left",
                    size = 3,
                    nudge_x = -.3) +
    # direction = "y") +
    geom_text_repel(data = . %>% filter(variable == "order2"),
                    aes(label = scales::percent(round(get(i), 3))),
                    hjust = "right",
                    size = 3,
                    nudge_x = .3) +
    # direction = "y") +
    geom_image(aes(image = image), size = .04, by = "height") +
    # scale_size_identity() +
    # geom_label(data = . %>% filter(variable == "order"),
    #            aes(label = round(image_score, 3)),
    #            size = 4,
    #            label.padding = unit(.05, "lines"),
    #            label.size = 0) + 
    # geom_label(data = . %>% filter(variable == "order2"),
    #            aes(label = round(k10.1, 3)),
    #            size = 4,
    #            label.padding = unit(.05, "lines"),
    #            label.size = 0) +
  labs(y = "", x = "", title = demo.labels[[i]]) +
    scale_color_manual(values = c("top" = "#1696d2",
                                  "bottom" = "#fdbf11",
                                  "diff" = "#db2b27",
                                  "not" = "gray75")) +
    scale_alpha_manual(values = c("top" = 1,
                                  "bottom" = 1,
                                  "diff" = 1,
                                  "not" = .3)) +
    scale_size_manual(values = c("top" = 2,
                                 "bottom" = 2,
                                 "diff" = 2,
                                 "not" = 1)) +
    guides(alpha = "none", color = "none", size = "none") +
    theme_void() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(face = "bold", hjust = .5),
          panel.background = element_rect(fill = "white", color = "white"))
) 

png("Figures/full_rankings_demos.png", res = 750, height = 16, width = 28, units = "in")
  ggpubr::ggarrange(plotlist = temp.list, align = "hv", ncol = 5, nrow = 3)
dev.off() 


# Durability slope graph for demos ----------------------------------------------------

rm(list=ls(pattern="table."))

for(i in c("women_score","men_score","asian_score","black_score",
                  "hisp_score","multi_score","white_score","less4_score",
                  "fourmore_score","dem_score","rep_score","ind_score",
                  "other_score")) {
  
  new_col_name <- paste0("order.", i)
  
  assign(paste0("table.", i), image.data.comb %>%
           select(image_score_new, as.name(i), IMAGE_NUMBER) %>%
           arrange(-image_score_new) %>%
           add_column(images) %>%
           rename(., image = .) %>%
           mutate(color = ifelse(as.numeric(rownames(.)) <= 3, "top", "not"),
                  color = ifelse(as.numeric(rownames(.)) >= 18, "bottom", color),
                  color = ifelse(as.numeric(rownames(.)) == 1, "toptop", color),
                  order = 1:20) %>%
           arrange(get(i)) %>%
           mutate(!!sym(new_col_name) := 20:1)
         
  )
  
}

list.all <- grep("table", names(.GlobalEnv), value = T)
list.all <- do.call("list", mget(list.all))

multi_inner <- Reduce(
  function(x, y, ...) merge(x, y, ...), 
  list.all
)


full.df <- melt(multi_inner, id.vars = c("IMAGE_NUMBER","image_score_new","image","color",
                                         c("women_score","men_score","asian_score","black_score",
                                           "hisp_score","multi_score","white_score","less4_score",
                                           "fourmore_score","dem_score","rep_score","ind_score",
                                           "other_score")))

image1.order <- full.df[full.df$IMAGE_NUMBER == "image1", c("variable","value")] %>%
  arrange(value) %>%
  pull(variable) %>%
  as.character()

durability.plot <- ggplot(full.df, aes(x = factor(variable, levels = image1.order), y = -value, group = IMAGE_NUMBER)) +
  geom_line(aes(color = color, alpha = color, size = color)) +
  geom_image(aes(image = image), size = .025, by = "height") +
  labs(y = "", x = "") +
  scale_color_manual(values = c("toptop" = "#c3cb71",
                                "top" = "#559e83",
                                "bottom" = "#ae5a41",
                                "not" = "gray75")) +
  scale_alpha_manual(values = c("toptop" = 1,
                                "top" = 1,
                                "bottom" = 1,
                                "not" = 0)) +
  scale_size_manual(values = c("toptop" = 2,
                               "top" = 2,
                               "bottom" = 2,
                               "not" = 0)) +
  guides(alpha = "none", color = "none", size = "none") +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) 

png("Figures/durability_demos.png", height = 24, width = 24, units = "in", res = 750)
  durability.plot
dev.off()


# Full slope graph for meanings --------------------------------------------------------

meaning.labels <- c("Chief Citizen","Chief Legislator","Chief of Party",
                 "Chief Executive","Chief of State","Commander in Chief",
                 "Chief Diplomat")

names(meaning.labels) <- c("citiz_score","leg_score","party_score","exec_score",
                        "state_score","command_score","diplo_score")

temp.list <- lapply(c("citiz_score","leg_score","party_score","exec_score",
                      "state_score","command_score","diplo_score"), function(i)
                        
                        image.data.comb %>%
                      select(image_score_new, as.name(i), IMAGE_NUMBER) %>%
                      arrange(-image_score_new) %>%
                      add_column(images) %>%
                      rename(., image = .) %>%
                      mutate(color = ifelse(as.numeric(rownames(.)) <= 3, "top", "not"),
                             color = ifelse(as.numeric(rownames(.)) >= 18, "bottom", color),
                             order = 1:20) %>%
                      arrange(get(i)) %>%
                      mutate(order2 = 20:1,
                             diff = image_score_new - get(i)) %>%
                      arrange(-abs(diff)) %>%
                      mutate(color = ifelse(as.numeric(rownames(.)) <= 3, "diff", color)) %>%
                      melt(id.vars = c("IMAGE_NUMBER","color","image_score_new",i,"image","diff")) %>%
                      ggplot(aes(x = variable, y = -value, group = IMAGE_NUMBER)) +
                      geom_line(aes(color = color, alpha = color, size = color)) +
                      geom_text_repel(data = . %>% filter(variable == "order"),
                                      aes(label = scales::percent(round(image_score_new, 3))),
                                      hjust = "left",
                                      size = 3,
                                      nudge_x = -.3) +
                      # direction = "y") +
                      geom_text_repel(data = . %>% filter(variable == "order2"),
                                      aes(label = scales::percent(round(get(i), 3))),
                                      hjust = "right",
                                      size = 3,
                                      nudge_x = .3) +
                      # direction = "y") +
                      geom_image(aes(image = image), size = .04, by = "height") +
                      # scale_size_identity() +
                      # geom_label(data = . %>% filter(variable == "order"),
                      #            aes(label = round(image_score, 3)),
                      #            size = 4,
                      #            label.padding = unit(.05, "lines"),
                      #            label.size = 0) + 
                      # geom_label(data = . %>% filter(variable == "order2"),
                      #            aes(label = round(k10.1, 3)),
                      #            size = 4,
                      #            label.padding = unit(.05, "lines"),
                      #            label.size = 0) +
                    labs(y = "", x = "", title = meaning.labels[[i]]) +
                      scale_color_manual(values = c("top" = "#1696d2",
                                                    "bottom" = "#fdbf11",
                                                    "diff" = "#db2b27",
                                                    "not" = "gray75")) +
                      scale_alpha_manual(values = c("top" = 1,
                                                    "bottom" = 1,
                                                    "diff" = 1,
                                                    "not" = .3)) +
                      scale_size_manual(values = c("top" = 2,
                                                   "bottom" = 2,
                                                   "diff" = 2,
                                                   "not" = 1)) +
                      guides(alpha = "none", color = "none", size = "none") +
                      theme_void() +
                      theme(axis.text = element_blank(),
                            axis.ticks = element_blank(),
                            plot.title = element_text(face = "bold", hjust = .5),
                            panel.background = element_rect(fill = "white", color = "white"))
) 

png("Figures/full_rankings_meaning.png", res = 750, height = 12, width = 28, units = "in")
  ggpubr::ggarrange(plotlist = temp.list, align = "hv", ncol = 5, nrow = 2)
dev.off() 


# Durability slope graph for meanings ----------------------------------------------------

rm(list=ls(pattern="table."))

for(i in c("citiz_score","leg_score","party_score","exec_score",
           "state_score","command_score","diplo_score")) {
  
  new_col_name <- paste0("order.", i)
  
  assign(paste0("table.", i), image.data.comb %>%
           select(image_score_new, as.name(i), IMAGE_NUMBER) %>%
           arrange(-image_score_new) %>%
           add_column(images) %>%
           rename(., image = .) %>%
           mutate(color = ifelse(as.numeric(rownames(.)) <= 3, "top", "not"),
                  color = ifelse(as.numeric(rownames(.)) >= 18, "bottom", color),
                  color = ifelse(as.numeric(rownames(.)) == 1, "toptop", color),
                  order = 1:20) %>%
           arrange(get(i)) %>%
           mutate(!!sym(new_col_name) := 20:1)
         
  )
  
}

list.all <- grep("table", names(.GlobalEnv), value = T)
list.all <- do.call("list", mget(list.all))

multi_inner <- Reduce(
  function(x, y, ...) merge(x, y, ...), 
  list.all
)


full.df <- melt(multi_inner, id.vars = c("IMAGE_NUMBER","image_score_new","image","color",
                                         c("citiz_score","leg_score","party_score","exec_score",
                                           "state_score","command_score","diplo_score")))

image1.order <- full.df[full.df$IMAGE_NUMBER == "image1", c("variable","value")] %>%
  arrange(value) %>%
  pull(variable) %>%
  as.character()

durability.plot <- ggplot(full.df, aes(x = factor(variable, levels = image1.order), y = -value, group = IMAGE_NUMBER)) +
  geom_line(aes(color = color, alpha = color, size = color)) +
  geom_image(aes(image = image), size = .025, by = "height") +
  labs(y = "", x = "") +
  scale_color_manual(values = c("toptop" = "#c3cb71",
                                "top" = "#559e83",
                                "bottom" = "#ae5a41",
                                "not" = "gray75")) +
  scale_alpha_manual(values = c("toptop" = 1,
                                "top" = 1,
                                "bottom" = 1,
                                "not" = 0)) +
  scale_size_manual(values = c("toptop" = 2,
                               "top" = 2,
                               "bottom" = 2,
                               "not" = 0)) +
  guides(alpha = "none", color = "none", size = "none") +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) 

png("Figures/durability_meaning.png", height = 24, width = 24, units = "in", res = 750)
  durability.plot
dev.off()

