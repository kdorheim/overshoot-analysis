# Generate the exisiting t/dt maps, I need to add the additional data to the 
# plot.... 


library(dplyr)
library(ggplot2)

theme_set(theme_bw())

new_df <- read.csv("data/new_matching_archive.csv")
  
new_df %>% 
  filter(experiment %in% c("ssp534-over")) -> 
  new_df

new_df %>%  
  group_by(model) %>% 
  summarise(counts = n_distinct(ensemble)) %>% 
  arrange(desc(counts)) %>% 
  pull(model)




stitches_df <- read.csv("data/stitiches-data/matching_archive.csv") %>% 
  filter(model %in% new_df$model)



exps <- setdiff(unique(new_df$experiment), "historical")
color_palette  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                    "#CC79A7", "#0072B2", "#D55E00", "#CC79A7")
names(color_palette) <- exps
color_palette <- color_palette[1:length(exps)]


WRITE_TO <- here::here("figs")


scns <- c("ssp126", "ssp245", "ssp370", "ssp585", "ssp119", "ssp434", "ssp460")      

stitches_df %>% 
  filter(experiment %in% scns) -> 
  stitches_df
  
split(x = stitches_df, f = stitches_df$model, drop = TRUE) %>% 
  lapply(function(stitches_df){

    m <- unique(stitches_df$model)
    
    new_df %>% 
      filter(model %in% m) %>% 
      filter(experiment != "historical") -> 
      new_to_plot
    
    stitches_df %>% 
      filter(model == model) %>% 
      filter(experiment %in% scns) %>% 
      select(fx, dx) %>% 
      distinct() -> 
      stitches_d_to_plot
    
    ggplot() + 
      geom_point(data = stitches_d_to_plot, 
                 aes(fx, 9 * dx, color = "stitches"), 
                 alpha = 0.5, shape = 19) + 
      geom_point(data = new_to_plot, 
                aes(fx, 9 * dx, color = experiment)) +
      labs(title = m, 
           x = "median temperature anomaly per window (C)", 
           y = "change in temperature anomaly per window (C)")  + 
      scale_color_manual(values = c(stitches = "grey", color_palette)) -> 
      fig ; fig
    ofile <- file.path(WRITE_TO, "over-shoot_only", paste0(m, "-temp_space-overshootOnly.png"))
    ggsave(filename = ofile, plot = fig, width = 8, height = 6)
    
  })


# New Data non SSP534 Over Shoot Runs 



new_df <- read.csv("data/new_matching_archive.csv")

new_df %>% 
  filter(experiment %in% c("ssp534-over")) %>%
  pull(model) %>% 
  unique() -> 
  mod

new_df <- read.csv("data/new_matching_archive.csv") %>% 
  filter(model %in% mod)



new_df %>% 
  group_by(model, experiment) %>%  
  filter(model ==  "UKESM1-0-LL") %>% 
  summarise(count = n_distinct(ensemble)) %>% 
View()






stitches_df <- read.csv("data/stitiches-data/matching_archive.csv") %>% 
  filter(model %in% new_df$model)



exps <- setdiff(unique(new_df$experiment), "historical")
color_palette  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                    "#CC79A7", "#0072B2", "#D55E00", "#CC79A7")
names(color_palette) <- exps
color_palette <- color_palette[1:length(exps)]


WRITE_TO <- here::here("figs")


scns <- c("ssp126", "ssp245", "ssp370", "ssp585", "ssp119", "ssp434", "ssp460")      

stitches_df %>% 
  filter(experiment %in% scns) -> 
  stitches_df

split(x = stitches_df, f = stitches_df$model, drop = TRUE) %>% 
  lapply(function(stitches_df){
    
    m <- unique(stitches_df$model)
    
    new_df %>% 
      filter(model %in% m) %>% 
      filter(experiment != "historical") -> 
      new_to_plot
    
    stitches_df %>% 
      filter(model == model) %>% 
      filter(experiment %in% scns) %>% 
      select(fx, dx) %>% 
      distinct() -> 
      stitches_d_to_plot
    
    ggplot() + 
      geom_point(data = stitches_d_to_plot, 
                 aes(fx, 9 * dx, color = "stitches"), 
                 alpha = 0.5, shape = 19) + 
      geom_point(data = new_to_plot, 
                 aes(fx, 9 * dx, color = experiment)) +
      labs(title = m, 
           x = "median temperature anomaly per window (C)", 
           y = "change in temperature anomaly per window (C)")  + 
      scale_color_manual(values = c(stitches = "grey", color_palette)) -> 
      fig ; fig
    ofile <- file.path(WRITE_TO, "new_data", paste0(m, "-temp_space-new.png"))
    ggsave(filename = ofile, plot = fig, width = 8, height = 6)
    
  })

new_df %>% 
  filter(experiment != "historical") -> 
  new_df


ggplot() + 
  geom_point(data = stitches_df, 
             aes(fx, 9 * dx, color = "stitches"), 
             alpha = 0.5, shape = 19) + 
  geom_point(data = new_df,  aes(fx, 9 * dx, color = experiment), 
            shape = 19) +
  scale_color_manual(values = c(stitches = "grey", color_palette)) + 
  labs(
       x = "median temperature anomaly per window (C)", 
       y = "change in temperature anomaly per window (C)")  + 
  scale_color_manual(values = c(stitches = "grey", color_palette)) -> 
  fig

ofile <- file.path(WRITE_TO, paste0("temp_space-new.png"))
ggsave(filename = ofile, plot = fig, width = 8, height = 6)

