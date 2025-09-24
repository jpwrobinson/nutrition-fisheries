

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/gdd"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/nutrition/global_intake_inadequacies/data/gdd/processed/GDD_2018_intakes_national.Rds")


# Build data
################################################################################

food_stats <- data_orig %>% 
  filter(factor_type=="Foods") %>% 
  group_by(factor) %>% 
  summarize(volume=sum(supply_med, na.rm=T)) %>% 
  ungroup() %>% 
  pull(factor)

# 
sort(unique(data$factor))


# Build data
data <- data_orig %>% 
  # Filter
  filter(sex=="Both sexes" & age_range=="All ages" & residence == "All residences" & education == "All education levels") %>% 
  filter(factor_type=="Foods") %>% 
  # Add sum of supplies
  group_by(country, iso3) %>% 
  mutate(total=sum(supply_med, na.rm=T),
         prop=supply_med/sum(supply_med),
         seafood=prop[factor=="Total seafoods"]) %>% 
  ungroup() %>% 
  # Format some foods
  mutate(factor=recode(factor,
                       "Yoghurt (including fermented milk)"="Yoghurt",
                       "Total seafoods"="Seafoods",
                       "Total processed meats"="Processed meats")) %>% 
  # Shorten some countries
  mutate(country=recode(country,
                        "Micronesia (Federated States of)"="Micronesia")) %>% 
  # Rename
  rename(food=factor)

# Food order
food_stats <- data %>% 
  group_by(food) %>% 
  summarize(volume=sum(supply_med, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(volume) %>% 
  pull(food) %>% rev()
foods <- c("Seafoods", food_stats[food_stats!="Seafoods"])

# Order data
data_ordered <- data %>% 
  mutate(food=factor(food, levels=foods))

# Subset data
freeR::uniq(data$region)
data1 <- data_ordered %>% 
  filter(region %in% c("East & Southeast Asia", "South Asia", "Latin America & Caribbean"))
data2 <- data_ordered %>% 
  filter(region %in% c("Former Soviet Union", "High-Income Countries"))
data3 <- data_ordered %>% 
  filter(region %in% c( "Sub-Saharan Africa", "Middle East & North Africa"))




# Build data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   plot.tag=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data1, aes(y=reorder(country, desc(seafood)), x=prop, fill=food)) +
  facet_grid(region~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE), color="grey30", lwd=0.1) +
  # Labels
  labs(x="Intake", y="", tag="A") +
  scale_x_continuous(breaks=c(0,0.5,1.0), labels = scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Food type", values=c(RColorBrewer::brewer.pal(9, "Blues")[7], 
                                               freeR::colorpal(RColorBrewer::brewer.pal(9, "YlOrBr"), 14))) +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = "none")
g1

# Plot data
g2 <- ggplot(data2, aes(y=reorder(country, desc(seafood)), x=prop, fill=food)) +
  facet_grid(region~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE), color="grey30", lwd=0.1) +
  # Labels
  labs(x="Intake", y="", tag="B") +
  scale_x_continuous(breaks=c(0,0.5,1.0), labels = scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Food type", values=c(RColorBrewer::brewer.pal(9, "Blues")[7], 
                                               freeR::colorpal(RColorBrewer::brewer.pal(9, "YlOrBr"), 14))) +  # Theme
  theme_bw() +  my_theme +
  theme(legend.position = "none")
g2

# Plot data
g3 <- ggplot(data3, aes(y=reorder(country, desc(seafood)), x=prop, fill=food)) +
  facet_grid(region~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE), color="grey30", lwd=0.1) +
  # Labels
  labs(x="Intake", y="", tag="C") +
  scale_x_continuous(breaks=c(0,0.5,1.0), labels = scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Food type", values=c(RColorBrewer::brewer.pal(9, "Blues")[7], 
                                               freeR::colorpal(RColorBrewer::brewer.pal(9, "YlOrBr"), 14))) +  # Theme
  theme_bw() +  my_theme +
  theme(legend.position = "right",
        legend.key.size = unit(0.2, "cm"))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1,
                             widths=c(0.27, 0.27, 1-(0.27*2)))


ggsave(g, filename=file.path(datadir, "daily_intakes.png"), 
       width=8.5, height=5.5, units="in", dpi=600)


