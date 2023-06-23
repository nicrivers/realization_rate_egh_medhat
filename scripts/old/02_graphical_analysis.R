# This file produces summary statistics and motivating graphical analysis

plot_data_aggregate <- rd %>%
  group_by(year, treated) %>%
  # Remove NA observations
  summarise(elec = mean(elec, na.rm=T),
            gas = mean(gas, na.rm=T),
            count = n()) %>%
  pivot_longer(cols=c("gas","elec"), names_to="energy_type") %>% 
  # I added this to allow scales to be the same
  # can just make it index=log(value) to revert
  group_by(energy_type) %>% 
  mutate(index = log(value)/first(log(value))) 

counts <- rd %>%
  filter(treated==TRUE) %>%
  pivot_longer(cols=c("gas","elec"), names_to="energy_type") %>%
  group_by(year, energy_type, post) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))


p1 <- ggplot(plot_data_aggregate, aes(x=year, y=index, colour=treated)) +
  geom_line() +
  geom_point() +
  facet_wrap(~energy_type) +
  labs(x=NULL) +
  theme_minimal() +
  scale_colour_brewer(palette="Set1") +
  labs(x=NULL,
       y="Index of log consumption")
p1_dims <- get_dim(p1)

p2 <- ggplot(counts, aes(x=year, y=perc, fill=post)) +
  geom_col() +
  facet_wrap(~energy_type) +
  theme_minimal() +
  scale_fill_brewer(palette="Set1") +
  labs(x=NULL,
       y="Percent of \nparticipants treated") +
  scale_y_continuous(labels=scales::percent_format())
  
p2_aligned <- set_dim(p2, p1_dims)

p1 + p2 + plot_layout(ncol=1, nrow=2, heights=c(2,1))
ggsave(p1 + p2 + plot_layout(ncol=1, nrow=2, heights=c(2,1)), filename = "../output_figures_tables/aggregate_trend_graph.png", width=6, height=6)


