# Histogram of rebate received vs. home assessed value

# Where will the breaks be in assessed value to create bins
breaks_val <- c(-Inf, 5e4, 1e5, 1.25e5, 1.5e5, 1.75e5, 2e5, 2.25e5, 2.5e5, 2.75e5, 3e5, 3.5e5, 4e5, 5e5, 7.5e5, Inf)

# Average rebate received vs. assessed value, including participants and non-participants
hd <- rd %>% 
  group_by(id) %>% 
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue),
            total_rebate = mean(total_paid_inc)) %>% 
  replace_na(list(total_rebate=0)) %>% 
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_total_rebate=mean(total_rebate), 
            sd_total_rebate = sd(total_rebate),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_total_rebate = sd_total_rebate / sqrt(count))


ggplot(hd, 
       aes(x=avg_value,
           y=mean_total_rebate,
           ymin = mean_total_rebate - 1.96*se_total_rebate,
           ymax = mean_total_rebate + 1.96*se_total_rebate)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Average rebate claimed", labels=scales::dollar_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0)
ggsave("../output_figures_tables/rebate_vs_assessvalue_all.png", width=6, height=6)

# Program participation vs. assessed value
hd <- rd %>% 
  group_by(id) %>% 
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue),
            participant = mean(treated)) %>% 
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_participate=mean(participant), 
            sd_participate = sd(participant),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_participate = sd_participate / sqrt(count))


ggplot(hd, 
       aes(x=avg_value,
           y=mean_participate,
           ymin = mean_participate - 1.96*se_participate,
           ymax = mean_participate + 1.96*se_participate)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Participation rate", labels=scales::percent_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0)
ggsave("../output_figures_tables/participate_vs_assessvalue_all.png", width=6, height=6)

# Rebate vs. assessed value conditional on participation
hd <- rd %>% 
  filter(treated == TRUE) %>%
  group_by(id) %>% 
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue),
            total_rebate = mean(total_paid_inc)) %>% 
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_total_rebate=mean(total_rebate, na.rm=T), 
            sd_total_rebate = sd(total_rebate, na.rm=T),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_total_rebate = sd_total_rebate / sqrt(count))


ggplot(hd, 
       aes(x=avg_value,
           y=mean_total_rebate,
           ymin = mean_total_rebate - 1.96*se_total_rebate,
           ymax = mean_total_rebate + 1.96*se_total_rebate)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Average rebate claimed", labels=scales::dollar_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0)
ggsave("../output_figures_tables/rebate_vs_assessvalue_participantsonly.png", width=6, height=6)

# Histogram of assessed value
hd <- rd %>% 
  group_by(id) %>% 
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue)) %>% 
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) %>% 
  group_by(assess_bin) %>% 
  summarise(avg_value = mean(TotalAssesmentValue),
            Count = n()) 

ggplot(hd,
       aes(x=avg_value,
           y=Count)) +
  geom_col() +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  theme_bw()
ggsave("../output_figures_tables/count_vs_assessvalue_all.png", width=6, height=6)