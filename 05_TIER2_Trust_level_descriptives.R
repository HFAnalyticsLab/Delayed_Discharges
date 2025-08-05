
# 05/08/2025 - Discharge Ready Data (DRD) Trust-level descriptive analysis

# Trust-level descriptives ####################################################

# 1 Variation by trust in change of the % of discharges delayed ###############

ggplot(data = figure_9_data, aes(x = reorder(org_code, difference), y = as.numeric(difference))) +
  geom_bar(stat = "identity", fill = "firebrick2", color = "black", linewidth = 0.3) +
  labs(
    title = "Absolute Difference in % Admissions with Delay between Apr-May 2025 and Apr-May 2024",
    x = "NHS Trust",
    y = "Absolute Difference in % Admissions with Delay"
  ) +
  theme(axis.text.x = element_blank())

# 2 Variation by trust in change in average delay length ######################

ggplot(data = figure_10_data, aes(x = reorder(org_code, difference), y = as.numeric(difference))) +
  geom_bar(stat = "identity", fill = "firebrick2", color = "black", linewidth = 0.3) +
  labs(
    title = "Difference in Average Delay Length between Apr-May 2025 vs Apr-May 2024",
    x = "NHS Trust",
    y = "Difference in Delayed LoS (Days)"
  ) +
  theme(axis.text.x = element_blank())

# 3 Variation by trust in change in the % of bed days used by delayed discharge ####

ggplot(data = figure_11_data, aes(x = reorder(org_code, difference), y = as.numeric(difference))) +
  geom_bar(stat = "identity", fill = "firebrick2", color = "black", linewidth = 0.3) +
  labs(
    title = "Absolute Diff. in % of Bed Days used for DD between Apr-May 2025 vs Apr-May 2024",
    x = "NHS Trust",
    y = "Absolute Difference in % of Bed Days used for Delayed Discharges"
  ) +
  theme(axis.text.x = element_blank())

# 4 Ranking trusts by the % of bed days used by delayed discharge ############

test <- figure_12_data %>% 
  filter(rank_change <= 10) %>%
  pivot_longer(
    cols = c(rank_pre, rank_post),
    names_to = "period",
    values_to = "rank"
  ) %>%
  mutate(
    period = ifelse(period == "rank_pre", "Pre", "Post")
  ) %>% 
  mutate(
    period = recode(period, rank_pre = "Pre", rank_post = "Post"),
    period = factor(period, levels = c("Pre", "Post"))  # ensure correct order
  ) %>% 
  select(org_code,period,rank)

ggplot(test, aes(x = period, y = rank, group = org_code)) +
  geom_line(aes(color = org_code), size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = org_code), 
            data = test %>% filter(period == "Pre"), 
            hjust = 1.2, size = 3) +
  geom_text(aes(label = org_code), 
            data = test %>% filter(period == "Post"), 
            hjust = -0.2, size = 3) +
  scale_y_reverse(
    breaks = seq(10, 110, by = 10) %>% c(1),
    limits = c(110, 1)
  ) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  labs(
    title = "Top 10 Best Improved Trusts by % bed days used for delayed discharge",
    x = NULL,
    y = "Rank"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(face = "bold"),
    legend.position = "none"
  )
