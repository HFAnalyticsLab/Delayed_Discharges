
# 05/08/2025 - Discharge Ready Data (DRD) Trust-level descriptive analysis

# Trust-level descriptives ####################################################

# Number of trusts reporting data for all periods

cols_to_check <- c("patients_discharged_volume", "average_delay_los_minus_0_day_delay")  

trust_completeness <- dd_file_acute_trusts_FINAL %>%
  group_by(org_code, month) %>%
  summarise(
    row_has_data = any(rowSums(!is.na(across(all_of(cols_to_check)))) > 0),
    .groups = "drop"
  ) %>%
  group_by(org_code) %>%
  summarise(
    months_with_data = sum(row_has_data),
    total_months     = n_distinct(dd_file_acute_trusts_FINAL$month),
    complete         = months_with_data == total_months
  )

sum(trust_completeness$complete == TRUE)

# 1 Variation by trust in change of the % of discharges delayed ###############

variation_delayed_plot <- ggplot(data = figure_9_data, aes(x = reorder(org_code, difference), y = as.numeric(difference))) +
  geom_bar(stat = "identity", fill = "firebrick2", color = "black", linewidth = 0.3) +
  labs(
    title = "Absolute Difference in % Admissions with Delay between Apr-May 2025 and Apr-May 2024",
    x = "NHS Trust",
    y = "Absolute Difference in % Admissions with Delay") +
  scale_y_continuous(
    limits = c(-10, 20),        
    breaks = seq(-10, 20, 2),
    labels = label_percent(scale = 1)) +
  theme(axis.text.x = element_blank())

variation_delayed_plot
ggsave("variation delayed.png", plot = variation_delayed_plot, width = 8, height = 6, dpi = 300)

# 2 Variation by trust in change in average delay length ######################

variation_delay_plot <- ggplot(data = figure_10_data, aes(x = reorder(org_code, difference), y = as.numeric(difference))) +
  geom_bar(stat = "identity", fill = "firebrick2", color = "black", linewidth = 0.3) +
  labs(
    title = "Difference in Average Delay Length between Apr-May 2025 vs Apr-May 2024",
    x = "NHS Trust",
    y = "Difference in Delayed LoS (Days)") +
  scale_y_continuous(
    limits = c(-8, 8),        
    breaks = seq(-8, 8, 2),
    labels = label_percent(scale = 1)) +
  theme(axis.text.x = element_blank())

variation_delay_plot
ggsave("variation delay.png", plot = variation_delay_plot, width = 8, height = 6, dpi = 300)

# 3 Variation by trust in change in the % of bed days used by delayed discharge ####

variation_bed_days_plot <- ggplot(data = figure_11_data, aes(x = reorder(org_code, difference), y = as.numeric(difference))) +
  geom_bar(stat = "identity", fill = "firebrick2", color = "black", linewidth = 0.3) +
  labs(
    title = "Absolute Diff. in % of Bed Days used for DD between Apr-May 2025 vs Apr-May 2024",
    x = "NHS Trust",
    y = "Absolute Difference in % of Bed Days used for Delayed Discharges") +
  scale_y_continuous(
    limits = c(-10, 14),        
    breaks = seq(-10, 14, 2),
    labels = label_percent(scale = 1)) +
  theme(axis.text.x = element_blank())

variation_bed_days_plot
ggsave("variation bed days.png", plot = variation_bed_days_plot, width = 8, height = 6, dpi = 300)

# 4 Ranking trusts by the % of bed days used by delayed discharge ############

test <- figure_12_data %>% 
  filter(difference <= -2.5) %>%
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

bed_days_ranking_plot <- ggplot(test, aes(x = period, y = rank, group = org_code)) +
  geom_line(aes(color = org_code), linewidth = 1.2) +
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
  scale_x_discrete(
    labels = c ("Pre"="Apr-Jun 2024", "Post"="Apr-Jun 2025"),
    expand = expansion(add = c(0.5, 0.5))) +
  labs(
    title = "Top 10 most improved trusts by % bed days used for delayed discharge, Apr-Jun 2024 vs Apr-Jun 2025",
    x = NULL,
    y = "Rank"
  ) +
  theme_minimal(base_size = 14) +
  theme_gray() +
  theme(axis.text.x = element_text(face = "bold"),
    legend.position = "none")

bed_days_ranking_plot
ggsave("ranking bed daysJun.png", plot = bed_days_ranking_plot, width = 10, height = 6, dpi = 300)

# 5 Most improved trusts selection ###############################################

seq(10, 110, by = 10) %>% c(1)

bed_days_ranking_quant_plot <- ggplot(data = figure_12_data, aes(x = reorder(org_code, difference), y = as.numeric(difference))) +
  geom_bar(stat = "identity", fill = "firebrick2", width = 0.5, color = "black", linewidth = 0.3) +
  geom_vline(aes(xintercept=quantile
                 (rank_change, 0.09)),
                  color=I("blue"),
                  linetype="solid",
                  linewidth= 1,
                  show.legend = T) +
  labs(
    title = "Absolute Diff. in % of Bed Days used for DD between Apr-June 2024 vs Apr-June 2025",
    x = "NHS Trust",
    y = "Absolute Difference in % of Bed Days used for Delayed Discharges") +
  scale_y_continuous(
    limits = c(-10, 14),        
    breaks = seq(-10, 14, 2),
    labels = label_percent(scale = 1)) +
  theme(axis.text.x = element_blank())

bed_days_ranking_quant_plot
ggsave("most improved trusts selection.png", plot = bed_days_ranking_quant_plot, width = 8, height = 6, dpi = 300)

# Greater than 3% reduction (6 trusts)
greater_than_3p_reduction <- figure_12_data %>%
  select(org_code,difference) %>%
  filter(difference <= -3)

# Greater than 2.5% reduction (10 trusts)
greater_than_2.5p_reduction <- figure_12_data %>%
  select(org_code,difference) %>%
  filter(difference <= -2.5)

# Greater than 2% reduction (14 trusts)
greater_than_2p_reduction <- figure_12_data %>%
  select(org_code,difference) %>%
  filter(difference <= -2)


# 6 Ranking trusts by the % of discharges that are delayed #####################

test2 <- figure_13_data %>% 
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

bed_days_ranking_plot <- ggplot(figure, aes(x = period, y = rank, group = org_code)) +
  geom_line(aes(color = org_code), linewidth = 1.2) +
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
    title = "Top 10 most improved trusts by % bed days used for delayed discharge",
    x = NULL,
    y = "Rank"
  ) +
  theme_minimal(base_size = 14) +
  theme_gray() +
  theme(axis.text.x = element_text(face = "bold"),
        legend.position = "none")


