# Program Name:  02_create_visualizations.R
# Author:        Ashwini Yermal Shanbhogue
# Date:          2026-02-01
# Purpose:       Create visualizations from pharmaverseadam::adae
# Input:         pharmaverseadam::adae
# Output:        AE Severity Distribution by Treatment.png, Top 10 Most Frequent Adverse Events.png
# Notes:

# Install and load required packages
# install.packages(c("pharmaverseadam", "admiral", "ggplot2", "dplyr", "scales", "logrx", "logrxaddin"))

library("pharmaverseadam")
library("ggplot2")
library("admiral")
library("dplyr", warn.conflicts = FALSE)
library("scales")
library("logrx")
library("logrxaddin")

# AE Severity Distribution by Treatment visualization
# Read ADAE data
ae <- pharmaverseadam::adae

# Subset required data
aesev <- ae %>% select(STUDYID, USUBJID, ACTARM, AESEV)

# Create stacked bar chart of AE Severity Distribution by Treatment
aesev_plot <- ggplot(aesev, aes(fill = AESEV, x = ACTARM)) +
  geom_bar(position = "stack", stat = "count") +
  ggtitle("AE Severity Distribution by Treatment") +
  xlab("Treatment Arm") +
  ylab("Count of AEs")

# Save created plot as PNG file
ggsave("question_03_tlg/output/AE Severity Distribution by Treatment.png", plot = aesev_plot)

# Top 10 Most Frequent Adverse Events visualization
# Select required columns from ae dataset
aeterm <- ae %>% select(STUDYID, USUBJID, AETERM)

# Count number of people affected by each AE and filter for top 10
num <- aeterm %>%
  group_by(AETERM) %>%
  summarize(count = n_distinct(USUBJID)) %>%
  arrange(desc(count)) %>%
  slice(1:10)

# Add column which shows total number of people affected by an AE (any AE)
fin_table <- num %>% mutate(n = n_distinct(aeterm$USUBJID))

# Calculate percentage and use binom.test() function to calculate the exact Clopper-Pearson confidence interval
ae_freq <- fin_table %>%
  rowwise() %>%
  mutate(
    test_result = list(binom.test(x = count, n = n, conf.level = 0.95)),
    percentage = round((count / n) * 100, digits = 2),
    LCI = round(test_result$conf.int[1] * 100, digits = 2),
    UCI = round(test_result$conf.int[2] * 100, digits = 2)
  ) %>%
  ungroup() %>%
  select(-test_result)

# Create dot plot of Top 10 Most Frequent Adverse Events with error bars
aefreq_plot <- ggplot(ae_freq, aes(x = reorder(AETERM, percentage), y = percentage)) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI),
    width = 0.2,
    color = "black"
  ) +
  geom_point(size = 4, color = "black") +
  coord_flip() +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = "n = 225 subjects; 95% Clopper- Pearson CIs",
    x = " ",
    y = "Percentage of Patients (%)"
  ) +
  theme(
    panel.background = element_rect(fill = "gray92"),
    panel.grid.major = element_line(color = "white"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  )

# View plot
aefreq_plot

# Save created plot as PNG file
ggsave("question_03_tlg/output/Top 10 Most Frequent Adverse Events.png", plot = aefreq_plot)
