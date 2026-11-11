
library(readxl)
library(tidyverse)

bagData <- read_excel(
  "/Users/olubunmi/Downloads/Langara_Fall2026_Synthetic_Students_1000.xlsx"
)

# Check the data
glimpse(bagData)

# Summarize convenience by carry style
summaryTable <- bagData %>%
  group_by(CarryStyle) %>%
  summarise(
    N = sum(!is.na(ConvenienceRating)),
    MeanConvenience = mean(ConvenienceRating, na.rm = TRUE),
    MedianConvenience = median(ConvenienceRating, na.rm = TRUE),
    Percent4or5 = mean(
      ConvenienceRating >= 4, na.rm = TRUE
    ) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(MeanConvenience))

print(summaryTable)


#Percentage Rating Convenience 4 or 5
highConvenience <- bagData %>%
  group_by(CarryStyle) %>%
  summarise(
    Percent4or5 =
      mean(ConvenienceRating >= 4) * 100
  )

highConvenience

#Percentage who prefer front carry
frontPreference <- bagData %>%
  group_by(CarryStyle) %>%
  summarise(
    PercentPreferFront =
      mean(PreferFrontCarry == "Yes") * 100
  )

frontPreference

#Summary Table
summaryTable <- bagData %>%
  group_by(CarryStyle) %>%
  summarise(
    MeanConvenience = mean(ConvenienceRating),
    MedianConvenience = median(ConvenienceRating),
    Percent4or5 = mean(ConvenienceRating >= 4) * 100,
    PercentPreferFront = mean(PreferFrontCarry == "Yes") * 100,
    N = n()
  )

summaryTable

#BarChart of percentage of students rating 4 0r 5 in the various carry styles

highConvenience <- bagData %>%
  filter(!is.na(ConvenienceRating)) %>%
  group_by(CarryStyle) %>%
  summarise(
    Percent4or5 = mean(ConvenienceRating >= 4) * 100,
    N = n(),
    .groups = "drop"
  )

ggplot(
  highConvenience,
  aes(
    x = reorder(CarryStyle, Percent4or5),
    y = Percent4or5,
    fill = CarryStyle
  )
) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f%%\n(n = %d)", Percent4or5, N)),
    vjust = -0.4,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    title = "High Convenience Ratings by Carry Style",
    subtitle = "Percentage of students rating convenience 4 or 5",
    x = "Carry Style",
    y = "Students (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# Check all column names
names(bagData)

# Inspect the relevant variables
glimpse(bagData)


library(tidyverse)

commuteSummary <- bagData %>%
  filter(
    !is.na(CommuteTimeMin),
    !is.na(CarryStyle),
    !is.na(CommuteMode)
  ) %>%
  group_by(CommuteMode, CarryStyle) %>%
  summarise(
    MeanTime = mean(CommuteTimeMin),
    MedianTime = median(CommuteTimeMin),
    N = n(),
    .groups = "drop"
  ) %>%
  arrange(CommuteMode, MeanTime)

print(commuteSummary, n = Inf)


ggplot(
  commuteSummary,
  aes(
    x = CommuteMode,
    y = MeanTime,
    fill = CarryStyle
  )
) +
  geom_col(position = position_dodge(width = 0.8)) +
  labs(
    title = "Commute Time by Carry Style and Commute Mode",
    subtitle = "Comparing mean commute duration across student groups",
    x = "Means of Commute",
    y = "Mean Commute Time (minutes)",
    fill = "Carry Style"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )



# Remove rows with missing values in the variables used
commuteData <- bagData %>%
  filter(
    !is.na(CommuteTimeMin),
    !is.na(CarryStyle),
    !is.na(CommuteMode),
    !is.na(CommuteLengthKm)
  ) %>%
  mutate(
    CarryStyle = factor(CarryStyle),
    CommuteMode = factor(CommuteMode)
  )

# Model without interaction
model1 <- lm(
  CommuteTimeMin ~ CarryStyle + CommuteMode +
    CommuteLengthKm,
  data = commuteData
)

# Model with interaction
model2 <- lm(
  CommuteTimeMin ~ CarryStyle * CommuteMode +
    CommuteLengthKm,
  data = commuteData
)

# Test the interaction
anova(model1, model2)



# Install once if you have not installed this package
install.packages("emmeans")

library(emmeans)

# Calculate adjusted mean commute times
adjustedMeans <- emmeans(
  model2,
  ~ CarryStyle | CommuteMode
)

# Display adjusted means for each commute mode
summary(adjustedMeans)

# Compare carry styles within each commute mode
pairwiseResults <- pairs(
  adjustedMeans,
  adjust = "tukey"
)

summary(pairwiseResults)


library(emmeans)
library(ggplot2)
library(tidyverse)

# Adjusted mean commute times from your fitted model
adjustedMeans <- emmeans(
  model2,
  ~ CarryStyle | CommuteMode
)

# Obtain confidence intervals
plotData <- as.data.frame(confint(adjustedMeans))

# Create the graph
ggplot(
  plotData,
  aes(x = CarryStyle, y = emmean)
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.15
  ) +
  facet_wrap(~ CommuteMode, scales = "free_y") +
  labs(
    title = "Commute Time by Carry Style and Commute Mode",
   # subtitle = "Means adjusted for commute distance; error bars show 95% confidence intervals",
    x = "Carry Style",
    y = "Adjusted Mean Commute Time (minutes)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(face = "bold")
  )


library(emmeans)
library(ggplot2)
library(tidyverse)

# Calculate adjusted means and confidence intervals
adjustedMeans <- emmeans(
  model2,
  ~ CarryStyle | CommuteMode
)

plotData <- as.data.frame(confint(adjustedMeans))

# Create the graph with numbers displayed
ggplot(plotData, aes(x = CarryStyle, y = emmean)) +
  geom_point(size = 3) +
  geom_text(
    aes(label = round(emmean, 1)),
    vjust = -1,
    size = 3.5
  ) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.15
  ) +
  facet_wrap(~ CommuteMode, scales = "free_y") +
  labs(
    title = "Adjusted Commute Time by Carry Style and Commute Mode",
    subtitle = "Numbers show estimated commute time in minutes",
    x = "Carry Style",
    y = "Adjusted Mean Commute Time (minutes)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(face = "bold")
  )


library(tidyverse)
library(ggplot2)

# Summarise convenience ratings by bag type and commute mode
bagConvenience <- bagData %>%
  filter(
    !is.na(BagType),
    !is.na(CommuteMode),
    !is.na(ConvenienceRating)
  ) %>%
  group_by(CommuteMode, BagType) %>%
  summarise(
    MeanConvenience = mean(ConvenienceRating),
    MedianConvenience = median(ConvenienceRating),
    NumberOfStudents = n(),
    .groups = "drop"
  ) %>%
  arrange(CommuteMode, desc(MeanConvenience))

# Display all results
print(bagConvenience, n = Inf)


ggplot(
  bagConvenience,
  aes(x = BagType, y = MeanConvenience)
) +
  geom_col() +
  geom_text(
    aes(label = round(MeanConvenience, 2)),
    vjust = -0.4,
    size = 3.5
  ) +
  facet_wrap(~ CommuteMode) +
  labs(
    title = "Bag Convenience by Commute Mode",
    subtitle = "Numbers above bars show average convenience ratings",
    x = "Bag Type",
    y = "Average Convenience Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  )


mostConvenientBag <- bagConvenience %>%
  group_by(CommuteMode) %>%
  slice_max(
    order_by = MeanConvenience,
    n = 1,
    with_ties = TRUE
  ) %>%
  ungroup()

print(mostConvenientBag, n = Inf)




ggplot(
  bagConvenience,
  aes(x = BagType, y = MeanConvenience, fill = BagType)
) +
  geom_col() +
  geom_text(
    aes(label = sprintf("%.2f", MeanConvenience)),
    vjust = -0.5,
    size = 3.5
  ) +
  facet_wrap(~ CommuteMode) +
  scale_fill_manual(
    values = c(
      "Backpack" = "#4E79A7",
      "Crossbody" = "#F28E2B",
      "Tote" = "#59A14F",
      "Sling" = "#E15759",
      "Other" = "#B07AA1"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Average Bag Convenience by Commute Mode",
    subtitle = "Average convenience rating for each bag type",
    x = "Bag Type",
    y = "Mean Convenience Rating",
    fill = "Bag Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

