install.packages("tidyverse")
library(tidyverse)

heart_data <- read_csv("heart_failure_clinical_records_dataset (1).csv")

# Here I checked for missing values by taking the sum of all the null values
sum(is.na(heart_data))

# This is where I removed duplicates
heart_data <- distinct(heart_data)

# Here I created a filtered out anyone that passed away
died_patients <- filter(heart_data, DEATH_EVENT == 1)

# 2. I used the arrange() function to sort the data by age
heart_data <- arrange(heart_data, desc(age))

# 4. Using mutate() I created a new column that determined if the person was in the young or old category (in terms of risk for heart diseas)
heart_data <- mutate(heart_data, age_group = ifelse(age < 60, "Young", "Old"))

# 5. Using the neww column I made above I found percentages of each age groups survial rate
survival_summary <- heart_data %>%
  group_by(age_group) %>%
  summarise(survival_rate = mean(DEATH_EVENT == 0))
print(survival_summary)

# Mean: average age of all patients
mean_age <- mean(heart_data$age)

# Median: the middle serum creatinine value when sorted
median_creatinine <- median(heart_data$serum_creatinine)

# Standard deviation: how much platelet counts vary from the average
sd_platelets <- sd(heart_data$platelets)

# Mode age of the dataset
mode_age <- heart_data %>%
  count(age) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(age)
print(mode_age)

# Hypothesis test: t-test for age difference by death event
# Null Hypothesis: There is no difference in the average age between patients who survived and those who died
# Alternative Hypothesis : There is a significant difference in average age between those who survived and those who died
alive <- filter(heart_data, DEATH_EVENT == 0)$age
dead <- filter(heart_data, DEATH_EVENT == 1)$age
t_test <- t.test(alive, dead)
print(t_test)

# Linear regression: I predicted the death event by age and the ejection fraction
model <- glm(DEATH_EVENT ~ age + ejection_fraction, data = heart_data, family = "binomial")
summary(model)

# Data Visualization
# Age Distribution
ggplot(heart_data, aes(x = age)) +
  geom_histogram() +
  labs(title = "Distribution of Age", x = "Age", y = "Count")

# 2. Ejection Fraction vs Age
ggplot(heart_data, aes(x = age, y = ejection_fraction, color = factor(DEATH_EVENT))) +
  geom_point() +
  labs(title = "Ejection Fraction vs Age", x = "Age", y = "Ejection Fraction", color = "Death Event")


# 3. Age Group Counts
ggplot(heart_data, aes(x = age_group, fill = factor(DEATH_EVENT))) +
  geom_bar(position = "dodge") +
  labs(title = "Death Event by Age Group", x = "Age Group", fill = "Death Event")
