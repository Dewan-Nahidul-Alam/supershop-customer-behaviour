# Installing and loading necessary packages for generating your unique data.
packages <- c("dplyr", "tidyr", "tidyverse")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("readr")
install.packages("skimr")
install.packages("factoextra")
install.packages("cluster")

library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(skimr)
library(factoextra)
library(cluster)


# 1. Loading the master dataset
master_df <- read_csv("customers_master_student.csv", show_col_types = FALSE)

# 2. Function to generate unique sample
make_student_sample <- function(master_df, student_id, n_sample = 5000) {
  seed_val <- suppressWarnings(as.integer(student_id))
  if (is.na(seed_val)) stop("student_id must be numeric!")
  if (n_sample > nrow(master_df)) stop("n_sample too large.")
  set.seed(seed_val %% 1000000)
  master_df |> slice_sample(n = n_sample)
}

# 3. Generating unique data
my_data <- make_student_sample(master_df, student_id = 108532043, n_sample = 5000)
# Save that data to a physical CSV file in working directory
readr::write_csv(my_data, "my_data.csv")

# 4. Checking data
glimpse(my_data)
head(my_data)1



#####################################################################################
# TASK 1
#####################################################################################

# Check structure and data types
str(my_data)

# Summary statistics to identify NAs and Scale issues
summary(my_data)

skim(my_data)



#####################################################################################
# TASK 2
#####################################################################################

# most_preferred_channel column based on the highest value among the three shares
my_data <- my_data |> 
  rowwise() |> 
  mutate(most_preferred_channel = case_when(
    online_share >= store_share & online_share >= app_share ~ "Online",
    store_share >= online_share & store_share >= app_share ~ "Store",
    app_share >= online_share & app_share >= store_share ~ "App",
    TRUE ~ "Mixed" # Handle edge cases (though rare with decimals)
  )) |>
  ungroup()

# Making membership_tier an ordered factor for better visualization
my_data$membership_tier <- factor(my_data$membership_tier, 
                                  levels = c("None", "Silver", "Gold", "Platinum"))

# 1. Last 12 months Order Number by Membership Tier
plot1 <- ggplot(my_data, aes(x = membership_tier, y = orders_12m, fill = membership_tier)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Last 12 months Order Number by Membership Tier",
       x = "Membership Tier", y = "Number of Orders in L12M") +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
plot1

# 2. Most preferred channel vs Return rate
tier_returns <- my_data |> 
  group_by(most_preferred_channel) |> 
  summarise(avg_return_rate = mean(return_rate, na.rm = TRUE))

plot2 <- ggplot(tier_returns, aes(x = most_preferred_channel, y = avg_return_rate, fill = most_preferred_channel)) +
          geom_bar(stat = "identity", alpha = 0.8, color = "black") +
          theme_minimal() +
          labs(title = "Average Return Rate by Most Preferred Channel",
               x = "Most Preferred Channel", 
               y = "Mean Return Rate") +
          theme(legend.position = "none",
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                axis.title = element_text(size = 16),
                axis.text = element_text(size = 14))
plot2

# 3. Average Delivery Delay by Region
plot3 <- my_data |> 
  filter(!is.na(delivery_delay_mean)) |> 
  group_by(region) |> 
  summarise(avg_delay = mean(delivery_delay_mean)) |> 
  ggplot(aes(x = region, y = avg_delay, group = 1)) +
  geom_line(color = "grey") +
  geom_point(size = 4, color = "darkgreen") +
  theme_light() +
  labs(title = "Average Delivery Delay by Region",
       x = "Region", y = "Mean Delivery Delay (Days)")+
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
plot3

# 4. Correlation between Avg Order Value and Customer's Order Frequency
plot4 <- ggplot(my_data, aes(x = orders_12m, y = avg_basket)) +
  geom_point(alpha = 0.4, color = "darkblue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Correlation between Avg Order Value \nand Customer's Order Frequency",
       x = "Order Frequency in Last 12 Months", y = "Avg Order Value ($)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
plot4

# 5. Impact of Delivery Delays on Support Tickets
plot5 <- ggplot(my_data, aes(x = delivery_delay_mean, y = support_tickets_12m)) +
  geom_jitter(alpha = 0.3, color = "red") +
  theme_minimal() +
  labs(title = "Impact of Delivery Delays on Support Tickets",
       x = "Average Delivery Delay (Days)", y = "Support Tickets") +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
plot5



#####################################################################################
# TASK 3
#####################################################################################

# 1. Variable Selection
clust_vars <- my_data |> 
  select(
    tenure_months, 
    recency_days, 
    orders_12m, 
    avg_basket,
    discount_share, 
    return_rate, 
    online_share, 
    store_share, 
    app_share,
    cat_grocery_share,
    cat_electronics_share,
    cat_fashion_share,
    cat_home_share
  )

# 2. Handling Missing Values
colSums(is.na(clust_vars))

# If NAs exist we remove those specific rows to ensure a clean distance matrix.
clust_data_clean <- na.omit(clust_vars)

# 3. Scaling / Standardization
clust_data_scaled <- scale(clust_data_clean)

clust_data_final <- as.data.frame(clust_data_scaled)

summary(clust_data_final)



#####################################################################################
# TASK 4
#####################################################################################

# 1. Elbow Method
elbow_plot <- fviz_nbclust(clust_data_final, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2, color = "red") +
  labs(title = "Elbow Method",
       subtitle = "Optimal k is where the marginal gain drops off") +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
elbow_plot

# 2. Silhouette Method
sil_plot <- fviz_nbclust(clust_data_final, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method",
       subtitle = "Highest average silhouette width indicates optimal k") +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
sil_plot



#####################################################################################
# TASK 5
#####################################################################################

# Clustering: k-Means

## 1. Fit the clustering model
set.seed(108532043)

# Perform k-means clustering with 5 clusters on the scaled data
kmeans_result <- kmeans(clust_data_final, centers = 5)

# View the mathematical results
print(kmeans_result)

## 2. Assign cluster labels to the original dataset
my_data$cluster <- factor(kmeans_result$cluster)

# Show top records with new cluster info
head(my_data)

# Get size counts for each cluster
summary(my_data$cluster)

## 3. Visualization for Profiling
# Plot for Cluster-wise Order Frequency vs Return Rate to see how segments differ in 'Value' and 'Loyalty'
ggplot(my_data, aes(x = avg_basket, y = return_rate, color = cluster)) +
  geom_point(alpha = 0.5, size = 2) +
  labs(title = "Cluster-wise Order Frequency vs Return Rate",
       x = "Order number in L12M",
       y = "Return Rate",
       color = "Cluster") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# Box plot to see Spending Behaviour (Avg Basket) across clusters
ggplot(my_data, aes(x = cluster, y = avg_basket, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Average Basket Value by Cluster",
       x = "Cluster",
       y = "Avg Basket ($)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

# Creating a summary table for category shares per cluster
cluster_profile <- my_data |>
  group_by(cluster) |>
  summarise(across(c(avg_basket, orders_12m, return_rate, 
                     cat_grocery_share, cat_electronics_share, 
                     cat_fashion_share, cat_home_share,
                     online_share, store_share, app_share), mean))

print(cluster_profile)



#####################################################################################
# TASK 6
#####################################################################################

# 1. Calculating Euclidean Distance to Cluster Center
centers <- kmeans_result$centers[kmeans_result$cluster, ] 
distances <- sqrt(rowSums((clust_data_final - centers)^2))

# 2. Identify the most distant points as anomalies using threshold as 2x the mean distance
threshold <- 2 * mean(distances)
my_data$is_anomaly <- distances > threshold

# 3. Visualizing Anomalies
# We plot support tickets vs return rate, highlighting the anomalies
ggplot(my_data, aes(x = return_rate, y = support_tickets_12m, color = is_anomaly)) +
  geom_point(alpha = 0.6, size = 3) +
  scale_color_manual(values = c("grey70", "red")) +
  theme_minimal() +
  labs(title = "Anomalous Customer Identification: \n Return Rate vs Support Ticket",
       x = "Return Rate", y = "Support Tickets (L12M)", color = 'Is Anomaly') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# 4. Profiling the unusual customers
anomalies_df <- my_data |> filter(is_anomaly == TRUE)
summary(anomalies_df |> select(avg_basket, return_rate, support_tickets_12m, delivery_delay_mean))
