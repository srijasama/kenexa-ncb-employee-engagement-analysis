# ============================================================
# Kenexa (NCB) Capstone - Analysis, Regression, Clustering
# Author: Srija Sama
# Note: Raw client data is confidential and NOT included in repo.
# This script expects a local Excel file OR a mock dataset.
# ============================================================

# Install packages (run once)
# install.packages(c("readxl", "dplyr", "ggplot2", "corrplot", "cluster"))

library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(cluster)

# ----------------------------
# 1) Load data
# ----------------------------
# Option A (Local Excel - do NOT upload this file)
# data <- read_excel("data/file.xlsx")

# Option B (Mock CSV for GitHub demo)
data <- read.csv("data/mock_kenexa_data.csv")

# Inspect
str(data)
summary(data)
colSums(is.na(data))

# ----------------------------
# 2) Feature prep
# ----------------------------
# Convert categorical variables (only if these columns exist)
if ("bloc" %in% names(data)) {
  data$bloc <- factor(data$bloc, labels = c("Non-Metro", "Metro"))
}
if ("ccon" %in% names(data)) {
  data$ccon <- factor(data$ccon, labels = c("Low", "High"))
}

# Select relevant numeric columns for correlation
corr_vars <- c("ecuso","equal","einvol","etra","ecomm","eteam","eeng",
               "cserq","cbrtel","cbr","cbrpb","cloy","teltr","prod")

corr_vars <- corr_vars[corr_vars %in% names(data)]  # safety

# ----------------------------
# 3) Correlation heatmap
# ----------------------------
if (length(corr_vars) > 2) {
  corr_matrix <- cor(data[, corr_vars], use = "complete.obs")

  png("visuals/correlation_heatmap.png", width = 1200, height = 800)
  corrplot(corr_matrix, method = "color", type = "upper",
           tl.col = "black", tl.cex = 0.8,
           title = "Correlation Heatmap: Employee, Customer & Performance",
           mar = c(0,0,2,0))
  dev.off()
}

# ----------------------------
# 4) Key plots
# ----------------------------
if (all(c("eeng","cloy") %in% names(data))) {
  p1 <- ggplot(data, aes(x = eeng, y = cloy)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Employee Engagement vs Customer Loyalty",
         x = "Employee Engagement",
         y = "Customer Loyalty")

  ggsave("visuals/engagement_vs_loyalty.png", plot = p1, width = 8, height = 5)
}

if (all(c("bloc","prod") %in% names(data))) {
  p2 <- ggplot(data, aes(x = bloc, y = prod, fill = bloc)) +
    geom_boxplot() +
    labs(title = "Branch Productivity by Location",
         x = "Branch Type", y = "Productivity Ratio") +
    theme(legend.position = "none")

  ggsave("visuals/productivity_by_location.png", plot = p2, width = 8, height = 5)
}

# ----------------------------
# 5) Regression models
# ----------------------------
if (all(c("prod","eeng","etra","ecomm","eteam","cserq","cloy") %in% names(data))) {
  model_prod <- lm(prod ~ eeng + etra + ecomm + eteam + cserq + cloy, data = data)
  print(summary(model_prod))
}

if (all(c("cloy","eeng","etra","ecomm","eteam","equal") %in% names(data))) {
  model_loyal <- lm(cloy ~ eeng + etra + ecomm + eteam + equal, data = data)
  print(summary(model_loyal))
}

# ----------------------------
# 6) K-means clustering
# ----------------------------
cluster_vars <- c("ecuso","equal","etra","ecomm","eteam","eeng","cloy","prod")
cluster_vars <- cluster_vars[cluster_vars %in% names(data)]

if (length(cluster_vars) >= 3) {
  cluster_scaled <- scale(data[, cluster_vars])

  set.seed(123)
  k3 <- kmeans(cluster_scaled, centers = 3, nstart = 25)
  data$cluster <- as.factor(k3$cluster)

  if (all(c("eeng","cloy","cluster") %in% names(data))) {
    p3 <- ggplot(data, aes(x = eeng, y = cloy, color = cluster)) +
      geom_point(size = 3) +
      labs(title = "Cluster Analysis of Branches",
           x = "Employee Engagement", y = "Customer Loyalty")

    ggsave("visuals/cluster_scatter.png", plot = p3, width = 8, height = 5)
  }

  # Export cluster-labeled data locally (do NOT push confidential data)
  # write.csv(data, "Kenexa_cleaned_clusters.csv", row.names = FALSE)
}

