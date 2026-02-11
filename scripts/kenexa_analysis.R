install.packages(c("readxl", "dplyr", "ggplot2", "corrplot"))
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)

# NOTE: Raw client data is confidential and not included
data <- read.csv("data/mock_kenexa_data.csv")
# Inspect and clean
str(data)
summary(data)
colSums(is.na(data))

# Convert categorical variables
data$bloc <- factor(data$bloc, labels=c("Non-Metro", "Metro"))
data$ccon <- factor(data$ccon, labels=c("Low", "High"))

# Normalize numeric variables
num_vars <- sapply(data, is.numeric)
data_scaled <- data %>% mutate(across(where(is.numeric), scale))
View(data)

dim(data)         # Number of rows and columns
names(data)       # Variable names
head(data)        # First few rows
# Select relevant numeric columns
corr_vars <- c("ecuso","equal","einvol","etra","ecomm","eteam","eeng",
               "cserq","cbrtel","cbr","cbrpb","cloy","teltr","prod")

corr_matrix <- cor(data[, corr_vars], use = "complete.obs")

corrplot(corr_matrix, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.8,
         title = "Correlation Heatmap: Employee, Customer & Performance",
         mar = c(0,0,2,0))

ggplot(data, aes(x = eeng, y = cloy)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Employee Engagement vs Customer Loyalty",
       x = "Employee Engagement",
       y = "Customer Loyalty")

ggplot(data, aes(x = bloc, y = prod, fill = bloc)) +
  geom_boxplot() +
  labs(title = "Branch Productivity by Location",
       x = "Branch Type", y = "Productivity Ratio") +
  theme(legend.position = "none")

model_prod <- lm(prod ~ eeng + etra + ecomm + eteam + cserq + cloy, data = data)
summary(model_prod)

model_loyal <- lm(cloy ~ eeng + etra + ecomm + eteam + equal, data = data)
summary(model_loyal)
library(cluster)

cluster_vars <- data[, c("ecuso","equal","etra","ecomm","eteam","eeng","cloy","prod")]
cluster_scaled <- scale(cluster_vars)

set.seed(123)
k3 <- kmeans(cluster_scaled, centers = 3, nstart = 25)
data$cluster <- as.factor(k3$cluster)

ggplot(data, aes(x = eeng, y = cloy, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "Cluster Analysis of Branches",
       x = "Employee Engagement", y = "Customer Loyalty")

write.csv(data, "~/Downloads/Kenexa_cleaned_clusters.csv", row.names = FALSE)

