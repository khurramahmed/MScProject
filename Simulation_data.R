
###############################################################
                # Creating Simulated Data #
###############################################################

set.seed(123)
size <- 100

var1 <- c(sample(c("1", "2", "3"), size = size, replace = TRUE, prob = c(1, 1, 1)),
          sample(c("1", "2", "3"), size = size, replace = TRUE, prob = c(1, 2, 3)),
          sample(c("1", "2"), size = size, replace = TRUE, prob = c(1, 1)),
          sample(c("1", "2"), size = size, replace = TRUE, prob = c(3, 1)),
          rep("3", size),
          sample(c("2", "3"), size = size, replace = TRUE, prob = c(2, 1))
)

var2 <- sample(c("1", "2", "3"), size = 6 * size, replace = TRUE) # Noise

var3 <- c(rnorm(size, 3.5, 2), rnorm(size, 7),
          rnorm(size, 10, 1.2), rnorm(size, 15, 2),
          rnorm(size, 20,2.5), rnorm(size, 28, 3))

var4 <- c(rep("2", size),
          sample(c("1", "2", "3"), size = size, replace = TRUE, prob = c(1, 1, 2)),
          sample(c("1", "2"), size = size, replace = TRUE, prob = c(2, 1)),
          rep("1", size),
          sample(c("1", "3"), size = size, replace = TRUE, prob = c(1, 3)),
          sample(c("1", "3"), size = size, replace = TRUE, prob = c(1, 1))
)

var5 <- c(rexp(6*size)) #Noise

clusters <- rep(1:6, each = size)

sim_data <- data.frame(var1, var2, var3, var4, var5, clusters)

###############################################################
                # Visualizing Simulated Data #
###############################################################

# Bar plots for categorical variables

ggplot(sim_data) + theme_classic() + 
        geom_bar(aes(fill = var1, x = clusters))

ggplot(sim_data) + theme_classic() + 
        geom_bar(aes(fill = var2, x = clusters))

ggplot(sim_data) + theme_classic() + 
        geom_bar(aes(fill = var4, x = clusters))

# Density plots for continuous variables

ggplot(sim_data) + theme_classic() + 
        geom_density(aes(color = factor(clusters), x = var3), lwd = 0.8)

ggplot(sim_data) + theme_classic() + 
        geom_density(aes(color = factor(clusters), x = var5), lwd = 0.8) 

# Testing Algorithms

rownames(sim_data) <- c(1:600)
sim_data$var1 <- as.factor(sim_data$var1)
sim_data$var2 <- as.factor(sim_data$var2)
sim_data$var4 <- as.factor(sim_data$var4)

sim_dist <- dist(sim_data[-6], method = "gower")

sim_rc <- rockCluster(as.matrix(sim_dist), n = 6, theta = 0.33, debug = TRUE) #Same result for all values below
print(sim_rc)
rf <- fitted(sim_rc)
table(sim_data$clusters, rf$cl)

sim_rc2 <- rockCluster(as.matrix(sim_dist), n=6, theta = 0.34, debug=TRUE)  #Same result for all values above
print(sim_rc2)
rf2 <- fitted(sim_rc2)
table(sim_data$clusters, rf2$cl)

sim_clust <- hclust(sim_dist, method = "ward.D2")
internal_validation(sim_clust, 6, sim_dist)
confusion_matrix(sim_data, sim_clust, 6)
external_validation(sim_data$clusters, cutree(sim_clust, 6), summary_stats = TRUE)

sim_clust2 <- hclust(sim_dist, method = "complete")
internal_validation(sim_clust2, 6, sim_dist)
confusion_matrix(sim_data, sim_clust2, 6)
external_validation(sim_data$clusters, cutree(sim_clust2, 6), summary_stats = TRUE)

sim_clust3 <- hclust(sim_dist, method = "single")
internal_validation(sim_clust3, 6, sim_dist)
confusion_matrix(sim_data, sim_clust3, 6)
external_validation(sim_data$clusters, cutree(sim_clust3, 6), summary_stats = TRUE)

sim_clust4 <- hclust(sim_dist, method = "average")
internal_validation(sim_clust4, 6, sim_dist)
confusion_matrix(sim_data, sim_clust4, 6)
external_validation(sim_data$clusters, cutree(sim_clust4, 6), summary_stats = TRUE)

sim_clust5 <- hclust(sim_dist, method = "centroid")
internal_validation(sim_clust5, 6, sim_dist)
confusion_matrix(sim_data, sim_clust5, 6)
external_validation(sim_data$clusters, cutree(sim_clust5, 6), summary_stats = TRUE)

