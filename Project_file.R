
load_packages()

data1 <- read_excel("KYND_data_history.xlsx", sheet = 1, col_types=c('text', 'text', 'text', 'numeric', 'text', 'text', 'text', 'text', 'logical', 'text', 'text', 'text', 'text', 'text'))

data1$org_id <- as.factor(data1$org_id)
data1$domain <- as.factor(data1$domain)
data1$record_type <- as.factor(data1$record_type)
data1$false_positive <- as.factor(data1$false_positive)
data1$issuer <- as.factor(data1$issuer)
data1$version <- as.factor(data1$version)
data1$product_category <- as.factor(data1$product_category)
data1$product_classification <- as.factor(data1$product_classification)
data1$risk_type <- as.factor(data1$risk_type)
data1$severity <- factor(data1$severity, levels = c('green', 'amber', 'red'))
data1$update_date <- as.factor(data1$update_date)
data1$update_time <- as.factor(data1$update_time)
data1$false_positive <- as.numeric(data1$false_positive == TRUE)
data1$severity_num <- as.integer(data1$severity)

dm <- data1[,c(-5,-7,-14)]
dm <- as.data.table(dm)

dm1 <- dm %>%  
        group_by(org_id) %>%
        summarise(domains = sum(!is.na(unique(domain))),
                  record_types = sum(!is.na(unique(record_type))),
                  versions = sum(!is.na(unique(version))),
                  update_dates = sum(!is.na(unique(update_date))),
                  update_times = sum(!is.na(unique(update_time))),
                  false_positive_ratio = sum(false_positive, na.rm = TRUE)/sum(false_positive == 0, na.rm = TRUE),
                  issuers = sum(!is.na(unique(issuer))),
                  product_categories = sum(!is.na(unique(product_category))),
                  product_classifications = sum(!is.na(unique(product_classification)))
                  )

d2 <- dm[,c(1,11,12)]

d3 <- d2 %>%
        group_by(org_id, risk_type) %>%
        summarise(count = n(),
                  min_sev = min(severity_num),
                  max_sev = max(severity_num)) %>%
        group_by(org_id, risk_type) %>%
        nest() %>%
        pivot_wider(names_from = risk_type, values_from = data) %>%
        unnest(cols = everything(), names_sep = "_")


final_data <- left_join(dm1, d3, by = "org_id")
names <- c(5,12,13,15,16,18,19,21,22,24,25,27,28,30,31,33,34,36,37,39,40,42,43,45,46,48,49,51,52,54,55,57,58,60,61,63,64,66,67,69,70,72,73,75,76,78,79,81,82,84,85,87,88,90,91,93,94,96,97,99,100,102,103,105,106,108,109,111,112,114,115,117,118,120,121,123,124,126,127,129,130,132,133,135,136,138,139,141,142,144,145,147,148,150,151,153,154)
final_data[,names] <- lapply(final_data[,names] , factor)

head(data1)
head(final_data)

write_xlsx(final_data, "D:\\Downloads\\Uni of Kent Course Material\\MAST8670 - Project\\Data and Code\\Dissertation\\final_data.xlsx")

set.seed(123)
pclust <- kproto(final_data [-1], k=3, nstart = 10) # Cant' work with NAs

####

distances <- dist(final_data[-1], method = "gower")

# rc <- rockCluster(as.matrix(distances), n=6, theta = 0.33, debug=TRUE)
# print(rc)
# rf = fitted(rc)
# rf$size
# table(final_data$org_id, rf$cl) # works best with binary data

# clust_data <- final_data[-1]

clust_ward <- hclust(distances, method = "ward.D2")

ggplot(clust_ward$height %>%
               as_tibble() %>%
               add_column(groups = length(clust_ward$height):1) %>%
               rename(height=value),
       aes(x=groups, y=height)) +
        geom_point() +
        geom_line() 
 
internal_validation(clust_ward, 4, distances)
internal_validation(clust_ward, 5, distances) # Dunn index is biased towards increasing number of clusters
chosen.clusters <- cutree(clust_ward, 4)
plot(chosen.clusters)
rect(-10, 0, 110, 1.5, col= rgb(1, 0, 0, alpha = 0.2), border = "black", lty = 2, lwd = 3)
rect(-10, 1.5, 110, 2.5, col= rgb(0, 1, 0, alpha = 0.2), border = "black", lty = 2, lwd = 3)
rect(-10, 2.5, 110, 3.5, col= rgb(0, 0, 1, alpha = 0.2), border = "black", lty = 2, lwd = 3)
rect(-10, 3.5, 110, 4.5, col= rgb(0, 1, 1, alpha = 0.2), border = "black", lty = 2, lwd = 3)
text(chosen.clusters, labels= names(chosen.clusters) , cex= 0.7, pos=3)

test1 <- cbind(chosen.clusters,final_data)

dend <- clust_ward %>% as.dendrogram %>%  ### USE THIS FOR DENDROGRAMS
        set("branches_k_color", k = 4) %>% set("branches_lwd", 2.5) %>%
        set("labels_cex", 0.6) %>% set("labels_colors", k = 4) %>%
        set("leaves_pch", 20) %>% set("leaves_cex", 0.8) %>% plot(horiz = FALSE)


# Bar plots for categorical variables

ggplot(test1) + theme_classic() + 
        geom_bar(aes(fill = update_dates, x = chosen.clusters), position = "fill")
ggplot(test1) + theme_classic() + 
        geom_bar(aes(fill = rag_service_known_vulnerability_min_sev, x = chosen.clusters), position = "fill")
ggplot(test1) + theme_classic() + 
        geom_bar(aes(fill = rag_service_known_vulnerability_max_sev, x = chosen.clusters), position = "fill")
ggplot(test1) + theme_classic() + 
        geom_bar(aes(fill = rag_service_port_max_sev, x = chosen.clusters), position = "fill")

# Density plots for continuous variables

ggplot(test1) + theme_classic() + 
        geom_density(aes(color = factor(chosen.clusters), x = domains), lwd = 0.8)
ggplot(test1) + theme_classic() + 
        geom_density(aes(color = factor(chosen.clusters), x = versions), lwd = 0.8)
ggplot(test1) + theme_classic() + 
        geom_density(aes(color = factor(chosen.clusters), x = issuers), lwd = 0.8)
ggplot(test1) + theme_classic() + 
        geom_density(aes(color = factor(chosen.clusters), x = product_categories), lwd = 0.8)
ggplot(test1) + theme_classic() + 
        geom_density(aes(color = factor(chosen.clusters), x = product_classifications), lwd = 0.8)
ggplot(test1) + theme_classic() + 
        geom_density(aes(color = factor(chosen.clusters), x = rag_service_known_vulnerability_count), lwd = 0.8)



# clust_complete <- hclust(distances, method = "complete")
# plot(clust_complete)
# plot(silhouette(cutree(clust_complete, 4),distances))
# dunn(distance = distances, cutree(clust_complete, 4))
# 
# clust_single <- hclust(distances, method = "single")
# plot(clust_single)
# plot(silhouette(cutree(clust_single, 4),distances))
# dunn(distance = distances, cutree(clust_single, 4))
# 
# clust_average <- hclust(distances, method = "average")
# plot(clust_average)
# plot(silhouette(cutree(clust_average, 4),distances))
# dunn(distance = distances, cutree(clust_average, 4))

clust_centroid <- hclust(distances, method = "centroid")

ggplot(clust_centroid$height %>%
               as_tibble() %>%
               add_column(groups = length(clust_centroid$height):1) %>%
               rename(height=value),
       aes(x=groups, y=height)) +
        geom_point() +
        geom_line() 

internal_validation(clust_centroid, 2, distances)
internal_validation(clust_centroid, 7, distances)

chosen.clusters <- cutree(clust_centroid, 2)
plot(chosen.clusters)
rect(-10, 0, 110, 1.5, col= rgb(1, 0, 0, alpha = 0.2), border = "black", lty = 2, lwd = 3)
rect(-10, 1.5, 110, 3, col= rgb(0, 1, 0, alpha = 0.2), border = "black", lty = 2, lwd = 3)
text(chosen.clusters, labels= names(chosen.clusters) , cex= 0.7, pos=1)

###############################################################
                   # Plotting Locations #
###############################################################

data2 <- read_excel("KYND_data_history.xlsx", sheet = 5)
icon_salesdata <- makeAwesomeIcon(text = data2$`count of org_id`)
df <- data.frame(longitude = data2$longitude, latitude = data2$latitude)
leaflet(df) %>% 
        addTiles() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        addAwesomeMarkers(lng = data2$longitude, lat = data2$latitude, icon=icon_salesdata)


mydata <- as.tibble(data2)

mydata <- mydata %>% 
        dplyr::rename(
                count_id = "count of org_id"
        )

mydata$region[mydata$region == "United States"] <- "USA"

world_map <- map_data("world")
world_map <- subset(world_map, region != "Antarctica")

b <- c(0, 5, 10, 15)

ggplot(mydata) +
        geom_map(dat = world_map, map = world_map, aes(map_id = region), fill = "lightgray") +
        geom_map(map = world_map, aes(map_id = region, fill = log(count_id)), size = 0.5) +
        geom_map(dat = world_map, map = world_map, aes(map_id = region), fill = NA, color = "#2b2d2f", size = 0.5) +
        scale_fill_gradientn(limits = c(1, 15), colours=c("yellow","orange", "red"), breaks=b, labels=format(b)) +
        expand_limits(x = world_map$long, y = world_map$lat)

