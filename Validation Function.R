internal_validation <- function(my_cluster, k, my_distance){
        set.seed(123)
        dend <- my_cluster %>% as.dendrogram %>%  ### USE THIS FOR DENDROGRAMS
                set("branches_k_color", k = k) %>% set("branches_lwd", 0.7) %>%
                set("labels_cex", 0.6) %>% set("labels_colors", k = k) %>%
                set("leaves_pch", 19) %>% set("leaves_cex", 0.5) %>% plot(horiz = FALSE)
        plot(silhouette(cutree(my_cluster, k), my_distance), col=1:k, border=NA)
        DI <- dunn(distance = my_distance, cutree(my_cluster, k))
        print(paste("Dunn index is ", round(DI, 5)))
}

confusion_matrix <- function(my_data, my_cluster, k){
        set.seed(123)
        truth <- as.factor(my_data$clusters)
        pred <- as.factor(cutree(my_cluster, k))
        confusionMatrix(pred, truth)
        table <- data.frame(confusionMatrix(pred, truth)$table)
        
        plotTable <- table %>%
                mutate(goodbad = ifelse(table$Prediction == table$Reference, "correct", "incorrect")) %>%
                group_by(Reference) %>%
                mutate(prop = Freq/sum(Freq))
        
        ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
                geom_tile() +
                geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
                scale_fill_manual(values = c(correct = "green", incorrect = "red")) +
                theme_bw() +
                xlim(rev(levels(table$Reference)))
}
