install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggrepel")
library(ggplot2)
library(dplyr)
library(ggrepel)

VolcanoCurcumin <- read_csv("C:/Users/Student/Desktop/GerardoIga/Curcuminwork/Figure 1/VolcanoPlot/VolcanoCurcumin.csv")


ggplot(VolcanoCurcumin, aes(x = log2FoldChange, y = -log10(pvalue))) +
  geom_point(
    aes(color = ifelse(abs(log2FoldChange) > 1 & -log10(pvalue) > 2, 
                       ifelse(log2FoldChange < 0, "grey", "lightcoral"), 
                       "lightblue")),
    size = 3
  ) +
  scale_color_manual(
    values = c("lightblue", "grey", "lightcoral"),
    labels = c("Not Significant", "Significant (Negative)", "Significant (Positive)")
  ) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +  # Threshold line
  labs(
    x = "Log2 Fold Change",
    y = "-log10(p-value)",
    color = "Legend"  # Change the legend title here
  ) +
  theme_minimal()

# Define the log2FoldChange threshold (adjust as needed)
log2FoldChange_threshold <- 1

log2FoldChange_thresholdn <- -1

genes_to_label <- c("namegane", "namegene2")

volcano_plot <- ggplot(VolcanoCurcumin, aes(x = log2FoldChange, y = -log10(pvalue))) +
  geom_point(
    aes(color = ifelse(abs(log2FoldChange) > 1 & -log10(pvalue) > 1, 
                       ifelse(log2FoldChange < 0, "grey", "lightcoral"), 
                       "lightblue")),
    size = 3
  ) +
  scale_color_manual(
    values = c("lightblue", "grey", "lightcoral"),
    labels = c("Down regulated", "Not sig", "Up regulated")
  ) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +  # Threshold line
  geom_vline(xintercept = log2FoldChange_threshold, linetype = "dashed", color = "black") +  # Vertical line for log2FoldChange
  geom_vline(xintercept = log2FoldChange_thresholdn, linetype = "dashed", color = "black") +
  labs(
    x = "Log2 Fold Change",
    y = "-log10(p-value)",
    color = "Legend"  # Change the legend title here
  ) +
  theme_minimal()

label_data <- VolcanoCurcumin %>%
  filter(gene_name %in% genes_to_label)

volcano_plot +
  geom_text_repel(VolcanoCurcumin = label_data, aes(label = gene_name), box.padding = 0.5, segment.size = 0.2)

