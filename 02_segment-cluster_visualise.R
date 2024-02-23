# Code for segmentation and clustering of either a barn owl individual or
# ANIMOVER_1 synthetic data



library(dplyr)
library(tseries)

library(parallelDist)
library(fastcluster)

library(factoextra)


df_DARs <- data.frame(readRDS(".../merged-DARs_ANIMOV.Rds"))
#df_DARs <- data.frame(readRDS(".../merged-DARs_barn-owl.Rds"))


df_DARs$speed <- (df_DARs$speed)/max(df_DARs_indiv$speed)
df_DARs$turning_angle <- (df_DARs$turning_angle)/pi
# Scaling to bring the variables to range on [0,1].


# Method to create segments and construct a representation

create_chunks <- function(df){

  df_chunks_DARs <- df |>
    group_by(seg=cut(T, breaks="60 sec")) |>  # For barn owl
    #group_by(seg=cut(T, breaks=seq(0, max(T), by=30))) |>  # For ANIMOV
    summarise(
      speed_mean = mean(speed, na.rm = TRUE),
      speed_std = sd(speed, na.rm = TRUE),
      turning_angle_mean = mean(turning_angle, na.rm = TRUE),
      turning_angle_std = sd(turning_angle, na.rm = TRUE),
      disp_ends = sqrt((x[n()] - x[1])^2 + (y[n()] - y[1])^2),
      disp_consec = sum(sqrt(diff(x)^2 + diff(y)^2)),
      n_pts = n()
    ) |>
    filter(n_pts >= 12) |>  # Only for the case of barn owls
    summarise(
      seg,
      speed_mean, speed_std, turning_angle_mean, turning_angle_std,
      rel_disp = disp_ends/disp_consec
    )
  
  df_chunks_DARs <- na.omit(df_chunks_DARs)
  
  return(df_chunks_DARs)
}

df_chunks_DARs = create_chunks(df_DARs)


# Hierarchical agglomerative clustering w/ Ward's criterion.

d <- parDist(as.matrix(df_chunks_DARs[c("speed_mean","turning_angle_mean","speed_std","turning_angle_std","rel_disp")]),
             method = "euclidean")
hierarc_res <- fastcluster::hclust(d, method="ward.D2")

hc_ward_8 <- cutree(hierarc_res, k=8)

centroids <- NULL
for(k in 1:8){
  centroids <- rbind(centroids, colMeans(df_chunks_DARs[c("speed_mean","turning_angle_mean","speed_std","turning_angle_std","rel_disp")]
                                         [hc_ward_8==k, , drop = FALSE]))
}
centroids

occupancy <- as.numeric(table(hc_ward_8))
occupancy


# Compute principal components.

pca_res <- prcomp(df_chunks_DARs[, c("speed_mean","turning_angle_mean","speed_std","turning_angle_std","rel_disp")],
                  scale=FALSE,
                  center = FALSE
)

# Eigenvalues
get_eigenvalue(pca_res)

fviz_pca_var(pca_res,
             col.var = "contrib",  # Color by contributions to the PC
             repel = TRUE,  # Avoid text overlapping
             scale = FALSE,
             center = FALSE
)

pca_res$x

# eigenvectors
pca_res$rotation


# Plot StaMEs in sp_mean-ta_mean space or PC1-PC2 space w/ different colours representing clusters.

cluster_data_PC <- data.frame(
  Cluster = as.factor(hc_ward_8),
  PC1 = pca_res$x[, 1],
  PC2 = pca_res$x[, 2]
)
var_expl <- round(pca_res$sdev^2 / sum(pca_res$sdev^2) * 100, 2)
# Create a cluster plot
ggplot(cluster_data_PC, aes(x = PC1, y = PC2, colour = Cluster)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("green1", "blue4", "lightblue1", "violet", "yellow1", "red1", "orange1", "red4")) +
  labs(title = "Cluster Plot on Principal Components",
       x = paste("PC1 (", var_expl[1], "%)", sep = ""),
       y = paste("PC2 (", var_expl[2], "%)", sep = "")) +
  theme_minimal()

cluster_data_SpTa <- data.frame(
  Cluster = as.factor(hc_ward_8),
  mean_speed = df_chunks_DARs_$speed_mean,
  mean_turning_angle = df_chunks_DARs$turning_angle_mean
)
# Create a cluster plot
ggplot(cluster_data_SpTa, aes(x = mean_speed, y = mean_turning_angle, colour = Cluster)) +
  geom_point(size = 3) +
  #scale_color_manual(values = "red4")
  scale_color_manual(values = c("green1", "blue4", "lightblue1", "violet", "yellow1", "red1", "orange1", "red4")) +
  labs(title = "Cluster Plot on mean speed and mean turning angle") +
  theme_minimal()