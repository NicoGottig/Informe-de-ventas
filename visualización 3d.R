library(tidyverse)
library(naniar)
library(class)
library(ggtext)
library(showtext)
library(RColorBrewer)
library(corrplot)

# Importar base de datos
rw <- readxl::read_excel("marketing_campaign.xlsx")
str(rw)
rw$Z_CostContact <- NULL
rw$Z_Revenue <- NULL

# No hay duplicados
unique(rw$ID) %>% length()

# Armamos dataset con gastos para analisis de componentes
df_pca <- rw[, c(10:15)]
str(df_pca)

pca <- FactoMineR::PCA(df_pca, scale.unit = T)
pca$var$cor
coords_pca <- data.frame(pca$ind$coord)

# Armamos clusters

# prueba de agrupamiento
hopkins = factoextra::get_clust_tendency(coords_pca,
                                         n=100, 
                                         seed=321)
cat("Hopkins =", hopkins$hopkins_stat)

# Algoritmo PAM
pam_cluster <- pam(x = coords_pca[,1:3],
                   k = 3, 
                   metric = "manhattan")

# Algoritmo DBSCAN
dbscan::kNNdistplot(coords_pca[,1:3], k = 10)
abline(h = 0.7, lty = 2)

db <- fpc::dbscan(coords_pca[, 1:3],
                  eps = 0.7, 
                  MinPts = 10)

factoextra::fviz_cluster(db, 
                         data = coords_pca[,1:3], 
                         stand = FALSE,
                         ellipse = FALSE, 
                         show.clust.cent = FALSE,
                         geom = "point",
                         palette = "jco", 
                         ggtheme = theme_classic())


# ALgoritmo Kmeans
factoextra::fviz_nbclust( 
  coords_pca[,1:3],
  FUNcluster=function(x, k) amap::Kmeans(x, k, method="manhattan"),
  method="silhouette", k.max=20, diss=dist(clustering_data_scaled, method="manhattan"))
                                                        
# PLOT 3D
library(plotly)
colors <- c('#4AC6B7', 
            '#1972A4', 
            '#965F8A', 
            '#FF7070',
            '#C61951')

# Cargo clusters
cluster_vector <- pam_cluster$silinfo$widths[,1]
coords_pca$cluster <- as.factor(cluster_vector)

# Grafico
plot_ly(data = coords_pca,
        x = ~Dim.1,
        y = ~Dim.2,
        z = ~Dim.3,
        color = ~cluster,
        colors = colors,
        type = "scatter3d",
        size = 8) %>% 
  layout(title = "Descripción de clientes")

