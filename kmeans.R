# ----------------------
# cluster
# ----------------------

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)
library(lubridate)

# clean data - bridge, beam
df = read_xlsx("data/Infrahack - Asset Data - DN Clean.xlsx")

df = df %>% 
  select(Element_ID, Length, Depth, Width) %>% 
  column_to_rownames('Element_ID') %>% 
  mutate(Depth = as.numeric(Depth)) %>% 
  mutate(Width = as.numeric(Width)) %>% 
  na.omit() %>% 
  scale()


set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")


k6 <- kmeans(df, centers = 6, nstart = 25)
k7 <- kmeans(df, centers = 7, nstart = 25)
k8 <- kmeans(df, centers = 8, nstart = 25)
k9 <- kmeans(df, centers = 9, nstart = 25)

# plots to compare

p1 <- fviz_cluster(k6, geom = "point", data = df) + ggtitle("k = 6")
p2 <- fviz_cluster(k7, geom = "point",  data = df) + ggtitle("k = 7")
p3 <- fviz_cluster(k8, geom = "point",  data = df) + ggtitle("k = 8")
p4 <- fviz_cluster(k9, geom = "point", data = df) + ggtitle("k = 9")

grid.arrange(p1, p2, p3, p4,nrow = 2)

# plot single
df %>% 
  kmeans(centers = 10, nstart = 25) %>% 
  fviz_cluster(geom = "point", data = df) + 
  ggtitle("k = 10")

