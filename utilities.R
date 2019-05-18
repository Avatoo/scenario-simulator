
#' Data import
import_mm <- function(df = read_xlsx("data/Infrahack - Asset Data - DN Clean.xlsx")) {
  df %>% 
    select(Element_ID, Asset_ID, Price, Length, Depth, Width) %>% 
    mutate(Depth = as.numeric(Depth)) %>% 
    mutate(Width = as.numeric(Width))
}

#' Clustering function 
cluster_mm <- function(df, k = 10) {
  
  df = df %>% 
    column_to_rownames('Element_ID') %>% 
    select(Length, Depth, Width) 
  
  df <- na.omit(df)
  df <- scale(df)
  
  res = list()
  
  set.seed(42)
  res[['data']] = df %>% 
    kmeans(centers = k, nstart = 25) 
  
  res[['p']] = res[['data']] %>% 
    fviz_cluster(geom = "point", data = df) + 
    ggtitle(paste0('k=', k))
  
  res
}



#' Run the baseline senario
run_senario_0 <- function(df, p, this_asset = NULL) {

  if (!is.null(this_asset)){
    df = df %>% filter(Asset_ID %in% this_asset)
  }# 'PROJECT-ASSET-210'
  
  df %>% 
    mutate(Time_Stamp = today()) %>% 
    mutate(Cluster = p$data$cluster) %>% 
    mutate(ID = row_number())
}


#' Run further refined senarios
run_senario_2 <- function(df1, this_asset = NULL,
                          standard_var = 'Depth', standard_method = 'max') {
  
  assertthat::has_name(df1, standard_var)
  
  standard_method = match.fun(standard_method)
  
  if (!is.null(this_asset)){
    df1 = df1 %>% filter(Asset_ID %in% this_asset)
  }
  
  assets = unique(df1$Asset_ID)
  clusters = unique(df1$Cluster)
  
  res = vector('list', length(c(assets, clusters)))
  
  for (i in seq_along(assets)){
    for (j in seq_along(clusters)) {
      df2 = df1 %>% 
        filter(Asset_ID == assets[i], Cluster == clusters[j]) 
      
      standard_value = standard_method(df2[[standard_var]])
      
      
      idx = which(df2[[standard_var]]==standard_value)
      standard_element = pull(df2[idx, 'Element_ID'])
      
      standard_price = pull(df2[idx, 'Price'])[1]
      
      standard_length = pull(df2[idx, 'Length'])[1]
      
      standard_depth = pull(df2[idx, 'Depth'])[1]
      
      standard_width = pull(df2[idx, 'Width'])[1]
      
      
      if (length(standard_element > 1)) {
        message('More than one Element_ID meet senario standard using: Element_ID ',
                standard_element[1])
      }
      standard_element = standard_element[1]
    
      # swap Element_ID with the one with optimal value
      res[[i+j-1]] = df2 %>% 
        mutate(Element_ID = standard_element) %>% 
        mutate(Price = standard_price) %>% 
        mutate(Length = standard_length) %>% 
        mutate(Depth = standard_depth) %>% 
        mutate(Width = standard_width) 
      
    }
  }
  
  res %>% 
    compact() %>% 
    bind_rows() %>% 
    mutate(Time_Stamp = today()) %>% 
    mutate(ID = row_number())

}


