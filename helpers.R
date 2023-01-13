#test to get data back out

get_data <- function(dataframe,
                     fav_gene){
  query <- stringr::str_to_title(fav_gene)
  fav_data <-
    dataframe %>%
    dplyr::filter(gene %in% query) %>%
    unnest(data)
  
  return(fav_data)
}
# df <- get_data(dataframe = tissue_data, fav_gene = "CUL4B")
# get_data("Cul4b")

multiomics_boxplot <- function(dataframe,
                         fav_gene)
{
  df <- get_data(dataframe = dataframe,
                 fav_gene = fav_gene)
  ggplot(df,aes(x = cell_type, y = value, fill = gene)) +
    geom_boxplot()+geom_jitter(width = 0.1)+
    facet_wrap(vars(Sex,tissue, data_type), scales = "free",ncol = 4)+ 
    theme_cowplot(rel_large = 16/14,font_size = 20)+stat_compare_means(aes(x = cell_type, y = value),label = "p.format",method = "t.test",size = 4, vjust=12)
}




cold_boxplot <- function(dataframe,
                               fav_gene)
{
  df <- get_data(dataframe = dataframe,
                 fav_gene = fav_gene)
  facet_factor<-subset(colnames(df),subset = colnames(df)!=c("gene","value"))
  ggplot(df,aes(x = temp, y = value, fill = gene)) +
    geom_boxplot()+geom_jitter(width = 0.1)+
    facet_wrap(vars(cell_type), scales = "free",ncol = 4)+ 
    theme_cowplot(rel_large = 9/14,font_size = 20)+stat_compare_means(aes(x = temp, y = value),label = "p.format",method = "t.test", size = 4,vjust=12)
}
