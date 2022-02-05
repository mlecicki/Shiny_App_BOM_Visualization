library(tidyverse)
library(networkD3)
#library(plotly)



boms_raw <- read_csv('data/BOMs.csv')

plants <- boms_raw %>%
  select(Plant) %>%
  unique() %>%
  pull()

plants <- 'plant_2'

mat_type <- boms_raw %>%
  select(Material_Type) %>%
  unique() %>%
  pull()

comp_type <- boms_raw %>%
  select(Component_Type) %>%
  unique() %>%
  pull()

boms_raw %>%
  filter(Plant %in% plants &
           Material_Type %in% mat_type &
           Component_Type %in% comp_type)


boms_materials <- boms_raw %>%
  select(Material) %>%
  unique()

bom_components <- boms_raw %>%
  select(Component) %>%
  unique() %>%
  rename(Material = Component)

bom_nodes <- union(boms_materials, bom_components)

len <- length(bom_nodes$Material) - 1

bom_nodes <- data.frame(node = c(0:len),
                        Material = bom_nodes$Material)
  
  

bom_links <- boms_raw %>%
  left_join(., bom_nodes, keep = FALSE) %>%
  rename(target = node) %>%
  left_join(., bom_nodes, by = c('Component' = 'Material'), keep = FALSE) %>%
  rename(source = node) %>%
  mutate(value = 1) %>%
  filter(Plant == 'plant_1') %>%
  data.frame()

bom_links %>%
  as_tibble() %>%
  select(Material, Component) %>%
  pivot_longer(Material, Component, names_to = 'Material_New')
  union(Material, Component, by = c('Material' = 'Component'))

# ---- material flow

networkD3::sankeyNetwork(Links = bom_links, 
                         Nodes = bom_nodes, 
                         Source = 'source', 
                         Target = 'target', 
                         Value = 'value', 
                         NodeID = 'Material',
                         units = 'Units')


# ---- flow aggregated to material type

boms_raw_agg <- boms_raw %>%
  mutate(Material_Type_LC_Stage = str_c(Material_Type, Material_Lifecycle_stage, sep = '_'),
         Component_Type_LC_Stage = str_c(Component_Type, Component_Lifecycle_Stage, sep = '_')) %>%
  group_by(Plant, Material_Type_LC_Stage, Component_Type_LC_Stage) %>%
  summarise(count = n())

  
unique_agg_nodes <- data.frame(node = c(Material_Type_LC_Stage = union(boms_raw_agg$Material_Type_LC_Stage, 
                       boms_raw_agg$Component_Type_LC_Stage)))

len_unique_agg_nodes <- length(unique_agg_nodes$node) - 1

bom_agg_nodes <- data.frame(node = c(0:len_unique_agg_nodes),
                            Material_Type_LC_Stage = unique_agg_nodes$node)
                            

bom_agg_links <- boms_raw_agg %>%
  left_join(., bom_agg_nodes, keep = FALSE) %>%
  rename(target = node) %>%
  left_join(., bom_agg_nodes, 
            by = c('Component_Type_LC_Stage' = 'Material_Type_LC_Stage'), 
            keep = FALSE) %>%
  rename(source = node) %>%
  rename(value = count) %>%
  data.frame()


networkD3::sankeyNetwork(Links = bom_agg_links, 
                         Nodes = bom_agg_nodes, 
                         Source = 'source', 
                         Target = 'target', 
                         Value = 'value', 
                         NodeID = 'Material_Type_LC_Stage',
                         units = 'Units')
