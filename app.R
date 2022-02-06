library(shiny)
library(tidyverse)
library(networkD3)


# ---- ETL

boms_raw <- read_csv('data/BOMs.csv') %>%
  mutate(value = 1)

# function to pull unique values and reduce code redundancy

pull_unique <- function(tib = boms_raw, var) {
  tib %>%
    select( {{ var }}) %>%
    unique() %>%
    pull()
}

plants <- pull_unique(var = Plant)

mat_types <- pull_unique(var = Material_Type)

comp_types <- pull_unique(var = Component_Type)

mat_lc_stage <- pull_unique(var = Material_Lifecycle_Stage)

comp_lc_stage <- pull_unique(var = Component_Lifecycle_Stage)

# ---- user interface

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Bill of material diagram",
             fluidRow(
               column(2, selectInput("plant", 
                                     "Select plant", 
                                     choices = plants,
                                     selected = plants,
                                     multiple = TRUE)),
               column(2, selectInput("mat_type", 
                                     'Select material type', 
                                     choices = mat_types,
                                     selected = mat_types,
                                     multiple = TRUE)),
               column(2, selectInput("comp_type",
                                     "Select component type",
                                     choices = comp_types,
                                     selected = comp_types,
                                     multiple = TRUE)),
               column(2, selectInput("mat_lifecycle",
                                     "Select material lifecycle",
                                     choices = mat_lc_stage,
                                     selected = mat_lc_stage,
                                     multiple = TRUE)),
               column(2, selectInput("comp_lifecycle",
                                     "Select component lifecycle",
                                     choices = comp_lc_stage,
                                     selected = comp_lc_stage,
                                     multiple = TRUE))
             ),
             br(),
             br(),
             fluidRow(
               sankeyNetworkOutput("sankey_plot"),
               res = 96),
             br(),
             fluidRow(
               dataTableOutput("details")),
             br(),
             fluidRow(
               actionButton('reset', 'Reset all filters')
               )
    ),
    tabPanel("BOM material type lifecycle status diagram",
             sidebarLayout(
               sidebarPanel(
                 selectInput('plant_tab2',
                             'Select Plant',
                             choices = plants,
                             selected = plants,
                             multiple = TRUE),
                 br(),
                 br(),
                 selectInput('mat_lc', 
                             'Select Material Type Lifecycle Stage', 
                             choices = mat_lc_stage,
                             selected = mat_lc_stage,
                             multiple = TRUE),
                 br(),
                 br(),
                 selectInput('comp_lc', 
                             'Select Component Type Lifecycle Stage', 
                             choices = comp_lc_stage,
                             selected = comp_lc_stage,
                             multiple = TRUE),
                 br(),
                 br(),
                 actionButton('reset_agg', 'Reset all filters'),
                 width = 3
               ),
               mainPanel(
                 fluidRow(
                  sankeyNetworkOutput('sankey_plot_agg'),
                  height = 1200),
                 br(),
                 br(),
                 fluidRow(
                   dataTableOutput('summary')
                 )
               )
             )
    )
)
)

# ---- server

server <- function(input, output, session) {
  
#-- first tab
  
  observeEvent(input$reset, {

    updateSelectInput(session, "plant",
                      choices = plants,
                      selected = plants)

    updateSelectInput(session, "comp_type",
                      choices = comp_types,
                      selected = comp_types)

    updateSelectInput(session, "mat_type",
                      choices = mat_types,
                      selected = mat_types)
    
    updateSelectInput(session, "mat_lifecycle",
                      choices = mat_lc_stage,
                      selected = mat_lc_stage)
    
    updateSelectInput(session, "comp_lifecycle",
                      choices = comp_lc_stage,
                      selected = comp_lc_stage)
  })
  
  selected <- reactive({boms_raw %>%
      filter(Plant %in% input$plant,
               Material_Type %in% input$mat_type,
               Component_Type %in% input$comp_type,
               Material_Lifecycle_Stage %in% input$mat_lifecycle,
               Component_Lifecycle_Stage %in% input$comp_lifecycle)
  })
  
  output$debug <- renderTable(selected())
  
  selected_materials <- reactive({selected() %>%
      select(Material) %>%
      unique()
      })
  
  selected_components <- reactive({selected() %>%
      select(Component) %>%
      unique() %>%
      rename(Material = Component)
    })

  
  selected_uniqe_materials  <- reactive({
    union(selected_materials(), selected_components())
  })
  
  len <- reactive(length(selected_uniqe_materials()$Material) - 1)

  selected_nodes <- reactive({
    data.frame(node = c(0:len()),
               Material = selected_uniqe_materials())
    })

  selected_links <- reactive({selected() %>%
      left_join(., selected_nodes(), keep = FALSE) %>%
      rename(target = node) %>%
      left_join(., selected_nodes(), 
                by = c('Component' = 'Material'), keep = FALSE) %>%
      rename(source = node) %>%
      data.frame()
  })

  output$sankey_plot <- renderSankeyNetwork({
   networkD3::sankeyNetwork(Links = selected_links(),
                            Nodes = selected_nodes(),
                            Source = 'source',
                            Target = 'target',
                            Value = 'value',
                            NodeID = 'Material',
                            units = 'Units',
                            fontSize = 20,
                            nodeWidth = 40)
 }
 )
  
  output$details <- renderDataTable(selected() %>% select (-value),
                                    options = list(pageLength = 10,
                                                   lengthMenu = c(10, 20, 30)))

# -- second tab
  
  observeEvent(input$reset_agg, {
    
    updateSelectInput(session, "plant_tab2",
                      choices = plants,
                      selected = plants)
    
    updateSelectInput(session, "mat_lc",
                      choices = mat_lc_stage,
                      selected = mat_lc_stage)
    
    updateSelectInput(session, "comp_lc",
                      choices = comp_lc_stage,
                      selected = comp_lc_stage)
  })
  
  selected_agg <- reactive({boms_raw %>%
      filter(Plant %in% input$plant_tab2 &
      Material_Lifecycle_Stage %in% input$mat_lc &
      Component_Lifecycle_Stage %in% input$comp_lc) %>%
      mutate(Material_Type_LC_Stage = str_c(Material_Type, Material_Lifecycle_Stage, sep = '_'),
             Component_Type_LC_Stage = str_c(Component_Type, Component_Lifecycle_Stage, sep = '_')) %>%
      group_by(Plant, Material_Type_LC_Stage, Component_Type_LC_Stage) %>%
      summarise(value = n()) %>%
      ungroup()
  })
  
  sel_mat_lc_stage <- reactive({selected_agg() %>%
      select(Material_Type_LC_Stage) %>%
      unique()
  })
  
  sel_comp_lc_stage <- reactive({selected_agg() %>%
      select(Component_Type_LC_Stage) %>%
      unique() %>%
      rename(Material_Type_LC_Stage = Component_Type_LC_Stage)
  })
  
  selected_uniqe_mat_LC_stage  <- reactive({
    union(sel_mat_lc_stage(), sel_comp_lc_stage())
  })
  
  len_agg <- reactive(length(selected_uniqe_mat_LC_stage()$Material_Type_LC_Stage) - 1)

  selected_nodes_agg <- reactive({
    data.frame(node = c(0:len_agg()),
               Material_Type_LC_Stage = selected_uniqe_mat_LC_stage())
  })

  selected_links_agg <- reactive({selected_agg() %>%
      left_join(., selected_nodes_agg(), keep = FALSE) %>%
      rename(target = node) %>%
      left_join(., selected_nodes_agg(),
                by = c('Component_Type_LC_Stage' = 'Material_Type_LC_Stage'),
                keep = FALSE) %>%
      rename(source = node) %>%
      data.frame()
  })
  

  output$sankey_plot_agg <- renderSankeyNetwork({
    networkD3::sankeyNetwork(Links = selected_links_agg(),
                             Nodes = selected_nodes_agg(),
                             Source = 'source',
                             Target = 'target',
                             Value = 'value',
                             NodeID = 'Material_Type_LC_Stage',
                             units = 'Units',
                             fontSize = 20,
                             nodeWidth = 40)
  }
  )
  
  output$summary <- renderDataTable(selected_agg() %>% rename(Count = value), 
                                    options = list(pageLength = 10,
                                                   lengthMenu = c(10, 20, 30)))

}

# ---- app run

shinyApp(ui = ui, server = server)