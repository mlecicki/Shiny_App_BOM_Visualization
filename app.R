library(shiny)
library(tidyverse)
library(networkD3)


# ---- ETL

boms_raw <- read_csv('data/BOMs.csv')

bom_materials <- boms_raw %>%
  select(Material) %>%
  unique()

bom_components <- boms_raw %>%
  select(Component) %>%
  unique() %>%
  rename(Material = Component)

plants <- boms_raw %>%
  select(Plant) %>%
  unique() %>%
  pull()

mat_types <- boms_raw %>%
  select(Material_Type) %>%
  unique() %>%
  pull()

comp_types <- boms_raw %>%
  select(Component_Type) %>%
  unique() %>%
  pull()

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
                                     multiple = TRUE))
               # column(2, selectInput('material', 
               #                       'Select material', 
               #                       choices = bom_materials,
               #                       selected = bom_materials,
               #                       multiple = TRUE)),
               # column(2, selectInput('component', 
               #                       'Select component', 
               #                       choices = bom_components,
               #                       selected = bom_components,
               #                       multiple = TRUE))
             ),
             br(),
             br(),
             fluidRow(
               # tableOutput('output_debug'),
               # textOutput('text_debug'),
               sankeyNetworkOutput("sankey_plot"),
               res = 96),
             br(),
             br(),
             fluidRow(
               actionButton('reset', 'Reset all filters')
               )
    ),
    tabPanel("BOM material type lifecycle status diagram")
    )
)

# ---- server

server <- function(input, output, session) {
  
  
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
  })
  
  
  selected <- reactive({boms_raw %>%
      filter(Plant %in% input$plant &
               Material_Type %in% input$mat_type &
               Component_Type %in% input$comp_type)
  })
  
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
  
  # output$text_debug <- renderText(len())

  selected_nodes <- reactive({
    data.frame(node = c(0:len()),
               Material = selected_uniqe_materials())
    })
  
  selected_links <- reactive({selected() %>%
      left_join(., selected_nodes(), keep = FALSE) %>%
      rename(target = node) %>%
      left_join(., selected_nodes(), by = c('Component' = 'Material'), keep = FALSE) %>%
      rename(source = node) %>%
      mutate(value = 1) %>%
      data.frame()
  })

#  output$output_debug <- renderTable(selected_links())

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

}

# ---- app run

shinyApp(ui = ui, server = server)