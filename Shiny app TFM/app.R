library(shinydashboard)
library(shiny)
library(ggplot2)
library(plotly)
library(mapSpain)
library(tidyverse)
library(readxl)
library(factoextra)


municipios <- read_xlsx("municipios.xlsx")
propiedad <- read_xlsx("propiedad.xlsx")


esp_can <- esp_get_country() 
can_prov <- esp_get_can_provinces()
can_box <- esp_get_can_box()
munic <- esp_get_munic()
provic <- esp_get_prov() |> mutate(CPRO = cpro)


mapa <- propiedad |> 
  select(CPRO, Propiedad11Prov, Herencia11Prov, Alquiler11Prov, Otro11Prov,
         Propiedad21Prov, Alquiler21Prov, Otro21Prov,
         PropiedadDifProv, AlquilerDifProv, OtroDifProv) |> 
  distinct(CPRO, .keep_all = TRUE)
mapa <- provic |> left_join(mapa, by = "CPRO")

p1 <- ggplot(esp_can) +
  geom_sf(color = "black", size = 0.7) +  
  geom_sf(data = can_prov) +
  geom_sf(data = can_box) + 
  geom_sf(data = mapa, aes(fill = Propiedad11Prov), color = "black", size = 0.3) +  
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "white") +  
  geom_sf_text(data = mapa, aes(label = round(Propiedad11Prov, 1), color = ifelse(Propiedad11Prov > 75, "white", "black")), 
               size = 3, family = "Montserrat", check_overlap = TRUE, nudge_y = -0.03) + 
  theme_void() +  
  labs(title = "% of Young People in 'Ownership' Residence Status in 2011")  + 
  coord_sf() +
  scale_fill_viridis_c(option = "C", direction = -1) +
  scale_color_identity()  

p2 <- ggplot(esp_can) +
  geom_sf(color = "black", size = 0.7) +
  geom_sf(data = can_prov) +
  geom_sf(data = can_box) +
  geom_sf(data = mapa, aes(fill = Herencia11Prov), color = "black", size = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "white") +  
  geom_sf_text(data = mapa, aes(label = round(Herencia11Prov, 1), color = "black"), 
               size = 3, family = "Montserrat", check_overlap = TRUE, nudge_y = -0.03) +
  theme_void() +
  labs(title = "% of Young People in 'Inheritance' Residence Status in 2011") +
  coord_sf() +
  scale_fill_viridis_c(option = "C", direction = -1) +
  scale_color_identity()

p3 <- ggplot(esp_can) +
  geom_sf(color = "black", size = 0.7) +
  geom_sf(data = can_prov) +
  geom_sf(data = can_box) +
  geom_sf(data = mapa, aes(fill = Alquiler11Prov), color = "black", size = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "white") +  
  geom_sf_text(data = mapa, aes(label = round(Alquiler11Prov, 1), color = "black"), 
               size = 3, family = "Montserrat", check_overlap = TRUE, nudge_y = -0.03) +
  theme_void() +
  labs(title = "% of Young People in 'Rent' Residence Status in 2011") +
  coord_sf() +
  scale_fill_viridis_c(option = "C", direction = -1) +
  scale_color_identity()

p4 <- ggplot(esp_can) +
  geom_sf(color = "black", size = 0.7) +
  geom_sf(data = can_prov) +
  geom_sf(data = can_box) +
  geom_sf(data = mapa, aes(fill = Otro11Prov), color = "black", size = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "white") +  
  geom_sf_text(data = mapa, aes(label = round(Otro11Prov, 1), color = "black"), 
               size = 3, family = "Montserrat", check_overlap = TRUE, nudge_y = -0.03) +
  theme_void() +
  labs(title = "% of Young People in 'Other' Residence Status in 2011") +
  coord_sf() +
  scale_fill_viridis_c(option = "C", direction = -1) +
  scale_color_identity()

p5 <- ggplot(esp_can) +
  geom_sf(color = "black", size = 0.7) +
  geom_sf(data = can_prov) +
  geom_sf(data = can_box) +
  geom_sf(data = mapa, aes(fill = Propiedad21Prov), color = "black", size = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "white") +  
  geom_sf_text(data = mapa, aes(label = round(Propiedad21Prov, 1), color = ifelse(Propiedad21Prov > 75, "white", "black")), 
               size = 3, family = "Montserrat", check_overlap = TRUE, nudge_y = -0.03) +
  theme_void() +
  labs(title = "% of Young People in 'Ownership' Residence Status in 2021") +
  coord_sf() +
  scale_fill_viridis_c(option = "C", direction = -1) +
  scale_color_identity()

p6 <- ggplot(esp_can) +
  geom_sf(color = "black", size = 0.7) +
  geom_sf(data = can_prov) +
  geom_sf(data = can_box) +
  geom_sf(data = mapa, aes(fill = Alquiler21Prov), color = "black", size = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "white") +  
  geom_sf_text(data = mapa, aes(label = round(Alquiler21Prov, 1), color = ifelse(Alquiler21Prov > 38, "white", "black")), 
               size = 3, family = "Montserrat", check_overlap = TRUE, nudge_y = -0.03) +
  theme_void() +
  labs(title = "% of Young People in 'Rent' Residence Status in 2021") +
  coord_sf() +
  scale_fill_viridis_c(option = "C", direction = -1) +
  scale_color_identity()

p7 <- ggplot(esp_can) +
  geom_sf(color = "black", size = 0.7) +
  geom_sf(data = can_prov) +
  geom_sf(data = can_box) +
  geom_sf(data = mapa, aes(fill = Otro21Prov), color = "black", size = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "white") +  
  geom_sf_text(data = mapa, aes(label = round(Otro21Prov, 1), color = ifelse(Otro21Prov > 19, "white", "black")), 
               size = 3, family = "Montserrat", check_overlap = TRUE, nudge_y = -0.03) +
  theme_void() +
  labs(title = "% of Young People in 'Other' Residence Status in 2021") +
  coord_sf() +
  scale_fill_viridis_c(option = "C", direction = -1) +
  scale_color_identity()

p8 <- ggplot(esp_can) +
  geom_sf(color = "black", size = 0.7) +
  geom_sf(data = can_prov) +
  geom_sf(data = can_box) +
  geom_sf(data = mapa, aes(fill = PropiedadDifProv), color = "black", size = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "white") +
  geom_sf_text(data = mapa, aes(label = round(PropiedadDifProv, 1)),
               color = "black", size = 2.4, check_overlap = TRUE, nudge_y = -0.03, family = "Montserrat") +
  theme_void() +
  labs(title = "Difference in % of Young People in 'Ownership' Residence Status\nfrom 2011 to 2021") +  # Added newline for better title layout
  coord_sf() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) 

p9<- ggplot(esp_can) +
  geom_sf(color = "black", size = 0.7) +
  geom_sf(data = can_prov) +
  geom_sf(data = can_box) +
  geom_sf(data = mapa, aes(fill = AlquilerDifProv), color = "black", size = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "white") +
  geom_sf_text(data = mapa, aes(label = round(AlquilerDifProv, 1)),
               color = "black", size = 2.4, check_overlap = TRUE, nudge_y = -0.03, family = "Montserrat") +
  theme_void() +
  labs(title = "Difference in % of Young People in 'Rent' Residence Status\nfrom 2011 to 2021") +  # Added newline for better title layout
  coord_sf() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) 

p10 <- ggplot(esp_can) +
  geom_sf(color = "black", size = 0.7) +
  geom_sf(data = can_prov) +
  geom_sf(data = can_box) +
  geom_sf(data = mapa, aes(fill = OtroDifProv), color = "black", size = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "white") +
  geom_sf_text(data = mapa, aes(label = round(OtroDifProv, 1)),
               color = "black", size = 2.4, check_overlap = TRUE, nudge_y = -0.03, family = "Montserrat") +
  theme_void() +
  labs(title = "Difference in % of Young People in 'Other' Residence Status\nfrom 2011 to 2021") +
  coord_sf() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0)


ui <- dashboardPage(
  dashboardHeader(title = "Plot Visualizer"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Maps by Tenure Regime", tabName = "maps", icon = icon("fa-solid fa-map")),
      menuItem("Clusterplot", tabName = "clusterplot", icon = icon("fa-solid fa-shapes")),
      menuItem("Scatterplot", tabName = "scatterplot", icon = icon("fa-solid fa-chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "maps",
              fluidRow(
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  selectInput("category", "Select the category", 
                              choices = c("2011", "2021", "Difference over the years")),
                  uiOutput("graph_selector")
                ),
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 9,
                  plotOutput("plot", height = "500px")
                )
              )
      ),
      tabItem(tabName = "clusterplot",
              fluidRow(
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("clusterplot", height = "600px")
                )
              )
      ),
      tabItem(tabName = "scatterplot",
              fluidRow(
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  textInput("search", "Search municipality:", "")
                ),
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 9,
                  plotlyOutput("scatterplot", height = "600px", width = "100%")
                )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$graph_selector <- renderUI({
    category <- input$category
    print(paste("Category selected:", category))  
    choices <- if (category == "2011") {
      c("Ownership", "Inheritance", "Rent", "Other")
    } else {
      c("Ownership", "Rent", "Other")
    }
    selectInput("graph", "Select tenure regime", choices = choices, selected = choices[1]) 
  })
  
  output$plot <- renderPlot({
    req(input$graph)
    req(input$category)
    category <- input$category
    graph <- input$graph
    
    print(paste("Category:", category, "Graph:", graph))  
    
    plot <- NULL
    if (category == "2011") {
      if (graph == "Ownership") {
        plot <- p1
      } else if (graph == "Inheritance") {
        plot <- p2
      } else if (graph == "Rent") {
        plot <- p3
      } else if (graph == "Other") {
        plot <- p4
      }
    } else if (category == "2021") {
      if (graph == "Ownership") {
        plot <- p5
      } else if (graph == "Rent") {
        plot <- p6
      } else if (graph == "Other") {
        plot <- p7
      }
    } else if (category == "Difference over the years") {
      if (graph == "Ownership") {
        plot <- p8
      } else if (graph == "Rent") {
        plot <- p9
      } else if (graph == "Other") {
        plot <- p10
      }
    }
    
    validate(
      need(!is.null(plot), "No plot available. Please check your inputs.")
    )
    
    print("Plot is ready to be rendered.")  
    print(plot) 
    
    plot
  }, height = 500, width = 700)
  
  
  output$scatterplot <- renderPlotly({
    search_term <- input$search
    filtered_data <- if (search_term != "") {
      municipios[grepl(search_term, municipios$MUN_LITERAL, ignore.case = TRUE), ]
    } else {
      municipios
    }
    scatterplot <- ggplot(filtered_data, aes(x = cusec, y = PropiedadDifMun, color = CCAA)) +
      geom_point(aes(text = paste(MUN_LITERAL, "PropiedadDifMun:", PropiedadDifMun))) +
      geom_text(aes(label = MUN_LITERAL), hjust = 1.5, vjust = 0.5, size = 2.5) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 1),
            axis.ticks.y = element_blank()) +
      labs(title = "Interactive Scatterplot - Municipalities x Ownership change",
           x = "PropiedadDifMun",
           y = "")
    plotly::ggplotly(scatterplot, tooltip = c("MUN_LITERAL", "PropiedadDifMun")) |>
      layout(
        hoverlabel = list(
          bgcolor = "white",
          bordercolor = "black",
          font = list(size = 12)
        ),
        xaxis = list(title = "PropiedadDifMun"),
        yaxis = list(title = "", tickvals = NULL)
      )
  })
  
  municipios <- municipios |> 
    mutate(MUN_LITERAL = ifelse(grepl("\\d", MUN_LITERAL), paste(MUN_LITERAL, PROV_LITERAL), MUN_LITERAL))
  
  names <- municipios$MUN_LITERAL
  prov <- municipios$PROV_LITERAL
  ccaa <- municipios$CCAA
  numeric_data <- municipios |>
    ungroup() |> 
    select_if(is.numeric)
  
  numeric_data <- scale(numeric_data)
  numeric_data <- as.data.frame(numeric_data)
  
  selected_vars <- numeric_data |> 
    select(PropiedadDifMun, AlquilerDifMun, OtroDifMun, Herencia11Mun, PorcentajeJovenDif, VaciasDif, SecundariasDif, PorcentajeNoEuropeaDif, DifPrecioHipotecaProv, ParoJuvenilDif, ProtegidaDif, PorcentajeTurísticas2021, PrecioAlquilerM2, RentaMedia2021)
  
  set.seed(123) 
  pca <- prcomp(selected_vars, scale = TRUE)
  fit <- kmeans(selected_vars, centers = 5, nstart = 100)
  
  output$clusterplot <- renderPlotly({
    p <- fviz_cluster(fit, data = selected_vars, geom = c("point"), ellipse.type = 'norm', pointsize = 1, show.clust.cent = FALSE) +
      theme_minimal() + geom_text(label = names, hjust = 0, vjust = 0, size = 2, check_overlap = TRUE) +
      scale_fill_brewer(palette = "Paired", guide = FALSE)
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)