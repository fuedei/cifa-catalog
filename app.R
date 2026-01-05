library(shiny)
library(dplyr)
library(readr)
library(DT)
library(markdown)

db <- read_csv("data/Cifa_Editado.csv") %>%
  dplyr::mutate(ID_interno = dplyr::row_number())

ui <- fluidPage(
  
  # theme = shinythemes::shinytheme("paper"),
  
    # ---- encabezado con logo + título ----
    tags$div(
      style = "text-align: center;",
      
      br(), br(),
      
      tags$img(
        src = "logo_fuedei.png",
        height = "40px"
      ),
      
      tags$h2("Catálogo de Insectos Fitófagos de la Argentina y sus plantas asociadas"),
      tags$h3("Catalog of Phytofagous insects of Argentina"),
      tags$p("Directores: Hugo A. Cordo – Guillermo Logarzo – Karen Braun – Osvaldo R. Di Iorio")
    ),
    
    tags$p(
      "Esta obra compila información publicada e inédita de la Argentina sobre insectos herbívoros y sus plantas",
      "asociadas desde 1861 hasta el 2004. Se presentan 20.000 asociaciones insecto-planta, que incluyen cerca de 3.400", 
      "especies de insectos, 2.400 especies de plantas y se citan más de 1.500 referencias bibliográficas."
    ),
    
  sidebarLayout(
    sidebarPanel(
      
      tabsetPanel(
        id = "filtro_tipo",
        
        tabPanel("Buscar por insectos",
                 
                 selectInput("orden", "Orden",
                             choices = c("", sort(unique(db$Orden)))),
                 
                 selectInput("familia", "Familia", choices = ""),
                 selectInput("subfamilia", "Subfamilia", choices = ""),
                 selectInput("genero", "Género", choices = ""),
                 selectInput("especie", "Especie", choices = "")
        ),
        
        tabPanel("Buscar por plantas",
                 
                 selectInput("genero_bot", "Género planta",
                             choices = c("", sort(unique(db$Genero_Bot)))),
                 
                 selectInput("especie_bot2", "Especie Planta",
                             choices = "")
        ),
        
        tabPanel("Búsqueda libre",
                 textInput("busqueda", "Buscar en toda la tabla:", "")
        ),
        
        tabPanel(
          "Información",
          
          fluidPage(
            
            mainPanel(
              includeMarkdown("texto_info.md")
            )
          )
        )
        
      ),
      
      br(),
      
      actionButton("reset", "Resetear búsqueda"),
      
      br(), br(), br(),
      
      tags$p("Fundación para el Estudio de Especies Invasivas (FuEDEI) - 2026"),
      tags$p("Adaptado a formato digital por A.F. Sánchez-Restrepo")
      
    ),
    
    mainPanel(
      h4("Resultados"),
      downloadButton("descargar", "Descargar resultados"),
      br(), br(),
      DTOutput("tabla"),
      
    )
  )
)



server <- function(input, output, session){
  
  safe <- function(x) {
    ifelse(is.na(x) | x == "", "—", x)
  }
  
  # ----- estado: mostrar o no resultados -----
  search_done <- reactiveVal(FALSE)
  
  # ----- filtros dinámicos insectos -----
  
  observeEvent(input$orden, {
    familias <- db %>%
      filter(Orden == input$orden) %>%
      pull(Familia) %>% unique() %>% sort()
    
    updateSelectInput(session, "familia", choices = c("", familias))
  })
  
  observeEvent(input$familia, {
    subf <- db %>%
      filter(Orden == input$orden,
             Familia == input$familia) %>%
      pull(Subfamilia) %>% unique() %>% sort()
    
    updateSelectInput(session, "subfamilia", choices = c("", subf))
  })
  
  observeEvent(input$subfamilia, {
    gen <- db %>%
      filter(Orden == input$orden,
             Familia == input$familia,
             Subfamilia == input$subfamilia) %>%
      pull(Genero) %>% unique() %>% sort()
    
    updateSelectInput(session, "genero", choices = c("", gen))
  })
  
  observeEvent(input$genero, {
    esp <- db %>%
      filter(Orden == input$orden,
             Familia == input$familia,
             Subfamilia == input$subfamilia,
             Genero == input$genero) %>%
      pull(Especie) %>% unique() %>% sort()
    
    updateSelectInput(session, "especie", choices = c("", esp))
  })
  
  # ----- filtros dinámicos plantas -----
  
  observeEvent(input$genero_bot, {
    espb <- db %>%
      filter(Genero_Bot == input$genero_bot) %>%
      pull(Especie_Bot2) %>% unique() %>% sort()
    
    updateSelectInput(session, "especie_bot2", choices = c("", espb))
  })
  
  # cuando el usuario empieza a filtrar insectos
  observeEvent(
    list(input$orden, input$familia, input$subfamilia, input$genero, input$especie),
    ignoreInit = TRUE,
    {
      search_done(TRUE)
    }
  )
  
  # cuando filtra plantas
  observeEvent(
    list(input$genero_bot, input$especie_bot2),
    ignoreInit = TRUE,
    {
      search_done(TRUE)
    }
  )
  
  # cuando usa búsqueda libre
  observeEvent(input$busqueda, ignoreInit = TRUE, {
    if (nzchar(input$busqueda)) search_done(TRUE)
  })
  
  # ----- reset -----
  observeEvent(input$reset, {
    
    updateSelectInput(session, "orden", selected = "")
    updateSelectInput(session, "familia", choices = "", selected = "")
    updateSelectInput(session, "subfamilia", choices = "", selected = "")
    updateSelectInput(session, "genero", choices = "", selected = "")
    updateSelectInput(session, "especie", choices = "", selected = "")
    
    updateSelectInput(session, "genero_bot", selected = "")
    updateSelectInput(session, "especie_bot2", choices = "", selected = "")
    
    updateTextInput(session, "busqueda", value = "")
    
    search_done(FALSE)
  })
  
  
  # ----- lógica de filtrado -----
  datos_filtrados <- reactive({
    
    if (!search_done()) return(NULL)
    
    if (input$filtro_tipo == "Buscar por insectos") {
      f <- db
      if (input$orden != "")      f <- f %>% filter(Orden == input$orden)
      if (input$familia != "")    f <- f %>% filter(Familia == input$familia)
      if (input$subfamilia != "") f <- f %>% filter(Subfamilia == input$subfamilia)
      if (input$genero != "")     f <- f %>% filter(Genero == input$genero)
      if (input$especie != "")    f <- f %>% filter(Especie == input$especie)
      return(f)
    }
    
    if (input$filtro_tipo == "Buscar por plantas") {
      f <- db
      if (input$genero_bot != "")   f <- f %>% filter(Genero_Bot == input$genero_bot)
      if (input$especie_bot2 != "") f <- f %>% filter(Especie_Bot2 == input$especie_bot2)
      return(f)
    }
    
    if (input$filtro_tipo == "Búsqueda libre" && input$busqueda != "") {
      texto <- tolower(input$busqueda)
      f <- db %>%
        filter(apply(., 1, function(r)
          any(grepl(texto, tolower(r), fixed = TRUE))))
      return(f)
    }
    
    NULL
  })
  
  output$descargar <- downloadHandler(
    
    filename = function() {
      paste0("resultados_resumen_", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      
      df <- datos_filtrados()
      
      if (is.null(df)) {
        write.csv(data.frame(), file, row.names = FALSE)
      } else {
        df %>%
          dplyr::select(
            ID,
            Orden, Familia, Subfamilia, Especie,
            Especie_Bot2,
            Referencia_corta
          ) %>%
          write.csv(file, row.names = FALSE)
      }
    }
  )
  
  
  
  # ----- resumen -----
  output$tabla <- renderDT({
    
    df <- datos_filtrados()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df <- df %>%
      select(
        ID_interno,   # ← lo usamos solo internamente
        ID,
        Orden, Familia, Subfamilia, 
        Especie_Insecto = Especie,
        Especies_Planta = Especie_Bot2,
        Referencia_corta
      ) %>%
      mutate(
        Ver = sprintf(
          '<a href="#" class="ver-detalle" data-id="%s">Ver detalle</a>',
          ID_interno
        )
      )
    
    
    datatable(
      df,
      escape = FALSE,
      selection = "none",
      options = list(
        pageLength = 10,
        columnDefs = list(
          list(visible = FALSE, targets = 0),   # ← oculta ID_interno
          list(orderable = FALSE, targets = ncol(df))
        )
      ),
      callback = JS(
        "
      table.on('click', 'a.ver-detalle', function() {
        var id = $(this).data('id');
        Shiny.setInputValue('ver_detalle', id, {priority: 'event'});
      });
    "
      )
    )%>%
      formatStyle(
        "Especie_Insecto",
        fontStyle = "italic"
      ) %>%
      formatStyle(
        "Especies_Planta",
        fontStyle = "italic"
      )
    
    
  })
  
  # detalle
  
  observeEvent(input$ver_detalle, {
    
    fila <- db %>% filter(ID_interno == input$ver_detalle)
    
    showModal(
      modalDialog(
        title = paste("Detalle — ID:", fila$ID),
        
        HTML(paste0(
          "<b>Familia:</b> ", safe(fila$Familia), "<br>",
          "<b>Subfamilia:</b> ", safe(fila$Subfamilia), "<br>",
          "<b>Especie:</b><i> ", safe(fila$Especie), "</i><br>",
          "<b>Planta asociada:</b><i> ", safe(fila$Especie_Bot2), "</i><br><br>",
          "<b>Referencia:</b><br>", safe(fila$Referencia_corta)
        )),
        
        easyClose = TRUE,
        footer = modalButton("Cerrar"),
        size = "l"
      )
    )
    
  })
  

  
}

shinyApp(ui, server)
