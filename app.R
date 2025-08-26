library(shiny)
library(shinydashboard)
library(DT)
library(lpSolve)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Problema de Transporte"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Configuração", tabName = "setup", icon = icon("cog")),
      menuItem("Solução", tabName = "solution", icon = icon("chart-bar")),
      menuItem("Sobre", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab de Configuração
      tabItem(tabName = "setup",
              fluidRow(
                box(
                  title = "Parâmetros do Problema",
                  width = 12,
                  numericInput("num_sources", "Número de Fontes", value = 3, min = 2, max = 10),
                  numericInput("num_destinations", "Número de Destinos", value = 4, min = 2, max = 10),
                  actionButton("generate_matrix", "Gerar Matriz de Custos", class = "btn-primary")
                )
              ),
              
              fluidRow(
                box(
                  title = "Ofertas das Fontes",
                  width = 6,
                  uiOutput("supply_inputs")
                ),
                box(
                  title = "Demandas dos Destinos",
                  width = 6,
                  uiOutput("demand_inputs")
                )
              ),
              
              fluidRow(
                box(
                  title = "Matriz de Custos de Transporte",
                  width = 12,
                  uiOutput("cost_matrix_input"),
                  br(),
                  actionButton("solve", "Resolver Problema", class = "btn-success"),
                  actionButton("reset", "Reiniciar", class = "btn-danger")
                )
              )
      ),
      
      # Tab de Solução
      tabItem(tabName = "solution",
              fluidRow(
                valueBoxOutput("total_cost_box", width = 4),
                valueBoxOutput("status_box", width = 4),
                valueBoxOutput("iterations_box", width = 4)
              ),
              
              fluidRow(
                box(
                  title = "Matriz de Custos",
                  width = 6,
                  DTOutput("cost_table")
                ),
                box(
                  title = "Solução Ótima - Alocação",
                  width = 6,
                  DTOutput("solution_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Resumo das Restrições",
                  width = 12,
                  DTOutput("constraints_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Visualização da Solução",
                  width = 12,
                  plotOutput("solution_plot", height = "400px")
                )
              )
      ),
      
      # Tab Sobre
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "Sobre a Aplicação",
                  width = 12,
                  tags$div(
                    tags$h3("Problema de Transporte"),
                    tags$p("Esta aplicação resolve problemas de transporte usando o método de programação linear."),
                    tags$p("O problema de transporte consiste em determinar a forma mais econômica de transportar mercadorias de várias fontes para vários destinos."),
                    tags$hr(),
                    tags$h4("Como usar:"),
                    tags$ol(
                      tags$li("Configure o número de fontes e destinos"),
                      tags$li("Preencha as ofertas de cada fonte"),
                      tags$li("Preencha as demandas de cada destino"),
                      tags$li("Preencha a matriz de custos de transporte"),
                      tags$li("Clique em 'Resolver Problema' para obter a solução ótima")
                    ),
                    tags$hr(),
                    tags$p("Desenvolvido com Shiny e lpSolve")
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    cost_matrix = NULL,
    supply = NULL,
    demand = NULL,
    solution = NULL
  )
  
  # Gerar inputs dinâmicos para ofertas
  output$supply_inputs <- renderUI({
    num_sources <- input$num_sources
    lapply(1:num_sources, function(i) {
      numericInput(paste0("supply_", i), paste("Oferta Fonte", i), 
                   value = sample(30:100, 1), min = 0)
    })
  })
  
  # Gerar inputs dinâmicos para demandas
  output$demand_inputs <- renderUI({
    num_dest <- input$num_destinations
    lapply(1:num_dest, function(i) {
      numericInput(paste0("demand_", i), paste("Demanda Destino", i), 
                   value = sample(20:80, 1), min = 0)
    })
  })
  
  # Gerar matriz de custos dinâmica
  output$cost_matrix_input <- renderUI({
    req(input$generate_matrix)
    
    num_sources <- input$num_sources
    num_dest <- input$num_destinations
    
    fluidRow(
      lapply(1:num_sources, function(i) {
        column(2,
               lapply(1:num_dest, function(j) {
                 numericInput(paste0("cost_", i, "_", j), 
                              paste0("F", i, "→D", j),
                              value = sample(5:20, 1), min = 0)
               })
        )
      })
    )
  })
  
  # Coletar dados quando o botão de resolver for clicado
  observeEvent(input$solve, {
    num_sources <- input$num_sources
    num_dest <- input$num_destinations
    
    # Coletar ofertas
    supply <- sapply(1:num_sources, function(i) {
      input[[paste0("supply_", i)]]
    })
    
    # Coletar demandas
    demand <- sapply(1:num_dest, function(i) {
      input[[paste0("demand_", i)]]
    })
    
    # Coletar custos
    costs <- matrix(0, nrow = num_sources, ncol = num_dest)
    for (i in 1:num_sources) {
      for (j in 1:num_dest) {
        costs[i, j] <- input[[paste0("cost_", i, "_", j)]]
      }
    }
    
    # Verificar se os dados são válidos
    if (any(is.na(supply)) || any(is.na(demand)) || any(is.na(costs))) {
      showNotification("Por favor, preencha todos os campos.", type = "error")
      return()
    }
    
    if (sum(supply) != sum(demand)) {
      showNotification("A oferta total deve ser igual à demanda total.", type = "error")
      return()
    }
    
    # Armazenar valores
    values$cost_matrix <- costs
    values$supply <- supply
    values$demand <- demand
    
    # Resolver o problema de transporte
    withProgress(message = 'Resolvendo problema...', value = 0, {
      incProgress(0.5, detail = "Calculando solução ótima")
      
      # Configurar e resolver o problema
      row.signs <- rep("=", num_sources)
      col.signs <- rep("=", num_dest)
      
      result <- lp.transport(values$cost_matrix, "min", 
                             row.signs, values$supply, 
                             col.signs, values$demand)
      
      values$solution <- list(
        solution_matrix = result$solution,
        total_cost = result$objval,
        status = result$status
      )
      
      incProgress(1, detail = "Solução encontrada")
    })
  })
  
  # Tabela de custos
  output$cost_table <- renderDT({
    req(values$cost_matrix)
    
    costs <- values$cost_matrix
    rownames(costs) <- paste("Fonte", 1:nrow(costs))
    colnames(costs) <- paste("Destino", 1:ncol(costs))
    
    datatable(costs, 
              options = list(dom = 't', pageLength = 10),
              caption = "Custos unitários de transporte (Fonte → Destino)") %>%
      formatStyle(names(costs), backgroundColor = '#f9f9f9')
  })
  
  # Tabela de solução
  output$solution_table <- renderDT({
    req(values$solution)
    
    solution <- values$solution$solution_matrix
    rownames(solution) <- paste("Fonte", 1:nrow(solution))
    colnames(solution) <- paste("Destino", 1:ncol(solution))
    
    datatable(solution, 
              options = list(dom = 't', pageLength = 10),
              caption = "Quantidade a transportar de cada fonte para cada destino") %>%
      formatStyle(names(solution), backgroundColor = '#e8f5e8')
  })
  
  # Tabela de restrições
  output$constraints_table <- renderDT({
    req(values$supply, values$demand, values$solution)
    
    solution <- values$solution$solution_matrix
    supply_used <- rowSums(solution)
    demand_met <- colSums(solution)
    
    constraints_df <- data.frame(
      Tipo = c(rep("Oferta", length(values$supply)), rep("Demanda", length(values$demand))),
      Item = c(paste("Fonte", 1:length(values$supply)), paste("Destino", 1:length(values$demand))),
      Limite = c(values$supply, values$demand),
      Utilizado = c(supply_used, demand_met),
      Atendido = c(ifelse(supply_used == values$supply, "Sim", "Não"),
                   ifelse(demand_met == values$demand, "Sim", "Não"))
    )
    
    datatable(constraints_df, 
              options = list(dom = 't', pageLength = 10),
              caption = "Verificação das restrições de oferta e demanda") %>%
      formatStyle('Atendido', 
                  backgroundColor = styleEqual(c("Sim", "Não"), c('#e8f5e8', '#ffebee')))
  })
  
  # Value Box - Custo Total
  output$total_cost_box <- renderValueBox({
    req(values$solution)
    valueBox(
      paste("R$", format(round(values$solution$total_cost, 2), big.mark = ",")),
      "Custo Total Mínimo",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  # Value Box - Status
  output$status_box <- renderValueBox({
    req(values$solution)
    status_text <- ifelse(values$solution$status == 0, "Ótimo Encontrado", "Erro na Solução")
    valueBox(
      status_text,
      "Status da Solução",
      icon = icon(ifelse(values$solution$status == 0, "check-circle", "exclamation-triangle")),
      color = ifelse(values$solution$status == 0, "purple", "red")
    )
  })
  
  # Value Box - Dimensões
  output$iterations_box <- renderValueBox({
    req(values$cost_matrix)
    valueBox(
      paste(nrow(values$cost_matrix), "×", ncol(values$cost_matrix)),
      "Fontes × Destinos",
      icon = icon("table"),
      color = "blue"
    )
  })
  
  # Gráfico da solução
  output$solution_plot <- renderPlot({
    req(values$solution)
    
    solution <- values$solution$solution_matrix
    df <- reshape2::melt(solution)
    colnames(df) <- c("Fonte", "Destino", "Quantidade")
    
    df$Fonte <- paste("Fonte", df$Fonte)
    df$Destino <- paste("Destino", df$Destino)
    
    ggplot(df, aes(x = Destino, y = Fonte, fill = Quantidade, label = round(Quantidade, 1))) +
      geom_tile(color = "white", linewidth = 1) +
      geom_text(color = "white", fontface = "bold", size = 5) +
      scale_fill_gradient(low = "#2196F3", high = "#FF5252", 
                          name = "Quantidade") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "right"
      ) +
      labs(
        title = "Matriz de Alocação Ótima",
        x = "Destinos",
        y = "Fontes"
      ) +
      coord_fixed()
  })
  
  # Reiniciar aplicação
  observeEvent(input$reset, {
    values$cost_matrix <- NULL
    values$supply <- NULL
    values$demand <- NULL
    values$solution <- NULL
    
    updateNumericInput(session, "num_sources", value = 3)
    updateNumericInput(session, "num_destinations", value = 4)
    
    showNotification("Aplicação reiniciada. Configure novos parâmetros.", type = "message")
  })
}

# Run the application
shinyApp(ui = ui, server = server)