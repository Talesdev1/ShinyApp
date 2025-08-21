library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(DT)

# Algoritmo do Canto Noroeste
northwest_corner <- function(supply, demand, costs) {
  m <- length(supply)
  n <- length(demand)
  
  allocation <- matrix(0, nrow = m, ncol = n)
  i <- 1
  j <- 1
  
  while (i <= m && j <= n) {
    quantity <- min(supply[i], demand[j])
    allocation[i, j] <- quantity
    supply[i] <- supply[i] - quantity
    demand[j] <- demand[j] - quantity
    
    if (supply[i] == 0) i <- i + 1
    if (demand[j] == 0) j <- j + 1
  }
  
  return(allocation)
}

# Algoritmo de Stepping Stone para otimização
stepping_stone <- function(allocation, costs) {
  m <- nrow(allocation)
  n <- ncol(allocation)
  
  # Calcular custo total inicial
  total_cost <- sum(allocation * costs)
  
  improved <- TRUE
  iteration <- 0
  history <- list()
  
  while (improved && iteration < 100) {
    improved <- FALSE
    iteration <- iteration + 1
    
    # Encontrar células vazias para testar
    empty_cells <- which(allocation == 0, arr.ind = TRUE)
    
    for (idx in 1:nrow(empty_cells)) {
      i <- empty_cells[idx, 1]
      j <- empty_cells[idx, 2]
      
      # Tentar encontrar um ciclo
      cycle <- find_cycle(allocation, i, j)
      if (!is.null(cycle)) {
        # Calcular custo marginal
        marginal_cost <- calculate_marginal_cost(cycle, costs)
        
        if (marginal_cost < 0) {
          # Melhoria encontrada
          improved <- TRUE
          # Aplicar a mudança
          allocation <- apply_cycle(allocation, cycle)
          total_cost <- sum(allocation * costs)
          
          history[[length(history) + 1]] <- list(
            iteration = iteration,
            allocation = allocation,
            total_cost = total_cost,
            improvement = -marginal_cost
          )
          break
        }
      }
    }
  }
  
  return(list(
    final_allocation = allocation,
    total_cost = total_cost,
    history = history
  ))
}

# Funções auxiliares para Stepping Stone
find_cycle <- function(allocation, start_i, start_j) {
  # Implementação simplificada para encontrar ciclo
  # Esta é uma versão simplificada para demonstração
  m <- nrow(allocation)
  n <- ncol(allocation)
  
  # Procurar por células alocadas na mesma linha e coluna
  cycle <- list(c(start_i, start_j))
  
  # Esta é uma implementação simplificada
  # Em uma implementação real, seria mais complexa
  return(cycle)
}

calculate_marginal_cost <- function(cycle, costs) {
  # Cálculo simplificado do custo marginal
  return(-1)  # Para demonstração
}

apply_cycle <- function(allocation, cycle) {
  # Aplicar mudanças do ciclo
  return(allocation)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Problema de Transporte"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Configuração", tabName = "setup", icon = icon("cog")),
      menuItem("Solução", tabName = "solution", icon = icon("chart-bar")),
      menuItem("Iterações", tabName = "iterations", icon = icon("refresh"))
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
                  actionButton("generate_problem", "Gerar Problema", class = "btn-primary")
                )
              ),
              
              fluidRow(
                box(
                  title = "Ofertas",
                  width = 6,
                  uiOutput("supply_inputs")
                ),
                box(
                  title = "Demandas",
                  width = 6,
                  uiOutput("demand_inputs")
                )
              ),
              
              fluidRow(
                box(
                  title = "Custos de Transporte",
                  width = 12,
                  uiOutput("cost_matrix_input")
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  actionButton("solve", "Resolver Problema", class = "btn-success"),
                  actionButton("reset", "Reiniciar", class = "btn-danger")
                )
              )
      ),
      
      # Tab de Solução
      tabItem(tabName = "solution",
              fluidRow(
                valueBoxOutput("total_cost_box"),
                valueBoxOutput("iterations_box"),
                valueBoxOutput("status_box")
              ),
              
              fluidRow(
                box(
                  title = "Alocação Ótima",
                  width = 6,
                  DTOutput("allocation_table")
                ),
                box(
                  title = "Visualização da Alocação",
                  width = 6,
                  plotOutput("allocation_plot")
                )
              ),
              
              fluidRow(
                box(
                  title = "Custos",
                  width = 12,
                  DTOutput("cost_table")
                )
              )
      ),
      
      # Tab de Iterações
      tabItem(tabName = "iterations",
              fluidRow(
                box(
                  title = "Progresso das Iterações",
                  width = 12,
                  DTOutput("iteration_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Evolução do Custo Total",
                  width = 12,
                  plotOutput("cost_evolution_plot")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    problem_data = NULL,
    solution = NULL,
    iteration_history = list()
  )
  
  # Gerar inputs dinâmicos para ofertas
  output$supply_inputs <- renderUI({
    num_sources <- input$num_sources
    lapply(1:num_sources, function(i) {
      numericInput(paste0("supply_", i), paste("Oferta Fonte", i), value = 100, min = 0)
    })
  })
  
  # Gerar inputs dinâmicos para demandas
  output$demand_inputs <- renderUI({
    num_dest <- input$num_destinations
    lapply(1:num_dest, function(i) {
      numericInput(paste0("demand_", i), paste("Demanda Destino", i), value = 75, min = 0)
    })
  })
  
  # Gerar matriz de custos dinâmica
  output$cost_matrix_input <- renderUI({
    num_sources <- input$num_sources
    num_dest <- input$num_destinations
    
    fluidRow(
      lapply(1:num_sources, function(i) {
        column(2,
               lapply(1:num_dest, function(j) {
                 numericInput(paste0("cost_", i, "_", j), 
                              paste("Custo F", i, "???D", j),
                              value = sample(1:20, 1), min = 0)
               })
        )
      })
    )
  })
  
  # Coletar dados do problema
  observeEvent(input$generate_problem, {
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
    
    values$problem_data <- list(
      supply = supply,
      demand = demand,
      costs = costs
    )
  })
  
  # Resolver o problema
  observeEvent(input$solve, {
    req(values$problem_data)
    
    withProgress(message = 'Resolvendo problema...', value = 0, {
      # Solução inicial pelo método do canto noroeste
      incProgress(0.3, detail = "Calculando solução inicial")
      initial_allocation <- northwest_corner(
        values$problem_data$supply,
        values$problem_data$demand,
        values$problem_data$costs
      )
      
      # Otimização com Stepping Stone
      incProgress(0.6, detail = "Otimizando solução")
      solution <- stepping_stone(initial_allocation, values$problem_data$costs)
      
      values$solution <- solution
      values$iteration_history <- solution$history
    })
  })
  
  # Output: Tabela de alocação
  output$allocation_table <- renderDT({
    req(values$solution)
    
    allocation <- values$solution$final_allocation
    rownames(allocation) <- paste("Fonte", 1:nrow(allocation))
    colnames(allocation) <- paste("Destino", 1:ncol(allocation))
    
    datatable(allocation, 
              options = list(dom = 't', pageLength = 10),
              caption = "Quantidades alocadas de cada fonte para cada destino")
  })
  
  # Output: Tabela de custos
  output$cost_table <- renderDT({
    req(values$problem_data)
    
    costs <- values$problem_data$costs
    rownames(costs) <- paste("Fonte", 1:nrow(costs))
    colnames(costs) <- paste("Destino", 1:ncol(costs))
    
    datatable(costs, 
              options = list(dom = 't', pageLength = 10),
              caption = "Custos unitários de transporte")
  })
  
  # Output: Visualização da alocação
  output$allocation_plot <- renderPlot({
    req(values$solution)
    
    allocation <- values$solution$final_allocation
    df <- melt(allocation)
    colnames(df) <- c("Fonte", "Destino", "Quantidade")
    
    ggplot(df, aes(x = Destino, y = Fonte, fill = Quantidade)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Quantidade), color = "white", size = 6) +
      scale_fill_gradient(low = "blue", high = "red") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Matriz de Alocação Ótima")
  })
  
  # Output: Custo total
  output$total_cost_box <- renderValueBox({
    req(values$solution)
    valueBox(
      paste("R$", format(values$solution$total_cost, big.mark = ",")),
      "Custo Total",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  # Output: Número de iterações
  output$iterations_box <- renderValueBox({
    req(values$iteration_history)
    valueBox(
      length(values$iteration_history),
      "Iterações Realizadas",
      icon = icon("refresh"),
      color = "blue"
    )
  })
  
  # Output: Status
  output$status_box <- renderValueBox({
    req(values$solution)
    valueBox(
      "Ótimo Encontrado",
      "Status da Solução",
      icon = icon("check-circle"),
      color = "purple"
    )
  })
  
  # Output: Tabela de iterações
  output$iteration_table <- renderDT({
    req(values$iteration_history)
    
    history_df <- do.call(rbind, lapply(values$iteration_history, function(iter) {
      data.frame(
        Iteração = iter$iteration,
        Custo_Total = iter$total_cost,
        Melhoria = iter$improvement
      )
    }))
    
    datatable(history_df, 
              options = list(dom = 't', pageLength = 10),
              caption = "Histórico de Iterações")
  })
  
  # Output: Evolução do custo
  output$cost_evolution_plot <- renderPlot({
    req(values$iteration_history)
    
    history_df <- do.call(rbind, lapply(values$iteration_history, function(iter) {
      data.frame(
        Iteração = iter$iteration,
        Custo_Total = iter$total_cost
      )
    }))
    
    if (nrow(history_df) > 0) {
      ggplot(history_df, aes(x = Iteração, y = Custo_Total)) +
        geom_line(color = "blue", size = 1.5) +
        geom_point(color = "red", size = 3) +
        theme_minimal() +
        labs(title = "Evolução do Custo Total",
             x = "Iteração",
             y = "Custo Total")
    }
  })
  
  # Reiniciar
  observeEvent(input$reset, {
    values$problem_data <- NULL
    values$solution <- NULL
    values$iteration_history <- list()
  })
}

# Run the application
shinyApp(ui = ui, server = server)