library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(DT)

# Algoritmo do Canto Noroeste - CORRIGIDO
northwest_corner <- function(supply, demand) {
  m <- length(supply)
  n <- length(demand)
  
  allocation <- matrix(0, nrow = m, ncol = n)
  supply_remaining <- supply
  demand_remaining <- demand
  
  i <- 1
  j <- 1
  
  while (i <= m && j <= n) {
    quantity <- min(supply_remaining[i], demand_remaining[j])
    allocation[i, j] <- quantity
    supply_remaining[i] <- supply_remaining[i] - quantity
    demand_remaining[j] <- demand_remaining[j] - quantity
    
    if (supply_remaining[i] == 0) i <- i + 1
    if (demand_remaining[j] == 0) j <- j + 1
  }
  
  return(allocation)
}

# Funções auxiliares para Stepping Stone - CORRIGIDAS
find_cycle <- function(allocation, start_i, start_j) {
  m <- nrow(allocation)
  n <- ncol(allocation)
  
  # Versão simplificada para demonstração
  # Em uma implementação real, seria mais complexa
  cycle <- list(c(start_i, start_j))
  
  # Procurar por células alocadas na mesma linha
  for (col in 1:n) {
    if (col != start_j && allocation[start_i, col] > 0) {
      cycle <- c(cycle, list(c(start_i, col)))
      # Procurar na coluna dessa célula
      for (row in 1:m) {
        if (row != start_i && allocation[row, col] > 0) {
          cycle <- c(cycle, list(c(row, col)))
          if (row == start_i || col == start_j) {
            return(cycle)
          }
        }
      }
    }
  }
  
  if (length(cycle) >= 4) {
    return(cycle)
  }
  
  return(NULL)
}

calculate_marginal_cost <- function(cycle, costs) {
  if (is.null(cycle) || length(cycle) < 4) return(0)
  
  marginal_cost <- 0
  sign <- 1
  
  for (i in 1:length(cycle)) {
    cell <- cycle[[i]]
    marginal_cost <- marginal_cost + sign * costs[cell[1], cell[2]]
    sign <- -sign
  }
  
  return(marginal_cost)
}

apply_cycle <- function(allocation, cycle) {
  if (is.null(cycle) || length(cycle) < 4) return(allocation)
  
  # Encontrar quantidade mínima nas células negativas (ímpares)
  min_quantity <- Inf
  for (i in seq(2, length(cycle), by = 2)) {
    cell <- cycle[[i]]
    quantity <- allocation[cell[1], cell[2]]
    if (quantity < min_quantity) {
      min_quantity <- quantity
    }
  }
  
  # Aplicar transferências
  for (i in 1:length(cycle)) {
    cell <- cycle[[i]]
    if (i %% 2 == 1) {  # Células positivas
      allocation[cell[1], cell[2]] <- allocation[cell[1], cell[2]] + min_quantity
    } else {  # Células negativas
      allocation[cell[1], cell[2]] <- allocation[cell[1], cell[2]] - min_quantity
    }
  }
  
  return(allocation)
}

# Algoritmo de Stepping Stone - CORRIGIDO
stepping_stone <- function(allocation, costs) {
  m <- nrow(allocation)
  n <- ncol(allocation)
  
  total_cost <- sum(allocation * costs)
  improved <- TRUE
  iteration <- 0
  history <- list()
  
  while (improved && iteration < 10) {
    improved <- FALSE
    iteration <- iteration + 1
    
    empty_cells <- which(allocation == 0, arr.ind = TRUE)
    
    for (idx in 1:nrow(empty_cells)) {
      i <- empty_cells[idx, 1]
      j <- empty_cells[idx, 2]
      
      cycle <- find_cycle(allocation, i, j)
      if (!is.null(cycle) && length(cycle) >= 4) {
        marginal_cost <- calculate_marginal_cost(cycle, costs)
        
        if (marginal_cost < 0) {
          improved <- TRUE
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

# UI - CORRIGIDA (adicionada verificação de balanço)
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
      tabItem(tabName = "setup",
              fluidRow(
                box(
                  title = "Parâmetros do Problema",
                  width = 12,
                  numericInput("num_sources", "Número de Fontes", value = 3, min = 2, max = 6),
                  numericInput("num_destinations", "Número de Destinos", value = 4, min = 2, max = 6),
                  actionButton("generate_problem", "Gerar Problema", class = "btn-primary")
                )
              ),
              
              fluidRow(
                box(
                  title = "Balanço Oferta vs Demanda",
                  width = 12,
                  status = "warning",
                  textOutput("balance_check"),
                  tags$head(tags$style("#balance_check{color: red; font-weight: bold;}"))
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

# Server - CORRIGIDO
server <- function(input, output, session) {
  
  values <- reactiveValues(
    problem_data = NULL,
    solution = NULL,
    iteration_history = list()
  )
  
  output$supply_inputs <- renderUI({
    num_sources <- input$num_sources
    lapply(1:num_sources, function(i) {
      numericInput(paste0("supply_", i), paste("Oferta Fonte", i), 
                   value = sample(80:150, 1), min = 1, step = 1)
    })
  })
  
  output$demand_inputs <- renderUI({
    num_dest <- input$num_destinations
    lapply(1:num_dest, function(i) {
      numericInput(paste0("demand_", i), paste("Demanda Destino", i), 
                   value = sample(50:120, 1), min = 1, step = 1)
    })
  })
  
  output$cost_matrix_input <- renderUI({
    num_sources <- input$num_sources
    num_dest <- input$num_destinations
    
    fluidRow(
      lapply(1:num_sources, function(i) {
        column(2,
               lapply(1:num_dest, function(j) {
                 numericInput(paste0("cost_", i, "_", j), 
                              paste("F", i, "→D", j),
                              value = sample(5:30, 1), min = 0, step = 1)
               })
        )
      })
    )
  })
  
  # Verificação de balanço em tempo real
  output$balance_check <- renderText({
    if (is.null(values$problem_data)) {
      return("Configure o problema primeiro")
    }
    
    total_supply <- sum(values$problem_data$supply)
    total_demand <- sum(values$problem_data$demand)
    
    if (total_supply == total_demand) {
      return("✓ Oferta e demanda estão balanceadas!")
    } else {
      return(paste("⚠ Desbalanceado! Oferta:", total_supply, 
                   " | Demanda:", total_demand, 
                   " | Diferença:", abs(total_supply - total_demand)))
    }
  })
  
  observeEvent(input$generate_problem, {
    num_sources <- input$num_sources
    num_dest <- input$num_destinations
    
    # Coletar ofertas
    supply <- sapply(1:num_sources, function(i) {
      input_val <- input[[paste0("supply_", i)]]
      if (is.null(input_val)) 100 else input_val
    })
    
    # Coletar demandas
    demand <- sapply(1:num_dest, function(i) {
      input_val <- input[[paste0("demand_", i)]]
      if (is.null(input_val)) 75 else input_val
    })
    
    # Coletar custos
    costs <- matrix(0, nrow = num_sources, ncol = num_dest)
    for (i in 1:num_sources) {
      for (j in 1:num_dest) {
        input_val <- input[[paste0("cost_", i, "_", j)]]
        costs[i, j] <- if (is.null(input_val)) sample(5:30, 1) else input_val
      }
    }
    
    values$problem_data <- list(
      supply = supply,
      demand = demand,
      costs = costs
    )
  })
  
  observeEvent(input$solve, {
    req(values$problem_data)
    
    withProgress(message = 'Resolvendo problema...', value = 0, {
      incProgress(0.3, detail = "Calculando solução inicial")
      
      # Verificar e ajustar oferta/demanda
      total_supply <- sum(values$problem_data$supply)
      total_demand <- sum(values$problem_data$demand)
      
      if (total_supply != total_demand) {
        showModal(modalDialog(
          title = "Erro de Balanceamento",
          paste("A oferta total (", total_supply, 
                ") deve ser igual à demanda total (", total_demand, ").",
                "Por favor, ajuste os valores."),
          easyClose = TRUE
        ))
        return()
      }
      
      initial_allocation <- northwest_corner(
        values$problem_data$supply,
        values$problem_data$demand
      )
      
      incProgress(0.6, detail = "Otimizando solução")
      solution <- stepping_stone(initial_allocation, values$problem_data$costs)
      
      values$solution <- solution
      values$iteration_history <- solution$history
    })
  })
  
  output$allocation_table <- renderDT({
    req(values$solution)
    
    allocation <- values$solution$final_allocation
    rownames(allocation) <- paste("Fonte", 1:nrow(allocation))
    colnames(allocation) <- paste("Destino", 1:ncol(allocation))
    
    datatable(allocation, 
              options = list(dom = 't', pageLength = 10),
              caption = "Quantidades alocadas de cada fonte para cada destino") %>%
      formatStyle(names(allocation), backgroundColor = 'lightblue')
  })
  
  output$cost_table <- renderDT({
    req(values$problem_data)
    
    costs <- values$problem_data$costs
    rownames(costs) <- paste("Fonte", 1:nrow(costs))
    colnames(costs) <- paste("Destino", 1:ncol(costs))
    
    datatable(costs, 
              options = list(dom = 't', pageLength = 10),
              caption = "Custos unitários de transporte") %>%
      formatStyle(names(costs), backgroundColor = 'lightgreen')
  })
  
  output$allocation_plot <- renderPlot({
    req(values$solution)
    
    allocation <- values$solution$final_allocation
    df <- melt(allocation)
    colnames(df) <- c("Fonte", "Destino", "Quantidade")
    
    ggplot(df, aes(x = Destino, y = Fonte, fill = Quantidade)) +
      geom_tile(color = "white", linewidth = 1) +
      geom_text(aes(label = Quantidade), color = "white", size = 6, fontface = "bold") +
      scale_fill_gradient(low = "blue", high = "red") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, face = "bold")) +
      labs(title = "Matriz de Alocação Ótima",
           x = "Destino", y = "Fonte")
  })
  
  output$total_cost_box <- renderValueBox({
    req(values$solution)
    valueBox(
      paste("R$", format(round(values$solution$total_cost, 2), big.mark = ",")),
      "Custo Total",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$iterations_box <- renderValueBox({
    if (is.null(values$iteration_history) || length(values$iteration_history) == 0) {
      iterations <- 0
    } else {
      iterations <- length(values$iteration_history)
    }
    
    valueBox(
      iterations,
      "Iterações Realizadas",
      icon = icon("refresh"),
      color = "blue"
    )
  })
  
  output$status_box <- renderValueBox({
    req(values$solution)
    valueBox(
      "Solução Calculada",
      "Status",
      icon = icon("check-circle"),
      color = "purple"
    )
  })
  
  output$iteration_table <- renderDT({
    if (is.null(values$iteration_history) || length(values$iteration_history) == 0) {
      return(datatable(data.frame(Mensagem = "Nenhuma iteração realizada")))
    }
    
    history_df <- do.call(rbind, lapply(values$iteration_history, function(iter) {
      data.frame(
        Iteração = iter$iteration,
        Custo_Total = round(iter$total_cost, 2),
        Melhoria = round(iter$improvement, 2)
      )
    }))
    
    datatable(history_df, 
              options = list(dom = 't', pageLength = 10),
              caption = "Histórico de Iterações") %>%
      formatStyle(names(history_df), backgroundColor = 'lightyellow')
  })
  
  output$cost_evolution_plot <- renderPlot({
    if (is.null(values$iteration_history) || length(values$iteration_history) == 0) {
      return(ggplot() + 
               geom_text(aes(x = 0.5, y = 0.5, label = "Nenhuma iteração realizada"), 
                         size = 6) +
               theme_void())
    }
    
    history_df <- do.call(rbind, lapply(values$iteration_history, function(iter) {
      data.frame(
        Iteração = iter$iteration,
        Custo_Total = iter$total_cost
      )
    }))
    
    # Calcular custo inicial
    initial_cost <- sum(northwest_corner(values$problem_data$supply, 
                                         values$problem_data$demand) * 
                          values$problem_data$costs)
    
    # Adicionar ponto inicial
    history_df <- rbind(data.frame(Iteração = 0, Custo_Total = initial_cost), history_df)
    
    ggplot(history_df, aes(x = Iteração, y = Custo_Total)) +
      geom_line(color = "blue", linewidth = 1.5) +
      geom_point(color = "red", size = 3) +
      geom_point(data = history_df[1, ], aes(x = Iteração, y = Custo_Total), 
                 color = "green", size = 4, shape = 17) +
      theme_minimal(base_size = 14) +
      labs(title = "Evolução do Custo Total",
           x = "Iteração",
           y = "Custo Total") +
      scale_x_continuous(breaks = unique(history_df$Iteração))
  })
  
  observeEvent(input$reset, {
    values$problem_data <- NULL
    values$solution <- NULL
    values$iteration_history <- list()
  })
}

# Run the application
shinyApp(ui = ui, server = server)