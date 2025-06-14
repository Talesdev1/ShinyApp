library(shiny)
library(ggplot2)
library(lpSolve)
library(reshape2)

# UI (Interface do Usuário)
ui <- fluidPage(
  titlePanel("Solução Gráfica de Problemas de Programação Linear"),
  sidebarLayout(
    sidebarPanel(
      h4("Função Objetivo"),
      numericInput("obj_x1", "Coeficiente de x1:", value = 3),
      numericInput("obj_x2", "Coeficiente de x2:", value = 2),
      radioButtons("obj_type", "Tipo de objetivo:", 
                   choices = c("Maximizar" = "max", "Minimizar" = "min"), 
                   selected = "max"),
      
      hr(),
      h4("Restrições"),
      numericInput("num_constraints", "Número de restrições:", 
                   value = 3, min = 1, max = 10),
      uiOutput("constraints_ui"),
      
      actionButton("solve", "Resolver PPL", class = "btn-primary")
    ),
    mainPanel(
      plotOutput("ppl_plot", height = "600px"),
      verbatimTextOutput("solution_output")
    )
  )
)

# Server (Lógica do Servidor)
server <- function(input, output, session) {
  
  # Gerar UI dinâmica para as restrições
  output$constraints_ui <- renderUI({
    num_constraints <- input$num_constraints
    lapply(1:num_constraints, function(i) {
      fluidRow(
        column(3, numericInput(paste0("con_a", i), "Coef. x1", value = ifelse(i == 1, 2, ifelse(i == 2, 1, 1)))),
        column(3, numericInput(paste0("con_b", i), "Coef. x2", value = ifelse(i == 1, 1, ifelse(i == 2, 1, 0)))),
        column(3, selectInput(paste0("con_dir", i), "Des.", 
                              choices = c("<=" = "<=", ">=" = ">=", "=" = "=="), 
                              selected = "<=")),
        column(3, numericInput(paste0("con_rhs", i), "Valor", value = ifelse(i == 1, 100, ifelse(i == 2, 80, 40))))
      )
    })
  })
  
  # Função para resolver e plotar o PPL (mesma função do código anterior adaptada)
  solve_ppl <- eventReactive(input$solve, {
    # Coletar dados da função objetivo
    objective <- c(input$obj_x1, input$obj_x2)
    objective_type <- input$obj_type
    
    # Coletar dados das restrições
    num_constraints <- input$num_constraints
    
    constraints <- matrix(nrow = num_constraints, ncol = 2)
    directions <- character(num_constraints)
    rhs <- numeric(num_constraints)
    
    for (i in 1:num_constraints) {
      constraints[i, ] <- c(input[[paste0("con_a", i)]], input[[paste0("con_b", i)]])
      directions[i] <- input[[paste0("con_dir", i)]]
      rhs[i] <- input[[paste0("con_rhs", i)]]
    }
    
    # Resolver o PPL
    solution <- lp(objective_type, objective, constraints, directions, rhs)
    
    if (solution$status != 0) {
      return(list(error = "O problema não tem solução viável ou é ilimitado."))
    }
    
    # Determinar limites do gráfico
    max_x <- max(ifelse(constraints[,1] != 0, rhs/constraints[,1], rhs), na.rm = TRUE) * 1.2
    max_y <- max(ifelse(constraints[,2] != 0, rhs/constraints[,2], rhs), na.rm = TRUE) * 1.2
    
    x_lim <- c(0, max_x)
    y_lim <- c(0, max_y)
    
    # Criar sequência de valores para x1
    x1 <- seq(x_lim[1], x_lim[2], length.out = 100)
    
    # Calcular restrições
    constraint_data <- data.frame(x1 = numeric(), value = numeric(), variable = character())
    
    for (i in 1:num_constraints) {
      a <- constraints[i, 1]
      b <- constraints[i, 2]
      c <- rhs[i]
      dir <- directions[i]
      
      if (b != 0) {
        x2_values <- (c - a * x1) / b
        constraint_data <- rbind(constraint_data,
                                 data.frame(x1 = x1, 
                                            value = x2_values,
                                            variable = paste0("R", i, ": ", 
                                                              a, "x1 + ", b, "x2 ", dir, " ", c),
                                            stringsAsFactors = FALSE))
      } else {
        x1_value <- c/a
        constraint_data <- rbind(constraint_data,
                                 data.frame(x1 = rep(x1_value, 2),
                                            value = c(y_lim[1], y_lim[2]),
                                            variable = paste0("R", i, ": ", 
                                                              a, "x1 ", dir, " ", c),
                                            stringsAsFactors = FALSE))
      }
    }
    
    # Encontrar vértices da região viável
    find_vertices <- function() {
      vertices <- data.frame(x1 = numeric(), x2 = numeric())
      
      for (i in 1:(num_constraints - 1)) {
        for (j in (i + 1):num_constraints) {
          A <- constraints[c(i, j), ]
          b <- rhs[c(i, j)]
          
          if (abs(det(A)) > 1e-10) {
            sol <- try(solve(A, b), silent = TRUE)
            if (!inherits(sol, "try-error") && all(sol >= 0)) {
              vertices <- rbind(vertices, data.frame(x1 = sol[1], x2 = sol[2]))
            }
          }
        }
      }
      
      for (i in 1:num_constraints) {
        if (constraints[i, 1] != 0) {
          x1_inter <- rhs[i] / constraints[i, 1]
          if (x1_inter >= 0) {
            vertices <- rbind(vertices, data.frame(x1 = x1_inter, x2 = 0))
          }
        }
        
        if (constraints[i, 2] != 0) {
          x2_inter <- rhs[i] / constraints[i, 2]
          if (x2_inter >= 0) {
            vertices <- rbind(vertices, data.frame(x1 = 0, x2 = x2_inter))
          }
        }
      }
      
      feasible_vertices <- data.frame(x1 = numeric(), x2 = numeric())
      
      if (nrow(vertices) > 0) {
        for (k in 1:nrow(vertices)) {
          point <- vertices[k, ]
          feasible <- TRUE
          
          for (i in 1:num_constraints) {
            lhs <- sum(constraints[i, ] * c(point$x1, point$x2))
            if ((directions[i] == "<=" && lhs > rhs[i] + 1e-10) || 
                (directions[i] == ">=" && lhs < rhs[i] - 1e-10)) {
              feasible <- FALSE
              break
            }
          }
          
          if (feasible) {
            feasible_vertices <- rbind(feasible_vertices, point)
          }
        }
      }
      
      origin_feasible <- TRUE
      for (i in 1:num_constraints) {
        lhs <- sum(constraints[i, ] * c(0, 0))
        if ((directions[i] == "<=" && lhs > rhs[i] + 1e-10) || 
            (directions[i] == ">=" && lhs < rhs[i] - 1e-10)) {
          origin_feasible <- FALSE
          break
        }
      }
      
      if (origin_feasible) {
        feasible_vertices <- rbind(feasible_vertices, data.frame(x1 = 0, x2 = 0))
      }
      
      if (nrow(feasible_vertices) > 0) {
        center <- c(mean(feasible_vertices$x1), mean(feasible_vertices$x2))
        angles <- atan2(feasible_vertices$x2 - center[2], feasible_vertices$x1 - center[1])
        feasible_vertices <- feasible_vertices[order(angles), ]
      }
      
      return(unique(feasible_vertices))
    }
    
    vertices <- find_vertices()
    
    # Criar o gráfico
    p <- ggplot() +
      geom_line(data = constraint_data, aes(x = x1, y = value, color = variable), size = 1) +
      geom_hline(yintercept = 0, color = "black") +
      geom_vline(xintercept = 0, color = "black") +
      labs(title = paste("Solução Gráfica do PPL -", 
                         ifelse(objective_type == "max", "Maximização", "Minimização")),
           x = "x1", y = "x2", color = "Restrições") +
      theme_minimal() +
      coord_cartesian(xlim = x_lim, ylim = y_lim) +
      scale_color_discrete(labels = function(x) gsub("R\\d+: ", "", x))
    
    if (nrow(vertices) >= 3) {
      p <- p + geom_polygon(data = vertices, aes(x = x1, y = x2), 
                            fill = "lightblue", alpha = 0.4)
    }
    
    p <- p + geom_point(aes(x = solution$solution[1], y = solution$solution[2]), 
                        color = "red", size = 3)
    
    if (nrow(vertices) > 0) {
      p <- p + 
        annotate("text", 
                 x = solution$solution[1] + 0.05 * diff(x_lim), 
                 y = solution$solution[2] + 0.05 * diff(y_lim),
                 label = sprintf("Ótimo: (%.2f, %.2f)\nZ = %.2f", 
                                 solution$solution[1], 
                                 solution$solution[2],
                                 solution$objval))
    }
    
    return(list(plot = p, 
                solution = solution,
                vertices = vertices,
                constraints_data = constraint_data))
  })
  
  # Renderizar o gráfico
  output$ppl_plot <- renderPlot({
    result <- solve_ppl()
    if (!is.null(result$error)) {
      plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE)
      text(0, 0, result$error, col = "red", cex = 1.5)
    } else {
      print(result$plot)
    }
  })
  
  # Mostrar a solução numérica
  output$solution_output <- renderPrint({
    result <- solve_ppl()
    if (!is.null(result$error)) {
      cat(result$error)
    } else {
      cat("Solução Ótima:\n")
      cat(sprintf("x1 = %.2f\nx2 = %.2f\n\nValor da função objetivo: %.2f",
                  result$solution$solution[1],
                  result$solution$solution[2],
                  result$solution$objval))
    }
  })
}

# Rodar o aplicativo
shinyApp(ui = ui, server = server)
