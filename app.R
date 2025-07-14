library(shiny)
library(shinydashboard)
library(DT)

# UI (User Interface)
ui <- dashboardPage(
  dashboardHeader(title = "Método Simplex"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Entrada de Dados", tabName = "dados", icon = icon("table")),
      menuItem("Solução", tabName = "solucao", icon = icon("calculator"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab de entrada de dados
      tabItem(tabName = "dados",
              fluidRow(
                box(width = 12, title = "Configuração do Problema", status = "primary",
                    numericInput("num_var", "Número de Variáveis de Decisão:", value = 2, min = 1),
                    textInput("obj_func", "Coeficientes da Função Objetivo (separados por vírgula):", "3,5"),
                    selectInput("tipo_otim", "Tipo de Otimização:", choices = c("Maximização" = "max", "Minimização" = "min"))
                )
              ),
              
              fluidRow(
                box(width = 12, title = "Restrições", status = "primary",
                    numericInput("num_restr", "Número de Restrições:", value = 3, min = 1),
                    uiOutput("restricoes_ui"),
                    actionButton("resolver", "Resolver Problema", class = "btn-success")
                )
              )
      ),
      
      # Tab de solução
      tabItem(tabName = "solucao",
              fluidRow(
                box(width = 12, title = "Solução do Problema", status = "success",
                    h3("Tableau Inicial"),
                    DTOutput("tableau_inicial"),
                    
                    h3("Iterações do Simplex"),
                    uiOutput("iteracoes_ui"),
                    
                    h3("Solução Ótima"),
                    verbatimTextOutput("solucao_otima"),
                    
                    h3("Valor Ótimo"),
                    verbatimTextOutput("valor_otimo")
                )
              )
      )
    )
  )
)

# Server (Lógica do Aplicativo)
server <- function(input, output, session) {
  # Gera os campos de entrada para as restrições
  output$restricoes_ui <- renderUI({
    req(input$num_restr, input$num_var)
    
    restricoes <- lapply(1:input$num_restr, function(i) {
      fluidRow(
        column(width = input$num_var * 2,
               lapply(1:input$num_var, function(j) {
                 numericInput(paste0("restr_", i, "_var_", j), 
                              paste0("Coeficiente x", j), 
                              value = ifelse(i == 1, ifelse(j == 1, 1, ifelse(j == 2 && i == 3, 2, 0)), 0))
               })
        ),
        column(width = 2,
               selectInput(paste0("operador_", i), "Operador",
                           choices = c("<=" = "<=", ">=" = ">=", "=" = "=="),
                           selected = "<=")
        ),
        column(width = 2,
               numericInput(paste0("termo_indep_", i), "Termo Indep.", value = ifelse(i == 1, 4, ifelse(i == 2, 12, 18)))
        )
      )
    })
    
    do.call(tagList, restricoes)
  })
  
  # Função simplex adaptada para o Shiny
  simplex_shiny <- function(obj, A, b, operadores, maximizar = TRUE) {
    if (!maximizar) {
      obj <- -obj
    }
    
    m <- nrow(A)
    n <- ncol(A)
    
    folga <- which(operadores == "<=")
    excesso <- which(operadores == ">=")
    igual <- which(operadores == "==")
    
    # Cria tableau aumentado
    tableau <- matrix(0, nrow = m + 1, ncol = n + length(folga) + length(excesso) * 2 + length(igual) + 1)
    tableau[1:m, 1:n] <- A
    tableau[1:m, ncol(tableau)] <- b
    
    # Adiciona variáveis de folga
    col_folga <- n + 1
    for (i in folga) {
      tableau[i, col_folga] <- 1
      col_folga <- col_folga + 1
    }
    
    # Adiciona variáveis de excesso e artificiais para >=
    for (i in excesso) {
      tableau[i, col_folga] <- -1
      tableau[i, col_folga + 1] <- 1
      col_folga <- col_folga + 2
    }
    
    # Adiciona variáveis artificiais para ==
    for (i in igual) {
      tableau[i, col_folga] <- 1
      col_folga <- col_folga + 1
    }
    
    tableau[m + 1, 1:n] <- -obj
    
    # Nomes das colunas
    colnames <- paste0("x", 1:n)
    if (length(folga) > 0) colnames <- c(colnames, paste0("f", 1:length(folga)))
    if (length(excesso) > 0) {
      colnames <- c(colnames, paste0("e", 1:length(excesso)))
      colnames <- c(colnames, paste0("a", 1:length(excesso)))
    }
    if (length(igual) > 0) colnames <- c(colnames, paste0("a", length(excesso) + 1:length(igual)))
    colnames <- c(colnames, "b")
    
    colnames(tableau) <- colnames
    rownames(tableau) <- c(paste0("R", 1:m), "Z")
    
    # Armazena o tableau inicial
    tableau_inicial <- tableau
    
    iteracoes <- list()
    iter_count <- 0
    
    while (TRUE) {
      linha_z <- tableau[nrow(tableau), -ncol(tableau)]
      if (all(linha_z >= 0)) {
        break
      }
      
      entra <- which.min(linha_z)
      ratios <- tableau[1:m, ncol(tableau)] / tableau[1:m, entra]
      ratios[tableau[1:m, entra] <= 0] <- Inf
      
      if (all(is.infinite(ratios))) {
        break
      }
      
      sai <- which.min(ratios)
      
      iter_count <- iter_count + 1
      
      # Armazena informações da iteração ANTES do pivoteamento
      iteracao <- list(
        entra = colnames(tableau)[entra],
        sai = rownames(tableau)[sai],
        tableau = tableau
      )
      iteracoes[[iter_count]] <- iteracao
      
      # Pivoteamento
      pivot <- tableau[sai, entra]
      tableau[sai, ] <- tableau[sai, ] / pivot
      
      for (i in 1:(m + 1)) {
        if (i != sai) {
          tableau[i, ] <- tableau[i, ] - tableau[i, entra] * tableau[sai, ]
        }
      }
    }
    
    # Solução ótima
    solucao <- rep(0, n)
    for (i in 1:n) {
      if (sum(tableau[1:m, i] == 1) == 1 && sum(tableau[1:m, i] != 0) == 1) {
        linha <- which(tableau[1:m, i] == 1)
        solucao[i] <- tableau[linha, ncol(tableau)]
      }
    }
    
    valor_otimo <- tableau[nrow(tableau), ncol(tableau)]
    if (!maximizar) valor_otimo <- -valor_otimo
    
    return(list(
      tableau_inicial = tableau_inicial,
      iteracoes = iteracoes,
      tableau_final = tableau,
      solucao = solucao,
      valor_otimo = valor_otimo
    ))
  }
  
  # Resolve o problema quando o botão é clicado
  observeEvent(input$resolver, {
    req(input$num_var, input$num_restr)
    
    # Coleta os dados de entrada
    obj <- as.numeric(unlist(strsplit(input$obj_func, ",")))
    
    A <- matrix(0, nrow = input$num_restr, ncol = input$num_var)
    b <- numeric(input$num_restr)
    operadores <- character(input$num_restr)
    
    for (i in 1:input$num_restr) {
      for (j in 1:input$num_var) {
        A[i, j] <- input[[paste0("restr_", i, "_var_", j)]]
      }
      operadores[i] <- input[[paste0("operador_", i)]]
      b[i] <- input[[paste0("termo_indep_", i)]]
    }
    
    maximizar <- input$tipo_otim == "max"
    
    # Executa o método simplex
    resultado <- simplex_shiny(obj, A, b, operadores, maximizar)
    
    # Armazena os resultados para exibição
    output$tableau_inicial <- renderDT({
      datatable(resultado$tableau_inicial, 
                options = list(dom = 't', scrollX = TRUE),
                rownames = TRUE)
    })
    
    output$iteracoes_ui <- renderUI({
      if (length(resultado$iteracoes) == 0) {
        return(p("O tableau inicial já é ótimo."))
      }
      
      iteracoes <- lapply(1:length(resultado$iteracoes), function(i) {
        tagList(
          h4(paste("Iteração", i)),
          p(strong("Variável que entra:"), resultado$iteracoes[[i]]$entra),
          p(strong("Variável que sai:"), resultado$iteracoes[[i]]$sai),
          DTOutput(paste0("tableau_iter_", i)),
          hr()
        )
      })
      
      # Renderiza os tableaus das iterações
      for (i in 1:length(resultado$iteracoes)) {
        local({
          ii <- i
          output[[paste0("tableau_iter_", ii)]] <- renderDT({
            datatable(resultado$iteracoes[[ii]]$tableau,
                      options = list(dom = 't', scrollX = TRUE),
                      rownames = TRUE)
          })
        })
      }
      
      do.call(tagList, iteracoes)
    })
    
    output$solucao_otima <- renderPrint({
      cat("Valores das variáveis de decisão:\n")
      for (i in 1:length(resultado$solucao)) {
        cat(paste0("x", i), "=", resultado$solucao[i], "\n")
      }
    })
    
    output$valor_otimo <- renderPrint({
      if (input$tipo_otim == "max") {
        cat("Valor máximo da função objetivo:", resultado$valor_otimo, "\n")
      } else {
        cat("Valor mínimo da função objetivo:", resultado$valor_otimo, "\n")
      }
    })
    
    # Muda para a aba de solução
    updateTabItems(session, "sidebar", "solucao")
  })
}

# Executa o aplicativo
shinyApp(ui, server)