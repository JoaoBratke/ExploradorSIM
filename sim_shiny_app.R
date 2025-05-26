library(shiny)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)


traduzir_idade <- function(codigos) {
  codigos <- as.integer(codigos)
  
  dplyr::case_when(
    codigos >= 0   & codigos <=  99  ~ "< 1 hora",
    codigos == 100                   ~ "1 hora",
    codigos >= 101 & codigos <= 123 ~ paste0(codigos - 100, " horas"),
    codigos == 124                   ~ "24 horas",
    codigos >= 125 & codigos <= 199 ~ "horas ignoradas (125-199)",
    
    codigos >= 200 & codigos <= 299 ~ paste0(codigos - 200, " dias"),
    codigos >= 300 & codigos <= 399 ~ paste0(codigos - 300, " meses"),
    codigos >= 400 & codigos <= 599 ~ paste0(codigos - 400, " anos"),
    
    codigos >= 600 & codigos <= 999 ~ "Idade ignorada",
    TRUE                            ~ NA_character_
  )
}


ui <- fluidPage(
  titlePanel("Explorador SIM - Sistema de Mortalidade"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Tabela' || input.tabs == 'Grafico'",
        tagList(
          selectInput("ufs", "UF:", choices = NULL, multiple = TRUE),
          selectInput("anos", "Ano:", choices = NULL, multiple = TRUE),
          selectInput("campo_agrupamento", "Campos para agrupar:", choices = NULL, multiple = TRUE),
          selectInput("campo_filtro", "Campos para filtrar:", choices = NULL, multiple = TRUE),
          uiOutput("valores_filtros"),
          actionButton("consultar", "Consultar")
        )
      ),
      conditionalPanel(
        condition = "input.tabs == 'Gerenciar dados'",
        DT::dataTableOutput("dados_banco")
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Gerenciar dados",
                           fluidRow(
                             column(6,
                                    selectInput("uf_download", "UF para baixar:", choices = c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF"), multiple = TRUE),
                                    selectInput("ano_download", "Ano para baixar:", choices = 2011:2023, multiple = TRUE),
                                    actionButton("baixar_dados", "Baixar e adicionar ao banco"),
                                    br(),
                                    actionButton("limpar_tudo", "Limpar todo o banco de dados"),
                                    actionButton("remover_ano_uf", "Remover dados do ano/UF selecionado"),
                                    verbatimTextOutput("log_operacoes")
                             )
                           )
                  ),
                  tabPanel("Tabela", DT::dataTableOutput("tabela")),
                  tabPanel("Grafico",
                           checkboxInput("usar_percentual", "Exibir percentual do total", value = FALSE),
                           selectInput("campo_facet", "Campos para facetar ou empilhar:", choices = NULL, multiple = TRUE),
                           plotOutput("grafico"))
      )
    )
  )
)

server <- function(input, output, session) {
  library(microdatasus)
  library(readr)
  con <- dbConnect(SQLite(), "sim_banco.sqlite")
  
  # Cria a tabela sim_obitos vazia se ainda nao existir
  if (!"sim_obitos" %in% dbListTables(con)) {
    estrutura_vazia <- tibble::tibble(
      UF = character(),
      ANO = integer(),
      IDADE = character(),
      IDADE_DESC = character(),
      SEXO = character(),
      RACACOR = character(),
      ESC = character(),
      CAUSABAS = character()
    )
    dbWriteTable(con, "sim_obitos", estrutura_vazia)
  }
  
  
  output$valores_filtros <- renderUI({
    req(input$campo_filtro)
    df <- tryCatch({
      dbGetQuery(con, "SELECT * FROM sim_obitos LIMIT 1000")
    }, error = function(e) return(NULL))
    
    if (is.null(df)) return(NULL)
    
    lapply(input$campo_filtro, function(campo) {
      valores <- sort(unique(na.omit(df[[campo]])))
      selectInput(
        inputId = paste0("filtro_", campo),
        label = paste("Filtrar por", campo, ":"),
        choices = valores,
        multiple = TRUE
      )
    })
  }) 
  # Renderizar tabela de dados ja baixados ao abrir o app
  output$dados_banco <- DT::renderDataTable({
    if ("sim_obitos" %in% dbListTables(con)) {
      dbGetQuery(con, "SELECT DISTINCT UF, ANO FROM sim_obitos ORDER BY UF, ANO")
    } else {
      data.frame(UF = character(0), ANO = integer(0))
    }
  })
  con <- dbConnect(SQLite(), "sim_banco.sqlite")
  
  # Preencher as choices dinamicamente
  observe({
    ufs <- dbGetQuery(con, "SELECT DISTINCT UF FROM sim_obitos")$UF
    anos <- dbGetQuery(con, "SELECT DISTINCT ANO FROM sim_obitos")$ANO
    updateSelectInput(session, "ufs", choices = ufs)
    updateSelectInput(session, "anos", choices = sort(anos))
    colunas <- dbListFields(con, "sim_obitos")
    colunas <- iconv(colunas, from = "latin1", to = "UTF-8")
    updateSelectInput(session, "campo_agrupamento", choices = colunas)
    updateSelectInput(session, "campo_filtro", choices = colunas)
    updateSelectInput(session, "campo_facet", choices = colunas)
    
  })
  
  dados_filtrados <- eventReactive(input$consultar, {
    req(input$anos, input$ufs)
    ufs_sql <- if ("Todos" %in% input$ufs) "UF IN (SELECT DISTINCT UF FROM sim_obitos)" else paste0("UF IN ('", paste(input$ufs, collapse = "','"), "')")
    anos_sql <- paste0("ANO IN (", paste(input$anos, collapse = ","), ")")
    query <- sprintf("SELECT * FROM sim_obitos WHERE %s AND %s", ufs_sql, anos_sql)
    df <- dbGetQuery(con, query)
    
    if (!is.null(input$campo_filtro)) {
      for (campo in input$campo_filtro) {
        valores <- input[[paste0("filtro_", campo)]]
        if (!is.null(valores) && length(valores) > 0) {
          df <- df[df[[campo]] %in% valores, ]
        }
      }
    }
    df
  })
  
  
  output$tabela <- DT::renderDataTable({
    req(dados_filtrados())
    req(length(input$campo_agrupamento) >= 1)
    
    tabela <- dados_filtrados() %>%
      count(across(all_of(input$campo_agrupamento)))
    
    
    DT::datatable(
      tabela,
      extensions = 'Buttons',
      filter = 'top',
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv'),
        pageLength = 10
      )
    )
  })

  
  output$grafico <- renderPlot({
    req(dados_filtrados())
    req(length(input$campo_agrupamento) == 1)
    
    campo <- input$campo_agrupamento[[1]]
    coluna_y <- if (input$usar_percentual) "percentual" else "valor"
    
    agrupado <- dados_filtrados() %>%
      count(categoria = !!sym(campo), !!!syms(input$campo_facet), name = "valor")
    
    if (input$usar_percentual) {
      agrupado <- agrupado %>%
        group_by(across(all_of(input$campo_facet))) %>%
        mutate(percentual = valor / sum(valor) * 100) %>%
        ungroup()
    }
    
    
      p <- ggplot(agrupado, aes(x = categoria, y = .data[[coluna_y]], fill = categoria)) +
        geom_col(position = if (length(input$campo_facet) >= 1) "dodge" else "stack") +
        labs(x = campo, y = if (input$usar_percentual) "% do total" else "Numero de obitos") +
        theme_minimal()
      
      if (length(input$campo_facet) == 1) {
        p <- p + facet_wrap(as.formula(paste("~", input$campo_facet)))
      } else if (length(input$campo_facet) == 2) {
        p <- p + facet_grid(as.formula(paste(input$campo_facet[1], "~", input$campo_facet[2])))
      }
      p
      
    
  })
  
  observeEvent(input$baixar_dados, {
    req(input$uf_download, input$ano_download)
    log_msg <- character()
    for (ano in input$ano_download) {
      for (uf in input$uf_download) {
          
          existentes <- dbGetQuery(con, sprintf(
            "SELECT 1 FROM sim_obitos WHERE UF = '%s' AND ANO = %d LIMIT 1" ,
            uf, as.numeric(ano)))
          
          if (nrow(existentes) == 0) {
            capture.output({
              dados_brutos <- fetch_datasus(year_start = as.numeric(ano), year_end = as.numeric(ano), uf = uf, information_system = "SIM-DO")
            }, type = "output") -> fetch_logs
            log_msg <- c(log_msg, unlist(fetch_logs))
            dados_tratados <- process_sim(dados_brutos) %>%
              mutate(ANO = ano, UF = uf,
                     IDADE_DESC = traduzir_idade(IDADE))
            
            colunas_existentes <- dbListFields(con, "sim_obitos")
            novas_colunas <- setdiff(names(dados_tratados), colunas_existentes)
            for (col in novas_colunas) {
              tipo <- class(dados_tratados[[col]])[1]
              tipo_sql <- if (tipo == "character") "TEXT" else if (tipo == "numeric" || tipo == "double") "REAL" else if (tipo == "integer") "INTEGER" else "TEXT"
              alter_sql <- sprintf("ALTER TABLE sim_obitos ADD COLUMN '%s' %s", col, tipo_sql)
              dbExecute(con, alter_sql)
            }
            dbWriteTable(
              con,
              "sim_obitos",
              dados_tratados[, intersect(names(dados_tratados), dbListFields(con, "sim_obitos"))],
              append = TRUE
            )
            log_msg <- c(log_msg, paste("Dados adicionados:", uf, ano))
          } else {
            log_msg <- c(log_msg, paste("Dados ja existem:", uf, ano))
          }
          
       
      }
    }
    
    output$log_operacoes <- renderPrint(log_msg)
    output$dados_banco <- DT::renderDataTable({
      dbGetQuery(con, "SELECT DISTINCT UF, ANO FROM sim_obitos ORDER BY UF, ANO")
    })
  })
  
  observeEvent(input$limpar_tudo, {
    dbExecute(con, "DELETE FROM sim_obitos")
    output$log_operacoes <- renderPrint("Banco limpo com sucesso.")
    output$dados_banco <- DT::renderDataTable({
      dbGetQuery(con, "SELECT DISTINCT UF, ANO FROM sim_obitos ORDER BY UF, ANO")
    })
  })
  
  observeEvent(input$remover_ano_uf, {
    req(input$uf_download, input$ano_download)
    for (ano in input$ano_download) {
      for (uf in input$uf_download) {
        dbExecute(con, sprintf("DELETE FROM sim_obitos WHERE UF = '%s' AND ANO = %d", uf, as.numeric(ano)))
      }
    }
    output$log_operacoes <- renderPrint("Dados removidos para as selecoes indicadas.")
    output$dados_banco <- DT::renderDataTable({
      dbGetQuery(con, "SELECT DISTINCT UF, ANO FROM sim_obitos ORDER BY UF, ANO")
    })
  })
  
  onStop(function() dbDisconnect(con))
}

shinyApp(ui, server)

