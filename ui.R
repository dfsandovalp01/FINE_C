#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(skin="red",
## DASHBOARD HEADER ####                      
    dashboardHeader(disable = F,
        title = "FinanCare" #tags$img(src="OP1_log-ConvertImage.png", style="width:122px", 
                         # tags$style(HTML('
                         #                 .skin-blue .main-header .logo {
                         #                    background:white;
                         #                    }'
                         #                 )
                         #     ))
        ),
    
## MENU OPCIONES (SIDEBAR) ####
    dashboardSidebar(
        sidebarMenu(
            menuItem("HOME", tabName = "home", icon = icon("dashboard")),
            menuItem("CALCULADORA FINE", tabName = "calculadora", icon = icon("percentage"),badgeLabel = "new", badgeColor = "green")
        )
    ),
    
## CUERPO APP (BODY) ####
    dashboardBody(
        tabItems(
            ## HOME ####
            tabItem(
                tabName = "home",
                    fluidRow(
                        tags$img(src="financare_font.png", width="100%")),
                tabsetPanel(
                    tabPanel('Clientes',
                             box(
                                 fluidRow(
                                     title = 'Datos Cliente',
                                     column(width=3,
                                            textInput('client_name','Nombre' )),
                                     column(width=3,
                                            textInput('client_apellido','Apellido')),
                                     column(width=3,
                                            textInput('client_cedula','C.C.')),
                                     column(width=3,
                                            textInput('client_asesor','Asesor')),
                                     column(width=3,
                                            textInput('client_numero', 'Numero')),
                                     column(width=3,
                                            textInput('client_correo', 'Correo'))
                                          ),
                                 fluidRow(actionButton('new_client', 'Añadir'))
                             )),
                    tabPanel("Aliados"),
                    tabPanel("Nosotros"),
                    tabPanel("Resultados")
                    
                )
                    ),
            
            ## CALCULADORA FINE ####
            tabItem(tabName = "calculadora",
                    fluidRow(
                        tags$img(src="OP1.jpeg", width="10%")
                    ),
                    fluidRow(
                        valueBoxOutput("beneficios"),
                        valueBoxOutput("nuevo_saldo"),
                        valueBoxOutput("ahorro_con_sf")
                        #valueBox("$11.694 MM", "CARTERA CASTIGADAS CREDITO CONSUMO", icon = icon("credit-card"), color = "red", width = 5 
                        #value
                        ),
                       
                        #valueBox("$216.497 M", "VENTAS COMPETENCIA 2019", icon = icon("city"), color = "yellow", width = 4), 
                        #valueBox("11", "# EMPRESAS", icon = icon("building"), color = "yellow", width = 2),# empresas 11
                    #fluidRow( 
                        
                     #   valueBox("$1.052 MM", "OPORTUNIDAD", icon = icon("money-bill-alt"), color = "green", width = 4) )
                    #    ),
                    ## DATOS ENTRADA CALC ####
                    box(
                        title = 'TASAS FINE',
                        width = 6,
                        fluidRow(
                            column(width = 3,
                            numericInput('tasa_matricula', 'Tasa de Matricula', 0.021)
                            ),
                            column(width = 3,
                                   numericInput('tasa_manejo','Tasa Manejo',0.0055)
                                   ),
                            column(width = 3,
                                   numericInput('tasa_exito', 'Tasa de Éxito', 0.15)
                                   ),
                            column(width = 3,
                                   numericInput('maxima.promesa', 'Máxima Promesa %', 70))
                            )),
                    
                    box(
                        title = "DATOS DE ENTRADA",
                        width = 12,
                        fluidRow(
                                
                            column(width=3,
                                   numericInput("deuda", "Total Deuda", 1000000, min=1000000)),
                                   #verbatimTextOutput("value")),
                            column(width=6,
                                   sliderInput("%_reduccion", "Promesa de Reducción %", min = 0, max = 100, 30)),
                                   #verbatimTextOutput('promesa')),
                            column(width=3,
                                   #offset = 2,
                                   numericInput("tiempo", "Tiempo del proceso", 18, min=6)),
                                   #sliderInput("tiempo", "Tiempo", min = 0, max = 48, 12)),
                            column(width=6,
                                   sliderInput('time', 'Tiempo', min = 1, max = 48, 12)),
                            column(width=3,
                                   dateInput('inicio.mora', 'Inicio Mora')),
                            column(width=3,
                                   verbatimTextOutput('bono.mora')),
                            column(width=3,
                                   verbatimTextOutput("time.difference"))
                            )
                        ),
                    fluidRow(
                        valueBoxOutput('prom.desc'),
                        valueBoxOutput('cuota')
                    ),
                    
                    ## CONDICIONES DE CONTRATO ####
                    box(title = "CONDICIONES DE CONTRATO",
                        width = 12,
                        fluidRow(
                            column(width=12,
                                   box(
                                       width = 3,
                                       title = "Matrícula",
                                       verbatimTextOutput("matricula")
                                   ),
                                   box(
                                       width = 3,
                                       title = "Prima de Exito",
                                       verbatimTextOutput("prima_exito")
                                       ),
                                   box(
                                       width = 3,
                                       title = "Hrs Mensuales",
                                       verbatimTextOutput("hrs_mensuales")
                                       ),
                                   box(
                                       width = 3,
                                       title = "Total Hrs",
                                       verbatimTextOutput("total_hrs"))
                                   )
                                ),
                        ## AMORTIZACIÓN ####
                        box(title = "AMORTIZACIÓN",
                            width = 6,
                            fluidRow(column(width = 4,
                                            dataTableOutput("amortizacion")
                                            )
                                     ),
                            fluidRow( column(width=12,
                                             plotOutput("resultados"))
                                      )
                            )
                            )
            ))
        
    )
))
