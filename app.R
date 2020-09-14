# SHINY APP INTELIGENCIA COMPETITIVA

############################################
# LIBRERÍAS
############################################

library(shiny)
library(shinyjs)  # Para eecutar comandos JS
library(DT)
library(htmltools)
library(httr)
library(timeDate)
library(anytime)
library(jsonlite)
library(stringr)
library(dplyr)
library(ggplot2)
library(rjson)
library(jsonlite)
library(leaflet)  #Libreía mapa
library(RCurl)
library(utils)
library(RColorBrewer)
library(scales)
library(openxlsx)
library(janitor)
library(reshape2)
library(hrbrthemes)
library(ggthemes)
library(plotly)
library(webshot2)
library(htmlwidgets)
library(shinyalert)
library(rmarkdown)
library(tinytex)
library(lubridate)
library(shinybusy)


#Scripts R apoyo externos
source("llamada_api_thb_borme.R")

# MUNICIPIOS
# ------------------------

municipios <- read.csv("Municipios.csv",header=TRUE,sep=" ",encoding="UTF-8")

incorporar_cero <- function(x)
{
    if(nchar(as.character(x))==4){x <- paste0("0",x)}else{x}
}

municipios$INE <- apply(municipios[,4,drop=F],1,incorporar_cero) %>% unlist()
municipios$AMB[municipios$Municipio=="Barcelona"] <- 1  #Barcelona es AMB, luego se filtrara a demanda
municipios$codi_municipi <- municipios$INE

# Creación columna M. Columna de integración de datos municipios de otros datasets
municipios$M <- municipios$Municipio %>%
    gsub(", els","",.) %>%
    gsub("els ","",.) %>%
    gsub(", la","",.) %>%
    gsub(", el","",.) %>%
    gsub("el ","",.) %>%
    gsub(", les","",.) %>%
    gsub("les ","",.) %>%
    gsub("la ","",.) %>%
    gsub(", l'","",.) %>%
    gsub("l'","",.) %>%
    gsub(", l","",.)
  
municipios$AMB[municipios$Municipio == "la Palma de Cervelló"] <- 1

mun <- municipios$Municipio[municipios$AMB == 1]

#Municipios ertes
municipios_ertes <- municipios
municipios_ertes <- municipios_ertes[,c(1,3,5,6,7,9)]


#Municipios Borme (solo provincia)
municipios <- municipios[municipios$Provincia == "Barcelona",]
m <- municipios$M
municipios <- municipios[,c(1,5)]


# MODIFICACIONES DE AYUDA
# Ayuda selección variables BORME
uno <- c("Constitució començament operacions","Constitució objecte social","Constitució domicili social","Constitució capital")
dos <- c("Canvi domicili social","Canvi objecte social")
tres <- c("Nomenament Adm. Únic","Nomenament vicepresident","Nomenament president")
cuatro <- c("Ampliació capital","Ampliació capital subscrit","Ampliació capital desemborsat","Ampliació capital resultant desemborsat","Reducció capital import reducció","Reducció capital resultant subscrit")
cinco <- "Fusió societats abosrbidas"
seis <- "Dissolució"
siete <- "Extinció"
ocho <- "Escissió"
nueve <- "Transformació"
diez <- "Situació Concursal Resolucions"
cuatro_uno <- c("Ampliació capital","Ampliació capital subscrit","Ampliació capital desemborsat","Ampliació capital resultant desemborsat")
cuatro_dos <- c("Reducció capital import reducció","Reducció capital resultant subscrit")

lista_variables_borme <- list(uno,dos,tres,cuatro,cinco,seis,siete,ocho,nueve,diez,cuatro_uno,cuatro_dos)
names(lista_variables_borme) <- c("uno","dos","tres","cuatro","cinco","seis","siete","ocho","nueve","diez","cuatro_uno","cuatro_dos")



# =======================
# ERTES
# =======================

# Carga datos
df_tipo_expediente <- read.xlsx(xlsxFile = "publicacio_erto_01072020.xlsx", sheet = 6, skipEmptyRows = TRUE)
df_expediente_econom <- read.xlsx(xlsxFile = "publicacio_erto_01072020.xlsx", sheet = 7, skipEmptyRows = TRUE)
df_expediente_trabajo <- read.xlsx(xlsxFile = "publicacio_erto_01072020.xlsx", sheet = 8, skipEmptyRows = TRUE)

# Eliminar fila 1, informativa
df_tipo_expediente <- df_tipo_expediente[-1,]
df_expediente_econom <- df_expediente_econom[-1,]
df_expediente_trabajo <- df_expediente_trabajo[-1,]

# Asignación nombre columnas
colnames(df_tipo_expediente) <- df_tipo_expediente[2,]
colnames(df_expediente_econom) <- df_expediente_econom[1,]
colnames(df_expediente_trabajo) <- df_expediente_trabajo[1,]

# Eliminar fila 1, es la misma que nombre columnas
df_tipo_expediente <- df_tipo_expediente[-2,]
df_expediente_econom <- df_expediente_econom[-1,]
df_expediente_trabajo <- df_expediente_trabajo[-1,]

# Trataimiento df expedientes
colnames(df_tipo_expediente)[4] <- paste(colnames(df_tipo_expediente)[4],df_tipo_expediente[1,4],sep = " ")
colnames(df_tipo_expediente)[6] <- paste(colnames(df_tipo_expediente)[6],df_tipo_expediente[1,6],sep = " ")
colnames(df_tipo_expediente)[8] <- paste(colnames(df_tipo_expediente)[8],df_tipo_expediente[1,8],sep = " ")
df_tipo_expediente <- df_tipo_expediente[-1,]

# Otros tratamientos
for(i in 4:9){
  df_tipo_expediente[,i] <- as.numeric(df_tipo_expediente[,i])
}
df_tipo_expediente <- df_tipo_expediente[-grep("Catalunya",df_tipo_expediente$Comarca),]
for(i in 4:26){
  df_expediente_econom[,i] <- as.numeric(df_expediente_econom[,i])
}
df_expediente_econom <- df_expediente_econom[-grep("Catalunya",df_expediente_econom$Comarca),]
for(i in 4:26){
  df_expediente_trabajo[,i] <- as.numeric(df_expediente_trabajo[,i])
}
df_expediente_trabajo <- df_expediente_trabajo[-grep("Catalunya",df_expediente_trabajo$Comarca),]


# Creación columna M. Columna de integración de datos municipios de otros datasets
df_tipo_expediente$M <- df_tipo_expediente$Municipi %>%
  gsub(", els","",.) %>%
  gsub("els ","",.) %>%
  gsub(", la","",.) %>%
  gsub(", el","",.) %>%
  gsub("el ","",.) %>%
  gsub(", les","",.) %>%
  gsub("les ","",.) %>%
  gsub("la ","",.) %>%
  gsub(", l'","",.) %>%
  gsub("l'","",.) %>%
  gsub(", l","",.)

df_expediente_econom$M <- df_expediente_econom$Municipi %>%
  gsub(", els","",.) %>%
  gsub("els ","",.) %>%
  gsub(", la","",.) %>%
  gsub(", el","",.) %>%
  gsub("el ","",.) %>%
  gsub(", les","",.) %>%
  gsub("les ","",.) %>%
  gsub("la ","",.) %>%
  gsub(", l'","",.) %>%
  gsub("l'","",.) %>%
  gsub(", l","",.)

df_expediente_trabajo$M <- df_expediente_trabajo$Municipi %>%
  gsub(", els","",.) %>%
  gsub("els ","",.) %>%
  gsub(", la","",.) %>%
  gsub(", el","",.) %>%
  gsub("el ","",.) %>%
  gsub(", les","",.) %>%
  gsub("les ","",.) %>%
  gsub("la ","",.) %>%
  gsub(", l'","",.) %>%
  gsub("l'","",.) %>%
  gsub(", l","",.)


#AMB
df_tipo_expediente$AMB <- municipios_ertes$AMB[match(df_tipo_expediente$M, municipios_ertes$M)]
df_expediente_econom$AMB <- municipios_ertes$AMB[match(df_expediente_econom$M, municipios_ertes$M)]
df_expediente_trabajo$AMB <- municipios_ertes$AMB[match(df_expediente_trabajo$M, municipios_ertes$M)]

# SIMULACIÓN FECHA ***************
df_tipo_expediente$fecha <- rep("2020-08-01",nrow(df_tipo_expediente))
df_expediente_econom$fecha <- rep("2020-08-01",nrow(df_expediente_econom))
df_expediente_trabajo$fecha <- rep("2020-08-01",nrow(df_expediente_trabajo))




#=====================================================
# INTERFAZ DE USUARIO
#=====================================================
ui <- fluidPage(style = "width: 100%; height: 100%;",
                
                #use_busy_spinner(spin = "fading-circle"),
                
                # Inicialización shinyjs
                useShinyjs(),
                useShinyalert(),
                withMathJax(),
                
                #titlePanel(title=div(
                #  a(href="http://www.amb.cat/",
                #    img(src="img/AMB_logo.png",style = 'width: 300px; high: 600px;')
                #    ),
                #  p("INTELIGENCIA COMPETITIVA EMPRESAS",style = "font-size:22px; display: inline-block; margin-left: 15px;  color: #463c32;"))),
                
                titlePanel(title=div(
                  a(href="http://www.amb.cat/",
                    img(src="img/AMB_logo.png",style = 'width: 300px; high: 600px;')
                  )
                  )),
                
                
                
                
                navbarPage(id ="menu", "Menú empreses",
                           
                           tabPanel("BORME",
                                    sidebarLayout(
                                      # Menú de datos
                                      sidebarPanel(
                                        
                                        selectInput("Municipio_principal", "Seleccioneu el territori de referència",
                                                    c(as.character(mun),"AMB", "AMB sense Barcelona ciutat", "Barcelona ciutat", "Barcelona provincia")
                                        ),
                                        radioButtons(inputId="comparaciones",label="Seleccioneu el territori de comparació",choices=c("AMB", "AMB sense Barcelona ciutat", "Barcelona ciutat", "Barcelona provincia", "Un altre municipi"),
                                                     selected = "Barcelona provincia"),
                                        selectInput("Municipio_comparaciones", "Seleccioneu el municipio de comparació",
                                                    municipios$Municipio[municipios$AMB == 1]),
                                        
                                        dateRangeInput("fechas_listado_borme","Seleccioneu el intervalo de dates",start = (Sys.Date() - 30), end = (Sys.Date() - 30)),
                                        
                                        checkboxGroupInput("variables_borme_listado", label = "Selección variables",
                                                           choices = list("Constitució" = 1,
                                                                          "Canvis" = 2,
                                                                          "Nomenaments" = 3,
                                                                          "Ampliació de capital" = 11,
                                                                          "Reducció de capital" = 12,
                                                                          "Fusió" = 5,
                                                                          "Dissolució" = 6,
                                                                          "Extinció" = 7,
                                                                          "Escissió" = 8,
                                                                          "Transformació" = 9,
                                                                          "Situació concursal" = 10),
                                                           selected = 1
                                        ),
                                        
                                        radioButtons("variables_mapa", "Selecció variables",
                                                     c("Constitució" = 1,
                                                       "Canvi de domicili social" = 2)),
                                        
                                        # Búsqueda por objeto social
                                        #textInput("palabra_clave_listado_borme", "Búsqueda por objeto social"),
                                        
                                        # Boton descarga borme
                                        downloadButton("descarga_borme_csv", "Descàrrega dades en .csv"),
                                        br(),
                                        downloadButton("descarga_borme_xlsx", "Descàrrega dades en .xlsx"),
                                        width=3
                                      ),
                                      
                                      mainPanel(
                                        tabsetPanel(id = "tabs_borme",
                                                    
                                                    tabPanel(id = "tab_listado",
                                                             "Llistat informatiu",   #Con span genero un popup de ayuda.
                                                             
                                                             # Panel DATAFRAME
                                                             fluidRow(style='padding-top: 18px;',
                                                                      uiOutput("texto_tabla_borme_listado"),
                                                                      dataTableOutput("tabla_borme_listado"),
                                                             ),
                                                    ),
                                                    
                                                    tabPanel(id = "tab_estadistica_basica",
                                                             "Estadística bàsica 1",   #Con span genero un popup de ayuda.
                                                             
                                                             # Panel DATAFRAME
                                                             fluidRow(style='padding-top: 18px;',
                                                                      uiOutput("texto_tabla_borme_eb1"),
                                                                      dataTableOutput("tabla_borme_eb1"),
                                                             ),
                                                             fluidRow(
                                                               column(width = 9,
                                                                      plotlyOutput("barras_borme_eb1", height = 400)
                                                               )
                                                               
                                                             ),
                                                    ),
                                                    
                                                    tabPanel(id = "tab_estadistica_basica_2",
                                                             "Estadística bàsica 2",   #Con span genero un popup de ayuda.
                                                             
                                                             # Panel DATAFRAME
                                                             fluidRow(style='padding-top: 18px;',
                                                                      uiOutput("texto_tabla_borme_eb2"),
                                                                      dataTableOutput("tabla_borme_eb2"),
                                                             ),
                                                             fluidRow(
                                                               column(width = 9,
                                                                      plotlyOutput("lineas_borme_eb2", height = 400)
                                                               )
                                                               
                                                             ),
                                                    ),
                                                    
                                                    tabPanel(id = "tab_capital",
                                                             "Modificacions capital",
                                                             
                                                             # Panel DATAFRAME
                                                             fluidRow(style='padding-bottom: 25px;',
                                                                      uiOutput("texto_tabla_borme_capital"),
                                                                      dataTableOutput("tabla_borme_capital"),
                                                             ),
                                                             fluidRow(
                                                               column(width = 9,
                                                                      plotlyOutput("lineas_borme_capital_ampliaciones", height = 400),
                                                                      br()
                                                               )
                                                               
                                                             ),
                                                             fluidRow(
                                                               column(width = 9,
                                                                      plotlyOutput("lineas_borme_capital_reducciones", height = 400)
                                                               )
                                                               
                                                             ),
                                                    ),
                                                    
                                                    tabPanel(id = "tab_mapa",
                                                             "Mapa",
                                                             # Panel MAPA
                                                             fluidRow(style='padding-bottom: 25px;',
                                                                      uiOutput("texto_borme_mapa"),
                                                                      leafletOutput("mapa_borme"),
                                                             ),
                                                             
                                                             # Panel DATAFRAME
                                                             fluidRow(
                                                               uiOutput("texto_tabla_borme_mapa"),
                                                               dataTableOutput("tabla_borme_mapa"),
                                                             )
                                                    )
                                        )
                                        
                                      )
                                    )
                           ),
                           
                           tabPanel("ERTES",
                                    sidebarLayout(
                                      # Menú de datos
                                      sidebarPanel(
                                        selectInput("Municipio_principal_ertes", "Seleccioneu el territori de referència",
                                                    c(as.character(mun),"AMB", "AMB sense Barcelona ciutat", "Barcelona ciutat", "Barcelona provincia", "Catalunya")
                                        ),
                                        
                                        dateRangeInput("fechas_listado_ertes","Seleccioneu l'interval de dates",start = "2020-08-01", end = Sys.Date()),
                                        
                                        radioButtons("variables_ertes", "Selecció variable",
                                                     c("Expedients" = 1,
                                                       "Treballadors" = 2)),
                                        
                                        checkboxGroupInput("variables_ertes_sec_ecc", label = "Selecció variables",
                                                           choices = c(colnames(df_expediente_econom)[4:25],"Top 3","Tots","Cap"),
                                                           selected = "Top 3"),
                                        
                                        # Boton descarga ertes
                                        downloadButton("descarga_ertes_csv", "Descàrrega dades en .csv"),
                                        br(),
                                        downloadButton("descarga_ertes_xlsx", "Descàrrega dades en .xlsx"),
                                        width=3
                                      ),
                                      
                                      mainPanel(
                                        tabsetPanel(id = "tabs_ertes",
                                                    
                                                    tabPanel(id = "expedientes",
                                                             "Expedients per tipus",
                                                             
                                                             fluidRow(style='padding-top: 18px;',
                                                                      uiOutput("texto_tabla_ertes_expedientes_recuento"),
                                                                      dataTableOutput("tabla_ertes_expedientes_recuento"),
                                                             ),
                                                             fluidRow(style='padding-top: 18px;',
                                                               column(width = 9,
                                                                      plotlyOutput("lineas_ertes_expedientes", height = 400)
                                                               )
                                                             ),
                                                             fluidRow(style='padding-top: 18px;',
                                                                      uiOutput("texto_tabla_ertes_media"),
                                                                      dataTableOutput("tabla_ertes_media"),
                                                             ),
                                                    ),
                                                    
                                                    tabPanel(id = "secc_econom",
                                                             "Expedients per secció econòmica",   
                                                             
                                                             # Panel DATAFRAME
                                                             fluidRow(style='padding-top: 18px;',
                                                                      uiOutput("texto_tabla_ertes_secc_recuento"),
                                                                      dataTableOutput("tabla_ertes_secc_recuento"),
                                                             ),
                                                             fluidRow(style='padding-top: 18px;',
                                                               column(width = 9,
                                                                      plotlyOutput("barras_ertes_secc", height = 400)
                                                               )
                                                             ),
                                                             fluidRow(style='padding-top: 18px;',
                                                               plotlyOutput("lineas_ertes_secc", height = 500)
                                                             ),
                                                    ),
                                                    
                                                    tabPanel(id = "mapa",
                                                             "Mapa",
                                                             fluidRow(style='padding-bottom: 25px;',
                                                                      uiOutput("texto_ertes_mapa"),
                                                                      leafletOutput("mapa_ertes", height = 600),
                                                             ),
                                                             
                                                             fluidRow(
                                                               uiOutput("texto_tabla_ertes_mapa"),
                                                               dataTableOutput("tabla_ertes_mapa"),
                                                             )
                                                    )
                                        )
                                        
                                      )
                                    )
                           ) # Cierre tabpanel ERTES
                ) # Cierre navbarPage
) # Cierre UI

######################################################
# LÓGICA DE SERVIDOR
######################################################

server <- function(input, output, session) {
    
    datos <- reactiveValues(borme=NULL)

    ###############################################
    # INICIALIZACIÓN LÓGICA DE VISUALIZACIÓN OBJETOS SHINY

    # Lógica selección territorio de comparación
    observeEvent(input$comparaciones, {
        if(input$comparaciones == "Un altre municipi"){
            shinyjs::show("Municipio_comparaciones")
        }else{
            shinyjs::hide("Municipio_comparaciones")
        }
    })
    
    #Lógica visualización selección variables en mapa
    observeEvent(input$tabs_borme, {
      if(input$tabs_borme == "Mapa"){
        shinyjs::hide("comparaciones")
        shinyjs::hide("variables_borme_listado")
        shinyjs::show("variables_mapa")
      }else if(input$tabs_borme == "Modificacions capital"){
        shinyjs::hide("comparaciones")
        shinyjs::hide("variables_borme_listado")
        shinyjs::hide("variables_mapa")
      }else if(input$tabs_borme == "Llistat informatiu"){
        shinyjs::hide("comparaciones")
        shinyjs::show("variables_borme_listado")
        shinyjs::hide("variables_mapa")
      }else if(input$tabs_borme == "Estadística bàsica 2"){
        shinyjs::show("comparaciones")
        shinyjs::show("variables_borme_listado")
        shinyjs::hide("variables_mapa")
      }else{
        shinyjs::show("comparaciones")
        shinyjs::show("variables_borme_listado")
        shinyjs::hide("variables_mapa")
      }
    })
    
    #Lógica visualización filtros ERTES
    observeEvent(input$tabs_ertes, {
      if(input$tabs_ertes == "Mapa"){
        shinyjs::show("variables_ertes_sec_ecc")
      }else{
        shinyjs::hide("variables_ertes_sec_ecc")
      }
    })
    
    #Actualización de checkboxes mediante selector Tots.
    observeEvent(input$variables_ertes_sec_ecc,{
      if(any(grepl("Tots",input$variables_ertes_sec_ecc))){
        updateCheckboxGroupInput(session, inputId = "variables_ertes_sec_ecc", 
                                 selected = c(colnames(df_expediente_econom)[4:25],"Tots")
        )
      }else if(any(grepl("Cap",input$variables_ertes_sec_ecc))){
        updateCheckboxGroupInput(session, inputId = "variables_ertes_sec_ecc", 
                                 selected = "Cap"
        )
      }else{
        updateCheckboxGroupInput(session, inputId = "variables_ertes_sec_ecc", 
                                 selected = input$variables_ertes_sec_ecc
        )
      }
    })
    
    observeEvent(input$fechas_listado_borme, {
      print("Entro fechas")
      #show_spinner() # show the spinner
      show_modal_spinner() # show the modal window
      datos$borme = llamada_api(as.character(input$fechas_listado_borme[1]), as.character(input$fechas_listado_borme[2]))
      remove_modal_spinner() # remove it when done
      #hide_spinner() # hide the spinner
      print("LLEGO 1")
    })
    

    #Selección de 2 meses mínimo para tab estadística básica 2
    #observeEvent(input$tabs_borme, {
    #  if(input$tabs_borme == "Estadística básica 2"){
    #    fecha_inicial <- Sys.Date()
    #    fecha_final <- Sys.Date()
    #    day(fecha_inicial) <- day(fecha_inicial)-(day(fecha_inicial)-1)  #día a 1
    #    month(fecha_inicial) <- month(fecha_inicial)-2  #menos 2 meses
    #    day(fecha_final) <- day(fecha_final)-(day(fecha_final)-1)  #día a 1
    #        updateDateRangeInput(session, "fechas_listado_borme",
    #                         start = fecha_inicial,
    #                         end = fecha_final
    #    )
    #  }
    #})



    #==========================================================================
    # FUNCIONES DE APOYO
    #==========================================================================

    #Obtención coordenadas municipio referencia
    coordenadas_municipio <- reactive({

        df_atributos <- atributos_Borme()
        municipio_ref <- df_atributos$value[df_atributos$key == "Municipio"]

        #Coordenadas de referencia del municipio con geocoder API
        #Endpoint geocoder API
        geocoder_endpoint <- "https://geocoder.api.here.com/6.2/geocode.json?app_id=HRwFz9rfbtRq63qGH4ZQ&app_code=aMRd84WGRs4h1591F-g82w&searchtext="

        coordenadas_ref_municipio <- jsonlite::fromJSON(paste(geocoder_endpoint, URLencode(municipio_ref),"%20(Espa%C3%B1a)",sep = ""))
        coordenadas_ref_municipio <- coordenadas_ref_municipio$Response$View$Result %>% as.data.frame()
        longitud_ref_municipio <- coordenadas_ref_municipio$Location$DisplayPosition$Longitude
        latitud_ref_municipio <- coordenadas_ref_municipio$Location$DisplayPosition$Latitude
        coor_referencia <- c(longitud_ref_municipio, latitud_ref_municipio)

        return(coor_referencia)
    })

    #==========================================================================
    # LLAMADAS API THINGSBOARD
    #==========================================================================

    # Llamada API atributos activo Borme
    atributos_Borme <- reactive({
        #Llamada a la API
        atributos_df <- llamada_atributos()

        return(atributos_df)
    })

    # Llamada a API datos Thingsboard BORME reactivo
    datos_estructurados_borme <- reactive({
        
        datos_borme <- datos$borme
        
        nombres <- c("Empresa","Fusión sociedades abosrbidas", "Modificaciones estatutarias",
                     "Cambio denominación social", "Cambio domicilio social", "Cambio objeto social",
                     "Ceses liquiSoli", "Ceses apoderado", "Ceses Adm. Único",
                     "Ceses liquidador", "Ceses liquidador mancomunado", "Ceses adminSolid",
                     "Ceses Adm. Mancomunado", "Ceses Soc. Prof", "Ceses depositorio",
                     "Ceses entid. Deposit.", "Ceses entid. Promo.", "Ceses consejero",
                     "Ceses vicepresidente", "Ceses presidente", "Ceses secretario",
                     "Nombramiento liquiSoli", "Nombramiento apoderado", "Nombramiento Adm. Único",
                     "Nombramiento liquidador", "Nombramiento liquidador mancomunado", "Nombramiento Adm. Solid",
                     "Nombramiento Soc. Prof", "Nombramiento auditor","Nombramiento Adm. Mancomunado",
                     "Nombramiento Entid. Deposit.", "Nombramiento Entid. Promo.", "Nombramiento consejero",
                     "Nombramiento vicepresidente","Nombramiento presidente", "Nombramiento secretario",
                     "Ampliación capital suscrito", "Ampliación capital resultante suscrito", "Ampliación capital desembolsado",
                     "Ampliación capital resultante desembolsado", "Ampliación capital", "Declaración unipersonalidad socio único",
                     "Reducción capital importe reducción","Reducción capital resultante suscrito", "Reelecciones Adm. Único",
                     "Reelecciones auditor", "Reelecciones auditor suplente", "Revocaciones auditor",
                     "Revocaciones apoderado", "Revocaciones apoderado mancomunado", "Revocaciones apoderadoSol",
                     "Situación Concursal Procedimiento", "Situación Concursal Resolución firme","Situación Concursal Fecha Resolución",
                     "Situación Concursal Proceso", "Situación Concursal Juzgado", "Situación Concursal Juez",
                     "Situación Concursal Resoluciones", "Escisión", "Transformación", "Disolución", "Extinción",
                     "Constitución comienzo operaciones", "Constitución objeto social","Constitución domicilio social",
                     "Constitución capital", "Otros conceptos","Datos registrales",
                     "Latitud", "Longitud","Municipio",
                     "Distancia respecto municipio en km", "Provincia","Fecha",
                     "Dentro"
        )

        #provincias <- tolower(atributos_Borme()$value[atributos_Borme()$key == "Provincias"])
        
        if(datos_borme == 0){
          return(0)
        }

        # Manejo de error: "inexistencia de datos para el intervalo de fechas seleccionado" de cara al usuario
        #shiny::validate(
        #need(datos_borme != 0, "¡Atenció!\nNo existeixen dades disponibles per a l'interval de dates seleccionat.\nSelecciona un altre interval si ho desitja.")
            #)

        #datos_borme <- datos_borme[str_detect(provincias, gsub("/.*","",tolower(datos_borme$Provincia))), ]
        colnames(datos_borme) <- nombres
        datos_borme <- datos_borme[,c(1,2,5,6,24,34,35,59,60,61,62,63,64,65,66,41,37,39,40,43,44,58,69,70,71,74)]

        #Generación forma jurídica
        forma_juridica <- c()
        for(i in 1:length(datos_borme$Empresa)){
            pos_ultimo_espacio <- gregexpr(" ",datos_borme$Empresa[i])[[1]][length(gregexpr(" ",datos_borme$Empresa[i])[[1]])]
            forma_juridica1 <- str_trim(substring(datos_borme$Empresa[i],pos_ultimo_espacio,nchar(datos_borme$Empresa[i])))
            if(nchar(forma_juridica1) > 3){
                nuevo_nombre <- gsub(" EN LIQUIDACION","",datos_borme$Empresa[i])
                pos_ultimo_espacio <- gregexpr(" ",nuevo_nombre)[[1]][length(gregexpr(" ",nuevo_nombre)[[1]])]
                forma_juridica1 <- str_trim(substring(nuevo_nombre,pos_ultimo_espacio,nchar(nuevo_nombre)))

                if(nchar(forma_juridica1) > 3){
                    forma_juridica1 <- "Otras"
                }
            }
            forma_juridica <- c(forma_juridica, forma_juridica1)
        }

        datos_borme$`Forma Jurídica` <- gsub("\\.","",forma_juridica)

        #AMB
        datos_borme$AMB <- municipios$AMB[match(datos_borme$Municipio, municipios$Municipio)]
        datos_borme$Fecha <- as.character(datos_borme$Fecha)
        
        nombres_catala <- c("Denominació social","Fusió societats abosrbidas","Canvi domicili social","Canvi objecte social","Nomenament Adm. Únic","Nomenament vicepresident",
                            "Nomenament president","Escissió","Transformació","Dissolució","Extinció","Constitució començament operacions","Constitució objecte social",
                            "Constitució domicili social","Constitució capital","Ampliació capital","Ampliació capital subscrit","Ampliació capital desemborsat",
                            "Ampliació capital resultant desemborsat","Reducció capital import reducció","Reducció capital resultant subscrit","Situació Concursal Resolucions",
                            "Latitud","Longitud","Municipio","Data","Forma Jurídica","AMB")
        colnames(datos_borme) <- nombres_catala

        return(datos_borme)
    })

    #==========================================================================
    # FILTRADOS
    #==========================================================================

    # 1) Filtrado datos BORME
    datos_filtrados_borme <- reactive({

        df <- datos_estructurados_borme()  #Llamada a API
        
        if(df == 0){
          return(0)
        }
        
        if(input$tabs_borme == "Mapa"){
          variables_entrada <- input$variables_mapa
        }else if(input$tabs_borme == "Modificacions capital"){
          variables_entrada <- "4"
        }else{
          variables_entrada <- input$variables_borme_listado
        }
        
        
        #1)Filtrado en funcón de tab
        df <- df[,c("Denominació social",unlist(lista_variables_borme[as.numeric(variables_entrada)]),"Latitud","Longitud","Municipio","Data","Forma Jurídica","AMB")]

        #df <- na.omit(df)
        
        if(ncol(df) == 0){
          return(0)
        }else if(is.null(variables_entrada)){
          return(1)
        }else if(is.null(colnames(df[,2:(ncol(df)-6)])) & ncol(df) != 8 ){
          return(2)
        }
        

        # 2)Filtro Agrupaciones
        if(input$Municipio_principal == "Barcelona provincia"){
            df <- df
        }else{
            switch(input$Municipio_principal,
                   "AMB"={
                       df <- df[df$AMB == 1,]
                   },
                   "AMB sense Barcelona ciutat"={
                       df <- df[df$AMB == 1 & df$Municipio != "Barcelona",]
                   },
                   "Barcelona ciutat"={
                       df <- df[df$Municipio == "Barcelona",]
                   },
                   {
                       df <- df[df$Municipio == input$Municipio_principal,]
                   }
            )
        }
        
        if(nrow(df) == 0){
          return(0)
        }
        
        #Eliminación filas con "-" en todas las columnas
        df[df=="-"] <- NA
        if(ncol(df) == 8){
          df <- df[!is.na(df[,2]),]
        }else{
          df <- df[rowSums(is.na(df[,2:(ncol(df)-6)])) != (ncol(df[,2:(ncol(df)-6)])), ]  #Se elimina si el número de columnas de la fila (las que se pueden seleccionar) con todo NA == al número de columnas (las que se pueden seleccionar)
        }
        
        if(nrow(df) == 0){
          return(0)
        }
        
        return(df)
    })

    # 2) Filtrado datos BORME TERRITORIO DE COMPARACIÓN
    datos_filtrados_borme_comparativa <- reactive({

        df <- datos_estructurados_borme()  #Llamada a API
        
        if(df == 0 | df == 1 | df == 2){
          return(df)
        }

        if(input$tabs_borme == "Mapa"){
          variables_entrada <- input$variables_mapa
        }else if(input$tabs_borme == "Modificacions capital"){
          variables_entrada <- "4"
        }else{
          variables_entrada <- input$variables_borme_listado
        }
        
        #1)Filtrado en funcón de tab
        df <- df[,c("Denominació social",unlist(lista_variables_borme[as.numeric(variables_entrada)]),"Latitud","Longitud","Municipio","Data","Forma Jurídica","AMB")]
        
        
        if(ncol(df) == 0){
          return(0)
        }else if(is.null(variables_entrada)){
          return(1)
        }else if(is.null(colnames(df[,2:(ncol(df)-6)])) & ncol(df) != 8 ){
          return(2)
        }
        

        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(ncol(df) != 0,
                 "")
        )
        #shiny::validate(
        #need(ncol(df[,2:(ncol(df)-6)]) != 0,
        #"¡Atenció!\nNo existeixen dades disponibles per al valor dels filtres seleccionats.\nModifica el valor dels filtres si ho desitja.")
        #)
        
        # 2)Filtro Agrupaciones
        if(input$comparaciones == "Barcelona provincia"){
            df <- df
        }else{
            switch(input$comparaciones,
                   "AMB"={
                       df <- df[df$AMB == 1,]
                   },
                   "AMB sense Barcelona ciutat"={
                       df <- df[df$AMB == 1 & df$Municipio != "Barcelona",]
                   },
                   "Barcelona ciutat"={
                       df <- df[df$Municipio == "Barcelona",]
                   },
                   "Un altre municipi"={
                       df <- df[df$Municipio == input$Municipio_comparaciones,]
                   }
            )
        }
        
        if(nrow(df) == 0){
          return(0)
        }else if(is.null(variables_entrada)){
          return(1)
        }else if(is.null(colnames(df[,2:(ncol(df)-6)])) & ncol(df) != 8 ){
          return(2)
        }

        #Eliminación "-" si están en todas las columnas
        df[df=="-"] <- NA
        if(ncol(df) == 8){
          df <- df[!is.na(df[,2]),]
        }else{
          df <- df[rowSums(is.na(df[,2:(ncol(df)-6)])) != (ncol(df[,2:(ncol(df)-6)])), ]  #Se elimina si el número de columnas (las que se pueden seleccionar) con todo NA == al número de columnas (las que se pueden seleccionar)
        }
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(nrow(df) != 0,
                 "")
        )

        return(df)
    })

    # ==========================================
    # FUNCIONES
    # ==========================================
    
    # 1) Función estadística básica 1: RECUENTO
    func_estadistica_basica <- function(df){
        df <- df
        
        if(df == 0 | df == 1 | df == 2){
          return(df)
        }

        # Recuento empresas por formas jurídicas
        df <- df %>%
            group_by(`Forma Jurídica`) %>%
            summarise(n = n())

        #Generación DF
        nombre_columnas <- as.vector(df[,1])
        df <- as.data.frame(t(df),row.names = FALSE,stringsAsFactors = FALSE)

        for(i in 1:ncol(df)){
            colnames(df)[i] <- nombre_columnas[i,]
            df[,i] <- as.numeric(df[,i])
        }
        df <- na.omit(df)

        if(ncol(df) > 1){
            #Suma a Otras formas jurídicas no reconocidas
            pos_no_sociedades <- grep("[0-9]",colnames(df))
            if(!identical(pos_no_sociedades,integer(0))){
                df$Otras <- df$Otras + as.numeric(df[,pos_no_sociedades])
                df <- df[,-pos_no_sociedades]
            }
            pos_no_sociedades <- grep("FP",colnames(df))
            if(!identical(pos_no_sociedades,integer(0))){
                df$Otras <- df$Otras + as.numeric(df[,pos_no_sociedades])
                df <- df[,-pos_no_sociedades]
            }

            #Orden de Otras al final en columnas
            df <- df[,order(colnames(df))]
            pos_otras <- grep("Otras",colnames(df))
            if(!identical(pos_otras,integer(0)) & ncol(df) > 2){
                orden <- order(colnames(df[,-pos_otras]))
                for(i in pos_otras:length(orden)){
                    orden[i] <- orden[i] + 1
                }
                df <- df[,c(orden,pos_otras)]
            }else{
              df <- df[,c(2,1)]
            }
        }
        
        return(df)
    }
    
    # 2) Función estadística básica 2: MEDIA, MAX, MIN Y PERCENTILES
    recuento_estadistica_basica_2 <- function(df, flag_return){
      df <- df
      
      if(df == 0 | df == 1 | df == 2){
        return(df)
      }
      
      #Recuento por mes y forma jurídica
      df <- df %>%
        group_by(`Forma Jurídica`, Mes) %>%
        summarise(Recompte = n())
      
      if(flag_return == 1){  # El valor de 1 es para la realización del gráfico de evolución.
        return(df)
      }
      
      # Cálculo estadístivas
      df <- df %>%
        group_by(`Forma Jurídica`) %>%
        summarise(
          Mitjana = round(mean(Recompte),0),
          Máx = max(Recompte),
          Mín = min(Recompte),
          `Desviació tipus` = round(sd(Recompte),2),
          `Percentil 25` = round(quantile(Recompte,0.25),2),
          `Percentil 75` = round(quantile(Recompte,0.75),2)
        )
      
      #Generación DF
      nombre_columnas <- as.vector(df[,1])
      df <- as.data.frame(t(df),row.names = FALSE,stringsAsFactors = FALSE)
      
      for(i in 1:ncol(df)){
        colnames(df)[i] <- nombre_columnas[i,]
        df[,i] <- as.numeric(df[,i])
      }
      
      #df <- na.omit(df)
      if(ncol(df) > 1){
        #Suma a Otras formas jurídicas no reconocidas
        pos_no_sociedades <- grep("[0-9]",colnames(df))
        if(!identical(pos_no_sociedades,integer(0))){
          df$Otras <- df$Otras + as.numeric(df[,pos_no_sociedades])
          df <- df[,-pos_no_sociedades]
        }
        pos_no_sociedades <- grep("FP",colnames(df))
        if(!identical(pos_no_sociedades,integer(0))){
          df$Otras <- df$Otras + as.numeric(df[,pos_no_sociedades])
          df <- df[,-pos_no_sociedades]
        }
        
        #Orden de Otras al final en columnas
        df <- df[,order(colnames(df))]
        pos_otras <- grep("Otras",colnames(df))
        if(!identical(pos_otras,integer(0)) & ncol(df) > 2){
          orden <- order(colnames(df[,-pos_otras]))
          for(i in pos_otras:length(orden)){
            orden[i] <- orden[i] + 1
          }
          df <- df[,c(orden,pos_otras)]
        }else{
          df <- df
        }
      }
      nombre_columnas <- colnames(df)
      df <- as.data.frame(df[-1,]) #La primera fila no es un valor numérico (resultado de pasar a númerico la forma jurídica)
      colnames(df) <- nombre_columnas
      rownames(df) <- c("Mitjana", "Máxim", "Mínim","Desviació tipus","Percentil 25", "Percentil 75")
      
      return(df)
    }

    # ===================================
    # REACTIVE APOYO FUNCIONES
    # ===================================
    
    # 1) Estadistica básica 1
    estadistica_basica_1 <- reactive({

        df_ref <- func_estadistica_basica(datos_filtrados_borme())
        
        if((df_ref == 0 | df_ref == 1 | df_ref == 2) & !is.data.frame(df_ref)){
          return(df_ref)
        }
        
        # Cálculo representación en el territorio de referencia
        df_representacion_en_territorio <- df_ref
        #Llenado con NAs
        for(i in 1:ncol(df_ref)){
          df_representacion_en_territorio[,i] <- rep(NA,nrow(df_ref))
        }
        for(i in 1:ncol(df_ref)){
          df_representacion_en_territorio[,i] <- round(100*(df_ref[,i]/sum(as.numeric(df_ref[1,]))),2)
        }
        
        df_comparativa <- func_estadistica_basica(datos_filtrados_borme_comparativa())

        if((df_comparativa == 0 | df_comparativa == 1 | df_comparativa == 2) & !is.data.frame(df_comparativa)){
          #df_comparativa <- df_ref
          #df_comparativa[1,] <- rep(NA,ncol(df_comparativa))
          
          df <- rbind(df_ref,df_representacion_en_territorio)
          
          df$Total <- c(sum(as.numeric(df[1,])),100)
          rownames(df) <- c("Recompte", "Representació en territori de referència (%)")
          
        }else{
          # Cálculo representación por formas jurídicas respecto terrritorio comparación
          df_representacion <- df_ref
          #Llenado con NAs
          for(i in 1:ncol(df_ref)){
            df_representacion[,i] <- rep(NA,nrow(df_ref))
          }
          for(i in 1:ncol(df_ref)){
            if(any(colnames(df_comparativa) %in% colnames(df_ref)[i])){
              df_representacion[,i] <- round(100*(df_ref[,i]/df_comparativa[,which(colnames(df_comparativa) %in% colnames(df_ref)[i])]),2)
            }
          }
          
          
          # Cálculo representación en el territorio de comparativa
          df_representacion_en_territorio_comparativa <- df_ref
          #Llenado con NAs
          for(i in 1:ncol(df_ref)){
            df_representacion_en_territorio_comparativa[,i] <- rep(NA,nrow(df_ref))
          }
          for(i in 1:ncol(df_ref)){
            df_representacion_en_territorio_comparativa[,i] <- round(100*(df_ref[,i]/sum(as.numeric(df_comparativa[1,]))),2)
          }
          
          df <- rbind(df_ref,df_representacion_en_territorio,df_representacion_en_territorio_comparativa,df_representacion)
          
          df$Total <- c(sum(as.numeric(df[1,])),100)
          if(is.na(sum(df[3,1:(ncol(df)-1)]))){
            df$Total[3] <- NA
          }else{
            df$Total[3] <- sum(df[3,1:(ncol(df)-1)])
          }
          if(is.na(sum(df[4,1:(ncol(df)-1)]))){
            df$Total[4] <- NA
          }else{
            df$Total[4] <- NA
          }
          
          if(nrow(df) == 1){
            rownames(df) <- c("Recompte")
          }else{
            rownames(df) <- c("Recompte", "Representació en territori de referència (%)", "Representació en territori de comparació (%)","Representació sobre forma jurídica en territori de comparació (%)")
          }
        }
        
        df
    })


    # 2) Estadistica básica 2
    estadistica_basica_2 <- reactive({

      #Validación de fechas
      flag_Avís <- ifelse(abs(month(input$fechas_listado_borme[1])-month(input$fechas_listado_borme[2])) >= 2 |
                             as.numeric(input$fechas_listado_borme[2] - input$fechas_listado_borme[1]) > 90,
                           0,1)
      if(flag_Avís == 1){return(0)}

      #Ajuste fechas usuario para cuadrar meses. Inicio de mes.
      fecha_inicial <- input$fechas_listado_borme[1]
      day(fecha_inicial) <- day(fecha_inicial)-(day(fecha_inicial)-1)
      fecha_final <- input$fechas_listado_borme[2]
      day(fecha_final) <- day(fecha_final)-(day(fecha_final)-1)

      df <- datos_filtrados_borme()
      if((df == 0 | df == 1 | df == 2) & !is.data.frame(df)){
        return(df)
      }
      
      df$Mes <- paste(year(df$Data),"/",month(df$Data),sep = "")  #Extracción de meses
      df_ref <- recuento_estadistica_basica_2(df,2)  #Flag 2 para devolución con cálculos estadísticos

      if((df_ref == 0 | df_ref == 1 | df_ref == 2) & !is.data.frame(df_ref)){
        return(df_ref)
      }
      
      df2 <- datos_filtrados_borme_comparativa()
      
      if(df2 == 0 | df2 == 1 | df2 == 2){
        df <- df_ref
      }else{
        df2$Mes <- paste(year(df2$Data),"/",month(df2$Data),sep = "")  #Extracción de meses
        df_comparativa <- recuento_estadistica_basica_2(df2, 2) #Flag 2 para devolución con cálculos estadísticos
        
        df_representacion <- df_ref
        #Llenado con NAs
        for(i in 1:ncol(df_ref)){
          df_representacion[,i] <- rep(NA,nrow(df_ref))
        }
        
        # Cálculo representación
        for(i in 1:ncol(df_ref)){
          if(any(colnames(df_comparativa) %in% colnames(df_ref)[i])){
            df_representacion[,i] <- round(100*(df_ref[,i]/df_comparativa[,which(colnames(df_comparativa) %in% colnames(df_ref)[i])]),2)
          }
        }
        
        df <- rbind(df_ref,df_representacion)
        
        if(nrow(df) == 1){
          rownames(df) <- c("Mitjana", "Máxim", "Mínim", "Desviació tipus", "Percentil 25", "Percentil 75")
        }else{
          rownames(df) <- c("Mitjana", "Máxim", "Mínim", "Desviació tipus", "Percentil 25", "Percentil 75",
                            "Comparació Mitjana (%)", "Comparació Máxim (%)", "Comparació Mínim (%)", "Comparació Desviació tipus (%)", "Comparació Percentil 25 (%)", "Comparació Percentil 75 (%)")
        }
      }
      
      print(df)
      
      df
    })


    #==========================================================================
    # GENERACIÓN TABLAS
    #==========================================================================

    # 0) Manejo de tablas presentes en los tabs BORME. Su función es agrupar las modificaciones comunes del conjunto de tablas de la aplicación
    manejo_tablas_borme <- reactive({

        df_tabla <- datos_filtrados_borme()

        # En caso de solo visualizar la variable empresa, se convierte a objeto tipo entero y hay que volver a reconvertirlo a data.frame
        if(typeof(df_tabla) == "integer"){
            df_tabla <- as.data.frame(df_tabla)
            names(df_tabla) <- "`Denominació social`"
        }
        
        if(is.null(nrow(df_tabla))){
          return(0)
        }

        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(df_tabla) > 0){
            row.names(df_tabla) <- seq(1,nrow(df_tabla))
        }

        return(df_tabla)

    })

    # 1) Tabla BORME listado informativo
    output$tabla_borme_listado <- renderDataTable({
      
        df_tabla <- manejo_tablas_borme()
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
          need(is.data.frame(df_tabla) & nrow(df_tabla) != 0,
               "¡Atenció!\nNo existeixen dades disponibles per al valor dels filtres seleccionats.\nModifica el valor dels filtres si ho desitja.")
        )
        
        df_tabla <- df_tabla[,1:(ncol(df_tabla)-6)] #Evita la visualización de las variables lat,long,municipio y provincia.

        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(!is.null(nrow(df_tabla) & is.data.frame(df_tabla)),
                 "¡Atenció!\nNo existeixen dades disponibles per al valor dels filtres seleccionats.\nModifica el valor dels filtres si ho desitja.")
        )
        #Límite visualización registros tabla
        tabla <- datatable(df_tabla, options = list(pageLength = 5,
                                                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                    scrollX=TRUE,
                                                    scrollCollapse=TRUE),
                           escape = FALSE)

        return(tabla)

    },options = list(scrollX = T))

    # 2) Tabla BORME ESTADÍSTICA BÁSICA 1
    output$tabla_borme_eb1 <- renderDataTable({

        df_tabla <- estadistica_basica_1()
 
        if(!is.data.frame(df_tabla)){
          # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
          shiny::validate(
            need(is.data.frame(df_tabla),
                 switch(as.character(df_tabla),
                        "0"={
                          "¡Atenció!\nNo existeixen dades disponibles per al valor dels filtres seleccionats.\nModifica el valor dels filtres si ho desitja."
                        },
                        "1"={
                          "¡Atenció!\nNo hi ha cap variable seleccionada.\nSi us plau, selecciona el menys una variable."
                        },
                        "2"={
                          "¡Atenció!\nNo existeixen dades disponibles per al valor dels filtres seleccionats.\nModifica el valor dels filtres si ho desitja."
                        }
                 )
            )
          )
        }

        #Límite visualización registros tabla
        tabla <- datatable(df_tabla, options = list(pageLength = 5,
                                                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                    scrollX=TRUE,
                                                    scrollCollapse=TRUE),
                           escape = FALSE)
        return(tabla)
    },options = list(scrollX = T))


    # 3) Tabla BORME ESTADÍSTICA BÁSICA 2
    output$tabla_borme_eb2 <- renderDataTable({

        df <- estadistica_basica_2()
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
          need(df != 0,
               "¡Atenció!\nEs necesario selecionar un periodo de 2 meses para el cálculo de estadísticas mensuales.\nModifica el valor dels filtres si ho desitja.")
        )
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
          need(df != 1,
               "¡Atenció!\nNo existeixen dades disponibles per al valor dels filtres seleccionats.\nModifica el valor dels filtres si ho desitja.")
        )
        
        #Límite visualización registros tabla
        tabla <- datatable(df, options = list(pageLength = 25,
                                                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                    scrollX=TRUE,
                                                    scrollCollapse=TRUE),
                           escape = FALSE)
        return(tabla)
    },options = list(scrollX = T))

    # 4) Tabla BORME MAPA
    output$tabla_borme_mapa <- renderDataTable({

      df_tabla <- manejo_tablas_borme()
      
      if(input$variables_mapa == 1){
        variable <- "Constitució objecte social"
      }else{
        variable <- "Canvi domicili social"
      }
      shiny::validate(
        need(any(grepl(variable,colnames(na.omit(df_tabla)))) & nrow(na.omit(df_tabla)) != 0,
             "¡Atenció!\nNo existeixen dades disponibles per al valor dels filtres seleccionats.\nModifica el valor dels filtres si ho desitja."
        )
      )
      
      if(input$variables_mapa == 1){
        df_tabla <- df_tabla[,c(1,2,3,4)]
      }else{
        df_tabla <- df_tabla[,c(1,2,3)]
      }
      
      #Límite visualización registros tabla
      tabla <- datatable(df_tabla, options = list(pageLength = 5,
                                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                  scrollX=TRUE,
                                                  scrollCollapse=TRUE),
                         escape = FALSE)
      return(tabla)
    })
    
    #5.1) Reactive ayuda a tabla modificaciones capital
    ayuda_borme_capital <- reactive({
      
      df <- manejo_tablas_borme()
      
      shiny::validate(
        need(nrow(df) != 0 & is.data.frame(df),
             "¡Atenció!\nNo hay datos suficientes como para generar el gráfico."
        )
      )
      
      # Paso a númerico capital
      for(i in 2:7){
        df[,i] <- gsub("[a-z].","",df[,i])
        df[,i] <- gsub("[.]","",df[,i])
        df[,i] <- gsub("[.]","",df[,i])
        df[,i] <- as.numeric(gsub("[,]",".",df[,i]))
      }
      
      #Incluir desembolsado (es la alternativa a capital, a veces aparece así)
      for(i in 1:nrow(df)){
        if(is.na(df$`Ampliació capital`[i])){
          df$`Ampliació capital`[i] <- df$`Ampliació capital desemborsat`[i]
          df$`Ampliació capital subscrit`[i] <- df$`Ampliació capital resultant desemborsat`[i]
        }
      }
      
      # Cálculo evoluciones ampliaciones y reducciones de capital
      df <- df %>%
        mutate(`Evolució ampliació` = 100*((`Ampliació capital subscrit` - (`Ampliació capital subscrit` - `Ampliació capital`))/(`Ampliació capital subscrit` - `Ampliació capital`))) %>%
        mutate(`Evolució reducció` = 100*(((`Reducció capital resultant subscrit` + `Reducció capital import reducció`)-`Reducció capital resultant subscrit`)/(`Reducció capital resultant subscrit` + `Reducció capital import reducció`)))
      
      `Evolució ampliació` <- na.omit(df$`Evolució ampliació`)
      `Evolució reducció` <- na.omit(df$`Evolució reducció`)

      # Llamada a función para filtrado de atípicos
      df <- df %>%
        group_by(`Forma Jurídica`) %>% 
        mutate(`Evolució ampliació` = detectar_atipicios(`Evolució ampliació`)) %>%
        mutate(`Evolució reducció` = detectar_atipicios(`Evolució reducció`))

      df3 <- df   # Data frame de referencia sin atípicos para el c'alculo de recuentos y medias
      
      # Cálculo medias de evolución y reducciones de capital
      df <- df3 %>%
        group_by(`Forma Jurídica`) %>%
        summarise(
          `Mitjana evolució ampliació` = round(mean(`Evolució ampliació`, na.rm = TRUE),2),
          `Mitjana evolució reducció` = round(mean(`Evolució reducció`, na.rm = TRUE),2)
        )
      
      #Generación DF
      nombre_columnas <- as.vector(df[,1])
      df <- as.data.frame(t(df),row.names = FALSE,stringsAsFactors = FALSE)
      
      for(i in 1:ncol(df)){
        colnames(df)[i] <- nombre_columnas[i,]
        df[,i] <- as.numeric(df[,i])
      }
      
      nombre_columnas <- colnames(df)
      df <- as.data.frame(df[-1,]) #La primera fila no es un valor numérico (resultado de pasar a númerico la forma jurídica)
      colnames(df) <- nombre_columnas
      
      #df <- na.omit(df)
      
      if(ncol(df) > 1){
        #Suma a Otras formas jurídicas no reconocidas
        pos_no_sociedades <- grep("[0-9]",colnames(df))
        if(!identical(pos_no_sociedades,integer(0))){
          df$Otras <- df$Otras + as.numeric(df[,pos_no_sociedades])
          df <- df[,-pos_no_sociedades]
        }
        pos_no_sociedades <- grep("FP",colnames(df))
        if(!identical(pos_no_sociedades,integer(0))){
          df$Otras <- df$Otras + as.numeric(df[,pos_no_sociedades])
          df <- df[,-pos_no_sociedades]
        }
        
        #Orden de Otras al final en columnas
        df <- df[,order(colnames(df))]
        pos_otras <- grep("Otras",colnames(df))
        if(!identical(pos_otras,integer(0)) & ncol(df) > 2){
          orden <- order(colnames(df[,-pos_otras]))
          for(i in pos_otras:length(orden)){
            orden[i] <- orden[i] + 1
          }
          df <- df[,c(orden,pos_otras)]
        }else{
          df <- df[,c(2,1,3:ncol(df))]
        }
      }
      
      #df <- df[,colSums(is.na(df))<nrow(df)]
      rownames(df) <- c("Evolució ampliació (%)", "Evolució reducció (%)")
      
      df$Total <- rowMeans(df, na.rm=TRUE)
      
      # Genearación df recuento ampliación
      df_recuento_ampliacion <- df3[,1:(ncol(df3)-1)]
      df_recuento_ampliacion <- df_recuento_ampliacion[!is.na(df_recuento_ampliacion$`Evolució ampliació`),]
      df_recuento_ampliacion <- df_recuento_ampliacion %>%
        group_by(`Forma Jurídica`) %>%
        summarise(`Recuento ampliació` = n()
        )
      
      if(nrow(df_recuento_ampliacion) != 0){
        #Generación df_recuento_ampliacion
        nombre_columnas <- as.vector(df_recuento_ampliacion[,1])
        df_recuento_ampliacion <- as.data.frame(t(df_recuento_ampliacion),row.names = FALSE,stringsAsFactors = FALSE)
        
        for(i in 1:ncol(df_recuento_ampliacion)){
          colnames(df_recuento_ampliacion)[i] <- nombre_columnas[i,]
          df_recuento_ampliacion[,i] <- as.numeric(df_recuento_ampliacion[,i])
        }
        
        nombre_columnas <- colnames(df_recuento_ampliacion)
        df_recuento_ampliacion <- as.data.frame(df_recuento_ampliacion[-1,]) #La primera fila no es un valor numérico (resultado de pasar a númerico la forma jurídica)
        colnames(df_recuento_ampliacion) <- nombre_columnas
        
        #df_recuento_ampliacion <- na.omit(df_recuento_ampliacion)
        
        if(ncol(df_recuento_ampliacion) > 1){
          #Suma a Otras formas jurídicas no reconocidas
          pos_no_sociedades <- grep("[0-9]",colnames(df_recuento_ampliacion))
          if(!identical(pos_no_sociedades,integer(0))){
            df_recuento_ampliacion$Otras <- df_recuento_ampliacion$Otras + as.numeric(df_recuento_ampliacion[,pos_no_sociedades])
            df_recuento_ampliacion <- df_recuento_ampliacion[,-pos_no_sociedades]
          }
          pos_no_sociedades <- grep("FP",colnames(df_recuento_ampliacion))
          if(!identical(pos_no_sociedades,integer(0))){
            df_recuento_ampliacion$Otras <- df_recuento_ampliacion$Otras + as.numeric(df_recuento_ampliacion[,pos_no_sociedades])
            df_recuento_ampliacion <- df_recuento_ampliacion[,-pos_no_sociedades]
          }
          
          #Orden de Otras al final en columnas
          df_recuento_ampliacion <- df_recuento_ampliacion[,order(colnames(df_recuento_ampliacion))]
          pos_otras <- grep("Otras",colnames(df_recuento_ampliacion))
          if(!identical(pos_otras,integer(0)) & ncol(df_recuento_ampliacion) > 2){
            orden <- order(colnames(df_recuento_ampliacion[,-pos_otras]))
            for(i in pos_otras:length(orden)){
              orden[i] <- orden[i] + 1
            }
            df_recuento_ampliacion <- df_recuento_ampliacion[,c(orden,pos_otras)]
          }else{
            df_recuento_ampliacion <- df_recuento_ampliacion
          }
        }
        
        rownames(df_recuento_ampliacion) <- "Recompte ampliacions"
        df_recuento_ampliacion$Total <- rowSums(df_recuento_ampliacion, na.rm=TRUE)
      }
      
      
      
      # Genearación df recuento reducción
      df_recuento_reduccion <- df3[,c(1:(ncol(df3)-2),ncol(df3))]
      df_recuento_reduccion <- df_recuento_reduccion[!is.na(df_recuento_reduccion$`Evolució reducció`),]
      df_recuento_reduccion <- df_recuento_reduccion %>%
        group_by(`Forma Jurídica`) %>%
        summarise(`Recuento reducció` = n()
        )
      
      if(nrow(df_recuento_reduccion) != 0){
        #Generación df_recuento_reduccion
        nombre_columnas <- as.vector(df_recuento_reduccion[,1])
        df_recuento_reduccion <- as.data.frame(t(df_recuento_reduccion),row.names = FALSE,stringsAsFactors = FALSE)
        
        for(i in 1:ncol(df_recuento_reduccion)){
          colnames(df_recuento_reduccion)[i] <- nombre_columnas[i,]
          df_recuento_reduccion[,i] <- as.numeric(df_recuento_reduccion[,i])
        }
        
        nombre_columnas <- colnames(df_recuento_reduccion)
        df_recuento_reduccion <- as.data.frame(df_recuento_reduccion[-1,]) #La primera fila no es un valor numérico (resultado de pasar a númerico la forma jurídica)
        colnames(df_recuento_reduccion) <- nombre_columnas
        
        #df_recuento_reduccion <- na.omit(df_recuento_reduccion)
        
        if(ncol(df_recuento_reduccion) > 1){
          #Suma a Otras formas jurídicas no reconocidas
          pos_no_sociedades <- grep("[0-9]",colnames(df_recuento_reduccion))
          if(!identical(pos_no_sociedades,integer(0))){
            df_recuento_reduccion$Otras <- df_recuento_reduccion$Otras + as.numeric(df_recuento_reduccion[,pos_no_sociedades])
            df_recuento_reduccion <- df_recuento_reduccion[,-pos_no_sociedades]
          }
          pos_no_sociedades <- grep("FP",colnames(df_recuento_reduccion))
          if(!identical(pos_no_sociedades,integer(0))){
            df_recuento_reduccion$Otras <- df_recuento_reduccion$Otras + as.numeric(df_recuento_reduccion[,pos_no_sociedades])
            df_recuento_reduccion <- df_recuento_reduccion[,-pos_no_sociedades]
          }
          
          #Orden de Otras al final en columnas
          df_recuento_reduccion <- df_recuento_reduccion[,order(colnames(df_recuento_reduccion))]
          pos_otras <- grep("Otras",colnames(df_recuento_reduccion))
          if(!identical(pos_otras,integer(0)) & ncol(df_recuento_reduccion) > 2){
            orden <- order(colnames(df_recuento_reduccion[,-pos_otras]))
            for(i in pos_otras:length(orden)){
              orden[i] <- orden[i] + 1
            }
            df_recuento_reduccion <- df_recuento_reduccion[,c(orden,pos_otras)]
          }else{
            df_recuento_reduccion <- df_recuento_reduccion
          }
        }
        
        #df_recuento_reduccion <- df_recuento_reduccion[,colSums(is.na(df_recuento_reduccion))<nrow(df_recuento_reduccion)]
        rownames(df_recuento_reduccion) <- "Recuento reducciones"
        df_recuento_reduccion$Total <- rowSums(df_recuento_reduccion, na.rm=TRUE)
      }
      
      recuento_ref_ampl <- data.frame(rep(NA,ncol(df)),stringsAsFactors = FALSE)
      recuento_ref_ampl <- as.data.frame(t(recuento_ref_ampl))
      colnames(recuento_ref_ampl) <- colnames(df)
      rownames(recuento_ref_ampl) <- "Recompte ampliacions"
      recuento_ref_reducc <- data.frame(rep(NA,ncol(df)),stringsAsFactors = FALSE)
      recuento_ref_reducc <- as.data.frame(t(recuento_ref_reducc))
      colnames(recuento_ref_reducc) <- colnames(df)
      rownames(recuento_ref_reducc) <- "Recompte reducciones"
      
      
      # Generación data frame referencia recuento ampliaciones
      namestoChange <- colnames(recuento_ref_ampl)[colnames(df_recuento_ampliacion) %in% colnames(recuento_ref_ampl)]
      for(i in 1:length(namestoChange)){
        if(any(colnames(df_recuento_ampliacion) %in% namestoChange[i])){
          recuento_ref_ampl[,namestoChange[i]] <- df_recuento_ampliacion[,namestoChange[i]]
        }
      }
      
      # Generación data frame referencia recuento reducciones
      namestoChange <- colnames(recuento_ref_reducc)[colnames(df_recuento_ampliacion) %in% colnames(recuento_ref_reducc)]
      for(i in 1:length(namestoChange)){
        if(any(colnames(df_recuento_reduccion) %in% namestoChange[i])){
          recuento_ref_reducc[,namestoChange[i]] <- df_recuento_reduccion[,namestoChange[i]]
        }
      }
      
      df <- rbind(recuento_ref_ampl,df[1,],recuento_ref_reducc,df[2,])
      df[2,] <- round(df[2,],2)
      df[4,] <- round(df[4,],2)
      
      return(df)
      
    })
    
    # 5.2) Tabla modificaciones de capital
    output$tabla_borme_capital <- renderDataTable({
      
      df <- ayuda_borme_capital()

      #Límite visualización registros tabla
      tabla <- datatable(df, options = list(pageLength = 5,
                                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                  scrollX=TRUE,
                                                  scrollCollapse=TRUE),
                         escape = FALSE)
      return(tabla)
    },options = list(scrollX = T))
  


    #==========================================================================
    # GENERACIÓN GRÁFICOS
    #==========================================================================

    # 1) Gráfico barras para estadística básica 1
    output$barras_borme_eb1 <- renderPlotly({

        df <- estadistica_basica_1()
        
        if(!is.data.frame(df)){
          return(NULL)
        }
        
        shiny::validate(
          need(ncol(df) > 0,
               "¡Atenció!\nNo hay datos suficientes como para generar el gráfico."
          )
        )
        
        if(ncol(df) > 1){df <- df[ , colnames(df)[1:(ncol(df)-1)], drop = FALSE]}
        
        
        # Definición de anchura de barra
        if(ncol(df) == 1){
          ancho <- 400
        }else{
          ancho <- 1
        }
        
        df <- as.data.frame(t(df))
        if(ncol(df) == 1){
          colnames(df) <- c("Recompte")
          texto <- paste("Forma jurídica: ", df$`Forma jurídica`,"<br>Recompte: ",df$Recompte,sep = "")
        }else{
          colnames(df) <- c("Recompte","Representació")
          texto <- paste("Forma jurídica: ", df$`Forma jurídica`,"<br>Recompte: ",df$Recompte, "<br>Representació (%): ",df$Representació,sep = "")
        }
        df$`Forma jurídica` <- rownames(df)
        
        g <- plot_ly(df, x = df$`Forma jurídica`, y = ~Recompte, type = 'bar', color = I("red"), width = ancho,
                     text = texto,
                     hoverinfo = 'text')
        g <- g %>% layout(
            title = list(text = paste('<b>Recompte per forma jurídica ', input$Municipio_principal, '</b>',sep = ''), y = -0.1),
            xaxis = list(
                title = "Forma jurídica")
        )

        g
    })

    # 2) Gráfico lineas para estadística básica 2
    output$lineas_borme_eb2 <- renderPlotly({

      df <- datos_filtrados_borme()
      
      # Manejo de error
      shiny::validate(
        need(df != 0 & df != 1 & df != 2 & nrow(df) != 0,
             "")
      )
      
      # Manejo de error
      df2 <- estadistica_basica_2()
      shiny::validate(
        need(df2 != 0 & df2 != 1 & df2 != 2 & nrow(df2) != 0,
             "")
      )
     
      
      df$Mes <- paste(year(df$Data),"/",month(df$Data),sep = "")  #Extracción de meses

      df <- recuento_estadistica_basica_2(df,1)  #Flag 1 para devolución recuento

      p <- df
      p <- p %>% plot_ly(x = ~Mes, y = ~Recompte, fill= ~`Forma Jurídica`, color = ~`Forma Jurídica`)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
      p <- p %>% layout(
        title = paste("<b>Recompte mensual ",input$Municipio_principal, " per forma jurídica</b>",sep = "")
      )

      p
    })
    
    # Función detección atipicos
    detectar_atipicios <- function(x, na.rm = TRUE) {
      cuantiles <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
      limite <- 1.5 * IQR(x, na.rm = na.rm)
      y <- x
      y[x < (cuantiles[1] - limite)] <- NA
      y[x > (cuantiles[2] + limite)] <- NA
      y
    }
    
    
    # APOYO A GRÁFICOS LÍNEA CAPITAL
    apoyo_graficos_capital <- reactive({
      
      df <- manejo_tablas_borme()
      
      shiny::validate(
        need(nrow(df) != 0 & is.data.frame(df),
             ""
        )
      )
      
      # Paso a númerico capital
      for(i in 2:7){
        df[,i] <- gsub("[a-z].","",df[,i])
        df[,i] <- gsub("[.]","",df[,i])
        df[,i] <- gsub("[.]","",df[,i])
        df[,i] <- as.numeric(gsub("[,]",".",df[,i]))
      }
      
      #Incluir desembolsado (es la alternativa a capital, a veces aparece así)
      for(i in 1:nrow(df)){
        if(is.na(df$`Ampliació capital`[i])){
          df$`Ampliació capital`[i] <- df$`Ampliació capital desemborsat`[i]
          df$`Ampliació capital subscrit`[i] <- df$`Ampliació capital resultant desemborsat`[i]
        }
      }
      
      # Cálculo evoluciones ampliaciones y reducciones de capital
      df <- df %>%
        mutate(`Evolució ampliació` = 100*((`Ampliació capital subscrit` - (`Ampliació capital subscrit` - `Ampliació capital`))/(`Ampliació capital subscrit` - `Ampliació capital`))) %>%
        mutate(`Evolució reducció` = 100*((`Reducció capital resultant subscrit` - `Reducció capital import reducció`)/(`Reducció capital resultant subscrit` - `Reducció capital import reducció`)))
      
      `Evolució ampliació` <- na.omit(df$`Evolució ampliació`)
      `Evolució reducció` <- na.omit(df$`Evolució reducció`)
      
      df$Mes <- paste(year(df$Data),"/",month(df$Data),sep = "")  #Extracción de meses
      df <- df[!is.na(df$`Evolució ampliació`) | !is.na(df$`Evolució reducció`),]
      
      # Llamada a función para filtrado de atípicos
      df <- df %>%
        group_by(`Forma Jurídica`) %>% 
        mutate(`Evolució ampliació` = detectar_atipicios(`Evolució ampliació`)) %>%
        mutate(`Evolució reducció` = detectar_atipicios(`Evolució reducció`))
      
      # Cálculo medias de evolución y reducciones de capital
      df <- df %>%
        group_by(`Forma Jurídica`,Mes) %>%
        summarise(
          `Mitjana evolució ampliació` = round(mean(`Evolució ampliació`, na.rm = TRUE),0),
          `Mitjana evolució reducció` = round(mean(`Evolució reducció`, na.rm = TRUE),0)
        )
      
      colnames(df)[3] <- "Evolució ampliació"
      colnames(df)[4] <- "Evolució reducció"
      
      return(df)
    })
    
    # 3.1) Líneas borme CAPITAL AMPLIACIONES
    output$lineas_borme_capital_ampliaciones <- renderPlotly({
      
      df <- apoyo_graficos_capital()
      
      p <- df
      p <- p %>% plot_ly(x = ~Mes, y = df$`Evolució ampliació`, fill= ~`Forma Jurídica`, color = ~`Forma Jurídica`)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
      p <- p %>% layout(
        title = paste("<b>Evolució mensual ampliacions de capital ",input$Municipio_principal, "</b>",sep = ""),
        yaxis = list(
          title = 'Evolució porcentual (%)'
        )
      )
      
      p
    })
    
    # 3.2) Líneas borme CAPITAL REDUCCIONES
    output$lineas_borme_capital_reducciones <- renderPlotly({
      
      df <- apoyo_graficos_capital()
      
      p <- df
      p <- p %>% plot_ly(x = ~Mes, y = df$`Evolució reducció`, fill= ~`Forma Jurídica`, color = ~`Forma Jurídica`)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
      p <- p %>% layout(
        title = paste("<b>Evolució mensual reducció de capital ",input$Municipio_principal, "</b>",sep = ""),
        yaxis = list(
          title = 'Evolució porcentual (%)'
        )
      )
      
      p
    })

    #==========================================================================
    # GENERACIÓN MAPAS
    #==========================================================================
    
    # Generación mapa Leaflet CAMBIO DOMICILIO SOCIAL BORME
    output$mapa_borme <- renderLeaflet({
      
      df_filtrados <- datos_filtrados_borme()
      
      shiny::validate(
        need(nrow(na.omit(df_filtrados)) != 0,
             "¡Atenció!\nNo existeixen dades disponibles per al valor dels filtres seleccionats.\nModifica el valor dels filtres si ho desitja."
        )
      )
      
      df_filtrados <- df_filtrados[df_filtrados$Latitud > 39 & df_filtrados$Latitud < 43,]
      
      if(input$variables_mapa == 1){
        variable <- "Constitució objecte social"
        pos <- 4
      }else{
        variable <- "Canvi domicili social"
        pos <- 2
      }
      shiny::validate(
        need(any(grepl(variable,colnames(na.omit(df_filtrados)))) & nrow(na.omit(df_filtrados)) != 0,
             "¡Atenció!\nNo existeixen dades disponibles per al valor dels filtres seleccionats.\nModifica el valor dels filtres si ho desitja."
        )
      )
      
      # Filtrado por selección de registro en la tabla
      filtrado_tabla <- input$tabla_borme_mapa_rows_selected
      if(length(filtrado_tabla)){
        df_filtrados <-  df_filtrados[filtrado_tabla, , drop = F]
      }
      
      #Inicialización popup
      empresas_popup <- df_filtrados$`Denominació social`[!is.na(df_filtrados$Latitud)]
      domicilio_social_popup <- df_filtrados[,pos]
      domicilio_social_popup <- domicilio_social_popup[!is.na(df_filtrados$Latitud)]
      popup <- paste("Denominació social: ", empresas_popup, "<br/>",
                     "Domicili social: ", domicilio_social_popup, sep = "") %>% lapply(htmltools::HTML)

      #Lat y long a numérico
      latitud <- as.numeric(df_filtrados$Latitud)
      longitud <- as.numeric(df_filtrados$Longitud)
      
      #Creación mapa
      leaflet() %>% addTiles() %>% addMarkers(lng = longitud,
                                              lat = latitud,
                                              popup = popup)
    })


    #==========================================================================
    # DESCARGA DE DATOS
    #==========================================================================

    # DESCARGA DATOS BORME csv
    output$descarga_borme_csv <- downloadHandler(

        filename = function() {
            paste("Datos_borme_", as.character(input$fechas_listado_borme[1]),"_", as.character(input$fechas_listado_borme[2]),".csv", sep="")
        },
        content = function(file) {
          if(input$tabs_borme == "Llistat informatiu"){
            df_tabla <- manejo_tablas_borme()
            df <- df_tabla[,1:(ncol(df_tabla)-6)] #Evita la visualización de las variables lat,long,municipio y provincia.
          }else if(input$tabs_borme == "Estadística bàsica 1"){
            df <- estadistica_basica_1()
          }else if(input$tabs_borme == "Estadística bàsica 2"){
            df <- estadistica_basica_2()
            df <- recuento_estadistica_basica_2(df,2)  #Flag 2 para devolución con cálculos estadísticos
          }else if(input$tabs_borme == "Modificacions capital"){
            df <- ayuda_borme_capital()
          }else{
            df_tabla <- manejo_tablas_borme()
            df <- df_tabla[,1:(ncol(df_tabla)-6)] #Evita la visualización de las variables lat,long,municipio y provincia.
          }
          write.csv(df, file, eol="\n", sep = ",")  # eol="\n" es para el encoding de caracteres en .csv
        }
    )

    #Descarga contratos en XLSX
    output$descarga_borme_xlsx <- downloadHandler(

        filename = paste0("Datos_borme_",as.character(input$fechas_listado_borme[1]),"_", as.character(input$fechas_listado_borme[2]),".xlsx"),
        content  = function(file) {
          
          if(input$tabs_borme == "Llistat informatiu"){
            df_tabla <- manejo_tablas_borme()
            df <- df_tabla[,1:(ncol(df_tabla)-6)] #Evita la visualización de las variables lat,long,municipio y provincia.
          }else if(input$tabs_borme == "Estadística bàsica 1"){
            df <- estadistica_basica_1()
          }else if(input$tabs_borme == "Estadística bàsica 2"){
            df <- estadistica_basica_2()
            df <- recuento_estadistica_basica_2(df,2)  #Flag 2 para devolución con cálculos estadísticos
          }else if(input$tabs_borme == "Modificacions capital"){
            df <- ayuda_borme_capital()
          }else{
            df_tabla <- manejo_tablas_borme()
            df <- df_tabla[,1:(ncol(df_tabla)-6)] #Evita la visualización de las variables lat,long,municipio y provincia.
          }
          
          wb <- createWorkbook()
          addWorksheet(wb, sheetName = "Datos")
          writeData(wb, sheet = 1, x = df)
          saveWorkbook(wb, file)
        }
    )
    
    #============================
    # TÍTULOS TABLAS
    #============================
    
    output$texto_tabla_borme_listado <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Llistat informatiu ", input$Municipio_principal, sep = ""))))
    )
    output$texto_tabla_borme_eb1 <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Recompte ", input$Municipio_principal, " i comparativa respecte ",
                                             ifelse(input$comparaciones == "Un altre municipi",input$Municipio_comparaciones, input$comparaciones)
                                             , sep = ""))))
    )
    output$texto_tabla_borme_eb2 <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Estadística descriptiva mensual ", input$Municipio_principal, sep = ""))))
    )
    output$texto_tabla_borme_capital <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Mitjana de evoluciones modificacions de capital ", input$Municipio_principal, sep = ""))))
    )
    output$texto_borme_mapa <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Informació geolocalitzada ", ifelse(input$variables_mapa == 1,"Constitucions ", "Canvis de domicili social "), input$Municipio_principal,sep = ""))))
    )
    output$texto_tabla_borme_mapa <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Llistat informatiu ", ifelse(input$variables_mapa == 1,"Constitucions ", "Canvis de domicili social "), input$Municipio_principal,sep = ""))))
    )
    
  
    
    
    
    
    
    
    
    
    
    
    #==========================================================================
    #==========================================================================
    #==========================================================================
    #==========================================================================
    #                                 ERTES
    #==========================================================================
    #==========================================================================
    #==========================================================================
    #==========================================================================
    
    #======================
    # FILTRADO
    #======================
    
    # 1) Filtros
    datos_filtrados_ertes_expedientes <- reactive({
      
      df <- df_tipo_expediente
   
      # 1) Filtro por fecha
      fecha_inicial <- as.Date(paste(substring(as.character(input$fechas_listado_ertes[1]),1,8),"01",sep = ""))
      fecha_final <- as.Date(paste(substring(as.character(input$fechas_listado_ertes[2]),1,8), "28",sep = ""))
      df <- df[df$fecha >= fecha_inicial & df$fecha <= fecha_final,]
      
      df$provincia <- municipios_ertes$Provincia[match(df$M, municipios_ertes$M)]
      
      # 2) Filtro territorio
      if(input$Municipio_principal_ertes == "Barcelona provincia"){
        #df <- df[match(m,df$M),]
        df <- df[df$provincia == "Barcelona",]
      }else{
        switch(input$Municipio_principal_ertes,
               "AMB"={
                 df <- df[df$AMB == 1,]
               },
               "AMB sense Barcelona ciutat"={
                 df <- df[df$AMB == 1 & df$Municipi != "Barcelona",]
               },
               "Barcelona ciutat"={
                 df <- df[df$Municipi == "Barcelona",]
               },
               "Catalunya"={
                 df <- df
               },
               {
                 municipio_prinicpal <- input$Municipio_principal_ertes %>%
                   gsub(", els","",.) %>%
                   gsub("els ","",.) %>%
                   gsub(", la","",.) %>%
                   gsub(", el","",.) %>%
                   gsub("el ","",.) %>%
                   gsub(", les","",.) %>%
                   gsub("les ","",.) %>%
                   gsub("la ","",.) %>%
                   gsub(", l'","",.) %>%
                   gsub("l'","",.) %>%
                   gsub(", l","",.)
                 df <- df[df$Municipi == municipio_prinicpal,]
               }
        )
      }
      
      #3) Filtro expedientes/personas
      if(input$variables_ertes == 1){  # 1 corresponde a expedientes
        df <- df[,c(3,4,6,8,11,12)]
      }else{
        df <- df[,c(3,5,7,9,11,12)]
      }
      
      df <- df[c(1:949),]
      
      #Gestión errores
      if(nrow(df) == 0){
        return(0)
      }
      
      return(df)
    })
    
    # 2) Filtro sección económica
    datos_filtrados_ertes_econom <- reactive({
      
      #1) Filtro expedientes/personas. Selección de un df u otro.
      if(input$variables_ertes == 1){  # 1 corresponde a expedientes
        df <- df_expediente_econom
      }else{
        df <- df_expediente_trabajo
      }
      
      # 2) Filtro por fecha
      fecha_inicial <- as.Date(paste(substring(as.character(input$fechas_listado_ertes[1]),1,8),"01",sep = ""))
      fecha_final <- as.Date(paste(substring(as.character(input$fechas_listado_ertes[2]),1,8), "28",sep = ""))
      df <- df[df$fecha >= fecha_inicial & df$fecha <= fecha_final,]

      df$provincia <- municipios_ertes$Provincia[match(df$M, municipios_ertes$M)]
      
      # 2) Filtro territorio
      if(input$Municipio_principal_ertes == "Barcelona provincia"){
        #df <- df[match(m,df$M),]
        df <- df[df$provincia == "Barcelona",]
      }else{
        switch(input$Municipio_principal_ertes,
               "AMB"={
                 df <- df[df$AMB == 1,]
               },
               "AMB sense Barcelona ciutat"={
                 df <- df[df$AMB == 1 & df$Municipi != "Barcelona",]
               },
               "Barcelona ciutat"={
                 df <- df[df$Municipi == "Barcelona",]
               },
               "Catalunya"={
                 df <- df
               },
               {
                 municipio_prinicpal <- input$Municipio_principal_ertes %>%
                   gsub(", els","",.) %>%
                   gsub("els ","",.) %>%
                   gsub(", la","",.) %>%
                   gsub(", el","",.) %>%
                   gsub("el ","",.) %>%
                   gsub(", les","",.) %>%
                   gsub("les ","",.) %>%
                   gsub("la ","",.) %>%
                   gsub(", l'","",.) %>%
                   gsub("l'","",.) %>%
                   gsub(", l","",.)
                 df <- df[df$M == municipio_prinicpal,]
               }
        )
      }
      
      df <- df[c(1:949),]
      
      #Gestión errores
      if(nrow(df) == 0){
        return(0)
      }
      
      return(df)
    })
    
    # 3) Filtrado para mapa
    datos_filtrados_mapa <- reactive({
      
      df <- datos_filtrados_ertes_econom() 
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "¡Atenció!\nNo existen datos disponibles para los filtros seleccionados.\nModifica los filtros si lo deseas.")
      )
      
      df <- func_recuento_econom(df,2) #Flag 0 para devolución recuento
      
      df$M <- df$Municipi %>%
        gsub(", els","",.) %>%
        gsub("els ","",.) %>%
        gsub(", la","",.) %>%
        gsub(", el","",.) %>%
        gsub("el ","",.) %>%
        gsub(", les","",.) %>%
        gsub("les ","",.) %>%
        gsub("la ","",.) %>%
        gsub(", l'","",.) %>%
        gsub("l'","",.) %>%
        gsub(", l","",.)
      
      df$latitud <- municipios_ertes$lat[match(df$M, municipios_ertes$M)]
      df$longitud <- municipios_ertes$lon[match(df$M, municipios_ertes$M)]
      
      if(any(grepl("Tots",input$variables_ertes_sec_ecc))){
        secciones <- input$variables_ertes_sec_ecc[1:(length(input$variables_ertes_sec_ecc)-1)]
      }else if(any(grepl("Cap",input$variables_ertes_sec_ecc))){
        secciones <- NULL
      }else if(any(grepl("Top 3",input$variables_ertes_sec_ecc))){
        secciones_economicas <- c("A. Agricultura, ramaderia i pesca","B. Indústries extractives", "C. Indústries manufactureres",               
                                  "D. Energia elèctrica i gas", "E. Aigua, sanejament i gestió de residus", "F.Construcció",                            
                                  "G. Comerç a l’engròs i al detall", "H. Transport i emmagatzematge", "I. Hostaleria",                             
                                  "J. Informació i comunicacions", "K. Activitats financeres i d’assegurances", "L. Activitats immobiliàries",                
                                  "M. Activitats professionals i tècniques", "N. Activitats administratives i auxiliars", "O. Adm. pública, Defensa i SS obligatòria",  
                                  "P. Educació", "Q. Activitats sanitàries i serveis socials", "R. Activitats artístiques i d’entreteniment",
                                  "S. Altres serveis", "T. Activitats de les llars", "U. Organismes extraterritorials",           
                                  "SE. Sense especificar")
        
        df2 <- df[,c("Municipi",secciones_economicas,"latitud","longitud")]
        
        Municipio <- c()
        seccion_economica <- c()
        recuento <- c()
        latitud <- c()
        longitud <- c()
        for(i in 1:nrow(df2)){
          Municipio <- c(Municipio, rep(df2$Municipi[i],(ncol(df2)-3)))  # Menos municipio, lat y long
          seccion_economica <- c(seccion_economica, colnames(df2)[2:(ncol(df2)-2)])  # Las 2 'ultimas son lat y long
          recuento <- c(recuento, as.numeric(df2[i,2:(ncol(df2)-2)]))
          latitud <- c(latitud, rep(df2$latitud[i],(ncol(df2)-3)))
          longitud <- c(longitud, rep(df2$longitud[i],(ncol(df2)-3)))
        }
        df2 <- data.frame(Municipio,seccion_economica,recuento,latitud,longitud,stringsAsFactors = FALSE)
        colnames(df2) <- c("Territori","Secció econòmica","Recompte","Latitud","Longitud")

        #Orden decreciente para visualizar los labels superpuestos en el mapa. Se ordena en base al territorio y al recuento
        a <- as.numeric(order(as.data.frame(df2[,1]),as.data.frame(df2[,3]),decreasing = TRUE))
        df2 <- df2[a,]
        
        # Cálculo df3 para el cálculo de top 3 secciones económicas
        df3 <- df2 %>%
          group_by(`Secció econòmica`) %>%
          summarise(suma = sum(Recompte))
        df3 <- df3[order(df3$suma,decreasing = TRUE),]
        
        secciones <- df3$`Secció econòmica`[1:3]
      }else{
        secciones <- input$variables_ertes_sec_ecc
      }
      
      df <- df[,c("Municipi",secciones,"latitud","longitud")]
      
      # Manejo de error: "inexistencia de datos para el intervalo de fechas seleccionado" de cara al usuario
      shiny::validate(
        need(ncol(df) > 3, "¡Atenció!\nNo existen datos disponibles para los filtros seleccionados.\nModifica los filtros si lo deseas.")
      )
      
      return(df)
    })
    
    #======================
    # FUNCIONES
    #======================
    
    # 1) Recuento tipo
    func_recuento_tipos <- function(df, flag_evol){
      df <- df
      
      df$fecha <- paste(year(df$fecha),"/",month(df$fecha),sep = "")  #Extracción de meses
      
      # Cálculo suma
      colnames(df) <- c("Territori","2","3","4","AMB","Fecha")
      
      if(flag_evol == 1){
        #Recuento por mes y forma jurídica
        df <- df %>%
          group_by(Fecha) %>%
          summarise(
            `Suma Força mayor` = sum(as.numeric(`2`),na.rm = TRUE),
            `Suma causes` = sum(as.numeric(`3`),na.rm = TRUE),
            `Suma Total` = sum(as.numeric(`4`),na.rm = TRUE)
            )
        
        df <- df[df$Fecha != "NA/NA",]
      
       # El valor de 1 es para la realización del gráfico de evolución.
        return(df)
      }
      
      fuerza_mayor <- sum(df$`2`,na.rm = TRUE)
      causas <- sum(df$`3`,na.rm = TRUE)
      total <- sum(df$`4`,na.rm = TRUE)
      Territori <- input$Municipio_principal_ertes
      df <- data.frame(Territori,fuerza_mayor,causas,total,stringsAsFactors = FALSE)

      colnames(df) <- c("Territori","Força mayor","Causes","Total")
      
      return(df)
    }
    
    # 2) Recuento sección económica
    func_recuento_econom <- function(df, flag_evol){
      df <- df
      
      df$fecha <- paste(year(df$fecha),"/",month(df$fecha),sep = "")  #Extracción de meses
      
      if(flag_evol == 1){  #Recuento por fecha
        
        df <- df[,c(4:26,29)]
        df <- df %>%
          group_by(fecha) %>%
          summarise_each(funs(sum(.,na.rm = TRUE)))
        
        df <- df[df$fecha != "NA/NA",]
        return(df)
        
      }else if(flag_evol == 2){ #Recuento por territorio
        df <- df[,c(3:26)]
        df <- df %>%
          group_by(Municipi) %>%
          summarise_each(funs(sum(.,na.rm = TRUE)))
        
        df <- df[!is.na(df$Municipi),]
        return(df)
      } 
      
      df2 <- as.data.frame(colSums(df[,4:26],na.rm = TRUE),row.names = NULL)
      df2 <- as.data.frame(t(df2),row.names = NULL)
      colnames(df2) <- colnames(df)[4:26]
      df2$Territori <- input$Municipio_principal_ertes
      df2 <- df2[,c(ncol(df2),1:(ncol(df2)-1))]

      return(df2)
    }
    
    
    
    #======================
    # TABLAS
    #======================
    
    # 1) Tabla ertes por tipo de expediente recuento
    output$tabla_ertes_expedientes_recuento <- renderDataTable({
      
      df <- datos_filtrados_ertes_expedientes() 
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "¡Atenció!\nNo existeixen dades disponibles per a l'interval de dates seleccionat.\nSelecciona un altre interval si ho desitja.")
      )
      
      df <- func_recuento_tipos(df,0) 
      
      tabla <- datatable(df, options = list(pageLength = 5,
                                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                  scrollX=TRUE,
                                                  scrollCollapse=TRUE),
                         rownames= FALSE,
                         escape = FALSE)
      return(tabla)
    })
    
    # 2) Tabla ertes media
    output$tabla_ertes_media <- renderDataTable({
      
      df <- datos_filtrados_ertes_expedientes()
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "")
      )
      
      # Cálculo suma
      colnames(df) <- c("Territori","2","3","4","AMB","Fecha")
      
      fuerza_mayor <- round(mean(as.numeric(df$`2`),na.rm = TRUE),0)
      causas <- round(mean(as.numeric(df$`3`),na.rm = TRUE),0)
      total <- round(mean(as.numeric(df$`4`),na.rm = TRUE),0)
      Territori <- input$Municipio_principal_ertes
      df <- data.frame(Territori,fuerza_mayor,causas,total,stringsAsFactors = FALSE)
      
      colnames(df) <- c("Territori","Força mayor","Causes","Total")
      
      tabla <- datatable(df, options = list(pageLength = 5,
                                            columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                            scrollX=TRUE,
                                            scrollCollapse=TRUE),
                         rownames= FALSE,
                         escape = FALSE)
      return(tabla)
    })
    
    # 3) tabla recuento sección económica
    output$tabla_ertes_secc_recuento <- renderDataTable({
      
      df <- datos_filtrados_ertes_econom() 
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "¡Atenció!\nNo existeixen dades disponibles per a l'interval de dates seleccionat.\nSelecciona un altre interval si ho desitja.")
      )
      
      df <- func_recuento_econom(df,0) 
      
      tabla <- datatable(df, options = list(pageLength = 5,
                                            columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                            scrollX=TRUE,
                                            scrollCollapse=TRUE),
                         rownames= FALSE,
                         escape = FALSE)
      return(tabla)
    })
    
    # 4) Tabla ertes sección económica MAPA
    output$tabla_ertes_mapa <- renderDataTable({
      
      df <- datos_filtrados_mapa()
      df <- df[,c(1:(ncol(df) - 2))]
      
      tabla <- datatable(df, options = list(pageLength = 5,
                                            columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                            scrollX=TRUE,
                                            scrollCollapse=TRUE),
                         rownames= FALSE,
                         escape = FALSE)
      return(tabla)
    })
  
    
    #======================
    # GRÁFICOS
    #======================
    
    # 1) Líneas recuento mes tipo expediente
    output$lineas_ertes_expedientes <- renderPlotly({
      
      df <- datos_filtrados_ertes_expedientes() 
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "")
      )
      
      df <- func_recuento_tipos(df,1) #Flag 1 para devolución recuento
      
      #Generación df para gráfico de líneas
      tipo <- c()
      fecha <- c()
      recuento <- c()
      for(i in 1:nrow(df)){
        tipo <- c(tipo,"Força mayor")
        tipo <- c(tipo,"Causes")
        tipo <- c(tipo,"Total")
        
        fecha <- c(fecha, rep(df$Fecha[i]))
        recuento <- c(recuento, as.numeric(df[i,2]))
        recuento <- c(recuento, as.numeric(df[i,3]))
        recuento <- c(recuento, as.numeric(df[i,4]))
      }
      df <- data.frame(tipo,fecha,recuento,stringsAsFactors = FALSE)
      colnames(df) <- c("Tipo","Mes","Recompte")

      p <- df
      p <- p %>% plot_ly(x = ~Mes, y = ~Recompte, fill= ~Tipo, color = ~Tipo)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
      p <- p %>% layout(
        title = list(text = paste('<b>Evolució mensual ', ifelse(input$variables_ertes == 1, "Expedients", "Treballadors"), " ", input$Municipio_principal_ertes, '</b>',sep = ''), y = -0.1)
      )
      
      p
    })
    
    
    # 2) TOP 5 secciones económicas con más expedientes o personas
    output$barras_ertes_secc <- renderPlotly({
      
      df <- datos_filtrados_ertes_econom() 
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "")
      )
      
      df <- func_recuento_econom(df,0) #Flag 0 para devolución recuento
      df <- df[,c(2:(ncol(df)-1))]
      df <- df[,order(df,decreasing = TRUE)]
      df <- df[,c(1:5)]
      
      #Generación df para gráfico de líneas
      seccion_economica <- c()
      recuento <- c()
      for(i in 1:nrow(df)){
        seccion_economica <- c(seccion_economica,colnames(df)[1:ncol(df)])
        recuento <- c(recuento, as.numeric(df[i,1:ncol(df)]))
      }
      df <- data.frame(seccion_economica,recuento,stringsAsFactors = FALSE)
      colnames(df) <- c("Secció econòmica", "Recompte")
      
      df$`Secció econòmica` <- factor(df$`Secció econòmica`, levels = c(as.character(df$`Secció econòmica`)))
      
      variable_expediente <- ifelse(input$variables_ertes == 1, "Expedients", "Treballadors")
      
      g <- plot_ly(df, x = df$`Secció econòmica`, y = ~Recompte, type = 'bar', colors = 'red',
                   text = paste("Secció econòmica: ", df$`Secció econòmica`,"<br>Recompte: ",df$Recompte,sep = ""),
                   hoverinfo = 'text')
      g <- g %>% layout(
        title = list(text = paste('<b>Top 5 seccions econòmiques ', ifelse(input$variables_ertes == 1, "Expedients", "Treballadors")," ", input$Municipio_principal_ertes, '</b>',sep = ''), y = -0.1),
        xaxis = list(
          title = "Secció econòmica")
      )
      
      g
      
    })
    
    # 3) Líneas recuento mes sección económica
    output$lineas_ertes_secc <- renderPlotly({
      
      df <- datos_filtrados_ertes_econom() 
      
      # Manejo de error
      shiny::validate(
        need(df != 0, "")
      )
      
      df <- func_recuento_econom(df,1) #Flag 1 para devolución recuento
      
      #Generación df para gráfico de líneas
      tipo <- c()
      fecha <- c()
      recuento <- c()
      for(i in 1:nrow(df)){
        tipo <- c(tipo, colnames(df)[2:ncol(df)])
        fecha <- c(fecha, rep(df$fecha[i]))
        recuento <- c(recuento, as.numeric(df[i,2:ncol(df)]))
      }
      df <- data.frame(tipo,fecha,recuento,stringsAsFactors = FALSE)
      colnames(df) <- c("Tipo","Mes","Recompte")
      
      p <- df
      p <- p %>% plot_ly(x = ~Mes, y = ~Recompte, fill= ~Tipo, color = ~Tipo)
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
      p <- p %>% layout(
        title = list(text = paste('<b>Evolució mensual ', ifelse(input$variables_ertes == 1, "Expedients", "Treballadors")," ", input$Municipio_principal_ertes, '</b>',sep = ''), y = -0.1),
        xaxis = list(
          title = "Secció econòmica")
      )
      
      p
    })
    
    
    #======================
    # MAPA
    #======================
    
    output$mapa_ertes <- renderLeaflet({

      df <- datos_filtrados_mapa()
      
      #Generación df para mapa
      Municipio <- c()
      seccion_economica <- c()
      recuento <- c()
      latitud <- c()
      longitud <- c()
      for(i in 1:nrow(df)){
        Municipio <- c(Municipio, rep(df$Municipi[i],(ncol(df)-3)))  # Menos municipio, lat y long
        seccion_economica <- c(seccion_economica, colnames(df)[2:(ncol(df)-2)])  # Las 2 'ultimas son lat y long
        recuento <- c(recuento, as.numeric(df[i,2:(ncol(df)-2)]))
        latitud <- c(latitud, rep(df$latitud[i],(ncol(df)-3)))
        longitud <- c(longitud, rep(df$longitud[i],(ncol(df)-3)))
      }
      df <- data.frame(Municipio,seccion_economica,recuento,latitud,longitud,stringsAsFactors = FALSE)
      colnames(df) <- c("Territori","Secció econòmica","Recompte","Latitud","Longitud")
      
      #Orden decreciente para visualizar los labels superpuestos en el mapa. Se ordena en base al territorio y al recuento
      a <- as.numeric(order(as.data.frame(df[,1]),as.data.frame(df[,3]),decreasing = TRUE))
      df <- df[a,]
      
      label <- paste("Territori: ", df$Territori, "<br/>",
                     "Secció econòmica: ", df$`Secció econòmica`, "<br/>",
                     "Recompte: ", df$Recompte, sep = "") %>% lapply(htmltools::HTML)
      
      
      if(max(round(df$Recompte)) < 1000){
        radios <-round(df$Recompte) * 3
      }else if(max(round(df$Recompte)) > 10000){
        radios <- rescale(round(df$Recompte), to = c(0.1, 1)) * 3000
      }else{
        radios <-round(df$Recompte)
      }
      
      pal <- colorFactor(palette = 'Set1', domain = df$gran_grup_CCO)
      
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% addCircles(data=df,lng=as.numeric(df$Longitud), lat=as.numeric(df$Latitud), weight = 2,
                                                                                label= label, radius=radios, color = ~pal(`Secció econòmica`)) %>% 
        addLegend(position = "bottomright", pal = pal, values = df$`Secció econòmica`)
    })
    
    #==========================================================================
    # DESCARGA DE DATOS
    #==========================================================================
    
    # DESCARGA DATOS BORME csv
    output$descarga_ertes_csv <- downloadHandler(
      
      filename = function() {
        paste("Datos_ERTES_", as.character(input$fechas_listado_ertes[1]),"_", as.character(input$fechas_listado_ertes[2]),".csv", sep="")
      },
      content = function(file) {
        if(input$tabs_borme == "Expedients per tipo"){
          df <- datos_filtrados_ertes_expedientes() 
          df <- func_recuento_tipos(df,0)
        }else if(input$tabs_borme == "Expedients per secció econòmica"){
          df <- datos_filtrados_ertes_econom() 
          df <- func_recuento_econom(df,0)
        }else{
          df <- datos_filtrados_mapa()
          df <- df[,c(1:(ncol(df) - 2))]
        }

        write.csv(df, file, eol="\n", sep = ",")  # eol="\n" es para el encoding de caracteres en .csv
      }
    )
    
    #Descarga contratos en XLSX
    output$descarga_ertes_xlsx <- downloadHandler(
      
      filename = paste0("Datos_ERTES_",as.character(input$fechas_listado_ertes[1]),"_", as.character(input$fechas_listado_ertes[2]),".xlsx", sep=""),
      content  = function(file) {
        if(input$tabs_borme == "Expedients per tipo"){
          df <- datos_filtrados_ertes_expedientes() 
          df <- func_recuento_tipos(df,0)
        }else if(input$tabs_borme == "Expedients per secció econòmica"){
          df <- datos_filtrados_ertes_econom() 
          df <- func_recuento_econom(df,0)
        }else{
          df <- datos_filtrados_mapa()
          df <- df[,c(1:(ncol(df) - 2))]
        }
        
        wb <- createWorkbook()
        addWorksheet(wb, sheetName = "Datos")
        writeData(wb, sheet = 1, x = df)
        saveWorkbook(wb, file)
      }
    )
    
    #============================
    # TÍTULOS TABLAS
    #============================
    
    output$texto_tabla_ertes_expedientes_recuento <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Recompte ", ifelse(input$variables_ertes == 1, "Expedients", "Treballadors"),
                                             " per tipus d'expedient ", input$Municipio_principal_ertes, sep = ""))))
    )
    output$texto_tabla_ertes_media <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Mitjana ", ifelse(input$variables_ertes == 1, "Expedients", "Treballadors"),
                                             " per tipus d'expedient ", input$Municipio_principal_ertes,sep = ""))))
    )
    output$texto_tabla_ertes_secc_recuento <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Recompte ", ifelse(input$variables_ertes == 1, "Expedients", "Treballadors"),
                                             " per secció econòmica ", input$Municipio_principal_ertes,sep = ""))))
    )
    output$texto_tabla_ertes_mapa <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Recompte ", ifelse(input$variables_ertes == 1, "Expedients", "Treballadors"),
                                             " per secció econòmica i territori ", input$Municipio_principal_ertes, sep = ""))))
    )
    
    output$texto_ertes_mapa <- renderUI(
      tags$div(id = "1",tags$h4(tags$b(paste("Recompte ", ifelse(input$variables_ertes == 1, "Expedients", "Treballadors"),
                                             " per secció econòmica i territori ", input$Municipio_principal_ertes, sep = ""))))
    )

}

# Run the application
shinyApp(ui = ui, server = server)
