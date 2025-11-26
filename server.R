# server.R (Código FINAL y ESTABILIZADO)

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2) 
library(shinyjs) 
library(readr) # Necesario para leer archivos CSV
library(readxl) # Para leer el archivo Excel
library(tibble) # Necesario para crear tibbles de emergencia

# --- 1. LECTURA DE DATOS DESDE ARCHIVOS CSV Y EXCEL ---
tryCatch({
  datos_banano_raw <- read_excel("Racimos Cosechados Semana 45-1 - 2025 - Total.xlsx") 
  user_base <- read.csv("usuarios.csv", sep = ";", stringsAsFactors = FALSE)
  
}, error = function(e) {
  warning(paste("Error al cargar o procesar archivos:", e$message))
  
  # Carga datos vacíos para que la app no colapse
  datos_banano_raw <<- data.frame(EMPRESA=character(), Lote=character(), PESO=numeric())
  user_base <<- tibble::tibble(user = "error@db.com", permissions = "SUPER_ADMIN", empresa_id = "ALL", name = "Error DB")
})


# View( datos_banano_raw)


server <- function(input, output, session) {
  
  user_email_js <- reactiveVal(NULL)
  
  # --- A. INICIALIZACIÓN DE FIREBASE Y COMPONENTES JS ---
  observe({
    api_key <- "AIzaSyC20-K42ErsY-bKKeHKBxIecJ6FaXbadXw" 
    auth_domain <- "bdspb-f17f3.firebaseapp.com" 
    
    js_init <- sprintf("
      var firebaseConfig = { 
        apiKey: '%s', 
        authDomain: '%s' 
      }; 
      var app = firebase.initializeApp(firebaseConfig);
      var auth = firebase.auth();
      
      auth.setPersistence(firebase.auth.Auth.Persistence.SESSION);

      auth.onAuthStateChanged(function(user) {
        if (user) {
          Shiny.setInputValue('firebase_user_email', user.email);
          document.getElementById('login_panel').style.display = 'none';
        } else {
          Shiny.setInputValue('firebase_user_email', null);
          document.getElementById('login_panel').style.display = 'block';
        }
      });

      $('#login_submit').on('click', function() {
        var email = $('#login_email').val();
        var password = $('#login_password').val();
        
        auth.signInWithEmailAndPassword(email, password)
          .catch(function(error) {
            if (error.code === 'auth/user-not-found') {
                auth.createUserWithEmailAndPassword(email, password)
                    .then(function() {
                        Shiny.setInputValue('login_status', 'Registro exitoso.', {priority: 'event'});
                    })
                    .catch(function(error) {
                        Shiny.setInputValue('login_status', error.message, {priority: 'event'});
                    });
            } else {
                Shiny.setInputValue('login_status', error.message, {priority: 'event'});
            }
          });
      });
      
      Shiny.addCustomMessageHandler('sign_out', function(message) {
          auth.signOut();
      });
    ", api_key, auth_domain)
    
    shinyjs::runjs(js_init)
  })
  
  # --- B. LÓGICA DE LOGIN Y LOGOUT EN R ---
  
  observeEvent(input$firebase_user_email, {
    user_email_js(input$firebase_user_email)
  })
  
  observeEvent(input$login_status, {
    showNotification(input$login_status, type = "error", duration = 5)
  })
  
  observeEvent(input$logout_btn, {
    # 1. Ejecutar el sign out de Firebase
    session$sendCustomMessage(type = 'sign_out', message = list())
    
    # 2. Ejecutar código JS para limpiar los campos de entrada
    shinyjs::runjs("$('#login_email').val(''); $('#login_password').val('');")
  })
  # --- 4. OBTENER INFORMACIÓN DEL USUARIO Y MAPEO DE ROLES ---
  user_info <- reactive({
    current_user_email <- user_email_js() 
    req(current_user_email) 
    
    user_data <- user_base %>%
      filter(user == current_user_email) %>%
      {
        if (nrow(.) == 0) {
          tibble(
            user = current_user_email,
            permissions = "OPERADOR_CADENA", 
            empresa_id = "EMP_DEFAULT",
            name = sub("@.*", "", current_user_email)
          )
        } else {
          .
        }
      }
    
    list(
      logged_in = TRUE,
      user = user_data$user[1],
      role = user_data$permissions[1],
      empresa_id = user_data$empresa_id[1],
      name = user_data$name[1]
    )
  })
  
  # --- 5. FILTRADO DE DATOS CRUCOS POR EMPRESA (Reactivo) ---
  
  datos_filtrados_crudos <- reactive({
    user <- user_info()
    req(user)
    
    req(nrow(datos_banano_raw) > 0)
    
    if (user$role == "SUPER_ADMIN") {
      return(datos_banano_raw)
    } else {
      # Filtra datos solo para la empresa del usuario
      return(datos_banano_raw %>% filter(EMPRESA == user$empresa_id))
    }
  })
  
  
  
  
  # --- 5B. Lógica de Mapeo y Preparación de Columnas (Estabilización) --- los llama los kpi que estan abajo aqui se preparan los datos 
  
  datos_dashboard <- reactive({
    data <- datos_filtrados_crudos()
    req(nrow(data) > 0)
        # *** CORRECCIÓN DE COLUMNAS Y PREPARACIÓN MÍNIMA ***
    data %>%
      # 1. Renombramos las columnas con nombres problemáticos a nombres internos sin espacios
      rename(
        LOTE_ID = Lote, 
        PESO_BRUTO = `Peso bruto`, # ¡CORREGIDO!
        CALIBRACION_SUP = `Calibracion superior`, # ¡CORREGIDO!
        SEMANA_COSECHA = `Semana de cosecha`, # ¡CORREGIDO!
        EMPRESA_ID_FILTRO = EMPRESA,
        Has = has
        
        # Asumiendo que las columnas RECUSADOS y TASA_RECHAZO existen o se definen en el Excel/raw
        # Si no existen, los KPIs de Rechazo seguirán fallando.
      ) %>%
      mutate(
        # 2. Aseguramos tipos de datos
        Ano = as.numeric(Ano),
        LOTE_ID = as.character(LOTE_ID),
        # AHORA CONVERTIMOS SEMANA_COSECHA A CHARACTER/FACTOR para el filtro
        SEMANA_COSECHA = as.character(SEMANA_COSECHA),
        # *** NUEVO: Aseguramos que 'Has' sea numérico para usarlo en cálculos ***
        Has = as.numeric(Has),
        # Aseguramos que 'Rechazado' sea un factor o carácter limpio (ej. mayúsculas)
        Rechazado = as.character(Rechazado)
        
      ) %>%
        
      
      # 3. Seleccionamos solo las columnas críticas para la agrupación y cálculo
      select(
        EMPRESA_ID_FILTRO, HACIENDA, Ano, LOTE_ID, SEMANA_COSECHA, Edad, Cinta, 
        PESO_BRUTO, CALIBRACION_SUP, # *** INCLUIMOS 'Has' EN EL DATASET DE TRABAJO ***
        Has, # Columnas estandarizadas
        
        
        
        # Mantener otras columnas necesarias para los KPIs/Tablas existentes
        `Peso raquis`, Rechazado, Recuperado, `Numero de manos`, palanca, Defecto, `Generador de merma`, EdDi, `Tipo de plantacion`, TPId, MC
        # Mantener TASA_RECHAZO y RECUSADOS (si existen en el data.frame)
      )
  })
  
  
  
  
  # --- 8A. FUNCIÓN DE DATOS FILTRADOS POR USUARIO (KPIs y Tabla) ---
  datos_para_kpi_y_tabla <- reactive({
    data <- datos_dashboard() 
    req(nrow(data) > 0)
    
    # *** APLICAR FILTROS EN CASCADA ***
    
    # Filtro por EMPRESA (Solo si el filtro existe y no es "Todas")
    if (!is.null(input$filtro_empresa) && input$filtro_empresa != "Todas") {
      data <- data %>% filter(EMPRESA_ID_FILTRO == input$filtro_empresa)
    }
    
    # Filtro por AÑO
    if (!is.null(input$filtro_ano) && input$filtro_ano != "Todos") {
      data <- data %>% filter(Ano == as.numeric(input$filtro_ano))
    }
    
    # Filtro por HACIENDA
    if (!is.null(input$filtro_hacienda) && input$filtro_hacienda != "Todas") {
      data <- data %>% filter(HACIENDA == input$filtro_hacienda)
    }
    
    # Filtro por SEMANA
    if (!is.null(input$filtro_semana) && input$filtro_semana != "Todos") {
      data <- data %>% filter(SEMANA_COSECHA == input$filtro_semana)
    }
    
    return(data)
  })
  
  
  # --- 8B. LÓGICA DEL REPORTE ADMINISTRATIVO (Agrupación y Resumen) ---
  reporte_promedios <- reactive({
    # AHORA TOMA LOS DATOS YA FILTRADOS
    data <- datos_para_kpi_y_tabla() 
    req(nrow(data) > 0)
    
    # AGRUPACIÓN: EMPRESA, HACIENDA y LOTE (sin la lógica de filtrado)
    data %>%
      group_by(
        EMPRESA_ID_FILTRO, 
        HACIENDA, 
        LOTE_ID 
      ) %>%
      summarise(
        Peso_Bruto_Promedio = mean(PESO_BRUTO, na.rm = TRUE), 
        Calibracion_Promedio = mean(CALIBRACION_SUP, na.rm = TRUE),
        Num_Manos_Promedio = mean(`Numero de manos`, na.rm = TRUE),
        # *** NUEVO CÁLCULO: Promedio de Edad ***
        Edad_Promedio = mean(Edad, na.rm = TRUE),
        
        # *** CÁLCULO SOLICITADO: Conteo de Racimos Rechazados (R.recusados) ***
        # Cuenta el número de filas donde la columna 'Rechazado' es igual a 'Si' (o 'S').
        # Nota: Asumo que el valor de rechazo es 'Si' basado en el contexto. Si es 'S', 
        # debes cambiar 'Si' por 'S'.
        R_recusados = sum(toupper(Rechazado) != 'NO', na.rm = TRUE),
    
        
        
        Total_Racimos = n(),
        
        # *** CORRECCIÓN: Hectáreas por Lote (Valor fijo) ***
        Hectareas = first(Has), # Usamos first() para obtener el valor único/fijo
        
        
        # *** NUEVO CÁLCULO: Racimos Procesados ***
        `R_procesados` = Total_Racimos - `R_recusados`,
        
        .groups = 'drop'
      ) %>%
      arrange(as.numeric(LOTE_ID)) %>%
    
    # *** NUEVO PASO: OCULTAR EMPRESA y HACIENDA ***
      # SELECCIÓN Y ORDEN DE COLUMNAS PARA LA TABLA
      select(
        LOTE_ID, 
        Hectareas,
        Peso_Bruto_Promedio, 
        Calibracion_Promedio, 
        Num_Manos_Promedio, 
        # *** SELECCIÓN DE LA NUEVA COLUMNA ***
        Edad_Promedio,
        Total_Racimos,
        
        # *** INCLUIMOS EL CONTEO CON EL NOMBRE SOLICITADO ***
        R_recusados,
        R_procesados
      )
    
    
    
  })
  
  
  
  
  
  
  # --- 8C. LÓGICA DEL REPORTE ADMINISTRATIVO (Agrupación por SEMANA) --- para el dashboard de semana 
  reporte_promedios_semana <- reactive({
    # AHORA TOMA LOS DATOS YA FILTRADOS
    data <- datos_para_kpi_y_tabla()
    req(nrow(data) > 0)
    
    # AGRUPACIÓN: EMPRESA, HACIENDA y SEMANA DE COSECHA
    data %>%
      group_by(
        EMPRESA_ID_FILTRO,
        HACIENDA,
        SEMANA_COSECHA # <<< CAMBIO CLAVE: Agrupación por Semana
      ) %>%
      summarise(
        Peso_Bruto_Promedio = mean(PESO_BRUTO, na.rm = TRUE),
        Calibracion_Promedio = mean(CALIBRACION_SUP, na.rm = TRUE),
        Num_Manos_Promedio = mean(`Numero de manos`, na.rm = TRUE),
        Edad_Promedio = mean(Edad, na.rm = TRUE),
        
        R_recusados = sum(toupper(Rechazado) != 'NO', na.rm = TRUE),
        Total_Racimos = n(),
        
        # *** ELIMINAMOS LA COLUMNA Hectareas de la sumatoria ***
        
        `R_procesados` = Total_Racimos - `R_recusados`,
        TASA_RECHAZO = (R_recusados / Total_Racimos) * 100,
        
        .groups = 'drop'
      ) %>%
      # Ordenamos por SEMANA_COSECHA
      arrange(SEMANA_COSECHA) %>%
      
      # SELECCIÓN Y ORDEN DE COLUMNAS PARA LA TABLA
      select(
        SEMANA_COSECHA, # <<< CAMBIO CLAVE: Columna de Semana
        # Hectareas, # Opcional: la quitamos por ser irrelevante en este resumen
        Peso_Bruto_Promedio,
        Calibracion_Promedio,
        Num_Manos_Promedio,
        Edad_Promedio,
        Total_Racimos,
        R_recusados,
        R_procesados,
        TASA_RECHAZO
      )
  })
  
  
  
  
  
  
  
  
  
  
  
  # --- 6. RENDERIZACIÓN DEL DASHBOARD Y MENÚ CONDICIONAL ---
  output$sidebar_menu <- renderUI({
    user <- user_info()
    
    logout_button <- actionButton("logout_btn", "Salir", icon = icon("sign-out-alt"), 
                                  style = "color: white; background-color: #d9534f; border-color: #d9534f;")
    
    dashboardPage(
      skin = "green",
      dashboardHeader(
        title = HTML("🍌 **SISBANLAM** | BI Bananero"),
        titleWidth = 300,
        tags$li(class = "dropdown", logout_button) 
      ),
      dashboardSidebar(
        width = 300,
        sidebarMenu(
          id = "tabs",
          # *** PESTAÑA 1 RENOMBRADA Y TABNAME CORREGIDO ***
          menuItem("📊 Reporte Administrativo por Lotes", tabName = "tab_reporte_admin", icon = icon("chart-bar")),
          # *** NUEVA PESTAÑA 2: REPORTE POR SEMANA ***
          menuItem("📅 Reporte Administrativo por Semana", tabName = "tab_reporte_admin_semana", icon = icon("calendar-alt")),
          menuItem("❌ Tasa de Rechazo", tabName = "tab_rechazo", icon = icon("times")),
          menuItem("🔬 Optimización por Edad", tabName = "tab_edad", icon = icon("leaf")),
          
          if (user$role %in% c("SUPER_ADMIN", "ADMIN_EMPRESA")) {
            menuItem("⚙️ Gestión Multi-Empresa", tabName = "tab_admin", icon = icon("user-shield"))
          },
          menuItem("💡 Acerca del Sistema", tabName = "tab_info", icon = icon("info-circle"))
        )
      ),
      dashboardBody(
        h6(paste("Bienvenido,", user$name, " (Rol:", user$role, ")"), icon("hand-peace")),
        
        # ******************************************************
        # *** SOLUCIÓN: FILTROS GLOBALES (Fuera de tabItems) ***
        # ******************************************************
        fluidRow(
          box(title = "Filtros de Exploración", status = "warning", solidHeader = TRUE, width = 12,
              column(width = 12,
                     column(width = 3, uiOutput("ui_filtro_empresa")),
                     column(width = 3, uiOutput("ui_filtro_ano")),
                     column(width = 3, uiOutput("ui_filtro_hacienda")),
                     column(width = 3, uiOutput("ui_filtro_semana"))
              )
          )
        ),
        
        tabItems(

          
                    # 1. Pestaña de Reporte Administrativo (Antes tab_rendimiento)
          # 1. Pestaña de Reporte Administrativo
          tabItem(tabName = "tab_reporte_admin",
                  h2("Reporte Administrativo: Parámetros de Producción por Lotes"),
                  
                  # *** KPIS (Reutilizan los filtros globales)
                  fluidRow(
                    # KPI's con ancho 6 para poner Peso y Calibración lado a lado
                    valueBoxOutput("kpi_peso_promedio", width = 3),
                    valueBoxOutput("kpi_calib_promedio", width = 3),
                    valueBoxOutput("kpi_edad_promedio", width = 3)
                    ),
                  # TABLA DE DATOS POR LOTES
                  fluidRow(
                    box(title = "Promedios de Producción por Lotes", status = "primary", solidHeader = TRUE, width = 12,
                        DT::dataTableOutput("table_promedios"))
                    )             
          ),
          
          
          
          # *** 2. NUEVA Pestaña de Reporte Administrativo POR SEMANA ***
          tabItem(tabName = "tab_reporte_admin_semana",
                  h2("Reporte Administrativo: Parámetros de Producción por Semana"),
                  
                
                  # KPIS para SEMANA (NUEVOS IDs para evitar el conflicto)
                  fluidRow(
                    valueBoxOutput("kpi_peso_promedio_semana", width = 3),
                    valueBoxOutput("kpi_calib_promedio_semana", width = 3),
                    valueBoxOutput("kpi_edad_promedio_semana", width = 3)
                  ),      
                  
                  # *** TABLA DE DATOS POR SEMANA ***
                  fluidRow(
                    box(title = "Promedios de Producción por Semana", status = "primary", solidHeader = TRUE, width = 12,
                        DT::dataTableOutput("table_promedios_semana")) # <<< NUEVA SALIDA DE TABLA
                  )
                  
          ),
          
          
                     # 2. Pestaña de Tasa de Rechazo (El tabItem original)
          tabItem(tabName = "tab_rechazo",
                  h2("Análisis de Pérdidas y Recusados"),
                  fluidRow(
                    box(title = "Tasa de Rechazo por Lote (%)", status = "danger", solidHeader = TRUE, width = 12,
                        plotOutput("plot_tasa_rechazo")),
                    box(title = "Detalle de Recusados", status = "warning", width = 12,
                        DT::dataTableOutput("table_rechazo"))
                  )
          ),
          
          # 3. Pestaña de Optimización por Edad 
          tabItem(tabName = "tab_edad",
                  h2("Optimización de Cosecha (Innovación)"),
                  h4("Aquí se implementará el análisis de dispersión para encontrar la edad de corte ideal.")
          ),
          
          # 4. Pestaña de Gestión Multi-Empresa (Solo visible según el rol)
          tabItem(tabName = "tab_admin",
                  h2("⚙️ Gestión de Usuarios y Empresas"),
                  p("Esta sección es visible solo para Administradores de Empresa y Super Administradores."),
                  tags$ul(
                    tags$li("Permitirá la creación y asignación de Subusuarios."),
                    tags$li("Actualmente, los roles se leen desde el archivo **usuarios.csv**.")
                  )
          ),
          
          # 5. Acerca del Sistema
          tabItem(tabName = "tab_info",
                  h2("Acerca de SISBANLAM"),
                  p("Sistema de Inteligencia de Negocios para la Producción Bananera.")
          )
        )
      )
      )
  })
  
  
  
  
  
  
  # --- 9. RENDERIZACIÓN DE LA TABLA DEL REPORTE ADMINISTRATIVO ---
output$table_promedios <- DT::renderDataTable({
  data <- reporte_promedios()
  req(nrow(data) > 0)
  
  DT::datatable(
    data, 
    
    # *** CAMBIO APLICADO: Añadir la clase 'cell-border' ***
    class = 'cell-border stripe',
    
    # *** CAMBIO 1: Habilitar la extensión de Botones ***
    extensions = 'Buttons',
    
    options = list(pageLength = 10, scrollX = TRUE, 
                   lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'Todas las filas')),
    
                   # *** CAMBIO 2: Definir la estructura (dom) e incluir los botones (B) ***
                   dom = 'lBfrtip', # 'B' incluye los botones, 'f' el filtro, 'r' loading, 't' tabla, 'i' info, 'p' paginación
                   buttons = list(
                     'copy', # Opción de copiar (Copy)
                     list(extend = 'csv', filename = 'Reporte_Administrativo'),
                     list(extend = 'excel', filename = 'Reporte_Administrativo', title = 'Reporte Administrativo por Lotes'),
                     
                     list(
                       extend = 'pdf', 
                       title = 'Reporte Administrativo por Lotes',
                       # 1. Orientación horizontal
                       orientation = 'landscape', 
                       # 2. Configuración de página (Ajuste de tamaño)
                       pageSize = 'A4'
                     )
           
         )
    ),
    
    rownames = FALSE
  ) %>%
    # --- 1. FORMATOS DE REDONDEO ---
    formatRound(columns = 'Peso_Bruto_Promedio', digits = 2) %>%
    formatRound(columns = 'Calibracion_Promedio', digits = 1) %>%
    formatRound(columns = 'Num_Manos_Promedio', digits = 1) %>%
    formatRound(columns = 'Edad_Promedio', digits =1) %>%
    formatRound(columns = 'Hectareas', digits = 2) %>% # Formato para Hectáreas
    
    # *** NUEVO FORMATO: R.recusados (Conteo) ***
    formatRound(columns =  'R_recusados' , digits = 0) %>%
    
    # *** NUEVO FORMATO: R_procesados (Conteo entero) ***
    formatRound(columns = 'R_procesados', digits = 0) %>%
    
    
    # --- 2. SEMAFORIZACIÓN De los paramtetros de produccion
    
    # --- 2. SEMAFORIZACIÓN DEL PESO BRUTO (Peso_Bruto_Promedio) ---
    formatStyle(
      'Peso_Bruto_Promedio',
      backgroundColor = styleInterval(
        c(45, 60), # Puntos de corte: <40 es 1, 40-50 es 2, >50 es 3
        c('red', 'yellow', 'lightgreen') # Colores: Rojo (bajo), Amarillo (advertencia), Verde (óptimo)
      )
    ) %>%
    
    # --- 3. SEMAFORIZACIÓN DE CALIBRACIÓN (Calibracion_Promedio) ---
    # Rango óptimo (Verde): 43.0 a 44.0
    # Rango de Advertencia (Amarillo): 42.0 a <43.0 Y >44.0 a 45.0
    # Rango Crítico (Rojo): <42.0 O >45.0
    
    formatStyle(
      'Calibracion_Promedio',
      # Definimos los 5 rangos con 4 puntos de corte: 42, 43, 44, 45
      backgroundColor = styleInterval(
        c(42, 43.5, 45, 45.5),
        c('red',    # < 42.0 (Crítico)
          'yellow',  # 42.0 a <43.0 (Advertencia)
          'lightgreen',# 43.5 a <45 (Óptimo)
          'yellow',  # 44.0 a <45.0 (Advertencia)
          'red')      # > 45.0 (Crítico)
      )
    ) %>%
    
 
    
    # --- 4. SEMAFORIZACIÓN DE NÚMERO DE MANOS (Num_Manos_Promedio) ---
    
 
    formatStyle(
      'Num_Manos_Promedio',
      backgroundColor = styleInterval(
        c(7, 8), # Puntos de corte: <7, 7-8, >8
        c('red', 'yellow', 'lightgreen')
      )
    ) %>%
    
    # --- 5. SEMAFORIZACIÓN DE EDAD (Lógica simplificada y corregida) ---
    # Rango óptimo 11-13 semanas. Todo lo que esté fuera es amarillo o rojo.
    # Lógica: <10 (Rojo), 10-14 (Amarillo/Verde), >14 (Rojo)
    formatStyle(
      'Edad_Promedio',
      # 1. Rojo para los extremos: <10 o >14
      backgroundColor = styleInterval(
        c(10, 14), 
        c('red', 'yellow', 'red')
      )
    ) %>%
    formatStyle(
      'Edad_Promedio',
      # 2. Sobrescribir con Verde solo el rango óptimo (11, 12, 13)
      backgroundColor = styleEqual(c(11, 12, 13), rep('lightgreen', 3))
    )
  
})
  



  

# --- 10. RENDERIZACIÓN DE LA TABLA DEL REPORTE ADMINISTRATIVO POR SEMANA ---
output$table_promedios_semana <- DT::renderDataTable({
  # *** CAMBIO CLAVE: Usamos la nueva función de resumen por semana ***
  data <- reporte_promedios_semana()
  req(nrow(data) > 0)
  
  DT::datatable(
    data,
    class = 'cell-border stripe',
    extensions = 'Buttons',
    
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'Todas las filas')),
      dom = 'lBfrtip',
      buttons = list(
        'copy', 
        list(extend = 'csv', filename = 'Reporte_Administrativo_Semana'),
        list(extend = 'excel', filename = 'Reporte_Administrativo_Semana', title = 'Reporte Administrativo por Semana'),
        list(extend = 'pdf', title = 'Reporte Administrativo por Semana')
      )
    ),
    
    rownames = FALSE
  ) %>%
    # --- FORMATOS DE REDONDEO ---
    formatRound(columns = 'Peso_Bruto_Promedio', digits = 2) %>%
    formatRound(columns = 'Calibracion_Promedio', digits = 1) %>%
    formatRound(columns = 'Num_Manos_Promedio', digits = 1) %>%
    formatRound(columns = 'Edad_Promedio', digits = 1) %>%
    formatRound(columns = 'TASA_RECHAZO', digits = 2) %>%
    formatRound(columns = 'R_recusados' , digits = 0) %>%
    formatRound(columns = 'R_procesados', digits = 0) %>%
    
    # *** SEMAFORIZACIÓN (Se aplica la misma lógica de Lotes) ***
    formatStyle(
      'Peso_Bruto_Promedio',
      backgroundColor = styleInterval(
        c(45, 60),
        c('red', 'yellow', 'lightgreen') 
      )
    ) %>%
    formatStyle(
      'Calibracion_Promedio',
      backgroundColor = styleInterval(
        c(42, 43.5, 45, 45.5),
        c('red', 'yellow', 'lightgreen', 'yellow', 'red')
      )
    ) %>%
    formatStyle(
      'Num_Manos_Promedio',
      backgroundColor = styleInterval(
        c(7, 8),
        c('red', 'yellow', 'lightgreen')
      )
    ) %>%
    formatStyle(
      'Edad_Promedio',
      backgroundColor = styleInterval(
        c(10, 14),
        c('red', 'yellow', 'red')
      )
    ) %>%
    formatStyle(
      'Edad_Promedio',
      backgroundColor = styleEqual(c(11, 12, 13), rep('lightgreen', 3))
    )
})






  
  
  # --- LÓGICA PARA GENERAR EL FILTRO DE SEMANAS (Añadir a Server.R) ---
  output$ui_filtro_semana <- renderUI({
    data <- datos_dashboard()
    req(nrow(data) > 0)
    
    # Obtener todas las semanas únicas y ordenarlas
    semanas <- unique(data$SEMANA_COSECHA)
    semanas <- sort(semanas)
    
    # Opciones, incluyendo 'Todos'
    opciones <- c("Todos", semanas)
    
    # Generar el control de selección
    selectInput(
      "filtro_semana", 
      "Filtrar por Semana:", 
      choices = opciones,
      selected = "Todos"
    )
  })
  
  
  
  
  
  
  
  
  # --- LÓGICA PARA GENERAR EL FILTRO DE HACIENDAS ---
  output$ui_filtro_hacienda <- renderUI({
    data <- datos_dashboard()
    req(nrow(data) > 0)
    
    opciones <- sort(unique(data$HACIENDA))
    opciones <- c("Todas", opciones)
    
    selectInput(
      "filtro_hacienda", 
      "Filtrar por Hacienda:", 
      choices = opciones,
      selected = "SAN HUMBERTO"
    )
  })
  
  # --- LÓGICA PARA GENERAR EL FILTRO DE AÑOS ---
  output$ui_filtro_ano <- renderUI({
    data <- datos_dashboard()
    req(nrow(data) > 0)
    
    # Aseguramos que 'Ano' sea una lista de caracteres para el filtro
    opciones <- sort(as.character(unique(data$Ano)), decreasing = TRUE)
    opciones <- c("Todos", opciones)
    
    selectInput(
      "filtro_ano", 
      "Filtrar por Año:", 
      choices = opciones,
      selected = "Todos"
    )
  })
  
  # --- LÓGICA PARA GENERAR EL FILTRO DE EMPRESAS (Solo si el usuario es SUPER_ADMIN) ---
  output$ui_filtro_empresa <- renderUI({
    user <- user_info()
    
    # Este filtro solo se debe mostrar si el usuario tiene acceso a múltiples empresas
    if (user$role != "SUPER_ADMIN") {
      return(NULL) # Si no es Super Admin, no mostramos este filtro.
    }
    
    data <- datos_dashboard()
    req(nrow(data) > 0)
    
    opciones <- sort(unique(data$EMPRESA_ID_FILTRO))
    opciones <- c("Todas", opciones)
    
    selectInput(
      "filtro_empresa", 
      "Filtrar por Empresa:", 
      choices = opciones,
      selected = "Todas"
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  # --- 7. LÓGICA DE KPIS Y GRÁFICOS (CORREGIDA CON NUEVOS NOMBRES) ---
  
  # KPI 1: Peso Promedio
  output$kpi_peso_promedio <- renderValueBox({
    # *** CAMBIO AQUÍ: Usamos la función con filtros ***
    data <- datos_para_kpi_y_tabla() 
    req(nrow(data) > 0)
    promedio <- mean(data$PESO_BRUTO, na.rm = TRUE) 
    valueBox(value = paste(round(promedio, 1), "Lb"), subtitle = "Peso Promedio Global", icon = icon("balance-scale"), color = "navy") 
  })
  
  
  # KPI 2: Calibración Promedio
  output$kpi_calib_promedio <- renderValueBox({
    # *** CAMBIO AQUÍ: Usamos la función con filtros ***
    data <- datos_para_kpi_y_tabla() 
    req(nrow(data) > 0)
    promedio <- mean(data$CALIBRACION_SUP, na.rm = TRUE) 
    valueBox(value = paste(round(promedio, 1), ""), subtitle = "Calibración Promedio", icon = icon("expand"), color = "green")
  })
  
  # # KPI 3: Tasa de Rechazo
  #output$kpi_tasa_rechazo <- renderValueBox({
    # *** CAMBIO AQUÍ: Usamos la función con filtros ***
  #  data <- datos_para_kpi_y_tabla() 
   # req(nrow(data) > 0)
    # Asumiendo que TASA_RECHAZO existe y es una proporción (Ej. 0.05 = 5%)
    # Si la columna TASA_RECHAZO contiene valores absolutos de tasa (Ej. 5, 10, 2), usamos `mean`
  #  tasa <- mean(data$TASA_RECHAZO, na.rm = TRUE)
   # valueBox(value = paste(round(tasa, 1), "%"), subtitle = "Tasa de Rechazo Promedio", icon = icon("fire"), color = "red") 
  #})
  
  # --- KPI 4: Edad Promedio ---
  output$kpi_edad_promedio <- renderValueBox({
    # Usa la función con filtros para que el KPI reaccione a la selección del usuario
    data <- datos_para_kpi_y_tabla() 
    req(nrow(data) > 0)
        # Calculamos la edad promedio
    promedio <- mean(data$Edad, na.rm = TRUE) 
        valueBox(
      value = paste(round(promedio, 1), ""), 
      subtitle = "Edad Promedio de Cosecha", 
      icon = icon("leaf"), 
      color = "maroon" # Un color distintivo
    ) 
  })
  
  ##########################################################################################################
  
  # --- 7.3. KPIS para el Reporte Administrativo POR SEMANA ---
  
  output$kpi_peso_promedio_semana <- renderValueBox({
    df_filtrado <- datos_para_kpi_y_tabla() # Reutiliza los datos ya filtrados
    peso_promedio <- mean(df_filtrado$PESO_BRUTO, na.rm = TRUE)
    valueBox(
      value =  paste(round(peso_promedio, 2), "Lb"), "Peso Promedio (Semana)", 
      icon = icon("weight-hanging"), color = "green"
    )
  })
  
  output$kpi_calib_promedio_semana <- renderValueBox({
    df_filtrado <- datos_para_kpi_y_tabla()
    calibre_promedio <- mean(df_filtrado$CALIBRACION_SUP, na.rm = TRUE)
    valueBox(
      value =  paste(round(calibre_promedio, 2), ""), "Calibre Promedio (Semana)", 
      icon = icon("ruler-horizontal"), color = "yellow"
    )
  })
  
  output$kpi_edad_promedio_semana <- renderValueBox({
    df_filtrado <- datos_para_kpi_y_tabla()
    edad_promedio <- mean(df_filtrado$Edad, na.rm = TRUE)
    valueBox(
      value =  paste(round(edad_promedio, 2), ""), "Edad Promedio (Semana)", 
      icon = icon("calendar-day"), color = "blue"
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  #################################################
  
  # Gráfico: Peso Promedio por Lote
  output$plot_peso_lote <- renderPlot({
    data <- datos_dashboard()
    req(nrow(data) > 0)
    # ¡CORREGIDO! Usando LOTE_ID y PESO_BRUTO
    data %>% 
      group_by(LOTE_ID) %>% summarise(Peso_Prom = mean(PESO_BRUTO)) %>% 
      ggplot(aes(x = LOTE_ID, y = Peso_Prom)) + 
      geom_bar(stat = "identity", fill = "#2E8B57") + 
      labs(y = "Peso Promedio (Kg)", x = "Lote") + 
      theme_minimal(base_size = 14) 
  })
  
  # Gráfico: Calibración Promedio por Lote
  output$plot_calib_lote <- renderPlot({
    data <- datos_dashboard()
    req(nrow(data) > 0)
    # ¡CORREGIDO! Usando LOTE_ID y CALIBRACION_SUP
    data %>% group_by(LOTE_ID) %>% summarise(Calib_Prom = mean(CALIBRACION_SUP)) %>% 
      ggplot(aes(x = LOTE_ID, y = Calib_Prom)) + 
      geom_bar(stat = "identity", fill = "#3c8dbc") + 
      labs(y = "Calibracion Promedio (mm)", x = "Lote") + 
      theme_minimal(base_size = 14)
  })
  
  # Gráfico: Tasa de Rechazo por Lote
 # output$plot_tasa_rechazo <- renderPlot({
 #   data <- datos_dashboard()
 #   req(nrow(data) > 0)
    # ¡CORREGIDO! Usando LOTE_ID
  #  data %>% ggplot(aes(x = LOTE_ID, y = TASA_RECHAZO)) + 
    #  geom_point(size = 3, color = "#d9534f") + 
    #  geom_segment(aes(x = LOTE_ID, xend = LOTE_ID, y = 0, yend = TASA_RECHAZO), color = "#d9534f") + 
    #  labs(y = "Tasa de Rechazo (%)", x = "Lote") + 
    #  theme_minimal(base_size = 14)
  #})
  
  # Tabla: Detalle de Recusados
  output$table_rechazo <- DT::renderDataTable({
    data <- datos_dashboard()
    req(nrow(data) > 0)
    # ¡CORREGIDO! Usando LOTE_ID, PESO_BRUTO, CALIBRACION_SUP
    data %>% select(LOTE_ID, Edad, PESO_BRUTO, CALIBRACION_SUP) %>% 
      DT::datatable(options = list(pageLength = 5), rownames = FALSE)
  })
}