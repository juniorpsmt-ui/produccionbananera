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
library(data.table)
library(lubridate)

# Al inicio de tu archivo
if (!require("data.table")) install.packages("data.table")
library(data.table)
# --- 1. LECTURA DE DATOS DESDE ARCHIVOS CSV Y EXCEL ---
tryCatch({
  # datos_banano_raw <- read_excel("Racimos Cosechados Semana 45-1 - 2025 - TotalCSV.csv") 
  # ANTES: datos_banano_raw <- read_csv2("Racimos Cosechados Semana 45-1 - 2025 - TotalCSV.csv")
  
  # ✅ CORRECCIÓN DE MEMORIA: Usar fread para lectura rápida y eficiente
  datos_banano_raw <- data.table::fread("Racimos Cosechados Semana 45-1 - 2025 - TotalCSV.csv", 
                                        sep = ";", 
                                        encoding = "UTF-8")
  
  # Convertir a tibble/data.frame si la lógica subsiguiente de dplyr lo requiere
  datos_banano_raw <- as_tibble(datos_banano_raw) 
  
  # print(sapply(datos_banano_raw, class))
  
  user_base <- read.csv("usuarios2.csv", sep = ";", stringsAsFactors = FALSE)
  
}, error = function(e) {
  warning(paste("Error al cargar o procesar archivos:", e$message))
  
  # Carga datos vacíos para que la app no colapse
  datos_banano_raw <<- data.frame(EMPRESA=character(), Lote=character(), PESO=numeric())
  user_base <<- tibble::tibble(user = "error@db.com", permissions = "SUPER_ADMIN", empresa_id = "ALL", name = "Error DB")
})


#View( datos_banano_raw)




server <- function(input, output, session) {
  
  
  
  
  user_email_js <- reactiveVal(NULL)
  
  
  
  # --- 3. INICIALIZACIÓN DE FIREBASE Y COMPONENTES JS (server.R) ---
  observe({
    # 🚨 Asegúrate de que estos datos sean correctos 🚨
    api_key <- "AIzaSyC20-K42ErsY-bKKeHKBxIecJ6FaXbadXw"  
    auth_domain <- "bdspb-f17f3.firebaseapp.com"  
    project_id <- "bdspb-f17f3" 
    
    js_init <- sprintf("

    // Variable global para acceder al objeto de autenticación
    var globalFirebaseAuth = null; 

    // Las funciones addCustomMessageHandler deben estar fuera de $(document).ready
    // para que R las reconozca inmediatamente.
    
    // 1. Manejador de Login (Usa la variable global y tiene chequeo fatal)
    Shiny.addCustomMessageHandler('do_login_js', function(data) {
        
        document.getElementById('login_message').innerHTML = '';
        
        // Chequeo de que el objeto AUTH esté cargado
        if (!globalFirebaseAuth) {
             var msg = 'ERROR FATAL: El servicio de autenticación no se cargó.';
             document.getElementById('login_message').innerHTML = msg;
             alert(msg); // Forzar la visibilidad del error
             return;
        }


 globalFirebaseAuth.signInWithEmailAndPassword(data.email, data.password)
            .catch(function(error) {
                var message = error.message;
                
                if (error.code === 'auth/user-not-found') {
                    // Lógica de registro
                    globalFirebaseAuth.createUserWithEmailAndPassword(data.email, data.password) 
                        .then(function() {
                            message = 'Registro exitoso. Intente iniciar sesión.';
                        })
                        .catch(function(error) {
                            message = 'Error al registrar: ' + error.message;
                        });
                }
                
                document.getElementById('login_message').innerHTML = message;
                Shiny.setInputValue('login_status', message, {priority: 'event'}); 
            });
    });

   
   
    // 2. Manejador de Guardado de Enfunde (Con chequeo de Firestore y try/catch agresivo)
    //Shiny.addCustomMessageHandler('save_enfunde_data', function(data) {
        // Usamos el try/catch para capturar cualquier error de ejecución inmediato (ej. Firestore no cargado).
    //    try {
      //      firebase.firestore().collection('enfunde_registros').add(data)
        //        .then(function(docRef) {
          //          Shiny.setInputValue('firebase_save_status', 'Registro de Enfunde exitoso. ID: ' + docRef.id, {priority: 'event'});
            //    })
              //  .catch(function(error) {
                    // Fallo de Firestore/Reglas de Seguridad
                //    console.error('Error de Firestore:', error.message);
                  //  alert('ERROR (Consola F12): ' + error.message); // Muestra el error de las reglas
                    //Shiny.setInputValue('firebase_save_status', 'Error al guardar en Firestore: ' + error.message, {priority: 'event'});
              //  });
      //  } catch (e) {
            // Fallo de inicialización
          //  alert('ERROR FATAL EN JS: ' + e.message);
          //  Shiny.setInputValue('firebase_save_status', 'ERROR INTERNO: Fallo en la llamada a Firestore (' + e.message + ')', {priority: 'event'});
    //   }
 //   });
    
    
    // otro manejador para guradr con id unicos
    
    // 2. Manejador de Guardado/Edición (Usando ID ÚNICO para evitar duplicados)
    Shiny.addCustomMessageHandler('save_enfunde_data', function(data) {
        try {
            // Creamos un ID único combinando Hacienda, Semana y Lote
          
            
            var idUnico = data.HACIENDA + '_' + data.SEMANA + '_' + data.LOTE;
            
            // Usamos .doc(idUnico).set(data) para que SOBREESCRIBA si ya existe
            firebase.firestore().collection('enfunde_registros').doc(idUnico).set(data)
                .then(function() {
                    Shiny.setInputValue('firebase_save_status', 'Guardado exitoso: ' + idUnico, {priority: 'event'});
                })
                .catch(function(error) {
                    console.error('Error de Firestore:', error.message);
                    alert('ERROR: ' + error.message);
                });
        } catch (e) {
            alert('ERROR FATAL EN JS: ' + e.message);
        }
    });
    
    
    
    
    

    // 3. Manejador de Sign Out (Corregido para usar variable global)
    Shiny.addCustomMessageHandler('sign_out', function(message) { 
        if (globalFirebaseAuth) {
            globalFirebaseAuth.signOut();
        } else {
            // Forzar la interfaz al login si el objeto auth falla
            document.getElementById('login_panel').style.display = 'block';
            alert('ERROR: Fallo al cerrar sesión. Limpie el caché del navegador si persiste.');
        }
    });



    // 🚨 BLOQUE CRÍTICO: Inicialización envuelta en $(document).ready
    $(document).ready(function() {
        var firebaseConfig = { 
          apiKey: '%s', 
          authDomain: '%s',
          projectId: '%s' // Campo CRÍTICO
        }; 
        
        var app = firebase.initializeApp(firebaseConfig);
        
        // 🚨 ASIGNAR LA INSTANCIA DE AUTH A LA VARIABLE GLOBAL
        globalFirebaseAuth = app.auth(); 
        
        // Desactivar Persistencia (NONE) para forzar login en cada inicio
        globalFirebaseAuth.setPersistence(firebase.auth.Auth.Persistence.NONE); 
        
        // Manejo de Estado (onAuthStateChanged)
        globalFirebaseAuth.onAuthStateChanged(function(user) {
          if (user) {
            Shiny.setInputValue('firebase_user_email', user.email, {priority: 'event'});
            document.getElementById('login_panel').style.display = 'none';
          } else {
            Shiny.setInputValue('firebase_user_email', null, {priority: 'event'});
            document.getElementById('login_panel').style.display = 'block';
          }
        });
    });


// 4. Manejador de Consulta Semanal con ALERTA de depuración
    Shiny.addCustomMessageHandler('consultar_enfunde_semanal', function(params) {
        console.log('Iniciando consulta para Semana:', params.semana, 'Hacienda:', params.hacienda);
        
        firebase.firestore().collection('enfunde_registros')
            .where('SEMANA', '==', parseInt(params.semana))
            .where('HACIENDA', '==', params.hacienda)
            .get()
            .then(function(querySnapshot) {
                var resultados = [];
                querySnapshot.forEach(function(doc) {
                    resultados.push(doc.data());
                });
                
                // --- ALERTA DE PRUEBA ---
               // alert('Firestore respondió: Se encontraron ' + resultados.length + ' registros para la semana ' + params.semana);
                
                Shiny.setInputValue('datos_desde_firestore', JSON.stringify(resultados));
            })
            .catch(function(error) {
                alert('Error en Firestore: ' + error.message);
                console.error('Error en Firestore consulta: ', error);
            });
    });

  ", api_key, auth_domain, project_id)
    
    shinyjs::runjs(js_init)
  })
  
  # --- B. LÓGICA DE LOGIN Y LOGOUT EN R ---
  
  observeEvent(input$firebase_user_email, {
    user_email_js(input$firebase_user_email)
  })
  
  ##############################

  ##############################
  
  
  observeEvent(input$login_status, {
    showNotification(input$login_status, type = "error", duration = 5)
  })
  
  
  
  
  
  #######################################
  

  #####################################
  
  
  
  
  
  ########################################
  
  
  
  observeEvent(input$logout_btn, {
    # 1. Ejecutar el sign out de Firebase
    session$sendCustomMessage(type = 'sign_out', message = list())
    
    # 2. Ejecutar código JS para limpiar los campos de entrada
    shinyjs::runjs("$('#login_email').val(''); $('#login_password').val('');")
  })
  
  ############################################################
  
  
  
  ############################################################
  
  # --- 4. MANEJADOR DEL BOTÓN DE LOGIN EN R (DEBE SER NUEVO) ---
  # Este bloque llama a la función 'do_login_js' en el JS
  observeEvent(input$login_submit, {
    
    email <- input$login_email
    password <- input$login_password
    
    # 2. Enviamos las credenciales al manejador JS que interactúa con Firebase
    session$sendCustomMessage(
      type = 'do_login_js', 
      message = list(email = email, password = password)
    )
  })
  
  
  
  
  
  # --- 4. OBTENER INFORMACIÓN DEL USUARIO Y MAPEO DE ROLES ---
  user_info <- reactive({
    current_user_email <- user_email_js() 
    req(current_user_email) 
    
    # Debug: Se activa el RLS
    # print(paste("Usuario autenticado:", current_user_email))
    
    
    user_data <- user_base %>%
      filter(user == current_user_email) %>%
      {
        if (nrow(.) == 0) {
          tibble(
            user = current_user_email,
            permissions = "OPERADOR_CADENA", 
            empresa_id = "EMP_DEFAULT",
            name = sub("@.*", "", current_user_email),
            
            
            # ✅ CORRECCIÓN CLAVE: Aseguramos que la columna exista con un valor por defecto
            jefe_sector_asignado = "N/A"
            
            
          )
        } else {
          .
        }
      }
    
    list(
      logged_in = TRUE,
      user = user_data$user[1],
      email = user_data$user,
      role = user_data$permissions[1],
      empresa_id = user_data$empresa_id[1],
      name = user_data$name[1],
      # ✅ CORRECCIÓN CLAVE: Usamos 'tryCatch' para asignar un valor por defecto si falla
      # El error 'size 0' ocurrirá si la columna no existe. Esto lo previene.
      jesector = tryCatch(user_data$jefe_sector_asignado[1], error = function(e) {"N/A"}) 
    )
  })
  
  
  
  
  
  
  # --- 5. FILTRADO DE DATOS CRUCOS POR EMPRESA (Reactivo) ---
  
  datos_filtrados_crudos <- reactive({
    user <- user_info()
    req(user)
    
    req(nrow(datos_banano_raw) > 0)
    
    # Debug: Se activa la carga de datos
    # print(paste("Cargando datos para rol:", user$role))
    
    data <- datos_banano_raw
    
    if (user$role == "SUPER_ADMIN") {
      return( data)
    }
    # 3. FILTRO ADICIONAL PARA JEFE DE SECTOR
    # Si el rol es JEFE_SECTOR, debe filtrar los datos por su propio nombre
    # *** REQ: Asumiendo que el campo user$name (o user$user) contiene el nombre del Jefe de Sector ***
    
    if (user$role == "JEFE_SECTOR") {
      # ✅ CORRECCIÓN 1: Aplicar limpieza de formato al valor del usuario
      user_jesector_limpio <- toupper(trimws(user$jesector))
      
      # Aplicamos el filtro usando el valor limpio y limpiando también la columna de la data
      data <- data %>% 
        dplyr::filter(toupper(trimws(jesector)) == user_jesector_limpio) 
    }
    
    return(data)
  })
  
  
  
  # --- 5B. Lógica de Mapeo y Preparación de Columnas (Estabilización) --- los llama los kpi que estan abaui se preparan los datos 
  
  datos_dashboard <- reactive({
    
    
    # user <- user_info()
    
    data <- datos_filtrados_crudos()
    req(nrow(data) > 0)
    
    # Debug: Se inicia la limpieza de datos
    #print("Iniciando limpieza y renombre de columnas en datos_dashboard")
    
    
    # *** CORRECCIÓN DE COLUMNAS Y PREPARACIÓN MÍNIMA ***
    data <- data %>%
      # 1. Renombramos las columnas con nombres problemáticos a nombres internos sin espacios
      rename(
        LOTE_ID = Lote, 
        PESO_BRUTO = `Peso bruto`, # ¡CORREGIDO!
        CALIBRACION_SUP = `Calibracion superior`, # ¡CORREGIDO!
        SEMANA_COSECHA = `Semana de cosecha`, # ¡CORREGIDO!
        EMPRESA_ID_FILTRO = EMPRESA,
        Has = has,
        # ✅ CAMBIO CLAVE 1: Renombrar para consistencia con el filtro
        
        
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
        Has, # Columnas estandarizadasJefe_Sector,
        
        jesector,
        
        
        # Mantener otras columnas necesarias para los KPIs/Tablas existentes
        `Peso raquis`, Rechazado, Recuperado, `Numero de manos`, palanca, Defecto, `Generador de merma`, EdDi, `Tipo de plantacion`, TPId, MC
        # Mantener TASA_RECHAZO y RECUSADOS (si existen en el data.frame)
      )
    
    
    # ------------------------------------------------------------------
    # --- 🔒 APLICACIÓN DEL FILTRO DE SEGURIDAD (RLS) ---
    # ------------------------------------------------------------------
    
    # Solo aplicamos el filtro si el usuario tiene el rol JEFE_SECTOR
    #  if (user$role == "JEFE_SECTOR") {
    #  data <- data %>%
    # Filtramos la columna renombrada ('Jefe_Sector') 
    # usando el valor capturado en user_info() ('user$jesector')
    #    dplyr::filter(Jefe_Sector == user$jesector)
    #  }
    
    # Asumo que el filtro por EMPRESA_ID_FILTRO ya lo tienes implementado en este punto si aplica.
    
    return(data)
    
    
    
  })
  
  #############################################################################
  
  
  # --- 8F. Datos para los filtros del Formulario (RLS) ---
  datos_form_enfunde <- reactive({
    data <- datos_dashboard()
    user <- user_info()
    
    # Si es JEFE_SECTOR, los datos ya están filtrados.
    # Si es SUPER_ADMIN o ADMIN, mostramos todas las opciones, aunque por RLS solo
    # nos interesa el JEFE_SECTOR.
    
    haciendas <- sort(unique(data$HACIENDA))
    lotes <- sort(unique(data$LOTE_ID))
    
    # Asumimos que la lista de cintas es fija o proviene de otra fuente
    cintas_colores <- c("NEGRO", "ROJO", "AZUL", "AMARILLO", "BLANCO", "VERDE", "CAFE", "LILA") 
    
    list(
      haciendas = haciendas,
      lotes = lotes,
      cintas = cintas_colores
    )
  })
  
  ###########################################################
  
  
  # --- 12. LÓGICA DE CÁLCULO DE TOTAL DE ENFUNDE ---
  enfunde_total <- reactive({
    sum(
      input$enfunde_lun,
      input$enfunde_mar,
      input$enfunde_mie,
      input$enfunde_jue,
      input$enfunde_vie,
      input$enfunde_sab,
      input$enfunde_dom,
      na.rm = TRUE
    )
  })
  
  output$kpi_enfunde_total <- renderInfoBox({
    infoBox(
      "Total Racimos (Semana)",
      value = tags$b(enfunde_total()),
      icon = icon("balance-scale"),
      color = "purple"
    )
  })
  
  # --- 13. LÓGICA DE ENVÍO DE DATOS A FIREBASE ---
  observeEvent(input$enfunde_submit, {
    
    
    
    # 1. VALIDACIÓN PREVIA
    total_dias <- as.numeric(enfunde_total())
    lote_valor <- trimws(input$enfunde_lote) # trimws quita espacios en blanco
    
    if (lote_valor == "" || lote_valor == "0" || total_dias <= 0) {
      showNotification(
        "Error: Debe ingresar un número de Lote y al menos un valor en los días.", 
        type = "error", 
        duration = 5
      )
      return() # Esto detiene la ejecución aquí y NO guarda nada
    }# 1. VALIDACIÓN PREVIA
    total_dias <- as.numeric(enfunde_total())
    lote_valor <- trimws(input$enfunde_lote) # trimws quita espacios en blanco
    
    if (lote_valor == "" || lote_valor == "0" || total_dias <= 0) {
      showNotification(
        "Error: Debe ingresar un número de Lote y al menos un valor en los días.", 
        type = "error", 
        duration = 5
      )
      return() # Esto detiene la ejecución aquí y NO guarda nada
    }
    
    # print("¡Botón de Enfunde presionado en R!")
    
    user <- user_info()
    req(user$logged_in)
    
    # 1. Capturamos los datos EXACTOS del formulario
    sem_actual <- as.numeric(input$enfunde_semana)
    # Importante: El JS espera 'hacienda' como string
    hacienda_actual <- as.character(input$enfunde_hacienda)
    
    # 1. Crear el objeto de datos que coincide con la estructura
    data_to_save <- list(
      SEMANA = as.numeric(input$enfunde_semana),  # Asegurar tipo
      HACIENDA = as.character(input$enfunde_hacienda),
      # El Jefe de Sector se toma del usuario logueado (RLS)
      `JEFE DE SECTOR` = user$name, 
      EMPRESA = user$empresa_id,
      LOTE = as.character(trimws(input$enfunde_lote)),
      `COLOR CINTA` = input$enfunde_cinta,
      
      # Valores diarios y totales
      LUNES = input$enfunde_lun,
      MARTES = input$enfunde_mar,
      MIERCOLES = input$enfunde_mie,
      JUEVES = input$enfunde_jue,
      VIERNES = input$enfunde_vie,
      SABADO = input$enfunde_sab,
      DOMINGO = input$enfunde_dom,
      TOTAL = as.numeric(enfunde_total()),      # Asegurar tipo
      
      # Metadata
      fecha_registro = as.character(Sys.time()),
      user_email = as.character(user$user)
    )
    
    # 2. Enviar el objeto de datos al manejador JS de Firebase (Paso 2)
    session$sendCustomMessage(type = 'save_enfunde_data', message = data_to_save)
    
    
    # 4. ACTUALIZACIÓN AUTOMÁTICA
    # Tu JS espera un objeto con: {semana: ..., hacienda: ...} 
    shinyjs::delay(1000, {
      session$sendCustomMessage(type = 'consultar_enfunde_semanal', 
                                message = list(
                                  semana = sem_actual, 
                                  hacienda = hacienda_actual
                                ))
    })
    
    
    
    
    updateSelectInput(session, "filtro_semana_consulta", selected = sem_actual)
    
    
    # --- LIMPIEZA DE CAMPOS DESPUÉS DE GUARDAR ---
    # Reseteamos los días a 0
    updateNumericInput(session, "enfunde_lun", value = 0)
    updateNumericInput(session, "enfunde_mar", value = 0)
    updateNumericInput(session, "enfunde_mie", value = 0)
    updateNumericInput(session, "enfunde_jue", value = 0)
    updateNumericInput(session, "enfunde_vie", value = 0)
    updateNumericInput(session, "enfunde_sab", value = 0)
    updateNumericInput(session, "enfunde_dom", value = 0)
    
    # También podemos resetear el lote o dejarlo para el siguiente
    updateTextInput(session, "enfunde_lote", value = "")
    
    # Notificación de éxito para estar seguros
    showNotification("Registro guardado con éxito", type = "message", duration = 3)
    
    
  } )
  
  
  ######################################
  
  
  
  # --- 14. Mostrar el estado de guardado de Firebase ---
  output$save_status_message <- renderText({
    req(input$firebase_save_status)
    
    # 🚨 DEBUG CRÍTICO: Imprime el valor recibido directamente en la Consola de R
    # print(paste("Mensaje de Firebase recibido en R:", input$firebase_save_status))
    
    # Muestra el mensaje de éxito o error que regresa Firebase JS
    input$firebase_save_status
  })
  
  
  
  ##########################################################################################
  
  
  
  
  
  
  
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
    
    # 4. FILTRO POR JEFE DE SECTOR (Nuevo Filtro)
    # Solo aplicamos este filtro si el usuario tiene permiso para elegir
    if (!is.null(input$filtro_jesector) && input$filtro_jesector != "Todos") {
      data <- data %>% filter(jesector == input$filtro_jesector)
    }
    
    # NOTA IMPORTANTE: Si el rol es JEFE_SECTOR, la función datos_filtrados_crudos()
    # ya ha aplicado un filtro permanente (user$name), por lo que el filtro de la UI
    # no afectará a ese usuario, lo cual es correcto.
    
    
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
        LOTE_ID = LOTE_ID,
        Hectareas = Hectareas,
        # Usamos saltos de línea en lugar de truncar
        `Peso Bruto<br>Promedio` = Peso_Bruto_Promedio,
        `Calibracion<br>Promedio` = Calibracion_Promedio,
        `Núm. Manos<br>Promedio` = Num_Manos_Promedio,
        `Edad<br>Promedio` = Edad_Promedio,
        `Racimos<br>Totales` = Total_Racimos,
        # Y así sucesivamente para las demás columnas...
        `Racimos<br>Recusados` = R_recusados,
        `Racimos<br>Procesados` = R_procesados
        # En reporte_promedios_semana: `Tasa<br>Rechazo` = TASA_RECHAZO
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
        
        
        
        # Usamos saltos de línea en lugar de truncar
        `Peso Bruto<br>Promedio` = Peso_Bruto_Promedio,
        `Calibracion<br>Promedio` = Calibracion_Promedio,
        `Núm. Manos<br>Promedio` = Num_Manos_Promedio,
        `Edad<br>Promedio` = Edad_Promedio,
        `Racimos<br>Totales` = Total_Racimos,
        # Y así sucesivamente para las demás columnas...
        `Racimos<br>Recusados` = R_recusados,
        `Racimos<br>Procesados` = R_procesados
        
        
        
        
        
      )
  })
  
  
  
  # --- 8D. LÓGICA DEL REPORTE POR JEFE DE SECTOR (Agrupación por Semana) ---
  
  reporte_sector_semana <- reactive({
    # Usamos los datos filtrados por la interfaz global
    data <- datos_para_kpi_y_tabla() 
    
    # *** REQ: Asumiendo que la columna 'Jefe_Sector' existe en data ***
    req(nrow(data) > 0)
    
    data %>%
      dplyr::group_by(jesector, SEMANA_COSECHA) %>% # Agrupamos por Jefe y Semana
      dplyr::summarise(
        Peso_Bruto_Promedio = mean(PESO_BRUTO, na.rm = TRUE),
        Calibracion_Promedio = mean(CALIBRACION_SUP, na.rm = TRUE),
        Total_Racimos = n(),
        .groups = 'drop'
      ) %>%
      dplyr::ungroup()
  })
  
  
  
  # --- 8E. LÓGICA DE CÁLCULO DE KPIS GLOBALES PARA EL SECTOR ---
  reporte_sector_kpis <- reactive({
    data <- datos_para_kpi_y_tabla()
    req(nrow(data) > 0)
    
    data %>%
      dplyr::summarise(
        Peso_Promedio_Sector = mean(PESO_BRUTO, na.rm = TRUE),
        Racimos_Totales = n(),
        # Cálculo de Rendimiento: (Peso Bruto Total / Total de Hectáreas Únicas)
        # NOTA: Para un cálculo preciso, se necesita la fecha/semana de cosecha. 
        # Simplificamos como el total de peso entre el total de racimos (ya tenemos el promedio),
        # o simplemente usamos un KPI de Peso Promedio.
        Hectareas_Unicas = sum(unique(Has), na.rm = TRUE) # Suma de hectáreas únicas en la selección
      )
  })
  
  
  
  
  
  
  
  
  # --- 6. RENDERIZACIÓN DEL DASHBOARD Y MENÚ CONDICIONAL ---
  output$sidebar_menu <- renderUI({
    
    
    
    
    
    user <- user_info()
    
    logout_button <- actionButton("logout_btn", "Salir", icon = icon("sign-out-alt"), 
                                  style = "color: white; background-color: #d9534f; border-color: #d9534f;")
    
    
    
    
    # ==========================================================
    # 🌟 NUEVA LÓGICA DE NAVEGACIÓN (INYECTADA AQUÍ)
    # ==========================================================
    shinyjs::runjs("
     // 1. VARIABLE DE CONTROL: Para saber si el clic es automático o del usuario
      window.navegandoAtras = false;

      $(document).off('click', '.sidebar-menu a').on('click', '.sidebar-menu a', function(e) {
        var tab = $(this).attr('data-value');
        
        // Si el clic es del usuario (NO es navegando atrás), guardamos en el historial
        if(!window.navegandoAtras && tab) {
          history.pushState({tab: tab}, '', '#' + tab);
        }
        // Reseteamos la bandera siempre
        window.navegandoAtras = false;
      });

      // 2. SENSOR DEL BOTÓN ATRÁS
      window.onpopstate = function(event) {
        if(event.state && event.state.tab) {
          // ACTIVAMOS LA LLAVE: Avisamos que este clic es automático
          window.navegandoAtras = true;
          
          // Movemos el slider
          $('.sidebar-menu a[data-value=\"' + event.state.tab + '\"]').click();
          
          // Sincronizamos con Shiny
          Shiny.setInputValue('tabsid', event.state.tab);
        }
      };
    ")
    # ==========================================================
    
    
    
    
    
    
    
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
          
          
          id ="tabsid",
          selected = "tab_enfunde_ingreso",  
          # *** PESTAÑA 1 RENOMBRADA Y TABNAME CORREGIDO ***
          menuItem("📊 Reporte Administrativo General por Lotes", tabName = "tab_reporte_admin", icon = icon("chart-bar")),
          # *** NUEVA PESTAÑA 2: REPORTE POR SEMANA ***
          menuItem("📅 Reporte Administrativo por Semana", tabName = "tab_reporte_admin_semana", icon = icon("calendar-alt")),
          
          # 🌟 NUEVA PESTAÑA: Reporte por Jefe de Sector
          menuItem("👤 Reporte por Sector/Jefe",  tabName = "tab_reporte_sector", icon = icon("user-tie")),
          
          # 🌟 NUEVO MENU: ENFUNDE
          menuItem("🏷️ Enfunde", tabName = "tab_enfunde_ingreso", icon = icon("tag") ),
          
          menuItem("❌ Tasa de Rechazo", tabName = "tab_rechazo", icon = icon("times")),
          menuItem("🔬 Optimización por Edad", tabName = "tab_edad", icon = icon("leaf")),
          menuItem("🔬 Estimacion de Produccion", tabName = "tab_Estimacion", icon = icon("leaf")),
          menuItem("🔬 Rolling de Produccion", tabName = "tab_Rolling", icon = icon("leaf")),
          
          if (user$role %in% c("SUPER_ADMIN", "ADMIN_EMPRESA")) {
            menuItem("⚙️ Gestión Multi-Empresa", tabName = "tab_admin", icon = icon("user-shield"))
          },
          menuItem("💡 Acerca del Sistema", tabName = "tab_info", icon = icon("info-circle"))
        )
      ),
      
      
      
      
      dashboardBody(
        tags$script(HTML("
    $(document).on('click', '.sidebar-menu a', function() {
      var tabName = $(this).attr('data-value');
      Shiny.setInputValue('tabsid_manual', tabName);
    });
  ")),
        ###############################################################################
        shinyjs::useShinyjs(),
        
        
        # *** ESTILOS CSS PARA COMPACTAR LA UI ***
        tags$head(
          
          
          
          tags$style(HTML("
          /* Bloqueo total al cargar la página */
   /* .bloqueado-inicial { display: none !important; }   */
        /*  #filtros_globales_container { display: none; }*/
          
             /* Compacta la altura de los boxes (Filtros y KPIs) */
             .compact-box-kpi .box-body,
             .compact-box-kpi .box-header {
                 padding-top: 2px !important;
                 padding-bottom: 5px !important;
                 margin-bottom: 0px !important;
             }
             .compact-box-kpi .form-group {
                 margin-bottom: 3px !important; /* Reduce el espacio debajo de los filtros */
             }

             /* Compacta los valueBoxes (KPIs) */
             .small-box {
                 min-height: 50px; /* Altura mínima reducida */
                 margin-bottom: 5px; /* Espacio inferior reducido */
             }
             .small-box h3 {
                 font-size: 18px; /* Tamaño de valor reducido */
                 margin: 0 0 5px 0;
             }
             .small-box p {
                 font-size: 11px; /* Tamaño de subtítulo reducido */
             }
             
             /* Compacta la tabla DataTable */
             .dataTables_wrapper .dataTables_paginate .paginate_button {
                 padding: 0.1em 0.5em; /* Reduce el padding de los botones de paginación */
             }
             
             /* Estilo para los encabezados de tabla multilinea y reducción de padding de celdas */
                .dataTables_wrapper table.dataTable td, 
                .dataTables_wrapper table.dataTable th {
                    padding: 3px 5px !important; 
                    white-space: nowrap; /* Evita que los datos de las filas se envuelvan */
                }
             
             
         "))
        ),
        
        # ******************************************************
        # *** FIN DE LA INYECCIÓN DE CSS ***
        # ********************
        
        
        h6(paste("Bienvenido,", user$name, " (Rol:", user$role, ")"), icon("hand-peace")),
        
        
        uiOutput("contenedor_filtros_global"),
        
        
        # ******************************************************
        # *** SOLUCIÓN: FILTROS GLOBALES (Fuera de tabItems) ***
        # ******************************************************
        
        
        
        
        tabItems(
          
          
          # 1. Pestaña de Reporte Administrativo (Antes tab_rendimiento)
          # 1. Pestaña de Reporte Administrativo
          tabItem(tabName = "tab_reporte_admin",
                  h2("Reporte Administrativo: Parámetros de Producción por Lotes"),
                  
                  
                  
                  # *** KPIS (Reutilizan los filtros globales)
                  # KPIS para LOTES (Mantienen los IDs originales)
                  # EN LUGAR DE fluidRow(), usaremos un box
                  box(title = "KPIs de Lote", status = "info", solidHeader = TRUE, width = 12, 
                      class = "compact-box-kpi", # Nueva clase para reducir altura
                      
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
                  
                  
                  
                  
                  # *** KPIS (Reutilizan los filtros globales)
                  # KPIS para LOTES (Mantienen los IDs originales)
                  # EN LUGAR DE fluidRow(), usaremos un box
                  box(title = "KPIs de Semana", status = "info", solidHeader = TRUE, width = 12, 
                      class = "compact-box-kpi", # Nueva clase para reducir altura
                      
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
          
          
          
          # 🌟 NUEVO TABITEM: Reporte por Jefe de Sector
          tabItem(tabName = "tab_reporte_sector",
                  h2("Reporte de Productividad por Jefe de Sector"),
                  
                  # Filtros (Reutiliza los filtros globales)
                  # Aquí puedes añadir un filtro específico si fuera necesario:
                  # column(width = 3, uiOutput("ui_filtro_jefe_sector")), 
                  
                  # KPIS (Opcional, pero recomendado)
                  fluidRow(
                    # Puedes usar nuevos KPIs con IDs únicos:
                    valueBoxOutput("kpi_racimos_sector", width = 3),
                    valueBoxOutput("kpi_rendimiento_sector", width = 3),
                    valueBoxOutput("kpi_peso_sector", width = 3)
                  ),
                  
                  # Gráficos Lineales (Tendencias)
                  fluidRow(
                    box(title = "Tendencia de Peso Promedio (Semana vs. Lote)", status = "primary", solidHeader = TRUE, width = 12,
                        tabBox(width = 12, 
                               # Tabulador 1: Peso por Semana
                               tabPanel("Peso por Semana", icon = icon("chart-line"), 
                                        plotOutput("plot_peso_sector_semana")),
                               # Tabulador 2: Peso por Lote
                               tabPanel("Peso por Lote", icon = icon("chart-area"),
                                        plotOutput("plot_peso_sector_lote"))
                        )
                    )
                  )
          ),
          
          
          # 🌟 NUEVO TABITEM: Ingreso de Datos de Enfunde
          tabItem(tabName = "tab_enfunde_ingreso",
                  h2("📝 Registro Diario de Enfunde"),
                  
                  
                  # 🚨 DEBUG TEMPORAL: Muestra el ID de la pestaña activa 🚨
                  verbatimTextOutput("debug_tab_id"),
                  
                  # Aquí irá el formulario (ver Paso 3)
                  uiOutput("ui_enfunde_form") 
          ),
          
          
          
          # 🌟 NUEVO TABITEM: Análisis de Enfunde (Vacío por ahora)
          tabItem(tabName = "tab_enfunde_analisis",
                  h2("📈 Análisis de Enfunde (KPIs y Tendencias)")
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
  
  # --- En tu server.R (cerca de tus otros outputs/reactives) ---
  
  # 🚨 FUNCIÓN DE DEBUGGING TEMPORAL 🚨
  # output$debug_tab_id <- renderPrint({
  # Imprime el valor de la pestaña activa (cuyo ID es "tabs" en el menú)
  # cat(paste("ID de Pestaña Activa (input$tabs):", input$tabs))
  # })
  
  
  
  
  # --- 9. RENDERIZACIÓN DE LA TABLA DEL REPORTE ADMINISTRATIVO ---
  output$table_promedios <- DT::renderDataTable({
    data <- reporte_promedios()
    req(nrow(data) > 0)
    
    DT::datatable(
      data, 
      escape = FALSE,
      # *** CAMBIO APLICADO: Añadir la clase 'cell-border' ***
      class = 'cell-border stripe',
      
      # *** CAMBIO 1: Habilitar la extensión de Botones ***
      extensions = 'Buttons',
      
      options = list(pageLength = 50, scrollX = TRUE, 
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
      formatRound(columns = 'Peso Bruto<br>Promedio', digits = 2) %>%
      formatRound(columns = 'Calibracion<br>Promedio', digits = 1) %>%
      formatRound(columns = 'Núm. Manos<br>Promedio', digits = 1) %>%
      formatRound(columns = 'Edad<br>Promedio', digits =1) %>%
      formatRound(columns = 'Hectareas', digits = 2) %>% # Formato para Hectáreas
      
      # *** NUEVO FORMATO: R.recusados (Conteo) ***
      formatRound(columns =  'Racimos<br>Recusados' , digits = 0) %>%
      
      # *** NUEVO FORMATO: R_procesados (Conteo entero) ***
      formatRound(columns ='Racimos<br>Procesados', digits = 0) %>%
      
      
      # --- 2. SEMAFORIZACIÓN De los paramtetros de produccion
      
      # --- 2. SEMAFORIZACIÓN DEL PESO BRUTO (Peso_Bruto_Promedio) ---
      formatStyle(
        'Peso Bruto<br>Promedio',
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
        'Calibracion<br>Promedio',
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
        'Núm. Manos<br>Promedio',
        backgroundColor = styleInterval(
          c(7, 8), # Puntos de corte: <7, 7-8, >8
          c('red', 'yellow', 'lightgreen')
        )
      ) %>%
      
      # --- 5. SEMAFORIZACIÓN DE EDAD (Lógica simplificada y corregida) ---
      # Rango óptimo 11-13 semanas. Todo lo que esté fuera es amarillo o rojo.
      # Lógica: <10 (Rojo), 10-14 (Amarillo/Verde), >14 (Rojo)
      formatStyle(
        'Edad<br>Promedio',
        # 1. Rojo para los extremos: <10 o >14
        backgroundColor = styleInterval(
          c(10, 14), 
          c('red', 'yellow', 'red')
        )
      ) %>%
      formatStyle(
        'Edad<br>Promedio',
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
      escape = FALSE,
      class = 'cell-border stripe',
      extensions = 'Buttons',
      
      options = list(
        pageLength = 50,
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
      formatRound(columns = 'Peso Bruto<br>Promedio', digits = 2) %>%
      formatRound(columns = 'Calibracion<br>Promedio', digits = 1) %>%
      formatRound(columns = 'Núm. Manos<br>Promedio', digits = 1) %>%
      formatRound(columns = 'Edad<br>Promedio', digits = 1) %>%
      #  formatRound(columns = 'TASA_RECHAZO', digits = 2) %>%
      formatRound(columns = 'Racimos<br>Recusados' , digits = 0) %>%
      formatRound(columns ='Racimos<br>Procesados', digits = 0) %>%
      
      # *** SEMAFORIZACIÓN (Se aplica la misma lógica de Lotes) ***
      formatStyle(
        'Peso Bruto<br>Promedio',
        backgroundColor = styleInterval(
          c(45, 60),
          c('red', 'yellow', 'lightgreen') 
        )
      ) %>%
      formatStyle(
        'Calibracion<br>Promedio',
        backgroundColor = styleInterval(
          c(42, 43.5, 45, 45.5),
          c('red', 'yellow', 'lightgreen', 'yellow', 'red')
        )
      ) %>%
      formatStyle(
        'Núm. Manos<br>Promedio',
        backgroundColor = styleInterval(
          c(7, 8),
          c('red', 'yellow', 'lightgreen')
        )
      ) %>%
      formatStyle(
        'Edad<br>Promedio',
        backgroundColor = styleInterval(
          c(10, 14),
          c('red', 'yellow', 'red')
        )
      ) %>%
      formatStyle(
        'Edad<br>Promedio',
        backgroundColor = styleEqual(c(11, 12, 13), rep('lightgreen', 3))
      )
  })
  
  
  
  
  
  
  ##################################
  
  # --- LÓGICA PARA GENERAR EL FILTRO DE SEMANAS (MODIFICADO) ---
  output$ui_filtro_semana <- renderUI({
    # 1. Intentar obtener semanas del archivo Excel
    data <- datos_dashboard()
    
    # 2. Determinar el límite según el año seleccionado
    # Usamos input$filtro_ano (asegúrate que este sea el ID de tu selector de año)
    req(input$filtro_ano)
    limite_semanas <- if (input$filtro_ano == 2026) 53 else 52
    
    if (!is.null(data) && nrow(data) > 0) {
      # Si hay datos en el Excel, extraemos esas semanas
      semanas_archivo <- unique(data$SEMANA_COSECHA)
      # Solo nos quedamos con las que no superen el límite legal del año
      semanas <- sort(semanas_archivo[semanas_archivo <= limite_semanas])
    } else {
      # Si el archivo falla, generamos la lista manual del 1 al límite
      semanas <- 1:limite_semanas
    }
    
    # Opciones, incluyendo 'Todos'
    opciones <- c("Todos", semanas)
    
    # Generar el control de selección
    selectInput(
      "filtro_semana", # ID que usa tu lógica de Firebase
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
  
  
  ################################
  
  
  # --- 7E. FILTRO DE JEFE DE SECTOR (para SUPER_ADMIN / ADMIN_EMPRESA) ---
  output$ui_filtro_jesector <- renderUI({
    user <- user_info()
    
    # Solo mostrar si el usuario puede ver múltiples sectores
    if (user$role %in% c("SUPER_ADMIN", "ADMIN_EMPRESA")) { 
      data <- datos_dashboard() # Usa la data ya filtrada por empresa/año, si aplica
      req(nrow(data) > 0)
      
      # Obtener los nombres únicos de Jefe de Sector, añadir 'Todos'
      choices <- sort(unique(data$jesector))
      
      selectInput(
        "filtro_jesector",
        "Filtrar por Jefe de Sector:",
        choices = c("Todos", choices),
        selected = "Todos"
      )
    }
  })
  
  ###########################################################################################
  
  # --- 11. RENDERIZACIÓN DEL FORMULARIO DE ENFUNDE (RLS) ---
  output$ui_enfunde_form <- renderUI({
    user <- user_info()
    form_data <- datos_form_enfunde()
    
    if (is.null(user$logged_in) || !user$logged_in) {
      return(h4("Debe iniciar sesión para ingresar datos."))
    }
    
    fluidPage(
      # PRIMER BLOQUE: FORMULARIO
      fluidRow(
        box(title = "Datos de Ubicación y Fecha", status = "primary", solidHeader = TRUE, width = 4,
            dateInput("enfunde_fecha", "Fecha de Registro", value = Sys.Date()),
            #numericInput("enfunde_semana", "Semana", value = as.numeric(format(Sys.Date(), "%W"))),
            selectInput("enfunde_semana", "Semana:", choices = 1:52, selected = 2),
            
            
            {
              h_choices <- form_data$haciendas
              if (user$role == "SUPER_ADMIN" || user$role == "ADMIN_EMPRESA") {
                prioridad <- "SAN HUMBERTO"
                if (prioridad %in% h_choices) {
                  # Lo ponemos al inicio de la lista
                  h_choices <- c(prioridad, setdiff(h_choices, prioridad))
                }
              }
              selectInput("enfunde_hacienda", "Hacienda ", choices = h_choices)
            },
            
            
            
            # selectInput("enfunde_hacienda", "Hacienda ", choices = form_data$haciendas),
            selectInput("enfunde_lote", "Lote ", choices = form_data$lotes),
            selectInput("enfunde_cinta", "Color de Cinta", choices = form_data$cintas)
        ),
        
        box(title = "Racimos Enfundados", status = "warning", solidHeader = TRUE, width = 4,
            numericInput("enfunde_lun", "Lunes", value = 0, min = 0),
            numericInput("enfunde_mar", "Martes", value = 0, min = 0),
            numericInput("enfunde_mie", "Miércoles", value = 0, min = 0),
            numericInput("enfunde_jue", "Jueves", value = 0, min = 0),
            numericInput("enfunde_vie", "Viernes", value = 0, min = 0),
            numericInput("enfunde_sab", "Sábado", value = 0, min = 0),
            numericInput("enfunde_dom", "Domingo", value = 0, min = 0)
        ),
        
        box(title = "Resumen y Envío", status = "success", solidHeader = TRUE, width = 4,
            h4(paste("Jefe de Sector:", user$name)),
            h4(paste("Empresa ID:", user$empresa_id)),
            infoBoxOutput("kpi_enfunde_total", width = 12),
            actionButton("enfunde_submit", "Guardar Informacion de Enfunde", icon = icon("save"), 
                         class = "btn-success btn-lg", width = "100%"),
            br(), br(),
            textOutput("save_status_message")
        )
      ), # <--- ESTA COMA ES OBLIGATORIA PARA SEPARAR LOS FLUIDROW
      
      # SEGUNDO BLOQUE: LISTADO/TABLA
      fluidRow(
        box(
          title = "Listado de Lotes y Registros Semanales", 
          status = "primary", # Cambiado a primary para que resalte
          solidHeader = TRUE, 
          width = 12,
          column(width = 12,
                 div(style = "display: inline-block; vertical-align:top; width: 250px;",
                     selectInput("filtro_semana_consulta", "Ver Historial (Semana):",
                                 choices = 1:52, # Esto se actualizará solo
                                 selected = 2)),
                 hr(),
                 # Contenedor para la tabla
                 DT::dataTableOutput("tabla_ingresos_semanales")
          )
        )
      )
    )
  })
  
  
  ###########################################
  
  
  
  
  # --- 11. RECEPTOR DE DATOS DESDE FIREBASE ---
  datos_semanales_firebase <- reactiveVal(NULL)
  
  valores_tabla_actual <- reactiveVal(NULL)
  
  
  
  observeEvent(input$datos_desde_firestore, {
    req(input$datos_desde_firestore)
    # Convertimos el texto JSON que envía JS a una tabla de R
    df <- as.data.frame(jsonlite::fromJSON(input$datos_desde_firestore))
    datos_semanales_firebase(df)
  })
  
  
  
  ######################################
  
  
  
  # --- EVENTO 1: CUANDO CAMBIA LA FECHA ---
  observeEvent(input$enfunde_fecha, {
    req(input$enfunde_fecha)
    
    # Calculamos semana según estándar bananero (ISO)
    sem_iso <- as.numeric(format(input$enfunde_fecha, "%V"))
    anio_act <- as.numeric(format(input$enfunde_fecha, "%Y"))
    
    # Actualizamos el selector de ARRIBA
    updateSelectInput(session, "enfunde_semana", selected = sem_iso)
    
    # Actualizamos el selector de ABAJO (Igualdad)
    updateSelectInput(session, "filtro_semana_consulta", selected = sem_iso)
    
    # Consultamos a Firebase con esa semana
    user <- user_info()
    shinyjs::runjs(sprintf("consultar_enfunde_semanal('%s', '%s', %s);", 
                           user$empresa_id, anio_act, sem_iso))
  })
  
  
  
  
  
  ######################################
  
  
  observeEvent(input$enfunde_semana, {
    req(input$enfunde_semana)
    
    # 1. El valor ya es la semana, no necesitas format() ni cálculos
    sem_elegida <- input$enfunde_semana
    
    # 2. Obtenemos el año actual del sistema o del input de fecha
    anio_act <- format(input$enfunde_fecha, "%Y")
    
    # 3. IGUALAMOS el de abajo con el de arriba
    # Solo cambiamos el valor seleccionado
    updateSelectInput(session, "filtro_semana_consulta", 
                      selected = sem_elegida)
    
    # 4. CONSULTA AUTOMÁTICA A FIREBASE
    user <- user_info()
    if (!is.null(user$logged_in) && user$logged_in) {
      shinyjs::runjs(sprintf("consultar_enfunde_semanal('%s', '%s', %s);", 
                             user$empresa_id, 
                             anio_act, 
                             sem_elegida))
    }
    
  }, priority = 10)
  
  
  
  
  ##########################################################################################
  
  output$tabla_ingresos_semanales <- DT::renderDataTable({

    u <- user_info() # Obtenemos el usuario
    
    # MODIFICACIÓN AQUÍ:
    # Si eres ADMIN, necesitamos TODOS los lotes de la hacienda seleccionada.
    # Si eres JEFE, usas tu función normal.
    if (u$role == "SUPER_ADMIN" || u$role == "ADMIN_EMPRESA") {
      # Buscamos en los datos crudos todos los lotes únicos de la hacienda actual
      df_lotes <- datos_banano_raw 
      mis_lotes <- sort(unique(df_lotes$Lote)) 
    } else {
      mis_lotes <- lotes_del_sector()
    }
    
    req(mis_lotes)
    
    
    
    # mis_lotes <- lotes_del_sector()
    # req(mis_lotes)
    
    # 1. Crear tabla molde
    tabla_final <- data.frame(
      LOTE = as.character(trimws(as.character(mis_lotes))),
      LUNES = 0, MARTES = 0, MIERCOLES = 0, JUEVES = 0, 
      VIERNES = 0, SABADO = 0, DOMINGO = 0,
      stringsAsFactors = FALSE
    )
    
    # 2. Recibir datos de JS
    datos_raw <- input$datos_desde_firestore
    
    if (!is.null(datos_raw) && datos_raw != "" && datos_raw != "[]") {
      datos_lista <- tryCatch({ jsonlite::fromJSON(datos_raw) }, error = function(e) { return(NULL) })
      
      if (!is.null(datos_lista)) {
        datos_reales <- as.data.frame(datos_lista)
        colnames(datos_reales) <- toupper(trimws(colnames(datos_reales)))
        
        if ("LOTE" %in% colnames(datos_reales)) {
          for(i in 1:nrow(datos_reales)) {
            lote_db <- as.character(trimws(as.character(datos_reales$LOTE[i])))
            fila_match <- which(tabla_final$LOTE == lote_db)
            
            if (length(fila_match) > 0) {
              dias <- c("LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO", "DOMINGO")
              for(d in dias) {
                if(d %in% colnames(datos_reales)) {
                  valor <- as.numeric(as.character(datos_reales[i, d]))
                  if(!is.na(valor)) {
                    tabla_final[fila_match, d] <- tabla_final[fila_match, d] + valor
                  }
                }
              }
            }
          }
        }
      }
    }
    
    # 4. Totales Horizontales
    tabla_final$TOTAL <- rowSums(tabla_final[, 2:8], na.rm = TRUE)
    
    # --- BLOQUE DE TOTALES POR DÍA (FOOTER) ---
    # Calculamos la suma de cada columna para el final
    sumas_dias <- colSums(tabla_final[, 2:9], na.rm = TRUE)
    
    sketch = htmltools::withTags(table(
      tableHeader(colnames(tabla_final)),
      tableFooter(c("TOTAL GENERAL", sumas_dias))
    ))
    # ------------------------------------------
    
    # Guardamos para el clic
    valores_tabla_actual(tabla_final)
    
    DT::datatable(tabla_final, 
                  container = sketch,  # Aquí inyectamos la fila de totales
                  rownames = FALSE, 
                  selection = 'single',
                  options = list(dom = 't', pageLength = 100, scrollX = TRUE))
    
  }, server = TRUE)
  
  #####################################################################
  # --- 12. CARGAR DATOS PARA EDICIÓN (CORREGIDO) ---
  observeEvent(input$tabla_ingresos_semanales_rows_selected, {
    
    # 1. Traemos la "foto" de la tabla que guardamos en la sección 11
    res <- valores_tabla_actual() 
    req(res)
    
    # 2. Obtenemos el índice que el usuario tocó
    idx <- input$tabla_ingresos_semanales_rows_selected
    req(idx)
    
    # 3. Extraemos la fila con total seguridad
    fila <- res[idx, ]
    
    # 4. Actualizamos los inputs arriba
    updateTextInput(session, "enfunde_lote", value = as.character(fila$LOTE))
    updateNumericInput(session, "enfunde_lun", value = as.numeric(fila$LUNES))
    updateNumericInput(session, "enfunde_mar", value = as.numeric(fila$MARTES))
    updateNumericInput(session, "enfunde_mie", value = as.numeric(fila$MIERCOLES))
    updateNumericInput(session, "enfunde_jue", value = as.numeric(fila$JUEVES))
    updateNumericInput(session, "enfunde_vie", value = as.numeric(fila$VIERNES))
    updateNumericInput(session, "enfunde_sab", value = as.numeric(fila$SABADO))
    updateNumericInput(session, "enfunde_dom", value = as.numeric(fila$DOMINGO))
    
    showNotification(paste("Editando Lote:", fila$LOTE), type = "message")
  })
  
  
  
  
  
  
  
  ###############################################
  
  # Usamos los lotes que ya vienen en tu objeto de formulario
  lotes_del_sector <- reactive({
    form_data <- datos_form_enfunde() # Esta es la reactiva que ya usas en tu renderUI
    req(form_data$lotes)
    return(form_data$lotes)
  })
  
  
  
  
  ###############################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################################
  
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
      value =  paste(round(peso_promedio, 2), "Lb"), 
      subtitle = "Peso Promedio (Semana)",  # << CORREGIDO
      icon = icon("weight-hanging"), color = "green"
    )
  })
  
  output$kpi_calib_promedio_semana <- renderValueBox({
    df_filtrado <- datos_para_kpi_y_tabla()
    calibre_promedio <- mean(df_filtrado$CALIBRACION_SUP, na.rm = TRUE)
    valueBox(
      value =  paste(round(calibre_promedio, 2), ""),
      subtitle = "Calibre Promedio (Semana)",  # << CORREGIDO
      icon = icon("ruler-horizontal"), color = "yellow"
    )
  })
  
  output$kpi_edad_promedio_semana <- renderValueBox({
    df_filtrado <- datos_para_kpi_y_tabla()
    edad_promedio <- mean(df_filtrado$Edad, na.rm = TRUE)
    valueBox(
      value =  paste(round(edad_promedio, 2), ""),
      subtitle = "Edad Promedio (Semana)",  # << CORREGIDO
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
  
  
  
  
  
  
  # --- 10B. RENDERIZACIÓN DE KPIS PARA EL REPORTE POR SECTOR ---
  
  # KPI 1: Peso Promedio del Sector
  output$kpi_peso_sector <- renderValueBox({
    df_kpis <- reporte_sector_kpis()
    req(df_kpis)
    
    valueBox(
      paste0(round(df_kpis$Peso_Promedio_Sector, 2), " Lb"),
      "Peso Promedio Total",
      icon = icon("weight-hanging"),
      color = "green"
    )
  })
  
  # KPI 2: Total de Racimos Cosechados
  output$kpi_racimos_sector <- renderValueBox({
    df_kpis <- reporte_sector_kpis()
    req(df_kpis)
    
    valueBox(
      format(df_kpis$Racimos_Totales, big.mark = ","),
      "Racimos Totales Cosechados",
      icon = icon("cubes"),
      color = "blue"
    )
  })
  
  # KPI 3: Rendimiento (Usamos Calibre Promedio como proxy de calidad del rendimiento)
  output$kpi_rendimiento_sector <- renderValueBox({
    # Usamos la data semanal para obtener un promedio de calibración (proxy de rendimiento)
    df_sector <- reporte_sector_semana()
    req(nrow(df_sector) > 0)
    
    calibre_promedio <- mean(df_sector$Calibracion_Promedio, na.rm = TRUE)
    
    valueBox(
      paste0(round(calibre_promedio, 1)),
      "Calibre Promedio Sector",
      icon = icon("ruler-horizontal"),
      color = "orange"
    )
  })
  
  
  
  # --- 11. Gráficos para el Reporte por Sector ---
  
  output$plot_peso_sector_semana <- renderPlot({
    df_sector <- reporte_sector_semana()
    req(nrow(df_sector) > 0)
    
    df_sector %>%
      ggplot(aes(x = SEMANA_COSECHA, y = Peso_Bruto_Promedio, group = jesector, color = jesector)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
        title = "Tendencia Semanal del Peso Promedio por Jefe de Sector",
        x = "Semana de Cosecha",
        y = "Peso Promedio (Lb)",
        color = "Jefe de Sector"
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas X
    
    
  })
  
  
  # --- 11. Gráficos para el Reporte por Sector (Continuación) ---
  
  # Gráfico de Tendencia de Peso por Lote (Pestaña 2)
  output$plot_peso_sector_lote <- renderPlot({
    data <- datos_para_kpi_y_tabla()
    req(nrow(data) > 0)
    
    # 1. Agrupar por Lote y Semana para obtener el Peso Promedio
    df_lote_semana <- data %>%
      dplyr::group_by(LOTE_ID, SEMANA_COSECHA) %>%
      dplyr::summarise(Peso_Promedio = mean(PESO_BRUTO, na.rm = TRUE), .groups = 'drop')
    
    req(nrow(df_lote_semana) > 0)
    
    # 2. Creación del gráfico
    df_lote_semana %>%
      ggplot(aes(x = SEMANA_COSECHA, y = Peso_Promedio, group = LOTE_ID, color = LOTE_ID)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = "Tendencia Semanal del Peso Promedio por Lote",
        x = "Semana de Cosecha",
        y = "Peso Promedio (Lb)",
        color = "Lote ID"
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      # Si tienes muchos lotes, ajusta la leyenda o usa facet_wrap si es necesario
      guides(color = guide_legend(ncol = 2)) 
  })
  
  
  
  
  
  # Variable que guardará la pestaña
  pestaña_activa <- reactiveVal("") # Empezamos en ingreso
  
  # Escuchamos el clic manual de JavaScript
  observeEvent(input$tabsid_manual, {
    pestaña_activa(input$tabsid_manual)
  })
  
  
  # Lógica: Si es Jefe de Sector y la pestaña es Ingreso, NO MOSTRAR
  # if (u$role == "JEFE_SECTOR" && actual == "tab_enfunde_ingreso") {
  #  return(NULL)
  #}
  
  
  
  
  output$contenedor_filtros_global <- renderUI({
    u <- user_info()
    req(u)
    
    actual <- pestaña_activa()
    
    # # LÓGICA DE VISIBILIDAD MEJORADA
    # if (u$role == "JEFE_SECTOR" ) {
    #   
    #   
    #   # BLOQUEO ESTRICTO: Solo si la variable dice 'tab_enfunde_ingreso'
    #   # Como al iniciar la variable es "", esta condición NO se cumple y los filtros se muestran.
    #   if (actual == "tab_enfunde_ingreso") {
    #     return(NULL)
    #   }
    # }
    
    
    
    # LÓGICA DE VISIBILIDAD MEJORADA
    if (u$role == "JEFE_SECTOR" || u$role == "SUPER_ADMIN" || u$role == "ADMIN_EMPRESA") {
      
      # BLOQUEO ESTRICTO: Solo si la pestaña es 'tab_enfunde_ingreso'
      if (actual == "tab_enfunde_ingreso") {
        return(NULL)
      }
    }
    
    
    
    # Solo bloqueamos si estamos SEGUROS de que es la pestaña de ingreso.
    # Si el valor es "" o NULL (instancia inicial), permitimos que se vean 
    # para que no tengas que hacer clic para que 'aparezcan'.
    # if (!is.null(actual) && actual == "tab_enfunde_ingreso") {
    #    return(NULL)
    # }
    # }
    
    # Si es Admin o es Jefe en otra pestaña, mostrar:
    fluidRow(
      box(title = "Filtros de Exploración", status = "warning", solidHeader = TRUE, width = 12,
          column(width = 12,
                 column(width = 2, uiOutput("ui_filtro_empresa")),
                 column(width = 2, uiOutput("ui_filtro_ano")),
                 column(width = 2, uiOutput("ui_filtro_hacienda")),
                 column(width = 2, uiOutput("ui_filtro_semana")),
                 
                 # El filtro de jefe solo para el Admin
                 if (u$role == "ADMIN") {
                   column(width = 2, uiOutput("ui_filtro_jesector"))
                 }
          )
      )
    )
  })
  
  
  
  # Este bloque debe estar en tu Server
  observeEvent(input$filtro_semana_consulta, {
    # Solo disparamos si hay una semana seleccionada
    req(input$filtro_semana_consulta, input$enfunde_hacienda)
    
    # Enviamos el mensaje al JavaScript que acabamos de modificar
    session$sendCustomMessage('consultar_enfunde_semanal', list(
      semana = input$filtro_semana_consulta,
      hacienda = input$enfunde_hacienda
    ))
  })
  
  
  # 2. Recibir los datos que JS nos devuelve
  datos_reales_fb <- reactive({
    # 'datos_desde_firestore' es lo que enviamos desde JS en el paso 1
    res <- input$datos_desde_firestore
    if (is.null(res) || length(res) == 0) return(NULL)
    
    # Convertimos la lista de JS a un DataFrame de R
    df <- do.call(rbind, lapply(res, as.data.frame))
    return(df)
  })
  
  
  
  
  
  
  
  
}