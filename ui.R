library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)

# --- INCLUSIÓN DE DEPENDENCIAS JAVASCRIPT DE FIREBASE (CRÍTICO) ---
# Solo necesitamos las librerías base, NO FirebaseUI

# Definición de la interfaz
ui <- fluidPage(
 
  
  
  tags$head(
    # 1. Firebase Core (app.js)
    tags$script(src = "https://www.gstatic.com/firebasejs/8.10.1/firebase-app.js"),
    # 2. Firebase AUTH (auth.js)
    tags$script(src = "https://www.gstatic.com/firebasejs/8.10.1/firebase-auth.js"),
    # 3. Firebase FIRESTORE (firestore.js)
    tags$script(src = "https://www.gstatic.com/firebasejs/8.10.1/firebase-firestore.js"),
    
    # 4. Inicializa el shinyjs (¡CRÍTICO!)
    shinyjs::useShinyjs(),
    
    # (Cualquier otro CSS o tags$head que tengas)
  ),
 
  
  # --- PANEL DE LOGIN MANUAL ---
  div(
    id = "login_panel",
    style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
                 background-color: #f7f7f7; display: block; z-index: 1000;",
    
    div(
      style = "width: 400px; margin: 100px auto; padding: 30px; 
                     background: white; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); text-align: center;",
      
      h1("Iniciar Sesión / Registrarse", style = "color: #2E8B57;"),
      p("Accede a tu cuenta de SISBANLAM"),
      
      # Campos de Login
      textInput("login_email", "Email", placeholder = "ej: sisbanlam@gmail.com"),
      passwordInput("login_password", "Contraseña", placeholder = "********"),
      actionButton("login_submit", "Ingresar / Registrar", class = "btn-success", 
                   style = "width: 100%; margin-top: 15px;"),
      
      # Contenedor para el estado de login (usado por JS)
      div(id = "login_message", style = "color: red; margin-top: 10px;")
    )
  ),
  
  # --- INTERFAZ PRINCIPAL ---
  uiOutput("sidebar_menu")
)