###############################################
# 🔧 Bootstrap de dependencias de la aplicación
# ---------------------------------------------
# Este script asegura que todas las librerías
# necesarias estén instaladas y cargadas antes
# de ejecutar la App de Muestreo de Auditoría.
#
# Funciones clave:
#   • Verifica qué paquetes están instalados.
#   • Instala automáticamente los que falten.
#   • Permite forzar reinstalación con una variable
#     de entorno (APP_BOOTSTRAP=TRUE).
#   • Carga silenciosamente todos los paquetes.
#   • Muestra la raíz del proyecto detectada por {here}.
###############################################

# =========================================================
# (0) Configuración del mirror CRAN
# ---------------------------------------------------------
# Evita el prompt interactivo al instalar paquetes y
# garantiza consistencia entre entornos Windows/Mac/Linux.
# =========================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))

# =========================================================
# (1) Listado de dependencias de la App
# ---------------------------------------------------------
# Incluye librerías de UI (Shiny), análisis estadístico,
# visualización, manejo de datos y generación de reportes.
# =========================================================
.paquetes <- c(
  # --- Sistema base y estructura de app ---
  "here", "shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets",

  # --- Manipulación y limpieza de datos ---
  "readxl", "readr", "openxlsx", "dplyr", "tidyr", "janitor",
  "data.table", "stringi", "scales",

  # --- Visualización y tableros ---
  "ggplot2", "highcharter", "reactable", "kableExtra", "gt",
  "formattable", "png", "htmltools", "viridisLite",

  # --- Estadística, modelado y muestreo ---
  "stats", "MASS", "fitdistrplus", "forecast", "jfa",

  # --- Reportes y documentos ---
  "rmarkdown", "officer", "flextable",

  # --- Utilidades y soporte ---
  "RcppRoll", "sunburstR", "d3r"
)

# =========================================================
# (2) Variable de control: forzar instalación
# ---------------------------------------------------------
# Si APP_BOOTSTRAP=TRUE en el entorno (por ejemplo:
#   export APP_BOOTSTRAP=TRUE   o   Sys.setenv(APP_BOOTSTRAP=TRUE)
# ), reinstalará todos los paquetes incluso si ya existen.
# =========================================================
.force_install <- isTRUE(as.logical(Sys.getenv("APP_BOOTSTRAP", "FALSE")))

# =========================================================
# (3) Función para instalar paquetes faltantes
# ---------------------------------------------------------
# pkgs  -> vector de nombres de paquetes
# force -> si es TRUE, reinstala todos los listados
# =========================================================
.instalar_si_faltan <- function(pkgs, force = FALSE) {
  ya_instalados <- rownames(installed.packages())
  faltan <- if (force) pkgs else setdiff(pkgs, ya_instalados)
  if (length(faltan)) {
    message("📦 Instalando paquetes: ", paste(faltan, collapse = ", "))
    install.packages(faltan, dependencies = TRUE, quiet = TRUE)
  }
}

# =========================================================
# (4) Función para cargar librerías silenciosamente
# ---------------------------------------------------------
# Utiliza suppressMessages() y quietly=TRUE para no saturar
# la consola con mensajes de carga o conflictos.
# =========================================================
.cargar_todos <- function(pkgs) {
  invisible(lapply(
    pkgs,
    function(p)
      suppressMessages(
        library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
      )
  ))
}

# =========================================================
# (5) Ejecutar el bootstrap de dependencias
# ---------------------------------------------------------
# Instala (si falta) y luego carga todas las librerías.
# =========================================================
.instalar_si_faltan(.paquetes, force = .force_install)
.cargar_todos(.paquetes)

# =========================================================
# (6) Información del entorno
# ---------------------------------------------------------
# Carga {here} y muestra la raíz del proyecto detectada.
# Esto facilita trazabilidad y validación de rutas.
# =========================================================
suppressMessages(library(here))
cat("\n✅ Librerías listas y entorno inicializado correctamente.\n")
cat("📂 Raíz del proyecto detectada por {here}: ", here(), "\n", sep = "")