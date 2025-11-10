########################################
# Bootstrap de dependencias de la App
########################################

# 0) Mirror fijo (evita el prompt del CRAN)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# 1) Paquetes requeridos por la App
.paquetes <- c(
  "here", "shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets",
  "readxl", "readr", "openxlsx", "dplyr", "tidyr", "janitor", "data.table", "stringi", "scales",
  "ggplot2", "highcharter", "reactable", "kableExtra", "gt", "formattable", "png", "htmltools", "viridisLite",
  "stats", "MASS", "fitdistrplus", "forecast", "jfa",
  "rmarkdown", "officer", "flextable",
  "RcppRoll", "sunburstR", "d3r"
)

# 2) ¿Forzar instalación? (por defecto NO)
#    Activa con APP_BOOTSTRAP=TRUE en el entorno si lo necesitas
.force_install <- isTRUE(as.logical(Sys.getenv("APP_BOOTSTRAP", "FALSE")))

# 3) Instalar solo lo que falte (o todo si .force_install = TRUE)
.instalar_si_faltan <- function(pkgs, force = FALSE) {
  ya_instalados <- rownames(installed.packages())
  faltan <- if (force) pkgs else setdiff(pkgs, ya_instalados)
  if (length(faltan)) {
    message("📦 Instalando paquetes: ", paste(faltan, collapse = ", "))
    install.packages(faltan, dependencies = TRUE, quiet = TRUE)
  }
}

# 4) Cargar silenciosamente
.cargar_todos <- function(pkgs) {
  invisible(lapply(
    pkgs,
    function(p) suppressMessages(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  ))
}

# 5) Ejecutar bootstrap mínimo
.instalar_si_faltan(.paquetes, force = .force_install)
.cargar_todos(.paquetes)

# 6) Info de sesión y raíz del proyecto
suppressMessages(library(here))
cat("✅ Librerías listas.\n")
cat("📂 Raíz del proyecto (here): ", here(), "\n", sep = "")