#####################################################################
#                         Muestreo financiero                       #
#                    Lanzador principal (runner)                    #
#####################################################################

########################
#  Opciones generales  #
########################
options(shiny.maxRequestSize = 100 * 1024 * 1024)  # 100 MB para uploads
options(encoding = "UTF-8")
options(scipen = 999)  # evitar notación científica
set.seed(as.integer(Sys.time()))  # semilla variable reproducible por sesión

########################
#  Raíz y directorios  #
########################
# Requiere que Librerias.R instale/cargue {here}. Si no, cargamos mínimo aquí.
suppressWarnings(suppressMessages({
  if (!requireNamespace("here", quietly = TRUE)) install.packages("here", quiet = TRUE)
  library(here)
}))

# Intentamos ubicar la carpeta de scripts de forma flexible:
# 1) App/v1/Scripts_dashboard (estructura propuesta)
# 2) Scripts_dashboard (ejecutado desde la carpeta App/v1)
# 3) Directorio del archivo actual (fallback)
cand <- c(
  here::here("App", "v1", "Scripts_dashboard"),
  here::here("Scripts_dashboard")
)
scripts_dir <- cand[file.exists(cand) | dir.exists(cand)]
if (length(scripts_dir) == 0) {
  # Fallback final: intenta usar el wd actual
  scripts_dir <- getwd()
  warning("No se encontró 'Scripts_dashboard'. Usando getwd(): ", scripts_dir)
} else {
  scripts_dir <- scripts_dir[1]
}
cat("📂 Scripts dir: ", scripts_dir, "\n", sep = "")

#################
#   Sources     #
#################
# Cargamos en orden estricto para que `ui` y `server` existan al final.
# chdir = TRUE permite que los paths relativos dentro de cada script funcionen.
# ⚠️ Cambio clave: local = .GlobalEnv para que los objetos queden disponibles para ui.R
cargar <- function(x) {
  archivo <- file.path(scripts_dir, x)
  if (!file.exists(archivo)) stop("No existe: ", archivo)
  # Antes: source(..., local = TRUE, ...)
  source(archivo, local = .GlobalEnv, chdir = TRUE, encoding = "UTF-8")
  cat("✅ Cargado: ", x, "\n", sep = "")
}

# 1) Dependencias
cargar("Librerias.R")    # aquí ya queda cargado {here} y demás libs
# 2) Parámetros
if (file.exists(file.path(scripts_dir, "Parametros.R"))) cargar("Parametros.R")
# 3) Layout/UI files
cargar("header.R")
cargar("sider.R")
cargar("body.R")

# Evita confusiones con la función base body()
if (is.function(get("body", envir = .GlobalEnv))) stop("El objeto `body` es una función; renómbralo en body.R (p.ej. body_ui).")

cargar("ui.R")
# 4) Lógica del servidor
cargar("server.R")

###################################
#   Host/Port y utilitarios IP    #
###################################
# Permite sobreescribir con variables de entorno:
APP_HOST <- Sys.getenv("APP_HOST", unset = getOption("shiny.host", "127.0.0.1"))
APP_PORT <- as.integer(Sys.getenv("APP_PORT", unset = "1001"))

# Intento opcional de mostrar IP local (no bloqueante si falla)
resolver_ip_local <- function() {
  os <- Sys.info()[["sysname"]]
  out <- NA_character_
  try({
    if (identical(os, "Darwin")) {                 # macOS
      out <- system2("ipconfig", c("getifaddr", "en0"), stdout = TRUE, stderr = FALSE)
      if (length(out) == 0 || is.na(out)) out <- system2("ipconfig", c("getifaddr", "en1"), stdout = TRUE, stderr = FALSE)
    } else if (identical(os, "Linux")) {
      out <- system("hostname -I", intern = TRUE)
      out <- strsplit(out, "\\s+")[[1]][1]
    } else if (grepl("Windows", os, ignore.case = TRUE)) {
      lines <- system("ipconfig", intern = TRUE)
      ipline <- grep("IPv4", lines, value = TRUE)
      if (length(ipline)) out <- sub(".*?:\\s*", "", ipline[1])
    }
  }, silent = TRUE)
  out[1]
}
ip_local <- resolver_ip_local()
if (!is.na(ip_local) && nzchar(ip_local)) {
  cat("🌐 IP local detectada: http://", ip_local, ":", APP_PORT, "/\n", sep = "")
}

###################################
#        Levantar la App          #
###################################
cat("🚀 Iniciando Shiny en ", APP_HOST, ":", APP_PORT, " …\n", sep = "")

# ui y server deben existir tras haber cargado ui.R y server.R
stopifnot(exists("ui"), exists("server"))

# Ejecuta la app
shiny::runApp(
  list(ui = ui, server = server),
  host = APP_HOST,
  port = APP_PORT,
  launch.browser = interactive()  # abre navegador si estás en sesión interactiva
)