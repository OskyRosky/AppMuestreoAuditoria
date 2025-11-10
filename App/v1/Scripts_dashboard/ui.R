###############################################################
# 📄 ui.R — Estructura principal del UI (shinydashboard)
# -------------------------------------------------------------
# Este script arma la interfaz de usuario ensamblando:
#   - header  : definido en header.R
#   - sidebar : definido en sider.R (objeto `sidebar`)
#   - body    : definido en body.R  (objeto `body`)
#
# Buenas prácticas aplicadas:
#   • Validación temprana de objetos requeridos.
#   • Título del navegador (tab) + meta tags para responsividad.
#   • Skin consistente (tema de shinydashboard).
#   • Hook opcional para CSS/JS personalizados sin romper nada.
###############################################################

# --- (1) Validaciones mínimas: asegura que las partes existen ----
stopifnot("El objeto `header` no existe."  = exists("header",  inherits = TRUE))
stopifnot("El objeto `sidebar` no existe." = exists("sidebar", inherits = TRUE))
stopifnot("El objeto `body` no existe."    = exists("body",    inherits = TRUE))

# --- (2) Metadatos y cabecera HTML adicional (opcional y seguro) ---
#     Aquí puedes inyectar favicon, CSS propio y meta viewport.
extra_head <- htmltools::tags$head(
  htmltools::tags$meta(charset = "utf-8"),
  htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
  # Favicon opcional (coloca un favicon en www/ si quieres habilitarlo)
  # htmltools::tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
  # CSS mínimo opcional (ejemplo: ajustar altura del contenido)
  htmltools::tags$style(htmltools::HTML("
    /* Ejemplo: reduce rebotes de scroll en contenedores largos */
    .content-wrapper, .right-side { overflow-x: hidden; }
    /* Ejemplo: mejora la legibilidad de títulos */
    .content-header > h1 { font-weight: 600; letter-spacing: .2px; }
  "))
)

# --- (3) Construcción del UI con shinydashboard -------------------
ui <- shinydashboard::dashboardPage(
  title = "Muestreo | CGR",  # Título en la pestaña del navegador
  skin  = "blue",            # Tema (blue, black, purple, green, red, yellow)
  header = tagList(extra_head, header),  # Inyecta meta/CSS y luego el header real
  sidebar = sidebar,
  body = body
  # NOTA: Si más adelante migras a shinydashboardPlus, podrías usar dashboardPagePlus
)

# --- (4) Fondo con gradiente (opcional). Requiere shinyWidgets ----
# Mantengo tu bloque original como referencia; coméntalo/actívalo a gusto.
# shinyWidgets::setBackgroundColor(
#   color = c("#F7FBFF", "#2171B5"),
#   gradient = c("linear", "radial"),
#   direction = c("bottom", "top", "right", "left"),
#   shinydashboard = TRUE
# )

# Fin del ui.R