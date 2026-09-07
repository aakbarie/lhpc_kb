# The Causalytics visual identity, in one place.
#
# Tokens and component patterns from ../causalytics/styles.css — the source
# of truth for the brand, not an approximation of the rendered site. Reading
# the deployed CSS first gave two wrong answers, because Quarto's defaults sit
# on top of the real values in the browser: headings are the system sans at
# weight 720 with tight negative tracking, NOT a condensed uppercase face, and
# eyebrows are blue mono rather than muted grey.
#
# This file exists because the instrument and the analytics app had a copy
# each. Two copies of a palette is one palette and one slowly diverging
# imitation of it — the second app already had a different `.stat` treatment
# the first did not, and nothing would have caught that.
#
# Every app sources this and adds only what is genuinely its own.

suppressPackageStartupMessages({ library(shiny); library(bslib) })

CAU <- list(
  ink = "#18212b", ink_soft = "#46515d", paper = "#f7f5ef", surface = "#fffefa",
  line = "#d4d0c5", blue = "#3159b8", blue_dark = "#203b78", signal = "#cf583f",
  step_grey = "#747d86", active_bg = "#eaf0ff",
  strip_bg = "#f3dfda", strip_fg = "#733223", footer_bg = "#e5eaf3",
  sans = '-apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif',
  mono = '"SFMono-Regular", Consolas, "Liberation Mono", monospace')

cau_theme <- function()
  bslib::bs_theme(version = 5, bg = CAU$surface, fg = CAU$ink, primary = CAU$blue,
                  base_font = CAU$sans, heading_font = CAU$sans, code_font = CAU$mono)

#' The site's eyebrow: blue mono, uppercase, with the signal-square pulse
#' that marks the brand throughout.
eyebrow <- function(..., pulse = FALSE)
  shiny::div(class = "cau-eyebrow", if (pulse) shiny::span(class = "cau-pulse"), ...)

#' Status chips reuse the system's own state colours rather than inventing a
#' palette. Complete is blue, as `.pipeline-step.is-complete`; pending is the
#' step grey; warning is the control-strip pair; breach is the signal. No
#' green is added, because the identity does not have one — a sixth colour
#' meaning "good" is the one thing that would read as off-brand.
cau_badge <- function(label, kind = c("neutral", "done", "pending", "warn", "bad")) {
  kind <- match.arg(kind)
  spec <- switch(kind,
    done    = c(CAU$blue, "#ffffff"),
    pending = c("transparent", CAU$step_grey),
    warn    = c(CAU$strip_bg, CAU$strip_fg),
    bad     = c(CAU$signal, "#ffffff"),
    neutral = c(CAU$ink, "#ffffff"))
  shiny::span(class = "cau-badge",
    style = paste0("background:", spec[1], ";color:", spec[2],
                   if (kind == "pending") paste0(";border:1px solid ", CAU$line) else ""),
    toupper(label))
}

#' The stylesheet. `%N$s` indexing throughout so the argument order below is
#' the only place a token position is asserted.
cau_css <- function() sprintf('
  body{background:%1$s;color:%2$s;font-family:%3$s;font-size:15px;}
  .cau-eyebrow{color:%4$s;font:700 .64rem %5$s;letter-spacing:.06em;
    text-transform:uppercase;display:flex;align-items:center;gap:.5rem;}
  .cau-pulse{background:%6$s;display:inline-block;height:8px;width:8px;flex:none;}
  h1,h2,h3,h4,.cau-display{font-family:%3$s;font-weight:720;
    letter-spacing:-.045em;line-height:1.02;color:%2$s;}
  .cau-badge{display:inline-block;font:650 .55rem %5$s;letter-spacing:.05em;
    padding:3px 7px;border-radius:2px;white-space:nowrap;}
  .cau-brand{font-weight:750;letter-spacing:-.02em;font-size:.98rem;
    display:flex;align-items:center;gap:.55rem;}
  .cau-brand::before{background:%6$s;content:"";display:inline-block;
    height:.62rem;width:.62rem;flex:none;}
  .cau-doctrine{color:%7$s;font:.72rem %3$s;border-left:3px solid %6$s;
    padding-left:.7rem;line-height:1.55;}
  .cau-strip{background:%8$s;color:%9$s;border-radius:3px;}
  .cau-rule{border-bottom:1px solid %10$s;}
  .cau-panel{background:%12$s;border:1px solid %10$s;border-radius:4px;}
  .card,.border,.border-bottom,.border-top,hr{border-color:%10$s !important;}
  .card{background:%1$s;border-radius:4px;}
  code{color:%4$s;background:transparent;font-family:%5$s;}
  .nav-link{color:%7$s !important;font:650 .7rem %5$s;letter-spacing:.05em;
    text-transform:uppercase;}
  .nav-link.active{color:%4$s !important;background:%11$s !important;
    box-shadow:inset 3px 0 0 %4$s;}
  .btn-primary{background:%4$s;border-color:%4$s;border-radius:3px;
    font-size:.78rem;font-weight:700;letter-spacing:.02em;}
  .form-control{border-color:%10$s;border-radius:3px;font-size:.85rem;}
  .stat{font:600 2rem %5$s;letter-spacing:-.03em;}
  .stat-label{font:650 .58rem %5$s;letter-spacing:.07em;text-transform:uppercase;
    color:%7$s;}
  table{width:100%%;font-size:13px;}
  th{font:650 .58rem %5$s;letter-spacing:.06em;text-transform:uppercase;
    color:%7$s;padding-bottom:6px;text-align:left;}
  td{padding:5px 8px 5px 0;border-bottom:1px solid %10$s;
    font-variant-numeric:tabular-nums;}
', CAU$surface, CAU$ink, CAU$sans, CAU$blue, CAU$mono, CAU$signal, CAU$ink_soft,
   CAU$strip_bg, CAU$strip_fg, CAU$line, CAU$active_bg, CAU$paper)

#' Everything an app needs in <head>: the stylesheet and the page title.
cau_head <- function(title)
  shiny::tags$head(shiny::tags$style(shiny::HTML(cau_css())),
                   shiny::tags$title(paste("Causalytics —", title)))

#' The wordmark, with its signal square.
cau_brand <- function() shiny::div(class = "cau-brand mb-1", "Causalytics")
