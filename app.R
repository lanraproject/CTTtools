required_packages <- c(
  "shiny","shinydashboard","shinyjs","shinycssloaders",
  "shinyWidgets","readxl","CTT","dplyr","ggplot2","DT",
  "psych","gridExtra","scales"
)
installed <- rownames(installed.packages())
for (p in required_packages) {
  if (!(p %in% installed)) install.packages(p)
}

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(shinycssloaders)
  library(shinyWidgets)
  library(readxl)
  library(CTT)
  library(dplyr)
  library(ggplot2)
  library(DT)
  library(psych)
  library(gridExtra)
  library(scales)
})

# ============================================================
#   HELPER FUNCTIONS
# ============================================================

# MSI
msi_transform <- function(data, scale_min = NULL, scale_max = NULL) {
  tryCatch({
    result         <- data
    all_scale_list <- list()
    for (j in 1:ncol(data)) {
      x    <- data[, j]
      n    <- sum(!is.na(x))
      cats <- sort(unique(x[!is.na(x)]))
      K    <- length(cats)
      if (K <= 1) next
      freq_tbl <- table(factor(x, levels = cats))
      VFreq    <- as.numeric(freq_tbl)
      VProp    <- VFreq / n
      VCum     <- cumsum(VProp)
      VZ    <- numeric(K)
      VDens <- numeric(K)
      for (i in 1:(K - 1)) {
        z_i      <- qnorm(pmin(pmax(VCum[i], 1e-6), 1 - 1e-6))
        VZ[i]    <- z_i
        VDens[i] <- dnorm(z_i)
      }
      VZ[K]    <- Inf
      VDens[K] <- 0
      VScale <- numeric(K)
      for (i in 1:K) {
        dens_lo <- if (i == 1) 0    else VDens[i - 1]
        dens_hi <- VDens[i]
        cum_lo  <- if (i == 1) 0    else VCum[i - 1]
        cum_hi  <- if (i == K) 1    else VCum[i]
        denom   <- cum_hi - cum_lo
        VScale[i] <- if (denom < 1e-10) NA else (dens_lo - dens_hi) / denom
      }
      sc_min <- min(VScale, na.rm = TRUE)
      VScale_final <- if (!is.null(scale_min)) VScale - sc_min + scale_min
      else VScale - sc_min
      all_scale_list[[colnames(data)[j]]] <- data.frame(
        Kategori  = cats,
        Frek      = VFreq,
        Proporsi  = round(VProp, 4),
        Kum       = round(VCum,  4),
        Densitas  = round(VDens, 4),
        Z         = round(ifelse(is.infinite(VZ), NA, VZ), 4),
        Nilai_MSI = round(VScale_final, 4)
      )
      for (k in seq_along(cats))
        result[!is.na(data[, j]) & data[, j] == cats[k], j] <- VScale_final[k]
    }
    list(data = as.data.frame(result), mapping = all_scale_list)
  }, error = function(e) list(data = data, mapping = list()))
}

detect_data_type <- function(df) {
  vals <- unlist(df); vals <- vals[!is.na(vals)]
  if (all(vals %in% c(0, 1))) "dikotomus" else "politomus"
}

manual_kr20 <- function(biner) {
  k  <- ncol(biner)
  p  <- colMeans(biner, na.rm = TRUE)
  q  <- 1 - p
  vt <- var(rowSums(biner, na.rm = TRUE), na.rm = TRUE)
  (k / (k - 1)) * (1 - sum(p * q) / vt)
}

manual_kr21 <- function(biner) {
  k  <- ncol(biner)
  m  <- mean(rowSums(biner, na.rm = TRUE), na.rm = TRUE)
  vt <- var(rowSums(biner, na.rm = TRUE), na.rm = TRUE)
  (k / (k - 1)) * (1 - (m * (k - m)) / (k * vt))
}

# ── Hitung tingkat kesulitan sesuai jenis instrumen ──────────
# Kognitif/dikotomus : p = proporsi benar
# Angket/Skala       : mean_ratio = mean(item) / skor_maks
# Checklist          : prevalence = proporsi respons = 1 (atau >= threshold)
hitung_difficulty <- function(data, jenis_instrumen,
                              skor_maks = NULL, threshold = 1) {
  result <- sapply(data, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    
    if (jenis_instrumen == "kognitif") {
      # proporsi benar (0/1)
      mean(x)
      
    } else if (jenis_instrumen == "angket") {
      # rata-rata / skor_maks  ->  indeks 0–1
      sm <- if (!is.null(skor_maks) && skor_maks > 0) skor_maks
      else max(x, na.rm = TRUE)
      mean(x) / sm
      
    } else if (jenis_instrumen == "checklist") {
      # proporsi yang >= threshold (biasanya 1)
      mean(x >= threshold)
      
    } else {
      mean(x)
    }
  })
  round(result, 4)
}

# Label interpretasi difficulty sesuai jenis instrumen
# Mengembalikan character vector (bukan factor) agar assignment ke data.frame aman
label_difficulty <- function(p_vals, jenis_instrumen) {
  result <- if (jenis_instrumen == "kognitif") {
    as.character(cut(p_vals,
                     breaks = c(-Inf, 0.001, .30, .70, .999, Inf),
                     labels = c("Sangat Sulit","Sulit","Sedang","Mudah","Sangat Mudah"),
                     right  = FALSE))
  } else if (jenis_instrumen == "angket") {
    as.character(cut(p_vals,
                     breaks = c(-Inf, 0.20, .40, .60, .80, Inf),
                     labels = c("Sangat Rendah","Rendah","Sedang","Tinggi","Sangat Tinggi"),
                     right  = FALSE))
  } else if (jenis_instrumen == "checklist") {
    as.character(cut(p_vals,
                     breaks = c(-Inf, 0.20, .40, .60, .80, Inf),
                     labels = c("Sangat Jarang","Jarang","Cukup Umum","Umum","Sangat Umum"),
                     right  = FALSE))
  } else {
    as.character(cut(p_vals,
                     breaks = c(-Inf, .30, .70, Inf),
                     labels = c("Rendah","Sedang","Tinggi"),
                     right  = FALSE))
  }
  # Pastikan panjang output = panjang input (jaga-jaga NA)
  if (length(result) != length(p_vals)) result <- rep(NA_character_, length(p_vals))
  result
}

# Nama kolom difficulty sesuai jenis instrumen
nama_difficulty <- function(jenis_instrumen) {
  switch(jenis_instrumen,
         "kognitif"  = "p_value (Proporsi Benar)",
         "angket"    = "Tingkat Dukungan (Rata-rata / Maks)",
         "checklist" = "Proporsi Kemunculan dengan Deskripsi Tertentu",
         "p_value"
  )
}

`%||%` <- function(a, b) tryCatch(a, error = function(e) b)

pal <- c("#4e9af1","#e74c3c","#2ecc71","#f39c12","#8e44ad",
         "#1abc9c","#d35400","#2980b9","#c0392b","#16a085")

theme_ctt <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = "#F8F9FA", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#DEE2E6"),
      plot.title    = element_text(face = "bold", size = 13, colour = "#1a2340"),
      plot.subtitle = element_text(size = 10, colour = "#6C757D"),
      legend.position = "bottom"
    )
}

# ============================================================
#   CSS
# ============================================================
app_css <- "
body{font-family:'Segoe UI',sans-serif;background:#f0f2f5;}
.main-sidebar{background:#1a2340!important;}
.sidebar-menu>li>a{color:#c8d3e8!important;font-size:13px;}
.sidebar-menu>li.active>a,.sidebar-menu>li>a:hover{
  background:#2e3d6b!important;color:#fff!important;
  border-left:3px solid #4e9af1!important;}
.logo{background:#111827!important;}
.box{border-radius:8px;box-shadow:0 2px 8px rgba(0,0,0,.08);}
.box.box-primary{border-top-color:#4e9af1;}
.box.box-success{border-top-color:#2ecc71;}
.box.box-warning{border-top-color:#f39c12;}
.box.box-danger {border-top-color:#e74c3c;}
.box.box-info   {border-top-color:#3498db;}
.btn-primary{background:#4e9af1;border-color:#3a85d8;border-radius:6px;font-weight:600;}
.btn-success,.btn-warning,.btn-danger{border-radius:6px;font-weight:600;}
.info-callout{background:#eaf4ff;border-left:4px solid #4e9af1;border-radius:6px;
  padding:12px 16px;margin-bottom:12px;font-size:13px;color:#1a3a5c;}
.warn-callout{background:#fff8e6;border-left:4px solid #f39c12;border-radius:6px;
  padding:12px 16px;margin-bottom:12px;font-size:13px;color:#7a5000;}
.success-callout{background:#eafff3;border-left:4px solid #2ecc71;border-radius:6px;
  padding:12px 16px;margin-bottom:12px;font-size:13px;color:#145a32;}
.instrumen-badge{display:inline-block;padding:3px 10px;border-radius:12px;
  font-size:11px;font-weight:700;letter-spacing:.5px;}
.badge-kognitif{background:#dbeafe;color:#1e40af;}
.badge-angket{background:#dcfce7;color:#166534;}
.badge-checklist{background:#fef3c7;color:#92400e;}
table.dataTable thead th{background:#1a2340;color:white;}
.formula-box{background:#f8f9fa;border:1px solid #dee2e6;border-radius:8px;
  padding:16px;font-family:'Courier New',monospace;font-size:15px;
  color:#333;margin:10px 0;text-align:center;}
"

# ============================================================
#   UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title     = tags$span(tags$b("Analisis Instrumen secara Klasik")),
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    useShinyjs(),
    tags$head(tags$style(HTML(app_css))),
    sidebarMenu(id = "tabs",
                menuItem("📁 Masukkan Data",   tabName = "data_input",   icon = icon("upload")),
                menuItem("📊 Analisis Butir",  tabName = "analisis",     icon = icon("chart-bar")),
                menuItem("🔁 Reliabilitas",    tabName = "reliabilitas", icon = icon("sync")),
                menuItem("🎯 Distraktor",      tabName = "distraktor",   icon = icon("crosshairs")),
                menuItem("ℹ️ Panduan",         tabName = "about",        icon = icon("info-circle"))
    ),
    hr(),
    tags$div(style = "padding:10px 15px;color:#8899bb;font-size:11px;",
             tags$b("Kalibrasi Instrumen secara Klasik (CTT)"), tags$br(),
             "v 0.1.0", tags$br(),
             "Saran Dongs")
  ),
  
  dashboardBody(
    tabItems(
      
      # ===========================================================
      # TAB 1 — MASUKKAN DATA
      # ===========================================================
      tabItem(tabName = "data_input",
              
              # Baris 0: Pilih Jenis Instrumen ──────────────────────
              fluidRow(
                box(
                  title = "🧭 Jenis Instrumen", status = "primary",
                  solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6,
                           radioButtons(
                             "jenis_instrumen",
                             "Pilih jenis instrumen Anda:",
                             choices = c(
                               "Tes Kognitif / Pilihan Ganda (Dikotomus 0/1)" = "kognitif",
                               "Angket / Skala Sikap (Politomus, misal Likert)" = "angket",
                               "Checklist / Observasi (Dikotomus Ya/Tidak)"    = "checklist"
                             ),
                             selected = "kognitif"
                           )
                    ),
                    column(6,
                           uiOutput("info_jenis_instrumen")
                    )
                  ),
                  # Opsi tambahan yang muncul kondisional
                  conditionalPanel(
                    condition = "input.jenis_instrumen == 'angket'",
                    fluidRow(
                      column(4,
                             numericInput("angket_skor_maks",
                                          "Skor maksimal butir (misal: 5 untuk Likert 1–5):",
                                          value = 5, min = 1, step = 1)
                      ),
                      column(8,
                             tags$div(class = "info-callout", style = "margin-top:22px;",
                                      tags$b("Tingkat Dukungan"), " = Rata-rata skor butir / Skor maksimal.", tags$br(),
                                      "Nilai mendekati 1 -> responden cenderung sangat setuju."
                             )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.jenis_instrumen == 'checklist'",
                    fluidRow(
                      column(4,
                             numericInput("checklist_threshold",
                                          "Nilai batas muncul (default: 1):",
                                          value = 1, min = 0, step = 1)
                      ),
                      column(8,
                             tags$div(class = "info-callout", style = "margin-top:22px;",
                                      tags$b("Proporsi Kemunculan dengan Deskripsi Tertentu"), " = Proporsi responden dengan respons ≥ batas.", tags$br(),
                                      "Nilai mendekati 1 -> perilaku sangat umum/sering muncul."
                             )
                      )
                    )
                  )
                )
              ),
              
              # Baris 1: Upload + Info/MSI ───────────────────────────
              fluidRow(
                box(title = "📁 Upload Data", status = "primary",
                    solidHeader = TRUE, width = 6,
                    tags$div(class = "info-callout",
                             tags$b("Format:"), " CSV atau XLSX. Baris = Peserta, Kolom = Butir.", tags$br(),
                             "Dikotomus: 0/1 | Politomus: 1,2,3,4,5,..", tags$br(),
                             "Untuk tes pilihan ganda: unggah kunci jawaban di panel bawah."
                    ),
                    fileInput("data_file", NULL,
                              accept      = c(".csv", ".xlsx", ".xls"),
                              placeholder = "Pilih file .csv atau .xlsx..."),
                    checkboxInput("header", "Baris pertama = nama kolom (header)", value = TRUE),
                    fluidRow(
                      column(6,
                             selectInput("sep", "Separator CSV:",
                                         choices = c("Koma" = ",", "Titik koma" = ";", "Tab" = "\t"))
                      ),
                      column(6,
                             numericInput("id_cols",
                                          "Kolom ID/metadata (dari kiri):",
                                          value = 0, min = 0)
                      )
                    ),
                    radioButtons("tipe_data", "Tipe Data:",
                                 choices  = c("Dikotomus (0/1 atau butuh file kunci jawaban)" = "biner",
                                              "Politomus"      = "poli"),
                                 inline   = TRUE),
                    actionButton("load_data", "🔄 Load Data",
                                 class = "btn-primary btn-block")
                ),
                
                box(title = "⚙️ Info Dataset", status = "info",
                    solidHeader = TRUE, width = 6,
                    verbatimTextOutput("data_type_info"),
                    uiOutput("data_summary_boxes"),
                    hr(),
                    
                    checkboxInput("use_msi",
                                  HTML("<b>Gunakan MSI (Method of Successive Intervals)</b><br>
                     <small>Hanya untuk data politomus / ordinal.</small>"),
                                  value = FALSE),
                    
                    conditionalPanel(
                      condition = "input.use_msi == true",
                      tags$div(
                        style = "background:#fff8e6;border-left:4px solid #f39c12;
                           border-radius:6px;padding:10px 14px;margin-bottom:8px;",
                        tags$b("⚙️ Pengaturan Skala MSI:"), tags$br(),
                        tags$small("Masukkan skala minimal instrumen Anda (misal: 1 untuk Likert 1–5)."),
                        tags$br(),
                        fluidRow(
                          column(6,
                                 numericInput("msi_min", "Skala Minimal:", value = 1, min = 0, step = 1)
                          ),
                          column(6,
                                 numericInput("msi_max", "Skala Maksimal:", value = 5, min = 1, step = 1)
                          )
                        ),
                        tags$small(style = "color:#aaa;font-style:italic;",
                                   "Tidak perlu MSI jika data politomus sudah berskala interval."),
                        actionButton("apply_msi", "🔁 Terapkan MSI",
                                     class = "btn-warning btn-sm btn-block",
                                     style = "margin-top:6px;")
                      )
                    ),
                    tags$div(class = "warn-callout",
                             "⚠️ MSI diabaikan untuk data dikotomus")
                )
              ),
              
              # Baris 2: Kunci jawaban
              conditionalPanel(
                condition = "input.tipe_data == 'biner'",
                fluidRow(
                  box(title = "🔑 Kunci Jawaban (Opsional: untuk data mentah pilihan ganda)",
                      status = "warning", solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(4,
                               fileInput("file_kunci", NULL,
                                         accept      = c(".csv", ".xlsx", ".xls"),
                                         placeholder = "File kunci jawaban..."),
                               selectInput("sheet_kunci", "Sheet (xlsx):", choices = NULL)
                        ),
                        column(8,
                               tags$div(class = "info-callout",
                                        "Format kunci: 1 baris × n kolom (satu opsi per butir),",
                                        " atau n baris × 1 kolom.", tags$br(),
                                        "Contoh: A, B, C, D, A, ... atau kolom tunggal berisi jawaban benar.", tags$br(),
                                        tags$b("Jika data sudah 0/1, kosongkan bagian ini.")
                               )
                        )
                      )
                  )
                )
              ),
              
              # Baris 3: Preview data
              fluidRow(
                box(title = "Preview Data (10 baris pertama)",
                    status = "success", solidHeader = TRUE, width = 12,
                    uiOutput("preview_label"),
                    withSpinner(DTOutput("data_preview"), type = 4, color = "#4e9af1"),
                    uiOutput("msi_mapping_ui")
                )
              )
      ),
      
      # ===========================================================
      # TAB 2 — ANALISIS BUTIR
      # ===========================================================
      tabItem(tabName = "analisis",
              fluidRow(
                valueBoxOutput("vb_nitems"),
                valueBoxOutput("vb_nresp"),
                valueBoxOutput("vb_alpha")
              ),
              
              # Info jenis instrumen aktif
              fluidRow(
                column(12,
                       uiOutput("analisis_instrumen_info")
                )
              ),
              
              fluidRow(
                box(title = "⚙️ Opsi Filter Butir", status = "warning",
                    solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(3,
                             sliderInput("cut_pbis", "Minimum r‑pbis:", 0, 1, 0.2, step = 0.01)
                      ),
                      column(3,
                             uiOutput("slider_p_min_ui")   # label dinamis
                      ),
                      column(3,
                             uiOutput("slider_p_max_ui")
                      ),
                      column(3,
                             tags$div(style = "margin-top:25px;",
                                      actionButton("apply_cut", "✅ Terapkan Filter",
                                                   class = "btn-warning btn-block")
                             ),
                             uiOutput("eliminated_ui")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "📋 Tabel Analisis Butir", status = "primary",
                    solidHeader = TRUE, width = 12,
                    uiOutput("tbl_difficulty_label"),
                    withSpinner(DTOutput("tbl_item"), type = 4, color = "#4e9af1"),
                    hr(),
                    downloadButton("dl_item", "⬇ Unduh CSV", class = "btn-primary")
                )
              ),
              fluidRow(
                box(title = uiOutput("judul_plot_kesulitan_ui"), status = "info",
                    solidHeader = TRUE, width = 6,
                    withSpinner(plotOutput("plot_kesulitan", height = 360),
                                type = 4, color = "#3498db")
                ),
                box(title = "📊 Daya Beda (r‑pbis)", status = "success",
                    solidHeader = TRUE, width = 6,
                    withSpinner(plotOutput("plot_dayabeda", height = 360),
                                type = 4, color = "#2ecc71")
                )
              ),
              fluidRow(
                box(title = "Peta Butir: Indeks Kesulitan vs Daya Beda",
                    status = "warning", solidHeader = TRUE, width = 7,
                    withSpinner(plotOutput("plot_scatter", height = 400),
                                type = 4, color = "#f39c12")
                ),
                box(title = "📋 Klasifikasi & Tindakan Butir",
                    status = "danger", solidHeader = TRUE, width = 5,
                    withSpinner(DTOutput("tbl_tindakan"), type = 4, color = "#e74c3c")
                )
              ),
              fluidRow(
                box(title = "📈 Distribusi Keterangan & Tindakan",
                    status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(6, withSpinner(plotOutput("pie_keterangan", height = 320),
                                            type = 4, color = "#4e9af1")),
                      column(6, withSpinner(plotOutput("pie_tindakan",   height = 320),
                                            type = 4, color = "#4e9af1"))
                    )
                )
              )
      ),
      
      # ===========================================================
      # TAB 3 — RELIABILITAS
      # ===========================================================
      tabItem(tabName = "reliabilitas",
              fluidRow(
                valueBoxOutput("vb_cronbach"),
                valueBoxOutput("vb_kr20"),
                valueBoxOutput("vb_kr21")
              ),
              fluidRow(
                box(title = "📉 Nilai Alpha jika Item Dihapus",
                    status = "primary", solidHeader = TRUE, width = 7,
                    withSpinner(plotOutput("plot_alpha_del", height = 420),
                                type = 4, color = "#4e9af1")
                ),
                box(title = "📋 Interpretasi Reliabilitas", status = "info",
                    solidHeader = TRUE, width = 5,
                    tags$div(class = "info-callout",
                             tags$b("Panduan koefisien reliabilitas:"), tags$br(),
                             "α / KR ≥ 0.80 -> Sangat Tinggi", tags$br(),
                             "0.60 – 0.80   -> Tinggi",          tags$br(),
                             "0.40 – 0.60   -> Cukup",           tags$br(),
                             "0.20 – 0.40   -> Rendah",          tags$br(),
                             "< 0.20        -> Sangat Rendah"
                    ),
                    tags$div(class = "warn-callout",
                             tags$b("Catatan:"), tags$br(),
                             "• KR-20 hanya bermakna untuk data dikotomus.", tags$br(),
                             "• Cronbach alpha berlaku untuk dikotomus maupun politomus.", tags$br(),
                             "• KR-20 ≥ KR-21 apabila varians antar butir tidak homogen."
                    ),
                    hr(),
                    tableOutput("tbl_rel_interp")
                )
              )
      ),
      
      # ===========================================================
      # TAB 4 — DISTRAKTOR
      # ===========================================================
      tabItem(tabName = "distraktor",
              fluidRow(
                box(title = "🎯 Pilih Butir", status = "info",
                    solidHeader = TRUE, width = 3,
                    uiOutput("sel_butir_dis"),
                    uiOutput("dis_legend_ui")   # legend dinamis sesuai jenis instrumen
                ),
                box(title = "📊 Distribusi Respons - Butir Terpilih",
                    status = "primary", solidHeader = TRUE, width = 9,
                    withSpinner(plotOutput("plot_dis_bar", height = 320),
                                type = 4, color = "#4e9af1"),
                    hr(),
                    withSpinner(DTOutput("tbl_dis_detail"), type = 4, color = "#4e9af1")
                )
              ),
              fluidRow(
                box(title = "📊 Distribusi Respons Semua Butir",
                    status = "warning", solidHeader = TRUE, width = 12,
                    withSpinner(plotOutput("plot_dis_all", height = 520),
                                type = 4, color = "#f39c12")
                )
              )
      ),
      
      # ===========================================================
      # TAB 5 — PANDUAN
      # ===========================================================
      tabItem(tabName = "about",
              fluidRow(
                box(title = "ℹ️ Panduan Analisis CTT", status = "info",
                    solidHeader = TRUE, width = 7,
                    tags$h5("Alur Analisis yang Direkomendasikan:"),
                    tags$ol(
                      tags$li("Pilih ", tags$b("Jenis Instrumen"), " di tab Masukkan Data"),
                      tags$li("Upload data -> ", tags$b("Masukkan Data")),
                      tags$li("Terapkan MSI jika data politomus/ordinal"),
                      tags$li("Lihat hasil butir -> ", tags$b("Analisis Butir")),
                      tags$li("Evaluasi reliabilitas -> ", tags$b("Reliabilitas")),
                      tags$li("Cek distraktor -> ", tags$b("Distraktor"),
                              " (untuk tes pilihan ganda)")
                    ),
                    tags$hr(),
                    tags$h5("Metode Indeks Kesulitan per Jenis Instrumen:"),
                    tags$table(class = "table table-bordered table-sm",
                               tags$thead(tags$tr(
                                 tags$th("Jenis"),
                                 tags$th("Metode"),
                                 tags$th("Interpretasi")
                               )),
                               tags$tbody(
                                 tags$tr(
                                   tags$td(tags$span(class="instrumen-badge badge-kognitif","Kognitif")),
                                   tags$td("p = Proporsi Benar"),
                                   tags$td("< .30 Sulit | .30–.70 Sedang | > .70 Mudah")
                                 ),
                                 tags$tr(
                                   tags$td(tags$span(class="instrumen-badge badge-angket","Angket")),
                                   tags$td("Tingkat Dukungan = Mean / Skor Maks"),
                                   tags$td("Tinggi -> responden cenderung menyetujui")
                                 ),
                                 tags$tr(
                                   tags$td(tags$span(class="instrumen-badge badge-checklist","Checklist")),
                                   tags$td("Tingkat Kemunculan = Proporsi Respons ≥ batas"),
                                   tags$td("Tinggi -> perilaku sangat umum/sering muncul")
                                 )
                               )
                    ),
                    tags$hr(),
                    tags$h5("Kriteria Kualitas Butir (Daya Beda):"),
                    tags$table(class = "table table-bordered table-sm",
                               tags$thead(tags$tr(
                                 tags$th("Kriteria"), tags$th("Kategori"), tags$th("Tindakan")
                               )),
                               tags$tbody(
                                 tags$tr(tags$td("r-pbis < 0.20"), tags$td("Jelek"),
                                         tags$td("Eliminasi / Revisi")),
                                 tags$tr(tags$td("r-pbis 0.20 - 0.29"), tags$td("Sedang"),
                                         tags$td("Revisi")),
                                 tags$tr(tags$td("r-pbis 0.30 - 0.39"), tags$td("Cukup Baik"),
                                         tags$td("Sedikit Revisi")),
                                 tags$tr(tags$td("r-pbis ≥ 0.40"), tags$td("Sangat Baik"),
                                         tags$td("Pertahankan"))
                               )
                    ),
                    tags$hr(),
                    tags$h5("Analisis Distraktor / Distribusi Respons:"),
                    tags$table(class = "table table-bordered table-sm",
                               tags$thead(tags$tr(
                                 tags$th("Jenis"), tags$th("Fokus Evaluasi")
                               )),
                               tags$tbody(
                                 tags$tr(
                                   tags$td(tags$span(class="instrumen-badge badge-kognitif","Kognitif")),
                                   tags$td("Efektivitas distraktor: setiap pengecoh dipilih minimal 5% peserta")
                                 ),
                                 tags$tr(
                                   tags$td(tags$span(class="instrumen-badge badge-angket","Angket")),
                                   tags$td("Distribusi respons merata")
                                 ),
                                 tags$tr(
                                   tags$td(tags$span(class="instrumen-badge badge-checklist","Checklist")),
                                   tags$td("Akurasi deskriptor: jika skor tidak pernah 1, evaluasi relevansi deskriptor")
                                 )
                               )
                    )
                ),
                box(title = "📚 Referensi & Package", status = "success",
                    solidHeader = TRUE, width = 5,
                    tags$ul(
                      tags$li("Allen & Yen (1979). ",
                              tags$i("Introduction to Measurement Theory."), " Brooks/Cole."),
                      tags$li("Ebel & Frisbie (1991). ",
                              tags$i("Essentials of Educational Measurement."), " Prentice Hall."),
                      tags$li("Lord & Novick (1968). ",
                              tags$i("Statistical Theories of Mental Test Scores.")),
                      tags$li("Mardapi (2008). ",
                              tags$i("Teknik Penyusunan Instrumen Tes dan Nontes."), " Mitra Cendekia.")
                    ),
                    tags$div(class = "info-callout",
                             tags$b("Package R:"), tags$br(),
                             "CTT, psych, dplyr, ggplot2, DT, shiny, shinydashboard"
                    )
                )
              )
      )
      
    ) # end tabItems
  )
)

# ============================================================
#   SERVER
# ============================================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    raw_data    = NULL,
    items_raw   = NULL,
    clean_data  = NULL,
    data_type   = NULL,
    kunci       = NULL,
    analisis    = NULL,
    msi_applied = FALSE,
    msi_mapping = NULL
  )
  
  # ── Sheet kunci ──────────────────────────────────────────
  observeEvent(input$file_kunci, {
    req(input$file_kunci)
    if (grepl("xlsx|xls", tools::file_ext(input$file_kunci$name), ignore.case = TRUE)) {
      sheets <- readxl::excel_sheets(input$file_kunci$datapath)
      updateSelectInput(session, "sheet_kunci", choices = sheets)
    }
  })
  
  # ── LOAD DATA ────────────────────────────────────────────
  observeEvent(input$load_data, {
    req(input$data_file)
    withProgress(message = "Memuat data...", value = 0, {
      tryCatch({
        ext <- tolower(tools::file_ext(input$data_file$name))
        df_raw <- if (ext %in% c("xlsx","xls")) {
          as.data.frame(read_excel(input$data_file$datapath,
                                   col_names = input$header))
        } else {
          read.csv(input$data_file$datapath, header = input$header,
                   sep = input$sep, stringsAsFactors = FALSE)
        }
        
        id_n   <- max(0L, as.integer(input$id_cols))
        item_df <- if (id_n > 0) df_raw[, -seq_len(id_n), drop = FALSE] else df_raw
        
        num_mask <- sapply(item_df, function(x) {
          xn <- suppressWarnings(as.numeric(as.character(x)))
          sum(!is.na(xn)) / max(length(xn), 1) > 0.5
        })
        if (sum(num_mask) == 0) {
          showNotification("⚠️ Tidak ada kolom numerik!", type = "error"); return()
        }
        item_num <- as.data.frame(lapply(item_df[, num_mask, drop = FALSE],
                                         function(x) as.numeric(as.character(x))))
        
        rv$raw_data   <- item_num
        rv$items_raw  <- item_num
        rv$data_type  <- detect_data_type(item_num)
        rv$msi_applied <- FALSE
        rv$msi_mapping <- NULL
        rv$kunci       <- NULL
        
        if (!is.null(input$file_kunci) && input$tipe_data == "biner") {
          ext_k <- tolower(tools::file_ext(input$file_kunci$name))
          kdf <- if (ext_k %in% c("xlsx","xls")) {
            as.data.frame(read_excel(input$file_kunci$datapath,
                                     sheet = input$sheet_kunci, col_names = FALSE))
          } else {
            read.csv(input$file_kunci$datapath, header = FALSE,
                     stringsAsFactors = FALSE)
          }
          k_vec <- if (nrow(kdf) == 1) unlist(kdf[1, ]) else unlist(kdf[, 1])
          rv$kunci <- as.character(k_vec)
        }
        
        if (input$tipe_data == "biner") {
          if (!is.null(rv$kunci)) {
            sc        <- CTT::score(items = item_num, key = rv$kunci,
                                    output.scored = TRUE)
            rv$clean_data <- as.data.frame(sc$scored)
          } else {
            rv$clean_data <- as.data.frame(lapply(item_num, function(x) as.integer(x)))
          }
        } else {
          rv$clean_data <- item_num
        }
        
        rv$analisis <- CTT::itemAnalysis(items = rv$clean_data, itemReport = TRUE)
        incProgress(1)
        showNotification(
          paste0("✅ Dimuat: ", nrow(item_num), " baris × ", ncol(item_num), " kolom."),
          type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste("❌", e$message), type = "error")
      })
    })
  })
  
  # ── TERAPKAN MSI ─────────────────────────────────────────
  observeEvent(input$apply_msi, {
    req(rv$raw_data)
    if (rv$data_type != "politomus") {
      showNotification("⚠️ MSI hanya untuk data politomus.", type = "warning"); return()
    }
    tryCatch({
      sm <- input$msi_min; sx <- input$msi_max
      if (sx <= sm) {
        showNotification("⚠️ Skala maksimal harus > minimal.", type = "error"); return()
      }
      msi_res        <- msi_transform(rv$raw_data, scale_min = sm, scale_max = sx)
      rv$clean_data  <- msi_res$data
      rv$msi_mapping <- msi_res$mapping
      rv$msi_applied <- TRUE
      rv$analisis    <- CTT::itemAnalysis(items = rv$clean_data, itemReport = TRUE)
      showNotification(paste0("✅ MSI diterapkan! Skala: ", sm, " – ", sx),
                       type = "message", duration = 4)
    }, error = function(e) showNotification(paste("❌ MSI Error:", e$message), type = "error"))
  })
  
  # ── Info jenis instrumen di tab Data Input ────────────────
  output$info_jenis_instrumen <- renderUI({
    info <- switch(input$jenis_instrumen,
                   "kognitif" = list(
                     cls  = "info-callout",
                     icon = "🧠",
                     judul = "Tes Kognitif / Pilihan Ganda",
                     isi  = "Indeks kesulitan = <b>Proporsi jawaban benar</b> (p-value). Rentang 0–1. Ideal: 0.30 – 0.70."
                   ),
                   "angket" = list(
                     cls  = "success-callout",
                     icon = "📝",
                     judul = "Angket / Skala Sikap",
                     isi  = "Indeks kesulitan = <b>Tingkat Dukungan</b> (Rata-rata butir / Skor Maks). Mengukur kecenderungan dukungan responden. Tidak ada jawaban benar/salah."
                   ),
                   "checklist" = list(
                     cls  = "warn-callout",
                     icon = "☑️",
                     judul = "Checklist / Observasi",
                     isi  = "Indeks kesulitan = <b>Proporsi Kemunculan dengan Deskripsi Tertentu</b> (Proporsi responden dengan respons ≥ batas). Tinggi = perilaku sangat umum."
                   )
    )
    tags$div(class = info$cls,
             tags$b(paste(info$icon, info$judul)), tags$br(),
             HTML(info$isi)
    )
  })
  
  # ── Info instrumen di tab Analisis ───────────────────────
  output$analisis_instrumen_info <- renderUI({
    req(rv$analisis)
    ji <- input$jenis_instrumen
    label_txt <- switch(ji,
                        "kognitif"  = "🧠 Kognitif: Indeks Kesulitan = Proporsi Benar",
                        "angket"    = "📝 Angket: Indeks = Tingkat Dukungan (Rata-rata / Skor Maks)",
                        "checklist" = "☑️ Checklist: Indeks = Proporsi Kemunculan dengan Deskripsi Tertentu (Proporsi Muncul)"
    )
    badge_cls <- switch(ji,
                        "kognitif"  = "badge-kognitif",
                        "angket"    = "badge-angket",
                        "checklist" = "badge-checklist"
    )
    tags$div(style = "padding: 0 15px 10px;",
             tags$span(class = paste("instrumen-badge", badge_cls),
                       style = "font-size:13px;padding:5px 14px;",
                       label_txt)
    )
  })
  
  # ── Slider label dinamis ──────────────────────────────────
  output$slider_p_min_ui <- renderUI({
    lbl <- switch(input$jenis_instrumen,
                  "kognitif"  = "Minimum p (kesulitan):",
                  "angket"    = "Minimum Tingkat Dukungan:",
                  "checklist" = "Minimum Proporsi Kemunculan:"
    )
    sliderInput("cut_p_min", lbl, 0, 1, 0.1, step = 0.01)
  })
  output$slider_p_max_ui <- renderUI({
    lbl <- switch(input$jenis_instrumen,
                  "kognitif"  = "Maksimum p (kesulitan):",
                  "angket"    = "Maksimum Tingkat Dukungan:",
                  "checklist" = "Maksimum Proporsi Kemunculan:"
    )
    sliderInput("cut_p_max", lbl, 0, 1, 0.9, step = 0.01)
  })
  
  output$judul_plot_kesulitan_ui <- renderUI({
    lbl <- switch(input$jenis_instrumen,
                  "kognitif"  = "📊 Tingkat Kesulitan (p-value)",
                  "angket"    = "📊 Tingkat Dukungan (Rata-rata / Skor Maks)",
                  "checklist" = "📊 Proporsi Kemunculan"
    )
    lbl
  })
  
  output$tbl_difficulty_label <- renderUI({
    req(rv$analisis)
    msg <- switch(input$jenis_instrumen,
                  "kognitif"  = "Kolom <b>p_value</b>: proporsi jawaban benar per butir (0 = semua salah, 1 = semua benar).",
                  "angket"    = "Kolom <b>Tingkat Dukungan</b>: rata-rata skor butir dibagi skor maksimal. Nilai tinggi -> responden cenderung endorse.",
                  "checklist" = "Kolom <b>Proporsi Kemunculan</b>: proporsi responden yang mencentang (≥ batas). Nilai tinggi -> perilaku sangat umum."
    )
    tags$div(class = "info-callout", style = "margin-bottom:8px;", HTML(msg))
  })
  
  # ── Output info ────────────────────────────────────────────
  output$data_type_info <- renderText({
    req(rv$data_type)
    paste0("Terdeteksi: ", toupper(rv$data_type))
  })
  
  output$data_summary_boxes <- renderUI({
    req(rv$raw_data)
    df <- rv$raw_data
    fluidRow(
      valueBox(nrow(df), "Peserta", icon = icon("users"),  color = "blue",   width = 4),
      valueBox(ncol(df), "Item",    icon = icon("list"),   color = "green",  width = 4),
      valueBox(toupper(rv$data_type), "Jenis",
               icon = icon("tag"), color = "orange", width = 4)
    )
  })
  
  output$preview_label <- renderUI({
    if (isTRUE(rv$msi_applied)) {
      tags$div(class = "info-callout", style = "margin-bottom:6px;",
               tags$b("🔁 Menampilkan data setelah transformasi MSI"),
               paste0(" (skala ", input$msi_min, " – ", input$msi_max, ")"))
    } else {
      tags$div(class = "info-callout", style = "margin-bottom:6px;",
               tags$b("📄 Menampilkan data asli (sebelum MSI)"))
    }
  })
  
  output$data_preview <- renderDT({
    req(rv$raw_data)
    preview_data <- if (isTRUE(rv$msi_applied) && !is.null(rv$clean_data))
      head(rv$clean_data, 10) else head(rv$raw_data, 10)
    datatable(preview_data,
              options = list(scrollX = TRUE, pageLength = 10, dom = "t"),
              class   = "table-bordered table-sm") %>%
      formatRound(which(sapply(preview_data, is.numeric)), digits = 4)
  })
  
  output$msi_mapping_ui <- renderUI({
    req(rv$msi_mapping)
    if (length(rv$msi_mapping) == 0) return(NULL)
    tagList(
      tags$hr(),
      tags$b("📊 Tabel Detail MSI per Item (Frek | Prop | Kum | Densitas | Z | Nilai MSI):"),
      tags$br(), tags$br(),
      lapply(names(rv$msi_mapping), function(nm) {
        m <- rv$msi_mapping[[nm]]
        tags$div(
          style = "margin-bottom:18px;",
          tags$b(paste0("Item: ", nm)),
          tags$table(
            class = "table table-bordered table-sm table-striped",
            style = "font-size:12px;width:auto;",
            tags$thead(tags$tr(
              tags$th("Kategori"), tags$th("Frekuensi"), tags$th("Proporsi"),
              tags$th("Kum. Prop"), tags$th("Densitas"), tags$th("Z"),
              tags$th(tags$b("Nilai MSI"))
            )),
            tags$tbody(lapply(seq_len(nrow(m)), function(i)
              tags$tr(
                tags$td(m$Kategori[i]),  tags$td(m$Frek[i]),
                tags$td(m$Proporsi[i]),  tags$td(m$Kum[i]),
                tags$td(m$Densitas[i]),
                tags$td(if (is.na(m$Z[i])) "—" else m$Z[i]),
                tags$td(tags$b(m$Nilai_MSI[i]))
              )
            ))
          )
        )
      })
    )
  })
  
  # ============================================================
  #   ANALISIS BUTIR — item_report menggunakan difficulty dinamis
  # ============================================================
  item_report <- reactive({
    req(rv$analisis, rv$clean_data)
    
    ir <- rv$analisis$itemReport
    ji <- input$jenis_instrumen
    
    # Hitung indeks difficulty sesuai jenis instrumen
    p_vals <- hitung_difficulty(
      data             = rv$clean_data,
      jenis_instrumen  = ji,
      skor_maks        = if (ji == "angket") input$angket_skor_maks else NULL,
      threshold        = if (ji == "checklist") input$checklist_threshold else 1
    )
    
    # Pastikan panjang p_vals = jumlah butir
    n_items <- nrow(ir)
    if (length(p_vals) != n_items) p_vals <- rep(NA_real_, n_items)
    
    # Label kategori (character vector, dijamin panjangnya sama)
    lbl_vals <- label_difficulty(p_vals, ji)
    
    # Nama kolom sesuai jenis instrumen
    col_diff <- switch(ji,
                       "kognitif"  = "p_value",
                       "angket"    = "Tingkat Dukungan",
                       "checklist" = "Proporsi Kemunculan",
                       "p_value"
    )
    lbl_col <- switch(ji,
                      "kognitif"  = "Tingkat_Kesulitan",
                      "angket"    = "Tingkat_Dukungan",
                      "checklist" = "Tingkat_Kemunculan",
                      "Tingkat_Kesulitan"
    )
    
    # Bangun data.frame sekaligus - hindari dynamic column assignment bertahap
    df <- data.frame(
      Butir           = as.character(ir$itemName),
      r_pbis          = round(as.numeric(ir$pBis),           4),
      alpha_if_deleted = round(as.numeric(ir$alphaIfDeleted), 4),
      stringsAsFactors = FALSE
    )
    df[[col_diff]] <- as.numeric(p_vals)
    df[[lbl_col]]  <- as.character(lbl_vals)
    
    df$Daya_Beda <- as.character(cut(df$r_pbis,
                                     breaks = c(-Inf, .20, .30, .40, Inf),
                                     labels = c("Jelek (< .20)","Sedang (.20-.29)",
                                                "Cukup Baik (.30-.39)","Sangat Baik (≥ .40)"),
                                     right  = FALSE))
    df
  })
  
  # Nama kolom difficulty aktif
  diff_col <- reactive({
    switch(input$jenis_instrumen,
           "kognitif"  = "p_value",
           "angket"    = "Tingkat Dukungan",
           "checklist" = "Proporsi Kemunculan"
    )
  })
  diff_label_col <- reactive({
    switch(input$jenis_instrumen,
           "kognitif"  = "Tingkat_Kesulitan",
           "angket"    = "Tingkat_Dukungan",
           "checklist" = "Tingkat_Kemunculan"
    )
  })
  
  item_report_filtered <- reactive({
    req(item_report())
    df <- item_report()
    dc <- diff_col()
    
    # Gunakan nilai default jika slider belum ter-render (NULL)
    p_min <- if (!is.null(input$cut_p_min))  input$cut_p_min  else 0.0
    p_max <- if (!is.null(input$cut_p_max))  input$cut_p_max  else 1.0
    pbis  <- if (!is.null(input$cut_pbis))   input$cut_pbis   else 0.0
    
    # Pastikan kolom diff ada sebelum filter
    if (!(dc %in% names(df))) return(list(kept = df, dropped = df[0, ]))
    
    p_col <- as.numeric(df[[dc]])
    r_col <- as.numeric(df$r_pbis)
    
    keep <- !is.na(r_col) & !is.na(p_col) &
      r_col >= pbis  &
      p_col >= p_min &
      p_col <= p_max
    
    list(kept = df[keep, , drop = FALSE], dropped = df[!keep, , drop = FALSE])
  })
  
  observeEvent(input$apply_cut, {
    req(rv$clean_data)
    kn <- item_report_filtered()$kept$Butir
    if (length(kn) < 2) {
      showNotification("⚠️ Terlalu banyak eliminasi. Longgarkan batas.", type = "warning")
      return()
    }
    rv$analisis <- CTT::itemAnalysis(
      items = rv$clean_data[, kn, drop = FALSE], itemReport = TRUE)
    showNotification(paste0("✅ ", length(kn), " butir dipertahankan."), type = "message")
  })
  
  output$eliminated_ui <- renderUI({
    req(item_report_filtered())
    dr <- item_report_filtered()$dropped$Butir
    if (length(dr) == 0) return(NULL)
    tags$div(class = "warn-callout", style = "margin-top:6px;font-size:11px;",
             tags$b(paste0(length(dr), " butir akan dieliminasi:")), tags$br(),
             paste(dr, collapse = ", "))
  })
  
  output$vb_nitems <- renderValueBox({
    n <- if (!is.null(rv$clean_data)) ncol(rv$clean_data) else "–"
    valueBox(n, "Jumlah Butir", icon = icon("list"), color = "blue")
  })
  output$vb_nresp <- renderValueBox({
    n <- if (!is.null(rv$clean_data)) nrow(rv$clean_data) else "–"
    valueBox(n, "Jumlah Responden", icon = icon("users"), color = "green")
  })
  output$vb_alpha <- renderValueBox({
    val <- if (!is.null(rv$analisis)) round(rv$analisis$alpha, 3) else "–"
    valueBox(val, "Cronbach alpha", icon = icon("check-circle"), color = "orange")
  })
  
  output$tbl_item <- renderDT({
    req(item_report())
    df <- item_report()
    dc <- diff_col()
    datatable(df,
              options  = list(scrollX = TRUE, pageLength = 20,
                              dom = "Bfrtip", buttons = c("copy","csv","excel")),
              class    = "table-bordered table-sm stripe hover",
              rownames = FALSE) %>%
      formatStyle(dc,
                  background     = styleColorBar(c(0,1), "#AED6F1"),
                  backgroundSize = "98% 88%") %>%
      formatStyle("r_pbis",
                  background     = styleColorBar(c(0,1), "#A9DFBF"),
                  backgroundSize = "98% 88%") %>%
      formatStyle("Daya_Beda",
                  color      = styleEqual(
                    c("Jelek (< .20)","Sedang (.20–.29)",
                      "Cukup Baik (.30–.39)","Sangat Baik (≥ .40)"),
                    c("#e74c3c","#f39c12","#2980b9","#27ae60")),
                  fontWeight = "bold")
  })
  
  output$dl_item <- downloadHandler(
    filename = function() paste0("analisis_butir_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(item_report(), f, row.names = FALSE)
  )
  
  # ── Plot kesulitan / endorsement / prevalensi ─────────────
  output$plot_kesulitan <- renderPlot({
    req(item_report())
    df  <- item_report()
    dc  <- diff_col()
    lc  <- diff_label_col()
    ji  <- input$jenis_instrumen
    
    x_lab <- switch(ji,
                    "kognitif"  = "p-value (Proporsi Benar)",
                    "angket"    = "Tingkat Dukungan",
                    "checklist" = "Proporsi Kemunculan"
    )
    judul <- switch(ji,
                    "kognitif"  = "Tingkat Kesulitan Butir",
                    "angket"    = "Tingkat Dukungan",
                    "checklist" = "Kemunculan per Butir"
    )
    sub_txt <- switch(ji,
                      "kognitif"  = "Garis putus: p = .30 dan p = .70",
                      "angket"    = "Semakin tinggi -> responden cenderung setuju",
                      "checklist" = "Semakin tinggi -> perilaku semakin umum"
    )
    
    # Warna kategori sesuai jenis
    fill_vals <- if (ji == "kognitif") {
      c("Sangat Sulit" = "#c0392b","Sulit" = "#e74c3c","Sedang" = "#27ae60",
        "Mudah" = "#f39c12","Sangat Mudah" = "#2980b9")
    } else if (ji == "angket") {
      c("Sangat Rendah" = "#c0392b","Rendah" = "#e74c3c","Sedang" = "#27ae60",
        "Tinggi" = "#f39c12","Sangat Tinggi" = "#2980b9")
    } else {
      c("Sangat Jarang" = "#c0392b","Jarang" = "#e74c3c","Cukup Umum" = "#27ae60",
        "Umum" = "#f39c12","Sangat Umum" = "#2980b9")
    }
    
    p <- ggplot(df, aes(x = reorder(Butir, .data[[dc]]),
                        y = .data[[dc]], fill = .data[[lc]])) +
      geom_col(width = .7) +
      scale_fill_manual(values = fill_vals, drop = FALSE) +
      coord_flip() +
      labs(x = NULL, y = x_lab, title = judul, subtitle = sub_txt, fill = NULL) +
      theme_ctt()
    
    # Garis referensi sesuai jenis
    if (ji == "kognitif") {
      p <- p + geom_hline(yintercept = c(.3,.7), linetype = "dashed", colour = "grey40")
    } else {
      p <- p + geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey40")
    }
    p
  })
  
  output$plot_dayabeda <- renderPlot({
    req(item_report())
    df <- item_report()
    ggplot(df, aes(x = reorder(Butir, r_pbis), y = r_pbis, fill = Daya_Beda)) +
      geom_col(width = .7) +
      geom_hline(yintercept = c(.2,.3,.4), linetype = "dashed", colour = "grey40") +
      scale_fill_manual(values = c(
        "Jelek (< .20)" = "#c0392b","Sedang (.20–.29)" = "#f39c12",
        "Cukup Baik (.30–.39)" = "#2980b9","Sangat Baik (≥ .40)" = "#27ae60")) +
      coord_flip() +
      labs(x = NULL, y = "r‑pbis", title = "Daya Beda Butir",
           subtitle = "Garis putus: .20, .30, .40") +
      theme_ctt()
  })
  
  output$plot_scatter <- renderPlot({
    req(item_report())
    df <- item_report()
    dc <- diff_col()
    lc <- diff_label_col()
    ji <- input$jenis_instrumen
    
    x_lab <- switch(ji,
                    "kognitif"  = "p-value (Kesulitan)",
                    "angket"    = "Tingkat Dukungan",
                    "checklist" = "Proporsi Kemunculan"
    )
    
    fill_vals <- if (ji == "kognitif") {
      c("Sangat Sulit" = "#c0392b","Sulit" = "#e74c3c","Sedang" = "#27ae60",
        "Mudah" = "#f39c12","Sangat Mudah" = "#2980b9")
    } else if (ji == "angket") {
      c("Sangat Rendah" = "#c0392b","Rendah" = "#e74c3c","Sedang" = "#27ae60",
        "Tinggi" = "#f39c12","Sangat Tinggi" = "#2980b9")
    } else {
      c("Sangat Jarang" = "#c0392b","Jarang" = "#e74c3c","Cukup Umum" = "#27ae60",
        "Umum" = "#f39c12","Sangat Umum" = "#2980b9")
    }
    
    ggplot(df, aes(x = .data[[dc]], y = r_pbis, label = Butir,
                   colour = .data[[lc]])) +
      annotate("rect", xmin = .3, xmax = .7, ymin = .2, ymax = 1,
               alpha = .07, fill = "#27ae60") +
      geom_point(size = 3.5, alpha = .9) +
      geom_text(vjust = -0.9, size = 3, colour = "#1a2340") +
      geom_hline(yintercept = 0.2, linetype = "dashed",
                 colour = "#e74c3c", linewidth = .8) +
      geom_vline(xintercept = c(.3,.7), linetype = "dashed",
                 colour = "#2980b9", linewidth = .8) +
      scale_colour_manual(values = fill_vals, drop = FALSE) +
      labs(x = x_lab, y = "r‑pbis (Daya Beda)",
           title = "Peta Butir: Indeks Kesulitan vs Daya Beda",
           subtitle = "Zona hijau = area ideal butir",
           colour = NULL) +
      theme_ctt()
  })
  
  tindakan_df <- reactive({
    req(item_report())
    df <- item_report()
    dc <- diff_col()
    ji <- input$jenis_instrumen
    
    # Untuk angket/checklist, tidak ada konsep "kunci salah"
    # Tindakan disesuaikan
    if (ji == "kognitif") {
      df %>% mutate(
        Tipe = case_when(
          .data[[dc]]  > 0.90                                       ~ 3L,
          .data[[dc]] >= 0.60 & .data[[dc]] <= 0.90 & r_pbis  > 0.15 ~ 1L,
          .data[[dc]] >= 0.60 & .data[[dc]] <= 0.90 & r_pbis <= 0.15 ~ 2L,
          .data[[dc]]  < 0.60 & r_pbis  > 0.15                     ~ 4L,
          .data[[dc]]  < 0.60 & r_pbis  < 0.00                     ~ 6L,
          .data[[dc]]  < 0.60 & r_pbis >= 0.00 & r_pbis <= 0.15   ~ 5L,
          TRUE ~ NA_integer_
        ),
        Keterangan = case_when(
          Tipe == 1 ~ "Butir Ideal",
          Tipe == 2 ~ "Daya Beda Buruk",
          Tipe == 3 ~ "Sangat Mudah",
          Tipe == 4 ~ "Sulit & Diskriminatif",
          Tipe == 5 ~ "Sulit & Tidak Diskriminatif",
          Tipe == 6 ~ "Indikasi Kunci Salah",
          TRUE ~ "Tidak Terklasifikasi"
        ),
        Tindakan = case_when(
          Tipe == 1 ~ "Pertahankan",
          Tipe == 2 ~ "Eliminasi",
          Tipe == 3 ~ "Pertahankan Bersyarat",
          Tipe == 4 ~ "Pertahankan",
          Tipe == 5 ~ "Eliminasi / Revisi",
          Tipe == 6 ~ "Revisi Kunci",
          TRUE ~ "Cek Data"
        )
      ) %>% select(Butir, Keterangan, Tindakan)
    } else {
      # Angket & Checklist: klasifikasi berdasarkan tingkat dukungan/kemunculan + daya beda
      df %>% mutate(
        Keterangan = case_when(
          r_pbis >= 0.40                                     ~ "Diskriminatif Baik",
          r_pbis >= 0.20 & r_pbis < 0.40                    ~ "Diskriminatif Sedang",
          r_pbis < 0.20  & r_pbis >= 0.00                   ~ "Kurang Diskriminatif",
          r_pbis < 0.00                                      ~ "Diskriminasi Negatif",
          TRUE ~ "Tidak Terklasifikasi"
        ),
        Tindakan = case_when(
          r_pbis >= 0.40                                     ~ "Pertahankan",
          r_pbis >= 0.20 & r_pbis < 0.40                    ~ "Revisi Minor",
          r_pbis < 0.20  & r_pbis >= 0.00                   ~ "Revisi / Eliminasi",
          r_pbis < 0.00                                      ~ "Eliminasi / Cek Redaksi",
          TRUE ~ "Cek Data"
        )
      ) %>% select(Butir, Keterangan, Tindakan)
    }
  })
  
  buat_pie <- function(df, var_col, judul) {
    df2 <- df %>% count(!!sym(var_col)) %>%
      mutate(prop  = n / sum(n),
             label = paste0(round(prop * 100, 1), "%"))
    clr <- setNames(pal[seq_len(nrow(df2))], df2[[var_col]])
    ggplot(df2, aes(x = "", y = n, fill = !!sym(var_col))) +
      geom_col(width = 1, colour = "white", linewidth = 1) +
      coord_polar("y") +
      geom_text(aes(label = label),
                position = position_stack(vjust = .5), size = 3.5) +
      scale_fill_manual(values = clr) +
      labs(title = judul, fill = NULL) +
      theme_void() +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold", hjust = .5, colour = "#1a2340"))
  }
  
  output$pie_keterangan <- renderPlot({
    req(tindakan_df()); buat_pie(tindakan_df(), "Keterangan", "Keterangan Butir")
  })
  output$pie_tindakan <- renderPlot({
    req(tindakan_df()); buat_pie(tindakan_df(), "Tindakan", "Tindakan Butir")
  })
  output$tbl_tindakan <- renderDT({
    req(tindakan_df())
    datatable(tindakan_df(),
              options  = list(scrollX = TRUE, pageLength = 15, dom = "frtip"),
              class    = "table-bordered table-sm stripe hover",
              rownames = FALSE, escape = FALSE)
  })
  
  # ============================================================
  #   RELIABILITAS
  # ============================================================
  rel_vals <- reactive({
    req(rv$clean_data)
    b  <- rv$clean_data
    ps <- psych::alpha(b, check.keys = FALSE)
    ca <- round(ps$total$raw_alpha, 4)
    if (requireNamespace("validateR", quietly = TRUE)) {
      kr20_v <- tryCatch(round(validateR::kr20(b)$kr20, 4),
                         error = function(e) round(manual_kr20(b), 4))
      kr21_v <- tryCatch(round(validateR::kr21(b)$kr21, 4),
                         error = function(e) round(manual_kr21(b), 4))
    } else {
      kr20_v <- round(manual_kr20(b), 4)
      kr21_v <- round(manual_kr21(b), 4)
    }
    list(alpha = ca, kr20 = kr20_v, kr21 = kr21_v)
  })
  
  output$vb_cronbach <- renderValueBox({
    req(rel_vals())
    valueBox(rel_vals()$alpha, "Cronbach alpha",   icon = icon("check-circle"), color = "blue")
  })
  output$vb_kr20 <- renderValueBox({
    req(rel_vals())
    valueBox(rel_vals()$kr20,  "KR-20",        icon = icon("check-square"), color = "green")
  })
  output$vb_kr21 <- renderValueBox({
    req(rel_vals())
    valueBox(rel_vals()$kr21,  "KR-21",        icon = icon("square"),       color = "orange")
  })
  
  output$plot_alpha_del <- renderPlot({
    req(item_report())
    df      <- item_report()
    overall <- if (!is.null(rv$analisis)) rv$analisis$alpha else NA
    ggplot(df, aes(x = reorder(Butir, alpha_if_deleted), y = alpha_if_deleted,
                   fill = alpha_if_deleted > overall)) +
      geom_col(width = .7) +
      geom_hline(yintercept = overall, linetype = "dashed",
                 colour = "#e74c3c", linewidth = 1) +
      annotate("text", x = 1, y = overall + .005,
               label = paste0("α = ", round(overall, 3)),
               colour = "#e74c3c", hjust = 0, size = 3.5) +
      scale_fill_manual(
        values = c("TRUE" = "#e74c3c", "FALSE" = "#27ae60"),
        labels = c("TRUE" = "Naik jika dihapus", "FALSE" = "Turun jika dihapus")) +
      coord_flip() +
      labs(x = NULL, y = "Nilai alpha jika item dihapus",
           title  = "Alpha if Item Deleted",
           subtitle = "Merah = hapus item ini meningkatkan reliabilitas",
           fill   = NULL) +
      theme_ctt()
  })
  
  output$tbl_rel_interp <- renderTable({
    data.frame(
      Koefisien = c("> 0.80","0.60 – 0.80","0.40 – 0.60","0.20 – 0.40","< 0.20"),
      Kategori  = c("Sangat Tinggi","Tinggi","Cukup","Rendah","Sangat Rendah")
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # ============================================================
  #   DISTRAKTOR
  # ============================================================
  dis_combined <- reactive({
    req(rv$items_raw)
    bind_rows(lapply(seq_len(ncol(rv$items_raw)), function(j) {
      as.data.frame(
        table(Response = as.character(rv$items_raw[[j]])),
        stringsAsFactors = FALSE
      ) %>% rename(Frekuensi = Freq) %>%
        mutate(Butir = colnames(rv$items_raw)[j])
    }))
  })
  
  output$sel_butir_dis <- renderUI({
    req(rv$items_raw)
    selectInput("butir_dis", "Pilih Butir:", choices = colnames(rv$items_raw))
  })
  
  # Legend distraktor dinamis sesuai jenis instrumen
  output$dis_legend_ui <- renderUI({
    ji <- input$jenis_instrumen
    if (ji == "kognitif") {
      tagList(
        tags$div(class = "info-callout",
                 "Analisis distraktor menggunakan data respons asli.",
                 tags$br(),
                 tags$b(style = "color:#27ae60;", "Hijau"), " = Kunci Jawaban.",
                 tags$br(),
                 tags$b(style = "color:#e74c3c;", "Merah"), " = Pengecoh.",
                 tags$br(),
                 tags$small("Pengecoh efektif dipilih minimal 5% peserta.")
        )
      )
    } else if (ji == "angket") {
      tagList(
        tags$div(class = "success-callout",
                 "Distribusi respons skala Likert.",
                 tags$br(),
                 "Perhatikan distribusi merata / skew.",
                 tags$br(),
                 tags$small("Ceiling/floor effect terjadi jika mayoritas respons menumpuk di satu ujung.")
        )
      )
    } else {
      tagList(
        tags$div(class = "warn-callout",
                 "Distribusi centang/tidak centang.",
                 tags$br(),
                 tags$b(style = "color:#27ae60;", "1"), " = Muncul/Ya.",
                 tags$br(),
                 tags$b(style = "color:#e74c3c;", "0"), " = Tidak Muncul.",
                 tags$br(),
                 tags$small("Jika selalu 0: evaluasi relevansi deskriptor.")
        )
      )
    }
  })
  
  output$plot_dis_bar <- renderPlot({
    req(input$butir_dis, dis_combined())
    df      <- dis_combined() %>% filter(Butir == input$butir_dis)
    ji      <- input$jenis_instrumen
    
    if (ji == "kognitif") {
      df$Tipe <- "Pengecoh"
      if (!is.null(rv$kunci)) {
        idx <- which(colnames(rv$items_raw) == input$butir_dis)
        if (length(idx) > 0 && idx <= length(rv$kunci))
          df$Tipe[df$Response == rv$kunci[idx]] <- "Kunci Jawaban"
      }
      clr_vals <- c("Kunci Jawaban" = "#2ecc71", "Pengecoh" = "#e74c3c")
      fill_col <- "Tipe"
      judul    <- paste("Distribusi Respons:", input$butir_dis)
    } else if (ji == "angket") {
      df$Tipe <- "Pilihan Respons"
      clr_vals <- setNames(colorRampPalette(c("#3498db","#2ecc71","#f39c12"))(nrow(df)),
                           df$Response)
      fill_col <- "Response"
      judul    <- paste("Distribusi Respons Angket:", input$butir_dis)
    } else {
      df$Tipe <- ifelse(as.numeric(df$Response) >= input$checklist_threshold,
                        "Muncul (≥ threshold)", "Tidak Muncul")
      clr_vals <- c("Muncul (≥ threshold)" = "#2ecc71", "Tidak Muncul" = "#e74c3c")
      fill_col <- "Tipe"
      judul    <- paste("Distribusi Respons Checklist:", input$butir_dis)
    }
    
    ggplot(df, aes(x = Response, y = Frekuensi,
                   fill = .data[[fill_col]])) +
      geom_col(colour = "white", width = .7) +
      geom_text(aes(label = Frekuensi), vjust = -0.4, size = 4.5) +
      scale_fill_manual(values = clr_vals) +
      labs(title = judul, x = "Pilihan", y = "Frekuensi", fill = NULL) +
      theme_ctt()
  })
  
  output$tbl_dis_detail <- renderDT({
    req(input$butir_dis, dis_combined())
    df <- dis_combined() %>% filter(Butir == input$butir_dis) %>%
      mutate(Proporsi = round(Frekuensi / sum(Frekuensi), 4)) %>%
      select(-Butir)
    datatable(df, rownames = FALSE,
              options = list(pageLength = 10, dom = "t"),
              class   = "table-bordered table-sm")
  })
  
  output$plot_dis_all <- renderPlot({
    req(dis_combined())
    ggplot(dis_combined(), aes(x = Response, y = Frekuensi, fill = Response)) +
      geom_col(colour = "white", width = .7) +
      geom_text(aes(label = Frekuensi), vjust = -0.3, size = 2.5) +
      facet_wrap(~ Butir, scales = "free") +
      scale_fill_manual(values = rep(pal, length.out = 26)) +
      labs(title = "Distribusi Respons Semua Butir",
           x = "Pilihan", y = "Frekuensi") +
      theme_ctt() +
      theme(legend.position = "none",
            axis.text.x     = element_text(size = 8))
  })
  
}

shinyApp(ui = ui, server = server)
