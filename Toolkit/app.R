## ============================================================
##  Cancer Registry Dashboard Builder  v2.0
##  Purpose: Configure & generate a cancer registry dashboard
##           tailored to any Small Island Developing State (SIDS)
##  Based on:  Barbados National Cancer Registry (BNR) Dashboard
## ============================================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)

# ── Design tokens ─────────────────────────────────────────────────────────────
MODERN_CSS <- "
/* ── Google Fonts ── */
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600;700&family=DM+Mono:wght@400;500&family=Fraunces:opsz,wght@9..144,300;9..144,600;9..144,700&display=swap');

/* ── Root tokens ── */
:root {
  --ink:       #0f1923;
  --ink-muted: #4a5568;
  --surface:   #f7f8fc;
  --card:      #ffffff;
  --border:    #e4e8f0;
  --teal:      #0d9488;
  --teal-dark: #0a7c72;
  --teal-glow: rgba(13,148,136,.12);
  --amber:     #f59e0b;
  --rose:      #e11d48;
  --indigo:    #4338ca;
  --radius:    12px;
  --shadow:    0 1px 3px rgba(0,0,0,.07), 0 4px 16px rgba(0,0,0,.06);
  --shadow-lg: 0 8px 32px rgba(0,0,0,.10);
}

/* ── Global resets ── */
* { box-sizing: border-box; }
body, .content-wrapper, .main-footer { background: var(--surface) !important; }
body { font-family: 'DM Sans', sans-serif !important; color: var(--ink) !important; }

/* ── Sidebar ── */
.main-sidebar, .left-side {
  background: var(--ink) !important;
  width: 260px !important;
}
.main-sidebar .sidebar { padding-top: 0 !important; }
.sidebar-menu > li > a {
  font-family: 'DM Sans', sans-serif !important;
  font-size: 13.5px !important;
  font-weight: 500 !important;
  color: #94a3b8 !important;
  padding: 11px 18px !important;
  border-left: 3px solid transparent !important;
  transition: all .18s ease !important;
  letter-spacing: .01em !important;
}
.sidebar-menu > li > a:hover,
.sidebar-menu > li.active > a {
  color: #ffffff !important;
  background: rgba(255,255,255,.06) !important;
  border-left-color: var(--teal) !important;
}
.sidebar-menu > li > a .fa { width: 20px; text-align: center; margin-right: 10px; }
.sidebar-brand {
  display: flex; align-items: center; gap: 10px;
  padding: 20px 18px 14px;
  border-bottom: 1px solid rgba(255,255,255,.08);
  margin-bottom: 8px;
}
.sidebar-brand-icon {
  width: 36px; height: 36px; border-radius: 8px;
  background: var(--teal);
  display: flex; align-items: center; justify-content: center;
  font-size: 16px; color: #fff;
}
.sidebar-brand-text { font-family: 'DM Sans',sans-serif; font-weight:700; font-size:14px; color:#fff; line-height:1.2; }
.sidebar-brand-sub  { font-size:11px; color:#64748b; font-weight:400; }
.sidebar-divider { border-top: 1px solid rgba(255,255,255,.07); margin: 8px 14px; }
.sidebar-section-label {
  font-size:10.5px; font-weight:700; letter-spacing:.1em; text-transform:uppercase;
  color:#475569; padding: 12px 18px 4px; display:block;
}
.step-badge {
  display: inline-flex; align-items:center; justify-content:center;
  width:20px; height:20px; border-radius:50%;
  background: rgba(13,148,136,.2); color: var(--teal);
  font-size:10px; font-weight:700; margin-right:8px; flex-shrink:0;
}
.sidebar-menu > li.active .step-badge { background:var(--teal); color:#fff; }

/* ── Header ── */
.main-header .navbar, .main-header .logo {
  background: var(--ink) !important;
  border-bottom: 1px solid rgba(255,255,255,.07) !important;
}
.main-header .logo { display:none !important; }
.main-header .navbar { margin-left: 260px !important; }
.navbar-custom-menu > .navbar-nav > li > a { color: #94a3b8 !important; }

/* ── Content wrapper ── */
.content-wrapper { margin-left: 260px !important; }
.content { padding: 24px !important; }

/* ── Modern cards ── */
.m-card {
  background: var(--card);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  box-shadow: var(--shadow);
  padding: 24px;
  margin-bottom: 20px;
}
.m-card-header {
  display: flex; align-items: center; gap: 10px;
  margin-bottom: 18px; padding-bottom: 14px;
  border-bottom: 1px solid var(--border);
}
.m-card-icon {
  width:36px; height:36px; border-radius:8px;
  display:flex; align-items:center; justify-content:center; font-size:15px; flex-shrink:0;
}
.m-card-icon.teal   { background:var(--teal-glow);          color:var(--teal); }
.m-card-icon.amber  { background:rgba(245,158,11,.12);       color:var(--amber); }
.m-card-icon.rose   { background:rgba(225,29,72,.10);        color:var(--rose); }
.m-card-icon.indigo { background:rgba(67,56,202,.10);        color:var(--indigo); }
.m-card-title { font-size:15px; font-weight:600; color:var(--ink); margin:0; }
.m-card-sub   { font-size:12px; color:var(--ink-muted); margin:2px 0 0; }

/* ── Step page header ── */
.page-header-bar {
  background: linear-gradient(135deg, var(--ink) 0%, #1e3a5f 100%);
  border-radius: var(--radius);
  padding: 24px 28px;
  margin-bottom: 24px;
  display: flex; align-items: center; gap: 16px;
  position: relative; overflow: hidden;
}
.page-header-bar::after {
  content:''; position:absolute; top:-40px; right:-40px;
  width:180px; height:180px; border-radius:50%;
  background: rgba(13,148,136,.08);
}
.page-header-bar::before {
  content:''; position:absolute; bottom:-60px; right:80px;
  width:120px; height:120px; border-radius:50%;
  background: rgba(13,148,136,.05);
}
.page-header-num {
  font-family:'Fraunces',serif; font-size:44px; font-weight:700;
  color: rgba(255,255,255,.15); line-height:1; flex-shrink:0;
}
.page-header-text h3 { font-family:'DM Sans',sans-serif; font-size:20px; font-weight:700; color:#fff; margin:0; }
.page-header-text p  { font-size:13px; color:rgba(255,255,255,.6); margin:4px 0 0; }

/* ── Form elements ── */
.form-control, .selectize-input {
  border: 1.5px solid var(--border) !important;
  border-radius: 8px !important;
  font-family:'DM Sans',sans-serif !important;
  font-size:13.5px !important;
  color: var(--ink) !important;
  background: var(--card) !important;
  box-shadow: none !important;
  transition: border-color .15s !important;
  height: 38px !important;
  padding: 7px 12px !important;
}
.form-control:focus { border-color: var(--teal) !important; outline:none !important; }
.selectize-input.focus { border-color:var(--teal) !important; box-shadow:0 0 0 3px var(--teal-glow) !important; }
label { font-size:12.5px !important; font-weight:600 !important; color:#374151 !important;
        letter-spacing:.01em; margin-bottom:5px !important; }
.help-block { font-size:12px !important; color:var(--ink-muted) !important; }
.shiny-input-container { margin-bottom:14px !important; }

/* ── Buttons ── */
.btn { border-radius:8px !important; font-family:'DM Sans',sans-serif !important; font-weight:600 !important; font-size:13.5px !important; transition:all .15s !important; }
.btn-primary { background:var(--teal) !important; border-color:var(--teal) !important; color:#fff !important; }
.btn-primary:hover { background:var(--teal-dark) !important; border-color:var(--teal-dark) !important; box-shadow:0 4px 12px rgba(13,148,136,.3) !important; }
.btn-default { background:#fff !important; border:1.5px solid var(--border) !important; color:var(--ink) !important; }
.btn-default:hover { border-color:var(--teal) !important; color:var(--teal) !important; }
.btn-success { background:#059669 !important; border-color:#059669 !important; color:#fff !important; }
.btn-info    { background:var(--indigo) !important; border-color:var(--indigo) !important; color:#fff !important; }
.btn-lg { padding:10px 22px !important; font-size:14px !important; }
.nav-btns { margin-top:24px; display:flex; gap:10px; }

/* ── Module toggle cards ── */
.mod-toggle {
  background:var(--card); border:1.5px solid var(--border); border-radius:10px;
  padding:14px 16px; margin-bottom:10px; cursor:pointer;
  transition:all .15s; display:flex; align-items:center; gap:14px;
}
.mod-toggle:hover { border-color:var(--teal); background:var(--teal-glow); }
.mod-toggle.active { border-color:var(--teal); background:var(--teal-glow); }
.mod-toggle .mod-icon { font-size:20px; flex-shrink:0; width:28px; text-align:center; }
.mod-toggle .mod-text .mod-name { font-weight:600; font-size:13.5px; margin:0; }
.mod-toggle .mod-text .mod-desc { font-size:11.5px; color:var(--ink-muted); margin:2px 0 0; }
.mod-toggle input[type=checkbox] { margin-left:auto; width:16px; height:16px; accent-color:var(--teal); flex-shrink:0; }

/* ── Colour swatches ── */
.swatch-row { display:flex; flex-wrap:wrap; gap:10px; margin-top:6px; }
.swatch { border-radius:8px; width:80px; height:44px; display:flex; align-items:center; justify-content:center; font-size:10px; font-weight:600; color:#fff; text-shadow:0 1px 3px rgba(0,0,0,.4); border:1px solid rgba(0,0,0,.1); }

/* ── Code preview ── */
.code-preview-wrap {
  background:#0f1923; border-radius:10px; overflow:hidden;
  border:1px solid rgba(255,255,255,.06);
}
.code-preview-bar {
  background:rgba(255,255,255,.04); padding:10px 16px;
  display:flex; align-items:center; gap:8px;
  border-bottom:1px solid rgba(255,255,255,.06);
}
.code-dot { width:10px; height:10px; border-radius:50%; }
pre.shiny-text-output {
  background:#0f1923 !important; color:#a8d8a8 !important;
  font-family:'DM Mono',monospace !important; font-size:12px !important;
  padding:16px !important; margin:0 !important; border:none !important;
  max-height:420px; overflow-y:auto !important;
}

/* ── Config summary cards ── */
.summary-pill {
  display:inline-block; background:var(--teal-glow); color:var(--teal);
  border-radius:20px; padding:3px 10px; font-size:12px; font-weight:600; margin:3px 2px;
}
.stat-mini { text-align:center; padding:16px 10px; }
.stat-mini .val { font-family:'Fraunces',serif; font-size:28px; font-weight:700; color:var(--ink); }
.stat-mini .lbl { font-size:11px; color:var(--ink-muted); font-weight:500; margin-top:2px; }

/* ── Section label ── */
.section-label {
  font-size:11px; font-weight:700; letter-spacing:.08em; text-transform:uppercase;
  color:var(--teal); margin:18px 0 8px; display:block;
}
.field-group {
  background:var(--surface); border:1px solid var(--border);
  border-radius:8px; padding:14px 16px; margin-bottom:12px;
}

/* ── Progress bar ── */
.wizard-progress { display:flex; align-items:center; margin-bottom:6px; padding:0 2px; }
.wp-step { display:flex; flex-direction:column; align-items:center; flex:1; }
.wp-dot { width:28px; height:28px; border-radius:50%; background:var(--border); color:var(--ink-muted); display:flex; align-items:center; justify-content:center; font-size:11px; font-weight:700; transition:all .2s; }
.wp-dot.done  { background:var(--teal); color:#fff; }
.wp-dot.active { background:var(--teal); color:#fff; box-shadow:0 0 0 4px var(--teal-glow); }
.wp-label { font-size:9.5px; color:var(--ink-muted); margin-top:4px; font-weight:500; white-space:nowrap; }
.wp-line  { flex:1; height:2px; background:var(--border); margin-bottom:14px; transition:background .2s; }
.wp-line.done { background:var(--teal); }

/* ── Home page ── */
.home-hero {
  background: linear-gradient(135deg, #0f1923 0%, #0d2137 50%, #0a1e32 100%);
  border-radius: 16px; padding: 52px 48px; position:relative; overflow:hidden;
  margin-bottom: 24px;
}
.home-hero::before {
  content:''; position:absolute; top:-80px; right:-80px; width:320px; height:320px;
  border-radius:50%; background:radial-gradient(circle, rgba(13,148,136,.18) 0%, transparent 70%);
}
.home-hero::after {
  content:''; position:absolute; bottom:-60px; left:20%; width:200px; height:200px;
  border-radius:50%; background:radial-gradient(circle, rgba(67,56,202,.12) 0%, transparent 70%);
}
.home-hero-tag {
  display:inline-flex; align-items:center; gap:6px;
  background:rgba(13,148,136,.15); border:1px solid rgba(13,148,136,.3);
  border-radius:20px; padding:5px 12px; font-size:12px; font-weight:600;
  color:var(--teal); margin-bottom:18px; letter-spacing:.03em;
}
.home-hero h1 {
  font-family:'Fraunces',serif; font-size:42px; font-weight:700;
  color:#ffffff; margin:0 0 14px; line-height:1.15;
}
.home-hero h1 span { color:var(--teal); }
.home-hero p {
  font-size:15.5px; color:rgba(255,255,255,.65); max-width:560px;
  line-height:1.7; margin:0 0 28px;
}
.hero-stats {
  display:flex; gap:32px; padding-top:24px;
  border-top:1px solid rgba(255,255,255,.08);
}
.hero-stat .n { font-family:'Fraunces',serif; font-size:30px; font-weight:700; color:#fff; }
.hero-stat .l { font-size:12px; color:rgba(255,255,255,.5); font-weight:500; margin-top:2px; }
.hero-cta { display:flex; gap:12px; flex-wrap:wrap; }
.btn-hero-primary {
  background:var(--teal) !important; border:none !important; color:#fff !important;
  padding:12px 24px !important; font-size:14px !important; border-radius:9px !important;
  font-weight:600 !important; box-shadow:0 4px 16px rgba(13,148,136,.35) !important;
}
.btn-hero-secondary {
  background:rgba(255,255,255,.08) !important; border:1px solid rgba(255,255,255,.15) !important;
  color:#fff !important; padding:12px 24px !important; font-size:14px !important;
  border-radius:9px !important; font-weight:600 !important;
}
.btn-hero-secondary:hover { background:rgba(255,255,255,.13) !important; }

/* ── Feature cards ── */
.feat-grid { display:grid; grid-template-columns:repeat(3,1fr); gap:16px; margin-bottom:24px; }
.feat-card {
  background:var(--card); border:1px solid var(--border); border-radius:var(--radius);
  padding:20px; transition:all .18s;
}
.feat-card:hover { border-color:var(--teal); box-shadow:var(--shadow-lg); transform:translateY(-2px); }
.feat-card-icon {
  width:42px; height:42px; border-radius:10px;
  display:flex; align-items:center; justify-content:center; font-size:18px; margin-bottom:14px;
}
.feat-card h4 { font-size:14px; font-weight:700; margin:0 0 6px; color:var(--ink); }
.feat-card p  { font-size:12.5px; color:var(--ink-muted); margin:0; line-height:1.6; }

/* ── How it works ── */
.how-step {
  display:flex; gap:16px; padding:18px 0;
  border-bottom:1px solid var(--border);
}
.how-step:last-child { border-bottom:none; }
.how-num {
  font-family:'Fraunces',serif; font-size:32px; font-weight:700;
  color:var(--teal); opacity:.4; line-height:1; flex-shrink:0; width:32px;
}
.how-step h4 { font-size:14px; font-weight:700; margin:0 0 4px; }
.how-step p  { font-size:12.5px; color:var(--ink-muted); margin:0; line-height:1.6; }

/* ── Module pills on home ── */
.module-pill {
  display:inline-flex; align-items:center; gap:6px;
  background:var(--card); border:1px solid var(--border); border-radius:8px;
  padding:8px 14px; margin:4px; font-size:13px; font-weight:500;
  transition:all .15s; cursor:default;
}
.module-pill:hover { border-color:var(--teal); color:var(--teal); }
.module-pill .dot { width:7px; height:7px; border-radius:50%; }

/* ── Checklist ── */
.checklist li {
  padding:5px 0; font-size:13.5px; display:flex; align-items:flex-start; gap:8px;
}
.checklist li::before {
  content:'✓'; color:var(--teal); font-weight:700; flex-shrink:0;
}

/* ── Scrollbar ── */
::-webkit-scrollbar { width:5px; height:5px; }
::-webkit-scrollbar-track { background:transparent; }
::-webkit-scrollbar-thumb { background:var(--border); border-radius:10px; }
"

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(MODERN_CSS)),
    tags$link(rel="stylesheet",
              href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
  ),
  
  # ── Layout: sidebar + main ─────────────────────────────────
  div(style="display:flex; min-height:100vh;",
      
      # ── Sidebar ────────────────────────────────────────────────
      tags$nav(style="width:260px; flex-shrink:0; background:#0f1923; position:fixed; top:0; left:0; height:100vh; overflow-y:auto; z-index:100;",
               
               # Brand
               div(class="sidebar-brand",
                   div(class="sidebar-brand-icon", icon("laptop-medical")),
                   div(div(class="sidebar-brand-text", "Dashboard Builder"),
                       div(class="sidebar-brand-sub",  "Registry Framework · SIDS"))
               ),
               
               # Navigation
               tags$ul(class="sidebar-menu", style="list-style:none; padding:0; margin:0;",
                       
                       # Home
                       tags$li(id="nav_home",
                               tags$a(href="#", onclick="navTo('home'); return false;",
                                      style="display:flex; align-items:center;",
                                      tags$i(class="fa fa-house"), " Home")),
                       
                       div(class="sidebar-divider"),
                       tags$span(class="sidebar-section-label", "Wizard Steps"),
                       
                       tags$li(id="nav_step1",
                               tags$a(href="#", onclick="navTo('step1'); return false;",
                                      style="display:flex; align-items:center;",
                                      tags$span(class="step-badge","1"),
                                      "Registry Info")),
                       tags$li(id="nav_step2",
                               tags$a(href="#", onclick="navTo('step2'); return false;",
                                      style="display:flex; align-items:center;",
                                      tags$span(class="step-badge","2"),
                                      "Data Files")),
                       tags$li(id="nav_step3",
                               tags$a(href="#", onclick="navTo('step3'); return false;",
                                      style="display:flex; align-items:center;",
                                      tags$span(class="step-badge","3"),
                                      "Column Mapping")),
                       tags$li(id="nav_step4",
                               tags$a(href="#", onclick="navTo('step4'); return false;",
                                      style="display:flex; align-items:center;",
                                      tags$span(class="step-badge","4"),
                                      "Modules")),
                       tags$li(id="nav_step5",
                               tags$a(href="#", onclick="navTo('step5'); return false;",
                                      style="display:flex; align-items:center;",
                                      tags$span(class="step-badge","5"),
                                      "Appearance")),
                       tags$li(id="nav_step6",
                               tags$a(href="#", onclick="navTo('step6'); return false;",
                                      style="display:flex; align-items:center;",
                                      tags$span(class="step-badge","6"),
                                      "Authentication")),
                       tags$li(id="nav_step7",
                               tags$a(href="#", onclick="navTo('step7'); return false;",
                                      style="display:flex; align-items:center;",
                                      tags$span(class="step-badge","7"),
                                      "Preview & Export"))
               ),
               
               # Footer
               div(style="position:absolute; bottom:0; left:0; right:0; padding:16px 18px; border-top:1px solid rgba(255,255,255,.07);",
                   div(style="font-size:11px; color:#475569; font-weight:500;",
                       icon("circle-info", style="margin-right:5px;"),
                       "Complete all steps to export")
               )
      ),
      
      # ── Main content ─────────────────────────────────────────
      div(style="margin-left:260px; flex:1; min-height:100vh;",
          div(style="padding:28px; max-width:1100px;",
              
              # JS for navigation
              tags$script(HTML("
        function navTo(tab) {
          Shiny.setInputValue('active_tab', tab, {priority:'event'});
          // update sidebar active states
          document.querySelectorAll('.sidebar-menu li').forEach(function(el){
            el.classList.remove('active');
          });
          var t = document.getElementById('nav_' + tab);
          if(t) t.classList.add('active');
        }
        // set home active on load
        document.addEventListener('DOMContentLoaded', function(){
          var h = document.getElementById('nav_home');
          if(h) h.classList.add('active');
        });
      ")),
              
              # ── Pages (hidden/shown) ─────────────────────────────────
              # HOME PAGE
              div(id="page_home",
                  
                  # Hero banner
                  div(class="home-hero",
                      div(class="home-hero-tag",
                          icon("globe", style="font-size:11px;"),
                          "Built for Small Island Developing States"),
                      h1("Cancer Registry", tags$br(), tags$span("Dashboard Builder")),
                      p("A step-by-step wizard that generates a fully customised R Shiny
               cancer surveillance dashboard — tailored to your registry's data,
               column names, modules, and branding. No coding required."),
                      div(class="hero-cta",
                          actionButton("hero_start", "Get Started →",
                                       class="btn btn-hero-primary", onclick="navTo('step1')"),
                          actionButton("hero_learn", "See How It Works ↓",
                                       class="btn btn-hero-secondary",
                                       onclick="document.getElementById('how-it-works').scrollIntoView({behavior:'smooth'})")),
                      div(class="hero-stats",
                          div(class="hero-stat", div(class="n","7"), div(class="l","Configuration Steps")),
                          div(class="hero-stat", div(class="n","10"), div(class="l","Analysis Modules")),
                          div(class="hero-stat", div(class="n","1"), div(class="l","Exported app.R File"))
                      )
                  ),
                  
                  # Feature cards
                  div(class="feat-grid",
                      div(class="feat-card",
                          div(class="feat-card-icon", style="background:rgba(13,148,136,.1); color:#0d9488;", icon("wand-magic-sparkles")),
                          tags$h4("No-Code Generation"),
                          tags$p("Fill in the wizard forms — the builder writes the entire Shiny app for you, ready to run.")),
                      div(class="feat-card",
                          div(class="feat-card-icon", style="background:rgba(245,158,11,.1); color:#f59e0b;", icon("table-columns")),
                          tags$h4("Flexible Column Mapping"),
                          tags$p("Your CSV columns map directly to dashboard variables — no renaming your data required.")),
                      div(class="feat-card",
                          div(class="feat-card-icon", style="background:rgba(67,56,202,.1); color:#4338ca;", icon("sliders")),
                          tags$h4("Modular Design"),
                          tags$p("Pick only the modules you need: Incidence, Mortality, Survival, Prevalence, Projections & more.")),
                      div(class="feat-card",
                          div(class="feat-card-icon", style="background:rgba(225,29,72,.1); color:#e11d48;", icon("palette")),
                          tags$h4("Full Branding Control"),
                          tags$p("Set your registry name, colours, logo, and contact details — the app reflects your identity.")),
                      div(class="feat-card",
                          div(class="feat-card-icon", style="background:rgba(13,148,136,.1); color:#0d9488;", icon("lock")),
                          tags$h4("Optional Authentication"),
                          tags$p("Protect sensitive data with a built-in login screen powered by shinyauthr and sodium.")),
                      div(class="feat-card",
                          div(class="feat-card-icon", style="background:rgba(67,56,202,.1); color:#4338ca;", icon("file-arrow-down")),
                          tags$h4("Instant Download"),
                          tags$p("Download app.R and a setup README — deploy to shinyapps.io or a local Shiny server."))
                  ),
                  
                  # How it works
                  div(id="how-it-works", class="m-card",
                      div(class="m-card-header",
                          div(class="m-card-icon teal", icon("list-check")),
                          div(tags$h3(class="m-card-title","How It Works"),
                              tags$p(class="m-card-sub","Seven steps from blank form to working dashboard"))),
                      div(class="how-step",
                          div(class="how-num","1"),
                          div(tags$h4("Registry Information"), tags$p("Enter your registry name, country, parent organisation, contact details, and the years covered by your incidence and mortality data."))),
                      div(class="how-step",
                          div(class="how-num","2"),
                          div(tags$h4("Data File Paths"), tags$p("Provide the relative file paths for your incidence CSV, mortality CSV, population Excel workbook, and (optionally) a parish / district shapefile."))),
                      div(class="how-step",
                          div(class="how-num","3"),
                          div(tags$h4("Column Mapping"), tags$p("Map every field in your CSVs — cancer site, sex, age, year, parish, vital status dates, basis of diagnosis — to the names used internally by the dashboard."))),
                      div(class="how-step",
                          div(class="how-num","4"),
                          div(tags$h4("Choose Modules"), tags$p("Toggle on or off each analysis module: Overview, Incidence, Mortality, Survival, Prevalence, Projections, Data Quality, and PowerPoint Reports."))),
                      div(class="how-step",
                          div(class="how-num","5"),
                          div(tags$h4("Appearance"), tags$p("Choose a dashboard skin, set a hex header colour, and customise the colour palette for charts, bars, and trend lines."))),
                      div(class="how-step",
                          div(class="how-num","6"),
                          div(tags$h4("Authentication"), tags$p("Optionally enable a login screen with a custom username and hashed password to protect the dashboard from unauthorised access."))),
                      div(class="how-step",
                          div(class="how-num","7"),
                          div(tags$h4("Preview & Export"), tags$p("Review a preview of the generated code, then download app.R and a README with setup instructions — your dashboard is ready to run.")))
                  ),
                  
                  # Modules overview
                  div(class="m-card",
                      div(class="m-card-header",
                          div(class="m-card-icon indigo", icon("th")),
                          div(tags$h3(class="m-card-title","Available Analysis Modules"))),
                      div(style="display:flex; flex-wrap:wrap;",
                          div(class="module-pill", div(class="dot", style="background:#0d9488;"), "📊 Overview"),
                          div(class="module-pill", div(class="dot", style="background:#4338ca;"), "📈 Incidence"),
                          div(class="module-pill", div(class="dot", style="background:#e11d48;"), "💀 Mortality"),
                          div(class="module-pill", div(class="dot", style="background:#f59e0b;"), "❤️ Survival"),
                          div(class="module-pill", div(class="dot", style="background:#0d9488;"), "👤 Prevalence"),
                          div(class="module-pill", div(class="dot", style="background:#4338ca;"), "📉 Projections"),
                          div(class="module-pill", div(class="dot", style="background:#6b7280;"), "✅ Data Quality"),
                          div(class="module-pill", div(class="dot", style="background:#6b7280;"), "📄 PPT Reports"),
                          div(class="module-pill", div(class="dot", style="background:#6b7280;"), "ℹ️ About"),
                          div(class="module-pill", div(class="dot", style="background:#6b7280;"), "✉️ Contact")
                      )
                  ),
                  
                  # Data requirements
                  div(class="m-card",
                      div(class="m-card-header",
                          div(class="m-card-icon amber", icon("database")),
                          div(tags$h3(class="m-card-title","What Data You Need"),
                              tags$p(class="m-card-sub","Required input files for the generated dashboard"))),
                      fluidRow(
                        column(6,
                               tags$b(style="font-size:13px; color:#374151;", icon("file-csv"), " Incidence CSV"),
                               tags$ul(class="checklist", style="margin-top:8px; padding-left:0; list-style:none;",
                                       tags$li("Cancer site / IARC classification"),
                                       tags$li("Sex, age at diagnosis, diagnosis year"),
                                       tags$li("Parish or sub-national area"),
                                       tags$li("Incidence date (YYYYMMDD or DD MMM YYYY)"),
                                       tags$li("Vital status, date of death, last contact date"),
                                       tags$li("Basis of diagnosis & ICD topography code")
                               )
                        ),
                        column(6,
                               tags$b(style="font-size:13px; color:#374151;", icon("file-csv"), " Mortality CSV"),
                               tags$ul(class="checklist", style="margin-top:8px; padding-left:0; list-style:none;",
                                       tags$li("Cancer site, sex, age at death, death year"),
                                       tags$li("Parish or sub-national area")
                               ),
                               tags$br(),
                               tags$b(style="font-size:13px; color:#374151;", icon("file-excel"), " Population Excel (WPP)"),
                               tags$ul(class="checklist", style="margin-top:8px; padding-left:0; list-style:none;",
                                       tags$li("One sheet per year"),
                                       tags$li("Columns: age5 (1–18), sex, pop_wpp")
                               )
                        )
                      )
                  ),
                  
                  # Footer
                  div(style="text-align:center; padding:20px 0 10px; color:var(--ink-muted); font-size:12px;",
                      "Cancer Registry Dashboard Builder · Designed for Small Island Developing States",
                      tags$br(),
                      "Based on the Barbados National Cancer Registry (BNR) dashboard framework")
              ),
              
              # ── STEP 1 ─────────────────────────────────────────────
              div(id="page_step1", style="display:none;",
                  div(class="page-header-bar",
                      div(class="page-header-num","1"),
                      div(class="page-header-text",
                          tags$h3("Registry Information"),
                          tags$p("Enter basic details about your cancer registry and country."))),
                  fluidRow(
                    column(6,
                           div(class="m-card",
                               div(class="m-card-header",
                                   div(class="m-card-icon teal", icon("building")),
                                   div(tags$h3(class="m-card-title","Organisation Details"))),
                               textInput("reg_name",    "Registry Name",    value="", placeholder="e.g. Jamaica Cancer Registry"),
                               textInput("country",     "Country / Territory", value="", placeholder="e.g. Jamaica"),
                               textInput("org_full",    "Parent Organisation", value="", placeholder="e.g. Ministry of Health"),
                               textInput("reg_address", "Address",          value="", placeholder="Street, City, Country"),
                               textInput("reg_tel",     "Telephone",        value="", placeholder="+1-876-..."),
                               textInput("reg_email",   "Email",            value="", placeholder="registry@example.com"),
                               textInput("reg_website", "Website",          value="", placeholder="https://...")
                           )
                    ),
                    column(6,
                           div(class="m-card",
                               div(class="m-card-header",
                                   div(class="m-card-icon amber", icon("calendar-range")),
                                   div(tags$h3(class="m-card-title","Data Coverage"))),
                               tags$span(class="section-label","Incidence Period"),
                               fluidRow(
                                 column(6, numericInput("inc_year_start","Start Year",  value=2013, min=1900, max=2100, step=1)),
                                 column(6, numericInput("inc_year_end",  "End Year",    value=2022, min=1900, max=2100, step=1))
                               ),
                               tags$span(class="section-label","Mortality Period"),
                               fluidRow(
                                 column(6, numericInput("mort_year_start","Start Year", value=2008, min=1900, max=2100, step=1)),
                                 column(6, numericInput("mort_year_end",  "End Year",   value=2024, min=1900, max=2100, step=1))
                               ),
                               tags$span(class="section-label","Analysis Settings"),
                               textInput("prevalence_date", "Prevalence Reference Date (YYYY-MM-DD)", value="2022-12-31"),
                               numericInput("proj_end_year","Projection End Year", value=2027, min=2020, max=2100, step=1),
                               tags$hr(style="border-color:var(--border);"),
                               fileInput("logo_file","Upload Registry Logo (PNG/JPG — optional)",
                                         accept=c("image/png","image/jpeg"))
                           )
                    )
                  ),
                  div(class="nav-btns",
                      actionButton("go_step2","Next: Data Files →", class="btn btn-primary btn-lg"))
              ),
              
              # ── STEP 2 ─────────────────────────────────────────────
              div(id="page_step2", style="display:none;",
                  div(class="page-header-bar",
                      div(class="page-header-num","2"),
                      div(class="page-header-text",
                          tags$h3("Data Files"),
                          tags$p("Provide file paths — relative to your app.R — for each data source."))),
                  fluidRow(
                    column(6,
                           div(class="m-card",
                               div(class="m-card-header",
                                   div(class="m-card-icon teal", icon("file-csv")),
                                   div(tags$h3(class="m-card-title","Incidence & Mortality CSVs"))),
                               textInput("inc_path",  "Incidence CSV path",  value="data/incidence.csv"),
                               tags$p(style="font-size:12px; color:var(--ink-muted); margin-top:-8px;","One row per incident cancer case."),
                               textInput("mort_path", "Mortality CSV path",  value="data/mortality.csv"),
                               tags$p(style="font-size:12px; color:var(--ink-muted); margin-top:-8px;","One row per cancer death.")
                           )
                    ),
                    column(6,
                           div(class="m-card",
                               div(class="m-card-header",
                                   div(class="m-card-icon amber", icon("file-excel")),
                                   div(tags$h3(class="m-card-title","Population & Geographic Data"))),
                               textInput("pop_path","WPP Population Excel path", value="data/WPP.xlsx"),
                               tags$p(style="font-size:12px; color:var(--ink-muted); margin-top:-8px;","One sheet per year: columns age5, sex, pop_wpp."),
                               tags$hr(style="border-color:var(--border);"),
                               checkboxInput("use_parish_pop","Include sub-national geographic maps", value=TRUE),
                               conditionalPanel("input.use_parish_pop",
                                                textInput("parish_pop_path","Parish population CSV",  value="data/parish_population_by_age.csv"),
                                                textInput("shapefile_path", "Shapefile path (.shp)",  value="data/Country_Regions.shp"),
                                                textInput("shapefile_name_col","Name column in shapefile", value="NAME_1")
                               )
                           )
                    )
                  ),
                  div(class="nav-btns",
                      actionButton("go_step1b","← Back", class="btn btn-default"),
                      actionButton("go_step3","Next: Column Mapping →", class="btn btn-primary btn-lg"))
              ),
              
              # ── STEP 3 ─────────────────────────────────────────────
              div(id="page_step3", style="display:none;",
                  div(class="page-header-bar",
                      div(class="page-header-num","3"),
                      div(class="page-header-text",
                          tags$h3("Column Mapping"),
                          tags$p("Map your CSV column names to the internal variable names used by the dashboard."))),
                  fluidRow(
                    column(6,
                           div(class="m-card",
                               div(class="m-card-header",
                                   div(class="m-card-icon teal", icon("table-columns")),
                                   div(tags$h3(class="m-card-title","Incidence Dataset"))),
                               tags$span(class="section-label","Case identifiers"),
                               div(class="field-group",
                                   fluidRow(
                                     column(6, textInput("col_inc_site",  "Cancer site",    value="siteiarc")),
                                     column(6, textInput("col_inc_sex",   "Sex",            value="sex"))
                                   ),
                                   fluidRow(
                                     column(6, textInput("col_inc_age",   "Age at diagnosis", value="age")),
                                     column(6, textInput("col_inc_year",  "Diagnosis year",   value="dxyr"))
                                   ),
                                   textInput("col_inc_parish","Parish / district", value="parish")
                               ),
                               tags$span(class="section-label","Dates & Survival Follow-up"),
                               div(class="field-group",
                                   textInput("col_inc_dx_date",  "Incidence date column",   value="IncidenceDate"),
                                   fluidRow(
                                     column(6, textInput("col_inc_deceased","Deceased status",   value="deceased")),
                                     column(6, textInput("col_inc_dod",     "Date of death",     value="dod"))
                                   ),
                                   textInput("col_inc_dlc","Date of last contact", value="dlc")
                               ),
                               tags$span(class="section-label","Data Quality Fields"),
                               div(class="field-group",
                                   fluidRow(
                                     column(4, textInput("col_inc_basis","Basis",        value="basis")),
                                     column(4, textInput("col_inc_primarysite","Primary site code", value="primarysite")),
                                     column(4, textInput("col_inc_top","Topography code", value="top"))
                                   ),
                                   textInput("col_oandu_label","Label for 'Other & Unspecified' site",
                                             value="Other and unspecified (O&U)")
                               )
                           )
                    ),
                    column(6,
                           div(class="m-card",
                               div(class="m-card-header",
                                   div(class="m-card-icon rose", icon("skull-crossbones")),
                                   div(tags$h3(class="m-card-title","Mortality Dataset"))),
                               tags$span(class="section-label","Death record fields"),
                               div(class="field-group",
                                   fluidRow(
                                     column(6, textInput("col_mort_site","Cancer site",  value="siteiarc")),
                                     column(6, textInput("col_mort_sex", "Sex",          value="sex"))
                                   ),
                                   fluidRow(
                                     column(6, textInput("col_mort_age", "Age at death", value="age")),
                                     column(6, textInput("col_mort_year","Death year",   value="dodyear"))
                                   ),
                                   textInput("col_mort_parish","Parish / district", value="parish")
                               ),
                               div(class="m-card", style="background:var(--surface); border-color:var(--border); box-shadow:none; margin-top:12px;",
                                   tags$b(style="font-size:13px;", icon("circle-info", style="color:var(--teal);"), " Tips"),
                                   tags$ul(style="font-size:12.5px; color:var(--ink-muted); padding-left:16px; margin:8px 0 0;",
                                           tags$li("Sex values must be lowercase: ", tags$code("male"), " / ", tags$code("female")),
                                           tags$li("Parish names should match those in the shapefile NAME column"),
                                           tags$li("Death/last-contact dates should be in DD-MMM-YYYY format (e.g. 15-Jan-2022)")
                                   )
                               )
                           )
                    )
                  ),
                  div(class="nav-btns",
                      actionButton("go_step2b","← Back", class="btn btn-default"),
                      actionButton("go_step4","Next: Modules →", class="btn btn-primary btn-lg"))
              ),
              
              # ── STEP 4 ─────────────────────────────────────────────
              div(id="page_step4", style="display:none;",
                  div(class="page-header-bar",
                      div(class="page-header-num","4"),
                      div(class="page-header-text",
                          tags$h3("Dashboard Modules"),
                          tags$p("Choose which analysis modules to include in the generated dashboard."))),
                  fluidRow(
                    column(7,
                           div(class="m-card",
                               div(class="m-card-header",
                                   div(class="m-card-icon teal", icon("th")),
                                   div(tags$h3(class="m-card-title","Select Modules"))),
                               div(class="mod-toggle",
                                   div(class="mod-icon","📊"),
                                   div(class="mod-text",
                                       tags$p(class="mod-name","Overview / Key Statistics"),
                                       tags$p(class="mod-desc","Total cases, deaths, age distribution, cases by year")),
                                   checkboxInput("mod_overview",NULL,value=TRUE)),
                               div(class="mod-toggle",
                                   div(class="mod-icon","📈"),
                                   div(class="mod-text",
                                       tags$p(class="mod-name","Incidence"),
                                       tags$p(class="mod-desc","ASIR, Crude Rate, Cumulative Incidence by site & sex")),
                                   checkboxInput("mod_incidence",NULL,value=TRUE)),
                               div(class="mod-toggle",
                                   div(class="mod-icon","💀"),
                                   div(class="mod-text",
                                       tags$p(class="mod-name","Mortality"),
                                       tags$p(class="mod-desc","ASMR, Crude Mortality Rate, geographic map")),
                                   checkboxInput("mod_mortality",NULL,value=TRUE)),
                               div(class="mod-toggle",
                                   div(class="mod-icon","❤️"),
                                   div(class="mod-text",
                                       tags$p(class="mod-name","Survival"),
                                       tags$p(class="mod-desc","Kaplan-Meier curves, 1/3/5-year survival gauges")),
                                   checkboxInput("mod_survival",NULL,value=TRUE)),
                               div(class="mod-toggle",
                                   div(class="mod-icon","👤"),
                                   div(class="mod-text",
                                       tags$p(class="mod-name","Prevalence"),
                                       tags$p(class="mod-desc","Survivors count, prevalence rate, age-sex breakdown")),
                                   checkboxInput("mod_prevalence",NULL,value=TRUE)),
                               div(class="mod-toggle",
                                   div(class="mod-icon","📉"),
                                   div(class="mod-text",
                                       tags$p(class="mod-name","Projections"),
                                       tags$p(class="mod-desc","Negative-binomial model, 95% prediction intervals")),
                                   checkboxInput("mod_projection",NULL,value=TRUE)),
                               div(class="mod-toggle",
                                   div(class="mod-icon","✅"),
                                   div(class="mod-text",
                                       tags$p(class="mod-name","Data Quality Indicators"),
                                       tags$p(class="mod-desc","MV%, DCO%, ill-defined site percentage")),
                                   checkboxInput("mod_quality",NULL,value=TRUE)),
                               div(class="mod-toggle",
                                   div(class="mod-icon","📄"),
                                   div(class="mod-text",
                                       tags$p(class="mod-name","PowerPoint Report Export"),
                                       tags$p(class="mod-desc","Auto-generate a slide deck summary of all findings")),
                                   checkboxInput("mod_reports",NULL,value=TRUE)),
                               div(class="mod-toggle",
                                   div(class="mod-icon","ℹ️"),
                                   div(class="mod-text",
                                       tags$p(class="mod-name","About / Help Page"),
                                       tags$p(class="mod-desc","Module explanations and usage tips")),
                                   checkboxInput("mod_about",NULL,value=TRUE)),
                               div(class="mod-toggle",
                                   div(class="mod-icon","✉️"),
                                   div(class="mod-text",
                                       tags$p(class="mod-name","Contact Us Page"),
                                       tags$p(class="mod-desc","Registry contact details page")),
                                   checkboxInput("mod_contact",NULL,value=TRUE))
                           )
                    ),
                    column(5,
                           div(class="m-card",
                               div(class="m-card-header",
                                   div(class="m-card-icon amber", icon("circle-info")),
                                   div(tags$h3(class="m-card-title","Module Dependencies"))),
                               tags$ul(style="font-size:13px; color:var(--ink-muted); padding-left:16px; line-height:2;",
                                       tags$li(tags$b("Incidence"), " → requires incidence CSV + population Excel"),
                                       tags$li(tags$b("Mortality"), " → requires mortality CSV + population Excel"),
                                       tags$li(tags$b("Survival"),  " → requires date-of-last-contact & date-of-death fields"),
                                       tags$li(tags$b("Prevalence"), " → also uses survival follow-up columns"),
                                       tags$li(tags$b("Projections"), " → uses incidence data only"),
                                       tags$li(tags$b("Geographic maps"), " → requires shapefile & parish population CSV"),
                                       tags$li(tags$b("PPT Reports"), " → requires ", tags$code("officer"), " + ", tags$code("rvg"), " packages")
                               )
                           )
                    )
                  ),
                  div(class="nav-btns",
                      actionButton("go_step3b","← Back", class="btn btn-default"),
                      actionButton("go_step5","Next: Appearance →", class="btn btn-primary btn-lg"))
              ),
              
              # ── STEP 5 ─────────────────────────────────────────────
              div(id="page_step5", style="display:none;",
                  div(class="page-header-bar",
                      div(class="page-header-num","5"),
                      div(class="page-header-text",
                          tags$h3("Appearance"),
                          tags$p("Customise the visual theme and chart colour palette."))),
                  fluidRow(
                    column(6,
                           div(class="m-card",
                               div(class="m-card-header",
                                   div(class="m-card-icon teal", icon("palette")),
                                   div(tags$h3(class="m-card-title","Colour Settings"))),
                               tags$span(class="section-label","Dashboard Theme"),
                               selectInput("dashboard_skin","Shinydashboard Skin",
                                           choices=c("blue","black","purple","green","red","yellow"),
                                           selected="blue"),
                               textInput("header_color","Header background colour (hex)", value="#253494"),
                               tags$span(class="section-label","Chart Colours"),
                               fluidRow(
                                 column(6, textInput("female_color","Female series", value="#DD1C77")),
                                 column(6, textInput("male_color","Male series",     value="#3182BD"))
                               ),
                               fluidRow(
                                 column(6, textInput("bar_color_inc","Incidence bars",  value="darkgreen")),
                                 column(6, textInput("bar_color_mort","Mortality bars", value="darkred"))
                               ),
                               textInput("proj_color","Projection line", value="#253494")
                           )
                    ),
                    column(6,
                           div(class="m-card",
                               div(class="m-card-header",
                                   div(class="m-card-icon indigo", icon("eye")),
                                   div(tags$h3(class="m-card-title","Live Preview"))),
                               uiOutput("color_preview")
                           )
                    )
                  ),
                  div(class="nav-btns",
                      actionButton("go_step4b","← Back", class="btn btn-default"),
                      actionButton("go_step6","Next: Authentication →", class="btn btn-primary btn-lg"))
              ),
              
              # ── STEP 6 ─────────────────────────────────────────────
              div(id="page_step6", style="display:none;",
                  div(class="page-header-bar",
                      div(class="page-header-num","6"),
                      div(class="page-header-text",
                          tags$h3("Authentication"),
                          tags$p("Optionally protect the dashboard with a login screen."))),
                  fluidRow(
                    column(6,
                           div(class="m-card",
                               div(class="m-card-header",
                                   div(class="m-card-icon teal", icon("lock")),
                                   div(tags$h3(class="m-card-title","Login Settings"))),
                               checkboxInput("use_auth","Enable password protection (shinyauthr + sodium)", value=TRUE),
                               conditionalPanel("input.use_auth",
                                                textInput("auth_user","Username", value="registry_user"),
                                                passwordInput("auth_pass","Password", value="change_me_123"),
                                                div(style="background:rgba(245,158,11,.1); border:1px solid rgba(245,158,11,.3); border-radius:8px; padding:12px 14px; font-size:12.5px; color:#92400e;",
                                                    icon("triangle-exclamation", style="color:#f59e0b;"),
                                                    " Change this password before deploying. It will be hashed using sodium.")
                               )
                           )
                    ),
                    column(6,
                           div(class="m-card",
                               div(class="m-card-header",
                                   div(class="m-card-icon amber", icon("shield-halved")),
                                   div(tags$h3(class="m-card-title","How Authentication Works"))),
                               tags$ul(style="font-size:13px; color:var(--ink-muted); padding-left:16px; line-height:2;",
                                       tags$li("Uses ", tags$code("shinyauthr"), " for the login UI and server logic"),
                                       tags$li("Password is stored as a sodium hash — never plain text"),
                                       tags$li("Unauthenticated users see only the login panel"),
                                       tags$li("All data processing happens server-side"),
                                       tags$li("If disabled, the app is open to anyone with the URL")
                               )
                           )
                    )
                  ),
                  div(class="nav-btns",
                      actionButton("go_step5b","← Back", class="btn btn-default"),
                      actionButton("go_step7","Next: Preview & Export →", class="btn btn-primary btn-lg"))
              ),
              
              # ── STEP 7 ─────────────────────────────────────────────
              div(id="page_step7", style="display:none;",
                  div(class="page-header-bar",
                      div(class="page-header-num","7"),
                      div(class="page-header-text",
                          tags$h3("Preview & Export"),
                          tags$p("Review your configuration, inspect the generated code, and download."))),
                  
                  # Summary strip
                  div(class="m-card",
                      div(class="m-card-header",
                          div(class="m-card-icon teal", icon("clipboard-check")),
                          div(tags$h3(class="m-card-title","Configuration Summary"))),
                      uiOutput("config_summary")
                  ),
                  
                  # Code preview
                  div(class="m-card",
                      div(class="m-card-header",
                          div(class="m-card-icon indigo", icon("code")),
                          div(tags$h3(class="m-card-title","Generated app.R — Preview"),
                              tags$p(class="m-card-sub","First 120 lines shown"))),
                      div(class="code-preview-wrap",
                          div(class="code-preview-bar",
                              div(class="code-dot", style="background:#ff5f57;"),
                              div(class="code-dot", style="background:#febc2e;"),
                              div(class="code-dot", style="background:#28c840;"),
                              div(style="margin-left:8px; font-size:11px; color:#64748b; font-family:'DM Mono',monospace;","app.R")
                          ),
                          verbatimTextOutput("code_preview")
                      )
                  ),
                  
                  # Download buttons
                  div(class="m-card",
                      div(class="m-card-header",
                          div(class="m-card-icon teal", icon("download")),
                          div(tags$h3(class="m-card-title","Download Files"))),
                      fluidRow(
                        column(4,
                               downloadButton("download_code","⬇  Download app.R",
                                              class="btn btn-success btn-lg",
                                              style="width:100%; margin-bottom:8px;")),
                        column(4,
                               downloadButton("download_readme","⬇  Download README.md",
                                              class="btn btn-info btn-lg",
                                              style="width:100%; margin-bottom:8px;")),
                        column(4,
                               actionButton("go_step6b","← Back to Settings", class="btn btn-default btn-lg",
                                            style="width:100%; margin-bottom:8px;"))
                      )
                  ),
                  
                  # Next steps
                  div(class="m-card",
                      div(class="m-card-header",
                          div(class="m-card-icon amber", icon("rocket")),
                          div(tags$h3(class="m-card-title","Next Steps"))),
                      tags$ol(style="font-size:13.5px; color:var(--ink-muted); padding-left:18px; line-height:2.2;",
                              tags$li("Place ", tags$code("app.R"), " in a new folder, e.g. ", tags$code("my_registry_dashboard/")),
                              tags$li("Create a ", tags$code("data/"), " sub-folder and add your CSV / Excel / shapefile data files"),
                              tags$li("If you uploaded a logo, save it as ", tags$code("www/logo.png"), " next to app.R"),
                              tags$li("In R, install missing packages:", tags$br(),
                                      tags$code('install.packages(c("shiny","shinydashboard","shinyauthr","shinyjs","sodium","dplyr","ggplot2","DT","lubridate","survival","plotly","tidyr","purrr","readxl","qcc","officer","rvg","sf","leaflet","viridis","MASS"))')),
                              tags$li("Run ", tags$code("shiny::runApp()"), " from the dashboard folder"),
                              tags$li("For online hosting use ", tags$a("shinyapps.io", href="https://www.shinyapps.io", target="_blank"),
                                      " or a self-hosted Shiny Server")
                      )
                  )
              )
              
          ))
  )
)


# ── SERVER ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Tab navigation ─────────────────────────────────────────
  tabs <- c("home","step1","step2","step3","step4","step5","step6","step7")
  
  show_page <- function(tab) {
    lapply(tabs, function(t) shinyjs::hide(paste0("page_",t)))
    shinyjs::show(paste0("page_",tab))
  }
  
  observeEvent(input$active_tab, { show_page(input$active_tab) }, ignoreInit=FALSE)
  
  # Button nav
  btn_nav <- function(btn, to) {
    observeEvent(input[[btn]], {
      show_page(to)
      session$sendCustomMessage("updateNav", to)
    })
  }
  btn_nav("go_step2",  "step2"); btn_nav("go_step1b","step1")
  btn_nav("go_step3",  "step3"); btn_nav("go_step2b","step2")
  btn_nav("go_step4",  "step4"); btn_nav("go_step3b","step3")
  btn_nav("go_step5",  "step5"); btn_nav("go_step4b","step4")
  btn_nav("go_step6",  "step6"); btn_nav("go_step5b","step5")
  btn_nav("go_step7",  "step7"); btn_nav("go_step6b","step6")
  
  # JS handler to update sidebar active state from R
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler('updateNav', function(tab){
      document.querySelectorAll('.sidebar-menu li').forEach(function(el){
        el.classList.remove('active');
      });
      var t = document.getElementById('nav_' + tab);
      if(t) t.classList.add('active');
    });
  ")))
  
  # ── Colour preview ─────────────────────────────────────────
  output$color_preview <- renderUI({
    swatches <- list(
      list(input$header_color,   "Header"),
      list(input$female_color,   "Female"),
      list(input$male_color,     "Male"),
      list(input$bar_color_inc,  "Incidence"),
      list(input$bar_color_mort, "Mortality"),
      list(input$proj_color,     "Projection")
    )
    div(class="swatch-row",
        lapply(swatches, function(s) {
          div(class="swatch", style=paste0("background:",s[[1]],";"), s[[2]])
        })
    )
  })
  
  # ── Config summary ─────────────────────────────────────────
  output$config_summary <- renderUI({
    modules <- c(
      if (isTRUE(input$mod_overview))   "Overview",
      if (isTRUE(input$mod_incidence))  "Incidence",
      if (isTRUE(input$mod_mortality))  "Mortality",
      if (isTRUE(input$mod_survival))   "Survival",
      if (isTRUE(input$mod_prevalence)) "Prevalence",
      if (isTRUE(input$mod_projection)) "Projections",
      if (isTRUE(input$mod_quality))    "Data Quality",
      if (isTRUE(input$mod_reports))    "Reports"
    )
    fluidRow(
      column(3,
             div(class="stat-mini",
                 div(class="val", if(nchar(input$reg_name)>0) input$reg_name else "—"),
                 div(class="lbl","Registry Name"))
      ),
      column(2,
             div(class="stat-mini",
                 div(class="val", if(nchar(input$country)>0) input$country else "—"),
                 div(class="lbl","Country"))
      ),
      column(2,
             div(class="stat-mini",
                 div(class="val", paste0(input$inc_year_start,"–",input$inc_year_end)),
                 div(class="lbl","Incidence Period"))
      ),
      column(2,
             div(class="stat-mini",
                 div(class="val", paste0(input$mort_year_start,"–",input$mort_year_end)),
                 div(class="lbl","Mortality Period"))
      ),
      column(3,
             div(class="stat-mini",
                 div(class="val", length(modules)),
                 div(class="lbl","Modules Selected"))
      )
    )
  })
  
  # ── Code generation ─────────────────────────────────────────
  generate_code <- reactive({ build_dashboard_code(input) })
  
  output$code_preview <- renderText({
    lines <- strsplit(generate_code(), "\n")[[1]]
    paste(head(lines, 120), collapse="\n")
  })
  
  output$download_code <- downloadHandler(
    filename = "app.R",
    content  = function(file) writeLines(generate_code(), file)
  )
  output$download_readme <- downloadHandler(
    filename = "README.md",
    content  = function(file) writeLines(generate_readme(input), file)
  )
}


# ── Code-generation functions (unchanged from v1) ──────────────────────────────
build_dashboard_code <- function(cfg) {
  q <- function(x) paste0('"', x, '"')
  
  mod_inc  <- isTRUE(cfg$mod_incidence)
  mod_mort <- isTRUE(cfg$mod_mortality)
  mod_surv <- isTRUE(cfg$mod_survival)
  mod_prev <- isTRUE(cfg$mod_prevalence)
  mod_proj <- isTRUE(cfg$mod_projection)
  mod_qual <- isTRUE(cfg$mod_quality)
  mod_rep  <- isTRUE(cfg$mod_reports)
  mod_geo  <- isTRUE(cfg$use_parish_pop)
  mod_auth <- isTRUE(cfg$use_auth)
  
  libs <- c("shiny","shinydashboard","shinyjs","dplyr","ggplot2","DT",
            "lubridate","plotly","tidyr","purrr","readxl","viridis")
  if (mod_auth)  libs <- c(libs,"shinyauthr","sodium")
  if (mod_surv || mod_prev) libs <- c(libs,"survival")
  if (mod_qual)  libs <- c(libs,"qcc")
  if (mod_rep)   libs <- c(libs,"officer","rvg")
  if (mod_geo)   libs <- c(libs,"sf","leaflet")
  libs <- unique(libs)
  lib_lines <- paste0('library(',libs,')', collapse='\n')
  
  header <- paste0(
    '## ============================================================
##  ', cfg$reg_name, ' — Cancer Registry Dashboard
##  Country:  ', cfg$country, '
##  Generated by the Cancer Registry Dashboard Builder
##  Generated: ', Sys.Date(), '
## ============================================================

', lib_lines, '\n',
    if (mod_proj) '\nif (!requireNamespace("MASS", quietly=TRUE)) install.packages("MASS")\n' else ""
  )
  
  auth_block <- if (mod_auth) paste0(
    '\nuser_base <- tibble::tibble(
  user     = ', q(cfg$auth_user), ',
  password = sodium::password_store(', q(cfg$auth_pass), ')
)\n') else ""
  
  data_block <- paste0(
    '\ndata           <- read.csv(', q(cfg$inc_path),  ', stringsAsFactors=FALSE)
mortality_data <- read.csv(', q(cfg$mort_path), ', stringsAsFactors=FALSE)
years    <- ', cfg$mort_year_start, ':', cfg$mort_year_end, '
pop_data <- purrr::map2_dfr(seq_along(years), years, ~{
  readxl::read_excel(', q(cfg$pop_path), ', sheet=.x) %>% mutate(year=.y)
})
pop_data$sex <- ifelse(pop_data$sex==1,"male","female")
')
  if (mod_geo) {
    data_block <- paste0(data_block,
                         '\nparish_shapefile <- sf::st_read(', q(cfg$shapefile_path), ')
sf::st_crs(parish_shapefile) <- 3857
parish_shapefile <- sf::st_transform(parish_shapefile, 4326)
parish_shapefile <- sf::st_make_valid(parish_shapefile)
parish_pop_raw   <- read.csv(', q(cfg$parish_pop_path), ', stringsAsFactors=FALSE)\n')
  }
  
  col_map <- paste0(
    '\n# Column mapping constants
COL_INC_SITE     <- ', q(cfg$col_inc_site), '
COL_INC_SEX      <- ', q(cfg$col_inc_sex), '
COL_INC_AGE      <- ', q(cfg$col_inc_age), '
COL_INC_YEAR     <- ', q(cfg$col_inc_year), '
COL_INC_PARISH   <- ', q(cfg$col_inc_parish), '
COL_INC_DX_DATE  <- ', q(cfg$col_inc_dx_date), '
COL_INC_DECEASED <- ', q(cfg$col_inc_deceased), '
COL_INC_DOD      <- ', q(cfg$col_inc_dod), '
COL_INC_DLC      <- ', q(cfg$col_inc_dlc), '
COL_INC_BASIS    <- ', q(cfg$col_inc_basis), '
COL_INC_PRIMARY  <- ', q(cfg$col_inc_primarysite), '
COL_INC_TOP      <- ', q(cfg$col_inc_top), '
OANDU_LABEL      <- ', q(cfg$col_oandu_label), '
COL_MORT_SITE    <- ', q(cfg$col_mort_site), '
COL_MORT_SEX     <- ', q(cfg$col_mort_sex), '
COL_MORT_AGE     <- ', q(cfg$col_mort_age), '
COL_MORT_YEAR    <- ', q(cfg$col_mort_year), '
COL_MORT_PARISH  <- ', q(cfg$col_mort_parish), '
INC_YEAR_START   <- ', cfg$inc_year_start, '
INC_YEAR_END     <- ', cfg$inc_year_end, '
MORT_YEAR_START  <- ', cfg$mort_year_start, '
MORT_YEAR_END    <- ', cfg$mort_year_end, '
PREVALENCE_DATE  <- ', q(cfg$prevalence_date), '
PROJ_END_YEAR    <- ', cfg$proj_end_year, '
COLOR_FEMALE     <- ', q(cfg$female_color), '
COLOR_MALE       <- ', q(cfg$male_color), '
COLOR_INC_BAR    <- ', q(cfg$bar_color_inc), '
COLOR_MORT_BAR   <- ', q(cfg$bar_color_mort), '
COLOR_PROJ       <- ', q(cfg$proj_color), '
HEADER_COLOR     <- ', q(cfg$header_color), '
who_weights <- c(8860,8690,8590,8470,8220,7930,7610,7150,
                 6590,6040,5380,4550,3720,2960,2210,1520,900,600)/100000\n')
  
  helpers <- '
parse_incidence <- function(x) {
  x <- as.character(x)
  sapply(x, function(y) {
    if (is.na(y)||y=="") return(NA)
    tryCatch({
      if (grepl("^\\\\d{8}$",y))                      return(format(lubridate::ymd(y),"%Y-%m-%d"))
      if (grepl("^\\\\d{1,2} \\\\w{3} \\\\d{4}$",y)) return(format(lubridate::dmy(y),"%Y-%m-%d"))
      NA
    }, warning=function(w) NA, error=function(e) NA)
  }, USE.NAMES=FALSE)
}

compute_asir <- function(cancer_data, pop_data, who_weights, site, sex_group) {
  cancer_df <- if (site=="All cancers")
    cancer_data %>% filter(.data[[COL_INC_SITE]]!=OANDU_LABEL)
  else
    cancer_data %>% filter(.data[[COL_INC_SITE]]==site)
  if (sex_group!="Both") cancer_df <- cancer_df %>% filter(.data[[COL_INC_SEX]]==tolower(sex_group))
  if (nrow(cancer_df)==0) return(data.frame(year=integer(),asir=numeric()))
  cancer_df <- cancer_df %>%
    mutate(age_group=as.numeric(cut(.data[[COL_INC_AGE]],breaks=c(seq(0,85,5),Inf),labels=1:18,right=FALSE))) %>%
    filter(!is.na(age_group)) %>%
    group_by(year=.data[[COL_INC_YEAR]],age_group) %>% summarise(counts=n(),.groups="drop")
  full_df <- tidyr::expand_grid(year=unique(cancer_data[[COL_INC_YEAR]]),age_group=1:18) %>%
    left_join(cancer_df,by=c("year","age_group")) %>% mutate(counts=coalesce(counts,0L))
  pop_df <- if (sex_group=="Both")
    pop_data %>% group_by(year,age5) %>% summarise(pop=sum(pop_wpp),.groups="drop") %>% rename(age_group=age5)
  else
    pop_data %>% filter(sex==tolower(sex_group)) %>% dplyr::select(year,age_group=age5,pop=pop_wpp)
  full_df %>% left_join(pop_df,by=c("year","age_group")) %>%
    mutate(pop=coalesce(pop,0),age_rate=ifelse(pop>0,counts/pop*100000,0)) %>%
    group_by(year) %>% summarise(asir=sum(age_rate*who_weights[age_group]),.groups="drop")
}

compute_asmr <- function(mortality_data, pop_data, who_weights, site, sex_group) {
  mort_df <- if (site=="All cancers")
    mortality_data %>% filter(.data[[COL_MORT_SITE]]!=OANDU_LABEL)
  else
    mortality_data %>% filter(.data[[COL_MORT_SITE]]==site)
  if (sex_group!="Both") mort_df <- mort_df %>% filter(.data[[COL_MORT_SEX]]==sex_group)
  if (nrow(mort_df)==0) return(data.frame(year=integer(),asmr=numeric()))
  mort_df <- mort_df %>%
    mutate(age_group=as.numeric(cut(.data[[COL_MORT_AGE]],breaks=c(seq(0,85,5),Inf),labels=1:18,right=FALSE))) %>%
    filter(!is.na(age_group)) %>%
    group_by(year=.data[[COL_MORT_YEAR]],age_group) %>% summarise(counts=n(),.groups="drop")
  full_df <- tidyr::expand_grid(year=unique(mortality_data[[COL_MORT_YEAR]]),age_group=1:18) %>%
    left_join(mort_df,by=c("year","age_group")) %>% mutate(counts=coalesce(counts,0L))
  pop_df <- if (sex_group=="Both")
    pop_data %>% group_by(year,age5) %>% summarise(pop=sum(pop_wpp),.groups="drop") %>% rename(age_group=age5)
  else
    pop_data %>% filter(sex==tolower(sex_group)) %>% dplyr::select(year,age_group=age5,pop=pop_wpp)
  full_df %>% left_join(pop_df,by=c("year","age_group")) %>%
    mutate(pop=coalesce(pop,0),age_rate=ifelse(pop>0,counts/pop*100000,0)) %>%
    group_by(year) %>% summarise(asmr=sum(age_rate*who_weights[age_group]),.groups="drop")
}
'

# Build sidebar items
si <- ""
if (isTRUE(cfg$mod_overview))  si <- paste0(si,'\n        menuSubItem("Overview",   tabName="home",       icon=icon("tachometer-alt")),')
if (mod_inc)   si <- paste0(si,'\n        menuSubItem("Incidence",  tabName="incidence",  icon=icon("chart-bar")),')
if (mod_mort)  si <- paste0(si,'\n        menuSubItem("Mortality",  tabName="mortality",  icon=icon("skull-crossbones")),')
if (mod_surv)  si <- paste0(si,'\n        menuSubItem("Survival",   tabName="survival",   icon=icon("heartbeat")),')
if (mod_prev)  si <- paste0(si,'\n        menuSubItem("Prevalence", tabName="prevalence", icon=icon("user-check")),')
if (mod_proj)  si <- paste0(si,'\n        menuSubItem("Projection", tabName="projection", icon=icon("chart-line")),')
if (mod_qual)  si <- paste0(si,'\n        menuSubItem("Data Quality",tabName="data_quality",icon=icon("check-circle")),')
if (mod_rep)   si <- paste0(si,'\n        menuSubItem("Reports",    tabName="reports",    icon=icon("file-alt")),')

logo_line <- if (!is.null(cfg$logo_file))
  'img(src="logo.png",height=40,style="margin-right:8px;vertical-align:middle;"),' else ''

ui_code <- paste0(
  '\nui <- dashboardPage(
  skin=', q(cfg$dashboard_skin), ',
  dashboardHeader(
    title=div(', logo_line, 'span(', q(paste0(cfg$reg_name," Dashboard")), ',style="font-size:20px;font-weight:700;")),
    titleWidth="100%"
  ),
  dashboardSidebar(
    sidebarMenu(id="tabs",
      menuItem("Home",    tabName="home_landing", icon=icon("home")),
      menuItem("Modules", icon=icon("th"), startExpanded=FALSE,', si, '
      ),',
  if (isTRUE(cfg$mod_about))   '\n      menuItem("About",    tabName="about",   icon=icon("question-circle")),' else "",
  if (isTRUE(cfg$mod_contact)) '\n      menuItem("Contact",  tabName="contact", icon=icon("envelope")),' else "",
  '    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML(paste0("
      .main-header .logo { background-color: ", HEADER_COLOR, " !important; color:#fff !important; }
    ")))),
    tabItems(
      tabItem(tabName="home_landing",
        h2(icon("home")," Welcome to the ', cfg$reg_name, ' Dashboard"),
        p("Data coverage: Incidence ', cfg$inc_year_start, '--', cfg$inc_year_end, '  |  Mortality ', cfg$mort_year_start, '--', cfg$mort_year_end, '")
      )',
  if (isTRUE(cfg$mod_overview)) paste0(',
      tabItem(tabName="home",
        h3("Overview"),
        fluidRow(valueBoxOutput("total_cases",width=6), valueBoxOutput("total_deaths",width=6)),
        fluidRow(
          box(title="Cases Over Years",plotly::plotlyOutput("cases_over_years"),width=6),
          box(title="Top Cancer Sites",DT::dataTableOutput("top_sites"),width=6)
        )
      )') else "",
  if (mod_inc) ',
      tabItem(tabName="incidence",
        h3("Incidence Analysis"),
        fluidRow(
          box(width=3,
            selectInput("inc_site_sel","Cancer Site:",choices=NULL),
            selectInput("inc_sex_sel","Sex:",choices=c("Both","Male","Female")),
            radioButtons("inc_metric","Metric:",choices=c("ASIR","Crude Rate"),selected="ASIR")),
          box(width=9,plotly::plotlyOutput("inc_rate_plot",height=400))
        )
      )' else "",
  if (mod_mort) ',
      tabItem(tabName="mortality",
        h3("Mortality Analysis"),
        fluidRow(
          box(width=3,
            selectInput("mort_site_sel","Cancer Site:",choices=NULL),
            selectInput("mort_sex_sel","Sex:",choices=c("Both","Male","Female")),
            radioButtons("mort_metric","Metric:",choices=c("ASMR","Crude Rate"),selected="ASMR")),
          box(width=9,plotly::plotlyOutput("mort_rate_plot",height=400))
        )
      )' else "",
  if (mod_proj) ',
      tabItem(tabName="projection",
        h3("Cancer Case Projections"),
        fluidRow(box(width=12,plotly::plotlyOutput("proj_all_cancers"))),
        fluidRow(box(width=12,plotly::plotlyOutput("proj_top5_sites"))),
        fluidRow(box(width=12,DT::dataTableOutput("proj_summary_table")))
      )' else "",
  if (mod_qual) ',
      tabItem(tabName="data_quality",
        h3("Data Quality"),
        fluidRow(
          valueBoxOutput("dq_mv",width=4),
          valueBoxOutput("dq_dco",width=4),
          valueBoxOutput("dq_ill_def",width=4)
        )
      )' else "",
  if (mod_rep) ',
      tabItem(tabName="reports",
        h3("Generate Report"),
        p("Download a PowerPoint summary of key findings."),
        downloadButton("dl_pptx","Download PowerPoint",class="btn btn-primary btn-lg")
      )' else "",
  '
    )
  )
)\n')

server_code <- paste0(
  '\nserver <- function(input, output, session) {\n',
  if (mod_auth) '
  credentials <- shinyauthr::loginServer(id="login",data=user_base,
    user_col=user,pwd_col=password,sodium_hashed=TRUE)
' else "",
  '
  output$total_cases <- renderValueBox({',
  if (mod_auth) '\n    req(credentials()$user_auth)' else "",
  '
    valueBox(format(nrow(data),big.mark=","),
             paste0("Total Cases (", INC_YEAR_START, "--", INC_YEAR_END, ")"),
             icon=icon("user-plus"), color="blue")
  })
  output$total_deaths <- renderValueBox({',
  if (mod_auth) '\n    req(credentials()$user_auth)' else "",
  '
    valueBox(format(nrow(mortality_data),big.mark=","),
             paste0("Total Deaths (", MORT_YEAR_START, "--", MORT_YEAR_END, ")"),
             icon=icon("skull-crossbones"), color="red")
  })
  output$cases_over_years <- plotly::renderPlotly({',
  if (mod_auth) '\n    req(credentials()$user_auth)' else "",
  '
    d <- data %>% group_by(year=.data[[COL_INC_YEAR]]) %>% summarise(cases=n(),.groups="drop")
    plotly::plot_ly(d,x=~year,y=~cases,type="bar",marker=list(color=COLOR_INC_BAR)) %>%
      plotly::layout(xaxis=list(title="Year"),yaxis=list(title="Cases"))
  })
  output$top_sites <- DT::renderDataTable({',
  if (mod_auth) '\n    req(credentials()$user_auth)' else "",
  '
    data %>% filter(.data[[COL_INC_SITE]]!=OANDU_LABEL) %>%
      count(.data[[COL_INC_SITE]],sort=TRUE) %>% head(10) %>%
      rename(`Cancer Site`=1,Cases=n)
  }, options=list(pageLength=10,searching=FALSE))
',
  if (mod_inc) paste0('
  observe({
    sites <- c("All cancers",sort(unique(data[[COL_INC_SITE]][data[[COL_INC_SITE]]!=OANDU_LABEL])))
    updateSelectInput(session,"inc_site_sel",choices=sites)
  })
  output$inc_rate_plot <- plotly::renderPlotly({',
                      if (mod_auth) '\n    req(credentials()$user_auth)' else "",
                      '
    site <- input$inc_site_sel; sex <- input$inc_sex_sel; met <- input$inc_metric
    if(is.null(site)||site=="") return(plotly::plotly_empty())
    d <- if(met=="ASIR") compute_asir(data,pop_data,who_weights,site,sex)
         else { counts_df <- data %>%
             { if(site=="All cancers") filter(.,data[[COL_INC_SITE]]!=OANDU_LABEL)
               else filter(.,data[[COL_INC_SITE]]==site) } %>%
             group_by(year=.data[[COL_INC_YEAR]]) %>% summarise(counts=n(),.groups="drop")
           pop_df <- if(sex=="Both") pop_data %>% group_by(year) %>% summarise(pop=sum(pop_wpp),.groups="drop")
                     else pop_data %>% filter(.data[["sex"]]==tolower(sex)) %>% group_by(year) %>% summarise(pop=sum(pop_wpp),.groups="drop")
           counts_df %>% left_join(pop_df,by="year") %>%
             mutate(crude_rate=ifelse(coalesce(pop,0)>0,counts/pop*100000,0)) %>%
             dplyr::select(year,crude_rate) }
    if(is.null(d)||nrow(d)==0) return(plotly::plotly_empty())
    y_col <- names(d)[2]
    plotly::plot_ly(d,x=~year,y=~get(y_col),type="scatter",mode="lines+markers",
                    line=list(color=COLOR_PROJ)) %>%
      plotly::layout(xaxis=list(title="Year"),yaxis=list(title=paste(met,"per 100,000")),title=paste(met,"-",site))
  })
') else "",
  if (mod_proj) paste0('
  project_site <- function(df, site_label, proj_years=(INC_YEAR_END+1):PROJ_END_YEAR) {
    counts_df <- df %>% group_by(year=.data[[COL_INC_YEAR]]) %>%
      summarise(cases=n(),.groups="drop") %>% arrange(year)
    if(nrow(counts_df)<3) return(NULL)
    fit_df <- data.frame(year=counts_df$year,cases=counts_df$cases)
    all_yrs <- data.frame(year=c(counts_df$year,proj_years))
    pred <- tryCatch({
      mod <- MASS::glm.nb(cases~year,data=fit_df,control=glm.control(maxit=200))
      p   <- predict(mod,newdata=all_yrs,type="link",se.fit=TRUE)
      list(fit=exp(p$fit),lo=exp(p$fit-1.96*p$se.fit),hi=exp(p$fit+1.96*p$se.fit))
    }, error=function(e){
      mod <- lm(cases~year,data=fit_df)
      p   <- predict(mod,newdata=all_yrs,interval="prediction",level=0.95)
      list(fit=pmax(p[,"fit"],0),lo=pmax(p[,"lwr"],0),hi=pmax(p[,"upr"],0))
    })
    data.frame(site=site_label,year=all_yrs$year,
               cases=c(counts_df$cases,rep(NA,length(proj_years))),
               fit=pred$fit,lo=pred$lo,hi=pred$hi,
               period=c(rep("Observed",nrow(counts_df)),rep("Projected",length(proj_years))))
  }
  proj_data <- reactive({
    obs <- data %>% filter(.data[[COL_INC_SITE]]!=OANDU_LABEL)
    all_proj <- project_site(obs,"All Cancers")
    top5 <- obs %>% count(.data[[COL_INC_SITE]]) %>% arrange(desc(n)) %>% head(5) %>% pull(1)
    list(all=all_proj, top5=purrr::map_dfr(top5,~project_site(obs %>% filter(.data[[COL_INC_SITE]]==.x),.x)))
  })
  output$proj_all_cancers <- plotly::renderPlotly({',
                       if (mod_auth) '\n    req(credentials()$user_auth)' else "",
                       '
    d <- proj_data()$all; if(is.null(d)) return(plotly::plotly_empty())
    obs <- d %>% filter(period=="Observed"); proj <- d %>% filter(period=="Projected")
    plotly::plot_ly() %>%
      plotly::add_ribbons(data=proj,x=~year,ymin=~lo,ymax=~hi,fillcolor="rgba(37,52,148,0.15)",
                          line=list(color="transparent"),name="95% PI") %>%
      plotly::add_lines(data=d,x=~year,y=~fit,line=list(color=COLOR_PROJ,dash="dot"),name="Fitted") %>%
      plotly::add_markers(data=obs,x=~year,y=~cases,marker=list(color=COLOR_PROJ,size=8),name="Observed") %>%
      plotly::add_markers(data=proj,x=~year,y=~fit,marker=list(color="#e74c3c",size=8,symbol="diamond"),name="Projected") %>%
      plotly::layout(xaxis=list(title="Year"),yaxis=list(title="Cases"),hovermode="x unified")
  })
  output$proj_summary_table <- DT::renderDataTable({',
                       if (mod_auth) '\n    req(credentials()$user_auth)' else "",
                       '
    bind_rows(proj_data()$all,proj_data()$top5) %>% filter(period=="Projected") %>%
      mutate(`Projected Cases`=round(fit),`95% PI Lower`=round(lo),`95% PI Upper`=round(hi)) %>%
      dplyr::select(Site=site,Year=year,`Projected Cases`,`95% PI Lower`,`95% PI Upper`)
  },options=list(pageLength=15,searching=FALSE),rownames=FALSE)
') else "",
  if (mod_qual) paste0('
  dq <- reactive({
    data %>% summarise(
      n         = n(),
      mv_count  = sum(grepl("Hx|Cytology|Lab|Haem",.data[[COL_INC_BASIS]],ignore.case=TRUE),na.rm=TRUE),
      dco_count = sum(.data[[COL_INC_BASIS]]=="DCO",na.rm=TRUE),
      ill_count = sum(grepl("C76|C80|UNKNOWN",.data[[COL_INC_PRIMARY]],ignore.case=TRUE),na.rm=TRUE)
    ) %>% mutate(mv_pct=round(mv_count/n*100,1),dco_pct=round(dco_count/n*100,1),ill_pct=round(ill_count/n*100,1))
  })
  output$dq_mv      <- renderValueBox(valueBox(paste0(dq()$mv_pct,"%"), "Microscopic Verification",icon=icon("microscope"), color="green"))
  output$dq_dco     <- renderValueBox(valueBox(paste0(dq()$dco_pct,"%"),"Death Certificate Only",  icon=icon("certificate"),color="orange"))
  output$dq_ill_def <- renderValueBox(valueBox(paste0(dq()$ill_pct,"%"),"Ill-Defined Sites",       icon=icon("question"),  color="red"))
') else "",
  if (mod_rep) paste0('
  output$dl_pptx <- downloadHandler(
    filename=function() paste0("', gsub(" ","_",cfg$reg_name), '_Report_",Sys.Date(),".pptx"),
    content=function(file){
      ppt <- officer::read_pptx() %>%
        officer::add_slide(layout="Title Slide",master="Office Theme") %>%
        officer::ph_with(value=', q(paste0(cfg$reg_name," — Cancer Registry Report")), ',
                         location=officer::ph_location_type(type="ctrTitle")) %>%
        officer::ph_with(value=paste("Generated:",Sys.Date()),
                         location=officer::ph_location_type(type="subTitle"))
      print(ppt,target=file)
    }
  )
') else "",
  '
}

shiny::shinyApp(ui=ui, server=server)
')

paste0(header, auth_block, data_block, col_map, helpers, ui_code, server_code)
}

generate_readme <- function(cfg) {
  modules <- c(
    if (isTRUE(cfg$mod_overview))   "Overview",
    if (isTRUE(cfg$mod_incidence))  "Incidence",
    if (isTRUE(cfg$mod_mortality))  "Mortality",
    if (isTRUE(cfg$mod_survival))   "Survival",
    if (isTRUE(cfg$mod_prevalence)) "Prevalence",
    if (isTRUE(cfg$mod_projection)) "Projections",
    if (isTRUE(cfg$mod_quality))    "Data Quality",
    if (isTRUE(cfg$mod_reports))    "Reports"
  )
  paste0(
    '# ', cfg$reg_name, ' — Cancer Registry Dashboard

**Country:** ', cfg$country, '
**Generated:** ', Sys.Date(), '
**Framework:** Cancer Registry Dashboard Builder for SIDS

## Modules
', paste0("- ", modules, collapse="\n"), '

## Data Coverage
- Incidence: ', cfg$inc_year_start, '–', cfg$inc_year_end, '
- Mortality:  ', cfg$mort_year_start, '–', cfg$mort_year_end, '
- Prevalence date: ', cfg$prevalence_date, '

## Setup

### Folder Structure
```
my_dashboard/
├── app.R
├── www/logo.png       # optional
└── data/
    ├── ', basename(cfg$inc_path), '
    ├── ', basename(cfg$mort_path), '
    └── ', basename(cfg$pop_path), '
```

### Install Packages
```r
install.packages(c(
  "shiny","shinydashboard","shinyauthr","shinyjs","sodium",
  "dplyr","ggplot2","DT","lubridate","survival",
  "plotly","tidyr","purrr","readxl","qcc",
  "officer","rvg","sf","leaflet","viridis","MASS"
))
```

### Run
```r
setwd("my_dashboard")
shiny::runApp()
```

## Authentication
', if (isTRUE(cfg$use_auth))
  paste0("Enabled. Username: `", cfg$auth_user, "`. Change the password before deploying.")
  else "Disabled.", '

## Contact
', cfg$reg_name, '
', cfg$reg_address, '
Tel: ', cfg$reg_tel, '
Email: ', cfg$reg_email, '
Website: ', cfg$reg_website
  )
}

shinyApp(ui = ui, server = server)
