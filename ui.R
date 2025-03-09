library(shiny) 
library(shinyhelper) 
library(data.table) 
library(Matrix) 
library(DT) 
library(magrittr) 
library(plotly)
library(dplyr)
library(shinydashboard)
library(shinythemes)
library(shinyBS) # For enhanced tooltips
library(shinyjs) # For enhanced UI interactions

# Load configuration data
sc1conf = readRDS("sc1conf.rds")
sc1def = readRDS("sc1def.rds")

### Start UI code 
shinyUI(
  navbarPage(
    id = "main_navbar",
    title = "",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinytheme("flatly"),
    # Add custom CSS to control plot resizing and enhance UI
    header = tagList(
      # Enable shinyjs
      useShinyjs(),
      
      tags$head(
        tags$style(HTML("
          /* Control plot resizing */
          .shiny-plot-output {
            max-width: 100%;
            margin: 0 auto;
          }
          
          /* Fix plot containers to prevent stretching */
          .plot-container {
            width: 100%;
            max-width: 1000px;
            margin: 0 auto;
          }
          
          /* Card styling for control panels */
          .control-card {
            background-color: white;
            border-radius: 8px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            padding: 15px;
            margin-bottom: 15px;
          }
          
          /* Enhance buttons */
          .action-button {
            border-radius: 4px;
            transition: all 0.3s;
          }
          .action-button:hover {
            box-shadow: 0 2px 5px rgba(0,0,0,0.2);
          }
              /* Add these new styles for subtle animations */
    .control-card {
      background-color: white;
      border-radius: 8px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      padding: 15px;
      margin-bottom: 15px;
      transition: all 0.3s ease; /* Add this for smooth transitions */
    }
    
    .control-card:hover {
      transform: translateY(-5px);
      box-shadow: 0 10px 20px rgba(0,0,0,0.1);
    }
    
    /* Optional: Add subtle animation to action buttons too */
    .action-button {
      border-radius: 4px;
      transition: all 0.3s;
    }
    
    .action-button:hover {
      box-shadow: 0 2px 5px rgba(0,0,0,0.2);
      transform: translateY(-2px);
    }
    
        /* Modern loading spinner for plots */
    .plot-container {
      position: relative;
      min-height: 200px; /* Ensure there's space for the spinner */
    }
    
    .plot-container.loading::before {
      content: '';
      position: absolute;
      top: 50%;
      left: 50%;
      width: 40px;
      height: 40px;
      margin-top: -20px;
      margin-left: -20px;
      border: 4px solid rgba(30, 58, 95, 0.1);
      border-radius: 50%;
      border-top: 4px solid #1e3a5f;
      animation: spin 1s linear infinite;
      z-index: 10;
    }
    
    .plot-container.loading::after {
      content: '';
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background: rgba(255, 255, 255, 0.7);
      z-index: 5;
    }
    
    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }
          
          /* Breadcrumb styling */
          .breadcrumb {
            background-color: #f8f9fa;
            border-radius: 4px;
            padding: 8px 15px;
            margin-bottom: 20px;
          }
          .breadcrumb-item {
            display: inline;
          }
          .breadcrumb-item+.breadcrumb-item:before {
            content: '/';
            padding: 0 5px;
            color: #6c757d;
          }
          .breadcrumb-item.active {
            color: #6c757d;
          }
          
          /* Loading indicator */
          .loading-indicator {
            position: fixed;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            background-color: rgba(255, 255, 255, 0.8);
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 0 15px rgba(0,0,0,0.2);
            z-index: 9999;
          }
          
          /* Help system */
          .help-tooltip {
            color: #1e3a5f;
            margin-left: 5px;
            cursor: pointer;
          }
          
          /* Consistent spacing */
          .section-title {
            margin-top: 20px;
            margin-bottom: 15px;
            color: #1e3a5f;
          }
          
          /* Visual hierarchy */
          .primary-action {
            background-color: #1e3a5f !important;
            color: white !important;
            font-weight: bold;
          }
          
          /* Collapsible sidebar */
          .sidebar-toggle {
            position: fixed;
            left: 0;
            top: 50%;
            background: #1e3a5f;
            color: white;
            padding: 10px;
            border-radius: 0 5px 5px 0;
            z-index: 100;
          }
          								/* Style for the entire navbar */
.navbar-default {
  background-color: #f8f8f8;
  border-color: #e7e7e7;
}

/* Style for the navbar links */
.navbar-default .navbar-nav > li > a {
  color: #555;
}

/* Style for active navbar item */
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus {
  color: white;
  background-color: #1e3a5f;
}
        "))
      ),
      
      # Loading indicator
      div(id = "loading-indicator", class = "loading-indicator", style = "display: none;",
          h4("Loading..."),
          img(src = "spinner.gif", height = "50px")
      ),
      
      # Top level dataset selection - appears in all views
      div(
        style = "padding: 10px 15px; background-color: #1e3a5f; width: 100%; border-bottom: 1px solid #ddd;",
        fluidRow(
          column(
            6,
            div(style = "color: white;",
                div(
                  class = "dropdown",
                  tags$button(
                    class = "btn btn-primary dropdown-toggle",
                    type = "button", 
                    id = "datasetDropdown",
                    "Dataset Selection",
                    tags$span(class = "caret")
                  ),
                  tags$ul(
                    class = "dropdown-menu",
                    tags$li(radioButtons("datasetChoice", "", 
                                       choices = c("Gene Expression Data" = "gene_expression", 
                                                 "Chromatin Accessibility" = "chromatin_accessibility"),
                                       selected = "gene_expression",
                                       inline = FALSE))
                  )
                )
            )
          ),
          column(6, align = "right",
                 div(class = "btn-group",
                     conditionalPanel(
                       condition = "input.main_navbar !== 'home'",
                       actionButton("returnToHome", "Return to Home", icon = icon("home"), 
                                    style = "margin-top: 5px; color: white; background-color: #2c4f7c; border: black;")
                     ),
                     # Session management buttons
                     actionButton("saveSession", "Save Session", icon = icon("save"), 
                                  style = "margin-top: 11px; margin-left: 5px; color: white; background-color: #2c4f7c; border: black;"),
                     actionButton("loadSession", "Load Session", icon = icon("upload"), 
                                  style = "margin-top: 11px; margin-left: 5px; color: white; background-color: #2c4f7c; border: black;")
                 )
          )
        )
      )
    ),
    # Add to the header section
tags$head(
    tags$script(HTML("
    $(document).on('shiny:outputinvalidated', function(event) {
      // Check if the output is a plot
      if(event.name.includes('oup1') || event.name.includes('oup2') || 
         event.name.includes('plot') || event.name.includes('oup')) {
        // Find the plot container and add loading class
        $('#' + event.name).closest('.plot-container').addClass('loading');
      }
    });
    
    $(document).on('shiny:value', function(event) {
      // Check if the output is a plot
      if(event.name.includes('oup1') || event.name.includes('oup2') || 
         event.name.includes('plot') || event.name.includes('oup')) {
        // Find the plot container and remove loading class
        $('#' + event.name).closest('.plot-container').removeClass('loading');
      }
    });
    ")),

# Add this CSS to your tags$head section in ui.R
# This will create custom scrollbars that work across most modern browsers

tags$style(HTML("
  /* Custom scrollbars */
  ::-webkit-scrollbar {
    width: 8px;
    height: 8px; /* Also style horizontal scrollbars */
  }
  
  ::-webkit-scrollbar-track {
    background: #f1f1f1;
    border-radius: 10px;
  }
  
  ::-webkit-scrollbar-thumb {
    background: #1e3a5f;
    border-radius: 10px;
  }
  
  ::-webkit-scrollbar-thumb:hover {
    background: #4682b4;
    cursor: pointer;
  }
  
  /* Firefox scrollbar styling (works in newer versions) */
  * {
    scrollbar-width: thin;
    scrollbar-color: #1e3a5f #f1f1f1;
  }
  
  /* Make sure DataTables scrollbars match the custom style */
  .dataTables_wrapper .dataTables_scrollBody::-webkit-scrollbar {
    width: 8px;
    height: 8px;
  }
  
  .dataTables_wrapper .dataTables_scrollBody::-webkit-scrollbar-track {
    background: #f1f1f1;
    border-radius: 10px;
  }
  
  .dataTables_wrapper .dataTables_scrollBody::-webkit-scrollbar-thumb {
    background: #1e3a5f;
    border-radius: 10px;
  }
  
  /* Style scrollbars in specific containers if needed */
  .control-card .stats-container::-webkit-scrollbar {
    width: 6px; /* Slightly smaller for nested containers */
    height: 6px;
  }
  
  .control-card .stats-container::-webkit-scrollbar-thumb {
    background: #4682b4; /* Lighter blue for contrast */
  }
")),

  tags$script("
  Shiny.addCustomMessageHandler('downloadData', function(data) {
    var link = document.createElement('a');
    link.href = 'data:' + data.contentType + ';base64,' + data.content;
    link.download = data.filename;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  });
"),
),
    ### Home/Landing Page
    tabPanel("Home", 
             value = "home",
             div(
               style = "padding-top: 60px; text-align: center;",
               # Background image with overlay
               tags$img(
                 style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-image: url('download(1).jpeg'); background-size: cover; background-position: center; opacity: 0.15; z-index: -1;"
               ),
               
               h1("Single-Cell Database", style = "font-size: 36px; margin-bottom: 20px; color: #1e3a5f;"),
               p("abstract and database intro goes here", 
                 style = "font-size: 18px; margin-bottom: 20px;"),
               p("Authors: blahblahbalhablh, link to publication. (2025)", 
                 style = "font-style: italic; margin-bottom: 40px;"),
               
               # Help system intro
# Replace the help system intro and dataset selection dropdown section with this new side-by-side layout

# Here's the updated version that ensures equal box size and consistent blue background
# Replace the existing help intro and dataset selection with:

div(
  style = "max-width: 1000px; margin: 0 auto 50px auto;",
  fluidRow(
    column(
      width = 6,
      div(
        style = "height: 250px;", # Set fixed height to ensure equality
        wellPanel(
          style = "background-color: #f0f7ff; border-radius: 8px; border-left: 4px solid #1e3a5f; padding: 15px; height: 100%;",
          h4("Getting Started", style = "color: #1e3a5f; margin-top: 0; text-align: center;"),
          div(
            style = "margin-bottom: 15px;",
            p("This app provides interactive visualizations for single-cell RNA sequencing data. Select a dataset type and explore different visualization options through the navigation cards.", 
              style = "text-align: center;")
          ),
          div(
            style = "text-align: center; margin-top: 20px;", # Center button and add margin
            actionButton("showTutorial", "Take a Tour", icon = icon("question-circle"), 
                        class = "btn-info", style = "min-width: 150px;")
          )
        )
      )
    ),
    column(
      width = 6,
      div(
        style = "height: 250px;", # Set fixed height to ensure equality
        wellPanel(
          style = "background-color: #f0f7ff; border-radius: 8px; border-left: 4px solid #1e3a5f; padding: 15px; height: 100%;",
          h4("Select Dataset Type", style = "color: #1e3a5f; margin-top: 0; text-align: center;"),
          div(
            style = "margin-top: 50px;", # Add margin to push content down for vertical centering
            p("Choose data type to explore:", style = "text-align: center; margin-bottom: 15px;"),
            div(
              style = "max-width: 400px; margin: 0 auto;", # Center the select input
              selectInput("homePageDataset", NULL, # Remove label as we're using the p tag above
                        choices = c("Gene Expression Data" = "gene_expression",
                                  "Chromatin Accessibility" = "chromatin_accessibility"),
                        selected = "gene_expression")
            )
          )
        )
      )
    )
  )
),
               
               # Navigation cards
               div(
                 style = "margin: 10px auto; max-width: 1200px; display: flex; flex-wrap: wrap; justify-content: center;",
                 
                 # Cell Info / Gene Expr card - only visible when gene_expression is selected
                 conditionalPanel(
                   condition = "input.homePageDataset == 'gene_expression'",
                   div(
                     class = "well",
                     style = "text-align: center; padding: 25px; margin: 15px; width: 260px; height: 300px; cursor: pointer; background-color: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); transition: transform 0.3s, box-shadow 0.3s;",
                     onclick = "Shiny.setInputValue('nav_selection', 'cellinfo');",
                     onmouseover = "this.style.transform='translateY(-5px)'; this.style.boxShadow='0 5px 15px rgba(0,0,0,0.2)';",
                     onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 2px 10px rgba(0,0,0,0.1)';",
                     icon("chart-line", "fa-3x", style = "color: #1e3a5f; margin-bottom: 15px;"),
                     h3("Cell Info / Gene Expr", style = "color: #1e3a5f;"),
                     p("Visualize cell info and gene expression on reduced dimensions"),
                   )
                 ),
                 
                 # Gene Coexpression card - only visible when gene_expression is selected
                 conditionalPanel(
                   condition = "input.homePageDataset == 'gene_expression'",
                   div(
                     class = "well",
                     style = "text-align: center; padding: 25px; margin: 15px; width: 260px; height: 300px; cursor: pointer; background-color: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); transition: transform 0.3s, box-shadow 0.3s;",
                     onclick = "Shiny.setInputValue('nav_selection', 'coexpression');",
                     onmouseover = "this.style.transform='translateY(-5px)'; this.style.boxShadow='0 5px 15px rgba(0,0,0,0.2)';",
                     onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 2px 10px rgba(0,0,0,0.1)';",
                     icon("project-diagram", "fa-3x", style = "color: #1e3a5f; margin-bottom: 15px;"),
                     h3("Gene Coexpression", style = "color: #1e3a5f;"),
                     p("Explore coexpression of two genes on reduced dimensions"),
                   )
                 ),
                 
                 # Statistical Plots card - only visible when gene_expression is selected
                 conditionalPanel(
                   condition = "input.homePageDataset == 'gene_expression'",
                   div(
                     class = "well",
                     style = "text-align: center; padding: 25px; margin: 15px; width: 260px; height: 300px; cursor: pointer; background-color: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); transition: transform 0.3s, box-shadow 0.3s;",
                     onclick = "Shiny.setInputValue('nav_selection', 'stats');",
                     onmouseover = "this.style.transform='translateY(-5px)'; this.style.boxShadow='0 5px 15px rgba(0,0,0,0.2)';",
                     onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 2px 10px rgba(0,0,0,0.1)';",
                     icon("chart-bar", "fa-3x", style = "color: #1e3a5f; margin-bottom: 15px;"),
                     h3("Statistical Plots", style = "color: #1e3a5f;"),
                     p("Generate violin plots, boxplots, and heatmaps"),
                   )
                 ),
                 
                 # Motif Analysis card - only visible when motif_data is selected
                 conditionalPanel(
                   condition = "input.homePageDataset == 'chromatin_accessibility'",
                   div(
                     class = "well",
                     style = "text-align: center; padding: 30px; margin: 15px; width: 260px; height: 300px; cursor: pointer; background-color: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); transition: transform 0.3s, box-shadow 0.3s;",
                     onclick = "Shiny.setInputValue('nav_selection', 'chrom_access');",
                     onmouseover = "this.style.transform='translateY(-5px)'; this.style.boxShadow='0 5px 15px rgba(0,0,0,0.2)';",
                     onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 2px 10px rgba(0,0,0,0.1)';",
                     icon("dna", "fa-3x", style = "color: #1e3a5f; margin-bottom: 15px;"),
                     h3("Chrom Access", style = "color: #1e3a5f;"),
                     p("Visualize Motif scores"),
                   )
                 )
               )
             )
    ),
        
### Modifications to ui.R
# Replace the existing Cell Info / Gene Expr, Gene Coexpression, and Statistics tabPanels 
# with a new combined Gene Expr tabPanel

# The code below shows the main part that needs to be changed:

### Replace the individual tabs with a combined Gene Expr tab
tabPanel(
  "Gene Expr",
  value = "gene_expr",
  div(style = "padding-top: 60px;",
      # Breadcrumb navigation
      div(
        class = "breadcrumb",
        tags$span(class = "breadcrumb-item", icon("home"), "Home"),
        tags$span(class = "breadcrumb-item active", "Gene Expression Analysis")
      ),
      
      conditionalPanel(
        condition = "input.datasetChoice == 'gene_expression'",
        
        # Create tabset panel for subtabs within Gene Expr
        tabsetPanel(
          id = "gene_expr_tabs",
          
          ### Tab1.a1: cellInfo vs geneExpr on dimRed (as a subtab)
          tabPanel(
            "Cell Info / Gene Expr",
            value = "cellinfo_subtab",
            
            # Title and description with info icon
            div(
              style = "display: flex; align-items: center; margin-bottom: 15px;",
              h4("Cell information vs gene expression on reduced dimensions", style = "margin: 0;"),
              actionLink("help_cellinfo", icon("question-circle"), style = "margin-left: 10px; color: #1e3a5f;")
            ),
            p("In this tab, users can visualise both cell information and gene ",  
              "expression side-by-side on low-dimensional representions."), 
            br(),
            
            # Main controls as cards
            fluidRow( 
              column( 
                3, 
                div(class = "control-card",
                    h4("Dimension Reduction", class = "section-title"), 
                    fluidRow( 
                      column( 
                        12, 
                        tipify(
                          selectInput("sc1a1drX", "X-axis:", choices = "X_umap1", selected = "X_umap1"),
                          "Select dimension for X-axis"
                        ), 
                        tipify(
                          selectInput("sc1a1drY", "Y-axis:", choices = "X_umap2", selected = "X_umap2"),
                          "Select dimension for Y-axis"
                        )
                      ) 
                    )
                )
              ),
              column( 
                3, 
                div(class = "control-card",
                    actionButton("sc1a1togL", "Toggle to subset cells", class = "btn-block"), 
                    conditionalPanel( 
                      condition = "input.sc1a1togL % 2 == 1", 
                      tipify(
                        selectInput("sc1a1sub1", "Cell information to subset:",
                                   choices = sc1conf[grp == TRUE]$UI, 
                                   selected = sc1def$grp1),
                        "Choose which cell information to use for subsetting"
                      ),
                      uiOutput("sc1a1sub1.ui"), 
                      div(
                        style = "display: flex; justify-content: space-between; margin-top: 10px;",
                        actionButton("sc1a1sub1all", "Select all", class = "btn btn-primary", style = "flex: 1; margin-right: 5px;"), 
                        actionButton("sc1a1sub1non", "Deselect all", class = "btn btn-primary", style = "flex: 1; margin-left: 5px;")
                      )
                    )
                )
              ),
              column( 
                6, 
                div(class = "control-card",
                    actionButton("sc1a1tog0", "Toggle graphics controls", class = "btn-block"), 
                    conditionalPanel( 
                      condition = "input.sc1a1tog0 % 2 == 1", 
                      fluidRow( 
                        column( 
                          6, 
                          tipify(
                            sliderInput("sc1a1siz", "Point size:", 
                                       min = 0, max = 4, value = 0.1, step = 0.25),
                            "Adjust the size of points in the plot"
                          ),
                          radioButtons("sc1a1psz", "Plot size:", 
                                       choices = c("Small", "Medium", "Large"), 
                                       selected = "Medium", inline = TRUE), 
                          radioButtons("sc1a1fsz", "Font size:", 
                                       choices = c("Small", "Medium", "Large"), 
                                       selected = "Medium", inline = TRUE) 
                        ), 
                        column( 
                          6, 
                          radioButtons("sc1a1asp", "Aspect ratio:", 
                                       choices = c("Square", "Fixed", "Free"), 
                                       selected = "Square", inline = TRUE), 
                          checkboxInput("sc1a1txt", "Show axis text", value = FALSE),
                          tipify(
                            checkboxInput("sc1a1fixed", "Fix aspect ratio", value = TRUE),
                            "Maintain consistent scaling between axes"
                          )
                        ) 
                      ) 
                    )
                )
              )
            ),
            # Plot containers styled as cards
            fluidRow( 
              column( 
                6, 
                div(class = "control-card",
                    h4("Cell information", class = "section-title"), 
                    fluidRow( 
                      column( 
                        6, 
                        tipify(
                          selectInput("sc1a1inp1", "Cell information:", 
                                     choices = sc1conf$UI, 
                                     selected = sc1def$meta1),
                          "Select cell information to display in the plot"
                        )
                      ), 
                      column( 
                        6, 
                        actionButton("sc1a1tog1", "Toggle plot controls", class = "btn-block"), 
                        conditionalPanel( 
                          condition = "input.sc1a1tog1 % 2 == 1", 
                          radioButtons("sc1a1col1", "Colour (Continuous data):", 
                                       choices = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple"), 
                                       selected = "Blue-Yellow-Red"), 
                          radioButtons("sc1a1ord1", "Plot order:", 
                                       choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                                       selected = "Original", inline = TRUE), 
                          checkboxInput("sc1a1lab1", "Show cell info labels", value = TRUE) 
                        ) 
                      ) 
                    ), 
                    div(class = "plot-container",
                        fluidRow(column(12, uiOutput("sc1a1oup1.ui")))), 
                    div(
                      style = "margin-top: 10px; display: flex; flex-wrap: wrap;",
                      downloadButton("sc1a1oup1.pdf", "Download PDF", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;"), 
                      downloadButton("sc1a1oup1.png", "Download PNG", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;")
                    ),
                    div(
                      style = "display: flex; flex-wrap: wrap; align-items: center; margin-top: 10px;",
                      div(style="margin-right: 10px;", 
                          numericInput("sc1a1oup1.h", "Height:", width = "100px", 
                                     min = 4, max = 20, value = 6, step = 0.5)), 
                      div(style="margin-right: 10px;", 
                          numericInput("sc1a1oup1.w", "Width:", width = "100px", 
                                     min = 4, max = 20, value = 8, step = 0.5))
                    ),
                    div(
                      style = "margin-top: 15px;",
                      actionButton("sc1a1tog9", "Toggle to show cell statistics", class = "btn-block")
                    ),
                    conditionalPanel( 
                      condition = "input.sc1a1tog9 % 2 == 1", 
                      h4("Cell numbers / statistics", class = "section-title"), 
                      radioButtons("sc1a1splt", "Split continuous cell info into:", 
                                   choices = c("Quartile", "Decile"), 
                                   selected = "Decile", inline = TRUE), 
                      dataTableOutput("sc1a1.dt") 
                    )
                )
              ),
              column( 
                6, 
                div(class = "control-card",
                    h4("Gene expression", class = "section-title"), 
                    fluidRow( 
                      column( 
                        6, 
                        tipify(
                          selectInput("sc1a1inp2", "Gene name:", choices = NULL),
                          "Select gene to visualize expression"
                        )
                      ), 
                      column( 
                        6, 
                        actionButton("sc1a1tog2", "Toggle plot controls", class = "btn-block"), 
                        conditionalPanel( 
                          condition = "input.sc1a1tog2 % 2 == 1", 
                          radioButtons("sc1a1col2", "Colour:", 
                                       choices = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple"), 
                                       selected = "White-Red"), 
                          radioButtons("sc1a1ord2", "Plot order:", 
                                       choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                                       selected = "Max-1st", inline = TRUE) 
                        ) 
                      ) 
                    ), 
                    div(class = "plot-container",
                        fluidRow(column(12, uiOutput("sc1a1oup2.ui")))), 
                    div(
                      style = "margin-top: 10px; display: flex; flex-wrap: wrap;",
                      downloadButton("sc1a1oup2.pdf", "Download PDF", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;"), 
                      downloadButton("sc1a1oup2.png", "Download PNG", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;")
                    ),
                    div(
                      style = "display: flex; flex-wrap: wrap; align-items: center; margin-top: 10px;",
                      div(style="margin-right: 10px;", 
                          numericInput("sc1a1oup2.h", "Height:", width = "100px", 
                                     min = 4, max = 20, value = 6, step = 0.5)), 
                      div(style="margin-right: 10px;", 
                          numericInput("sc1a1oup2.w", "Width:", width = "100px", 
                                     min = 4, max = 20, value = 8, step = 0.5))
                    )
                )
              )
            )
          ),
          
          ### Tab1.b2: Gene coexpression plot (as a subtab)
          tabPanel(
            "Gene Coexpression",
            value = "coexpression_subtab",
            
            # Coexpression subtabs - keep existing internal tab structure
            tabsetPanel(
              id = "coexpression_tabs",
              
              # UMAP Visualization Tab
              tabPanel(
                "UMAP Visualization",
                # Title and description with info icon
                div(
                  style = "display: flex; align-items: center; margin-bottom: 15px;",
                  h4("Coexpression of two genes on reduced dimensions", style = "margin: 0;"),
                  actionLink("help_coexpression", icon("question-circle"), style = "margin-left: 10px; color: #1e3a5f;")
                ),
                p("In this tab, users can visualise the coexpression of two genes on low-dimensional representions."),
                br(),
                
                # Main controls as cards
                fluidRow( 
                  column( 
                    3, 
                    div(class = "control-card",
                        h4("Dimension Reduction", class = "section-title"), 
                        fluidRow( 
                          column( 
                            12,
                            tipify(
                              selectInput("sc1b2drX", "X-axis:", choices = "X_umap1", selected = "X_umap1"),
                              "Select dimension for X-axis"
                            ),
                            tipify(
                              selectInput("sc1b2drY", "Y-axis:", choices = "X_umap2", selected = "X_umap2"),
                              "Select dimension for Y-axis"
                            )
                          ) 
                        )
                    )
                  ),
                  column( 
                    3, 
                    div(class = "control-card",
                        actionButton("sc1b2togL", "Toggle to subset cells", class = "btn-block"), 
                        conditionalPanel( 
                          condition = "input.sc1b2togL % 2 == 1", 
                          tipify(
                            selectInput("sc1b2sub1", "Cell information to subset:",
                                      choices = sc1conf[grp == TRUE]$UI, 
                                      selected = sc1def$grp1),
                            "Choose which cell information to use for subsetting"
                          ),
                          uiOutput("sc1b2sub1.ui"), 
                          div(
                            style = "display: flex; justify-content: space-between; margin-top: 10px;",
                            actionButton("sc1b2sub1all", "Select all", class = "btn btn-primary", style = "flex: 1; margin-right: 5px;"),
                            actionButton("sc1b2sub1non", "Deselect all", class = "btn btn-primary", style = "flex: 1; margin-left: 5px;")
                          )
                        )
                    )
                  ),
                  column( 
                    6, 
                    div(class = "control-card",
                        actionButton("sc1b2tog0", "Toggle graphics controls", class = "btn-block"), 
                        conditionalPanel( 
                          condition = "input.sc1b2tog0 % 2 == 1", 
                          fluidRow( 
                            column( 
                              6, 
                              tipify(
                                sliderInput("sc1b2siz", "Point size:", 
                                          min = 0, max = 4, value = 0.1, step = 0.25),
                                "Adjust the size of points in the plot"
                              ),
                              radioButtons("sc1b2psz", "Plot size:", 
                                        choices = c("Small", "Medium", "Large"), 
                                        selected = "Medium", inline = TRUE), 
                              radioButtons("sc1b2fsz", "Font size:", 
                                        choices = c("Small", "Medium", "Large"), 
                                        selected = "Medium", inline = TRUE)
                                        ), 
                            column( 
                              6, 
                              radioButtons("sc1b2asp", "Aspect ratio:", 
                                        choices = c("Square", "Fixed", "Free"), 
                                        selected = "Square", inline = TRUE), 
                              checkboxInput("sc1b2txt", "Show axis text", value = FALSE),
                              tipify(
                                checkboxInput("sc1b2fixed", "Fix aspect ratio", value = TRUE),
                                "Maintain consistent scaling between axes"
                              )
                            ) 
                          ) 
                        )
                    )
                  )
                ),
                # Gene coexpression section without centered divider
                fluidRow( 
                  column( 
                    3, 
                    div(class = "control-card",
                        h4("Gene Expression", class = "section-title"), 
                        tipify(
                          selectInput("sc1b2inp1", "Gene 1:", choices = NULL),
                          "Select first gene to visualize"
                        ),
                        tipify(
                          selectInput("sc1b2inp2", "Gene 2:", choices = NULL),
                          "Select second gene to visualize"
                        ),
                        actionButton("sc1b2tog1", "Toggle plot controls", class = "btn-block"), 
                        conditionalPanel( 
                          condition = "input.sc1b2tog1 % 2 == 1", 
                          radioButtons("sc1b2col1", "Colour:", 
                                    choices = c("Red (Gene1); Blue (Gene2)", 
                                                "Orange (Gene1); Blue (Gene2)", 
                                                "Red (Gene1); Green (Gene2)", 
                                                "Green (Gene1); Blue (Gene2)"), 
                                    selected = "Red (Gene1); Blue (Gene2)"), 
                          radioButtons("sc1b2ord1", "Plot order:", 
                                    choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                                    selected = "Max-1st", inline = TRUE) 
                        )
                    )
                  ),
                  column( 
                    6,
                    div(class = "control-card",
                        div(class = "plot-container",
                            uiOutput("sc1b2oup1.ui")), 
                        div(
                          style = "margin-top: 10px; display: flex; flex-wrap: wrap;",
                          downloadButton("sc1b2oup1.pdf", "Download PDF", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;"),
                          downloadButton("sc1b2oup1.png", "Download PNG", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;")
                        ),
                        div(
                          style = "display: flex; flex-wrap: wrap; align-items: center; margin-top: 10px;",
                          div(style="margin-right: 10px;", 
                              numericInput("sc1b2oup1.h", "Height:", width = "100px", 
                                        min = 4, max = 20, value = 8, step = 0.5)), 
                          div(style="margin-right: 10px;", 
                              numericInput("sc1b2oup1.w", "Width:", width = "100px", 
                                        min = 4, max = 20, value = 10, step = 0.5))
                        )
                    )
                  ),
                  column( 
                    3,
                    div(class = "control-card",
                        style="border-left: 2px solid #1e3a5f; padding-left: 15px;",
                        uiOutput("sc1b2oup2.ui"), 
                        div(
                          style = "margin-top: 10px; display: flex; flex-wrap: wrap;",
                          downloadButton("sc1b2oup2.pdf", "Download PDF", class = "btn-sm", style = "margin-right: 5px;"),
                          downloadButton("sc1b2oup2.png", "Download PNG", class = "btn-sm")
                        ),
                        br(), 
                        h4("Cell numbers", class = "section-title"), 
                        dataTableOutput("sc1b2.dt")
                    )
                  )
                )
              ),
              # Gene-Gene Scatter Plot Tab
              tabPanel(
                "Gene-Gene Scatter Plot",
                # Title and description with info icon
                div(
                  style = "display: flex; align-items: center; margin-bottom: 15px;",
                  h4("Gene-Gene scatter plot", style = "margin: 0;"),
                  actionLink("help_gene_scatter", icon("question-circle"), style = "margin-left: 10px; color: #1e3a5f;")
                ),
                p("In this tab, users can visualise the relationship between expression levels of two genes using a scatter plot, with optional coloring by cell metadata."),
                br(),
                
                fluidRow(
                  column(
                    3,
                    div(class = "control-card",
                        actionButton("sc1b2_scatter_togL", "Toggle to subset cells", class = "btn-block"), 
                        conditionalPanel( 
                          condition = "input.sc1b2_scatter_togL % 2 == 1", 
                          tipify(
                            selectInput("sc1b2_scatter_sub1", "Cell information to subset:",
                                      choices = sc1conf[grp == TRUE]$UI, 
                                      selected = sc1def$grp1),
                            "Choose which cell information to use for subsetting"
                          ),
                          uiOutput("sc1b2_scatter_sub1.ui"), 
                          div(
                            style = "display: flex; justify-content: space-between; margin-top: 10px;",
                            actionButton("sc1b2_scatter_sub1all", "Select all", class = "btn btn-primary", style = "flex: 1; margin-right: 5px;"),
                            actionButton("sc1b2_scatter_sub1non", "Deselect all", class = "btn btn-primary", style = "flex: 1; margin-left: 5px;")
                          )
                        )
                    )
                  ),
                  column(
                    9,
                    div(class = "control-card",
                        actionButton("sc1b2_scatter_tog0", "Toggle graphics controls", class = "btn-block"), 
                        conditionalPanel( 
                          condition = "input.sc1b2_scatter_tog0 % 2 == 1", 
                          fluidRow(
                            column(4,
                                   tipify(
                                     sliderInput("sc1b2_scatter_siz", "Point size:", 
                                               min = 0, max = 4, value = 0.1, step = 0.25),
                                     "Adjust the size of points in the plot"
                                   )
                            ),
                            column(4,
                                   radioButtons("sc1b2_scatter_psz", "Plot size:", 
                                             choices = c("Small", "Medium", "Large"), 
                                             selected = "Medium", inline = TRUE)
                            ),
                            column(4,
                                   radioButtons("sc1b2_scatter_fsz", "Font size:", 
                                             choices = c("Small", "Medium", "Large"), 
                                             selected = "Medium", inline = TRUE)
                            )
                          )
                        )
                    )
                  )
                ),
                
                # Gene-Gene scatter content without centered divider
                fluidRow( 
                  column( 
                    3, 
                    div(class = "control-card",
                        h4("Gene Expression", class = "section-title"), 
                        tipify(
                          selectInput("sc1b2_scatter_inp1", "Gene 1 (X-axis):", choices = NULL),
                          "Select gene for X-axis"
                        ),
                        tipify(
                          selectInput("sc1b2_scatter_inp2", "Gene 2 (Y-axis):", choices = NULL),
                          "Select gene for Y-axis"
                        ),
                        tipify(
                          checkboxInput("sc1b2_scatter_color", "Colour by cell information", value = FALSE),
                          "Color points by selected cell information"
                        ),
                        conditionalPanel(
                          condition = "input.sc1b2_scatter_color == true",
                          tipify(
                            selectInput("sc1b2_scatter_colorby", "Cell information:", 
                                      choices = sc1conf$UI, 
                                      selected = sc1conf$UI[1]),
                            "Select cell information for coloring points"
                          )
                        ),
                        tipify(
                          checkboxInput("sc1b2_scatter_fixed", "Fix aspect ratio", value = TRUE),
                          "Maintain consistent scaling between axes"
                        )
                    )
                  ),
                  column( 
                    9,
                    div(class = "control-card",
                        div(class = "plot-container",
                            uiOutput("sc1b2_scatter_plot.ui")), 
                        div(
                          style = "margin-top: 10px; display: flex; flex-wrap: wrap;",
                          downloadButton("sc1b2_scatter_plot.pdf", "Download PDF", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;"),
                          downloadButton("sc1b2_scatter_plot.png", "Download PNG", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;")
                        ),
                        div(
                          style = "display: flex; flex-wrap: wrap; align-items: center; margin-top: 10px;",
                          div(style="margin-right: 10px;", 
                              numericInput("sc1b2_scatter_plot.h", "Height:", width = "100px", 
                                        min = 4, max = 20, value = 8, step = 0.5)), 
                          div(style="margin-right: 10px;", 
                              numericInput("sc1b2_scatter_plot.w", "Width:", width = "100px", 
                                        min = 4, max = 20, value = 10, step = 0.5))
                        ),
                        br(),
                        div(class = "control-card",
                            style = "margin-top: 15px; background-color: #f8f9fa;",
                            div(style = "text-align: center; margin-bottom: 5px;",
                                h4("Correlation Statistics", style = "margin-bottom: 8px; color: #1e3a5f;")),
                            div(class = "stats-container", 
                                style = "width: 100%; max-width: 800px; overflow-x: auto; font-family: monospace; white-space: pre-wrap; margin: 0 auto; padding: 10px; background-color: transparent; border-radius: 4px;",
                                verbatimTextOutput("sc1b2_scatter_stats"))
                        )
                    )
                  )
                )
              )
            )
          ),
          
          ### Statistics Tab (as a subtab with its own internal tabsetPanel)
          tabPanel(
            "Statistics",
            value = "stats_subtab",
            
            tabsetPanel(
              id = "stats_tabs",
              
              ### Tab1.c1: violinplot / boxplot 
              tabPanel(
                "Violinplot / Boxplot",
                # Title and description with info icon
                div(
                  style = "display: flex; align-items: center; margin-bottom: 15px;",
                  h4("Cell information / gene expression violin plot / box plot", style = "margin: 0;"),
                  actionLink("help_violin", icon("question-circle"), style = "margin-left: 10px; color: #1e3a5f;")
                ),
                p("In this tab, users can visualise the gene expression or continuous cell information (e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters)."),
                br(),
                fluidRow( 
                  column( 
                    3, 
                    div(class = "control-card",
                        h4("Plot Configuration", class = "section-title"),
                        tipify(
                          selectInput("sc1c1inp1", "Cell information (X-axis):", 
                                    choices = sc1conf[grp == TRUE]$UI, 
                                    selected = sc1def$grp1),
                          "Select categorical cell information for X-axis"
                        ),
                        tipify(
                          selectInput("sc1c1inp2", "Cell Info / Gene name (Y-axis):", choices = NULL),
                          "Select cell information or gene for Y-axis"
                        ),
                        radioButtons("sc1c1typ", "Plot type:", 
                                   choices = c("violin", "boxplot"), 
                                   selected = "violin", inline = TRUE), 
                        checkboxInput("sc1c1pts", "Show data points", value = FALSE),
                        
                        div(style = "margin-top: 15px;"),
                        actionButton("sc1c1togL", "Toggle to subset cells", class = "btn-block"), 
                        conditionalPanel( 
                          condition = "input.sc1c1togL % 2 == 1", 
                          tipify(
                            selectInput("sc1c1sub1", "Cell information to subset:", 
                                      choices = sc1conf[grp == TRUE]$UI, 
                                      selected = sc1def$grp1),
                            "Choose which cell information to use for subsetting"
                          ),
                          uiOutput("sc1c1sub1.ui"), 
                          div(
                            style = "display: flex; justify-content: space-between; margin-top: 10px;",
                            actionButton("sc1c1sub1all", "Select all", class = "btn btn-primary", style = "flex: 1; margin-right: 5px;"),
                            actionButton("sc1c1sub1non", "Deselect all", class = "btn btn-primary", style = "flex: 1; margin-left: 5px;")
                          )
                        ),
                        
                        div(style = "margin-top: 15px;"),
                        actionButton("sc1c1tog", "Toggle graphics controls", class = "btn-block"), 
                        conditionalPanel( 
                          condition = "input.sc1c1tog % 2 == 1", 
                          tipify(
                            sliderInput("sc1c1siz", "Data point size:",  
                                      min = 0, max = 4, value = 0.1, step = 0.25),
                            "Adjust the size of data points in the plot"
                          ),
                          radioButtons("sc1c1psz", "Plot size:", 
                                    choices = c("Small", "Medium", "Large"), 
                                    selected = "Medium", inline = TRUE), 
                          radioButtons("sc1c1fsz", "Font size:", 
                                    choices = c("Small", "Medium", "Large"), 
                                    selected = "Medium", inline = TRUE),
                          tipify(
                            checkboxInput("sc1c1fixed", "Fix aspect ratio", value = TRUE),
                            "Maintain consistent scaling between axes"
                          )
                        )
                    )
                  ),
                  column(
                    9, 
                    div(class = "control-card",
                        div(class = "plot-container",
                            uiOutput("sc1c1oup.ui")),  
                        div(
                          style = "margin-top: 10px; display: flex; flex-wrap: wrap;",
                          downloadButton("sc1c1oup.pdf", "Download PDF", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;"),
                          downloadButton("sc1c1oup.png", "Download PNG", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;")
                        ),
                        div(
                          style = "display: flex; flex-wrap: wrap; align-items: center; margin-top: 10px;",
                          div(style="margin-right: 10px;", 
                              numericInput("sc1c1oup.h", "Height:", width = "100px", 
                                          min = 4, max = 20, value = 8, step = 0.5)), 
                          div(style="margin-right: 10px;", 
                              numericInput("sc1c1oup.w", "Width:", width = "100px", 
                                          min = 4, max = 20, value = 10, step = 0.5))
                        )
                    )
                  )
                )
              ),
              ### Tab1.d1: Multiple gene expr - Bubbleplot / Heatmap
              tabPanel(
                "Bubbleplot / Heatmap",
                # Title and description with info icon
                div(
                  style = "display: flex; align-items: center; margin-bottom: 15px;",
                  h4("Gene expression bubbleplot / heatmap", style = "margin: 0;"),
                  actionLink("help_heatmap", icon("question-circle"), style = "margin-left: 10px; color: #1e3a5f;")
                ),
                p("In this tab, users can visualise the gene expression patterns of multiple genes grouped by categorical cell information (e.g. library / cluster)."),
                p("The normalised expression are averaged, log-transformed and then plotted."),
                br(),
                
                fluidRow( 
                  column( 
                    3,
                    div(class = "control-card",
                        h4("Plot Configuration", class = "section-title"),
                        tipify(
                          textAreaInput("sc1d1inp", HTML("List of gene names <br /> 
                                                        (Max 50 genes, separated <br /> 
                                                         by , or ; or newline):"), 
                                        height = "200px", 
                                        value = paste0(sc1def$genes, collapse = ", ")),
                          "Enter gene names separated by commas, semicolons, or newlines"
                        ),
                        tipify(
                          selectInput("sc1d1grp", "Group by:", 
                                    choices = sc1conf[grp == TRUE]$UI, 
                                    selected = sc1conf[grp == TRUE]$UI[1]),
                          "Select categorical cell information for grouping"
                        ),
                        radioButtons("sc1d1plt", "Plot type:", 
                                   choices = c("Bubbleplot", "Heatmap"), 
                                   selected = "Bubbleplot", inline = TRUE), 
                        tipify(
                          checkboxInput("sc1d1scl", "Scale gene expression", value = TRUE),
                          "Scale gene expression values across samples"
                        ),
                        tipify(
                          checkboxInput("sc1d1row", "Cluster rows (genes)", value = TRUE),
                          "Cluster genes by expression pattern similarity"
                        ),
                        tipify(
                          checkboxInput("sc1d1col", "Cluster columns (samples)", value = FALSE),
                          "Cluster samples by expression pattern similarity"
                        ),
                        
                        div(style = "margin-top: 15px;"),
                        actionButton("sc1d1togL", "Toggle to subset cells", class = "btn-block"), 
                        conditionalPanel( 
                          condition = "input.sc1d1togL % 2 == 1", 
                          tipify(
                            selectInput("sc1d1sub1", "Cell information to subset:", 
                                        choices = sc1conf[grp == TRUE]$UI, 
                                        selected = sc1def$grp1),
                            "Choose which cell information to use for subsetting"
                          ),
                          uiOutput("sc1d1sub1.ui"), 
                          div(
                            style = "display: flex; justify-content: space-between; margin-top: 10px;",
                            actionButton("sc1d1sub1all", "Select all", class = "btn btn-primary", style = "flex: 1; margin-right: 5px;"),
                            actionButton("sc1d1sub1non", "Deselect all", class = "btn btn-primary", style = "flex: 1; margin-left: 5px;")
                          )
                        ),
                        div(style = "margin-top: 15px;"),
                        actionButton("sc1d1tog", "Toggle graphics controls", class = "btn-block"), 
                        conditionalPanel( 
                          condition = "input.sc1d1tog % 2 == 1", 
                          radioButtons("sc1d1cols", "Colour scheme:", 
                                     choices = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple"), 
                                     selected = "Blue-Yellow-Red"), 
                          radioButtons("sc1d1psz", "Plot size:", 
                                     choices = c("Small", "Medium", "Large"), 
                                     selected = "Medium", inline = TRUE), 
                          radioButtons("sc1d1fsz", "Font size:", 
                                     choices = c("Small", "Medium", "Large"), 
                                     selected = "Medium", inline = TRUE),
                          tipify(
                            checkboxInput("sc1d1fixed", "Fix aspect ratio", value = TRUE),
                            "Maintain consistent scaling between axes"
                          )
                        )
                    )
                  ),
                  column(
                    9, 
                    div(class = "control-card",
                        h4(htmlOutput("sc1d1oupTxt"), class = "section-title"), 
                        div(class = "plot-container",
                            uiOutput("sc1d1oup.ui")), 
                        div(
                          style = "margin-top: 10px; display: flex; flex-wrap: wrap;",
                          downloadButton("sc1d1oup.pdf", "Download PDF", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;"),
                          downloadButton("sc1d1oup.png", "Download PNG", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;")
                        ),
                        div(
                          style = "display: flex; flex-wrap: wrap; align-items: center; margin-top: 10px;",
                          div(style="margin-right: 10px;", 
                              numericInput("sc1d1oup.h", "Height:", width = "100px", 
                                        min = 4, max = 20, value = 10, step = 0.5)), 
                          div(style="margin-right: 10px;", 
                              numericInput("sc1d1oup.w", "Width:", width = "100px", 
                                        min = 4, max = 20, value = 10, step = 0.5))
                        )
                    )
                  )
                )
              )
            )
          )
        )
      )
  )
),
    ### New Motif Analysis Tab
### Motif Analysis Tab with Subtabs
tabPanel(
  "Chrom Access",
  value = "chrom_access",
  div(style = "padding-top: 60px;",
      # Breadcrumb navigation
      div(
        class = "breadcrumb",
        tags$span(class = "breadcrumb-item", icon("home"), "Home"),
        tags$span(class = "breadcrumb-item active", "Chrom Access")
      ),
      
      # Title and description with info icon
      div(
        style = "display: flex; align-items: center; margin-bottom: 15px;",
        h4("Motif score visualization on reduced dimensions", style = "margin: 0;"),
        actionLink("help_motif", icon("question-circle"), style = "margin-left: 10px; color: #1e3a5f;")
      ),
      p("In this tab, users can visualize motif scores on low-dimensional representations or against cell information."),
      br(),
      
      # Add tabsetPanel for subtabs
      tabsetPanel(
        id = "motif_tabs",
        
        # Visualization subtab
        tabPanel(
          "Motif Visualization",
          # Main controls as cards
          fluidRow(
            column(
              3, 
              div(class = "control-card",
                  h4("Dimension Reduction", class = "section-title"),
                  fluidRow(
                    column(
                      12,
                      tipify(
                        selectInput("motifdrX", "X-axis:", choices = "X_umap1", selected = "X_umap1"),
                        "Select dimension for X-axis"
                      ),
                      tipify(
                        selectInput("motifdrY", "Y-axis:", choices = "X_umap2", selected = "X_umap2"),
                        "Select dimension for Y-axis"
                      )
                    )
                  )
              )
            ),
            column(
              3,
              div(class = "control-card",
                  actionButton("motiftogL", "Toggle to subset cells", class = "btn-block"),
                  conditionalPanel(
                    condition = "input.motiftogL % 2 == 1",
                    tipify(
                      selectInput("motifsub1", "Cell information to subset:",
                                choices = sc1conf[grp == TRUE]$UI,
                                selected = sc1def$grp1),
                      "Choose which cell information to use for subsetting"
                    ),
                    uiOutput("motifsub1.ui"),
                    div(
                      style = "display: flex; justify-content: space-between; margin-top: 10px;",
                      actionButton("motifsub1all", "Select all", class = "btn btn-primary", style = "flex: 1; margin-right: 5px;"),
                      actionButton("motifsub1non", "Deselect all", class = "btn btn-primary", style = "flex: 1; margin-left: 5px;")
                    )
                  )
              )
            ),
            column(
              6,
              div(class = "control-card",
                  actionButton("motiftog0", "Toggle graphics controls", class = "btn-block"),
                  conditionalPanel(
                    condition = "input.motiftog0 % 2 == 1",
                    fluidRow(
                      column(
                        6,
                        tipify(
                          sliderInput("motifsiz", "Point size:",
                                    min = 0, max = 4, value = 0.1, step = 0.25),
                          "Adjust the size of points in the plot"
                        ),
                        radioButtons("motifpsz", "Plot size:",
                                    choices = c("Small", "Medium", "Large"),
                                    selected = "Medium", inline = TRUE),
                        radioButtons("motiffsz", "Font size:",
                                    choices = c("Small", "Medium", "Large"),
                                    selected = "Medium", inline = TRUE)
                      ),
                      column(
                        6,
                        radioButtons("motifasp", "Aspect ratio:",
                                    choices = c("Square", "Fixed", "Free"),
                                    selected = "Square", inline = TRUE),
                        checkboxInput("motiftxt", "Show axis text", value = FALSE),
                        tipify(
                          checkboxInput("motiffixed", "Fix aspect ratio", value = TRUE),
                          "Maintain consistent scaling between axes"
                        )
                      )
                    )
                  )
              )
            )
          ),
          
          # Plot containers styled as cards
          fluidRow(
            column(
              6, 
              div(class = "control-card",
                  h4("Motif scores", class = "section-title"),
                  fluidRow(
                    column(
                      6,
                      tipify(
                        selectInput("motif", "Motif:", 
                                  choices = list.files("motif_data", pattern = "*.csv") %>% 
                                           sub(".csv$", "", .),
                                  selected = list.files("motif_data", pattern = "*.csv")[1] %>% 
                                           sub(".csv$", "", .)),
                        "Select motif to visualize"
                      )
                    ),
                    column(
                      6,
                      actionButton("motiftog1", "Toggle plot controls", class = "btn-block"),
                      conditionalPanel(
                        condition = "input.motiftog1 % 2 == 1",
                        radioButtons("motifcol1", "Color scheme:",
                                    choices = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple"),
                                    selected = "Blue-Yellow-Red"),
                        radioButtons("motiford1", "Plot order:",
                                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                                    selected = "Original", inline = TRUE)
                      )
                    )
                  ),
                  div(class = "plot-container",
                      fluidRow(column(12, uiOutput("motifoup1.ui")))),
                  div(
                    style = "margin-top: 10px; display: flex; flex-wrap: wrap;",
                    downloadButton("motifoup1.pdf", "Download PDF", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;"),
                    downloadButton("motifoup1.png", "Download PNG", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;")
                  ),
                  div(
                    style = "display: flex; flex-wrap: wrap; align-items: center; margin-top: 10px;",
                    div(style="margin-right: 10px;", 
                        numericInput("motifoup1.h", "Height:", width = "100px",
                                   min = 4, max = 20, value = 6, step = 0.5)),
                    div(style="margin-right: 10px;", 
                        numericInput("motifoup1.w", "Width:", width = "100px",
                                   min = 4, max = 20, value = 8, step = 0.5))
                  )
              )
            ),
            column(
              6, 
              div(class = "control-card",
                  h4("Cell information", class = "section-title"),
                  fluidRow(
                    column(
                      6,
                      tipify(
                        selectInput("motifcel", "Cell information:", 
                                  choices = sc1conf[dimred == FALSE]$UI,
                                  selected = sc1conf[dimred == FALSE]$UI[1]),
                        "Select cell information to display alongside motif data"
                      )
                    ),
                    column(
                      6,
                      actionButton("motiftog2", "Toggle plot controls", class = "btn-block"),
                      conditionalPanel(
                        condition = "input.motiftog2 % 2 == 1",
                        radioButtons("motifcol2", "Color (Continuous data):",
                                    choices = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple"),
                                    selected = "Blue-Yellow-Red"),
                        radioButtons("motiford2", "Plot order:",
                                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                                    selected = "Original", inline = TRUE)
                      )
                    )
                  ),
                  div(class = "plot-container",
                      fluidRow(column(12, uiOutput("motifoup2.ui")))),
                  div(
                    style = "margin-top: 10px; display: flex; flex-wrap: wrap;",
                    downloadButton("motifoup2.pdf", "Download PDF", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;"),
                    downloadButton("motifoup2.png", "Download PNG", class = "btn-sm", style = "margin-right: 5px; margin-bottom: 5px;")
                  ),
                  div(
                    style = "display: flex; flex-wrap: wrap; align-items: center; margin-top: 10px;",
                    div(style="margin-right: 10px;", 
                        numericInput("motifoup2.h", "Height:", width = "100px",
                                   min = 4, max = 20, value = 6, step = 0.5)),
                    div(style="margin-right: 10px;", 
                        numericInput("motifoup2.w", "Width:", width = "100px",
                                   min = 4, max = 20, value = 8, step = 0.5))
                  )
              )
            )
          )
        ),
        
        # Data Table subtab
        tabPanel(
          "Motif Data Table",
          div(class = "control-card",
              style = "margin-top: 20px;",
              h4("Raw Motif Data", class = "section-title"),
              div(
                style = "display: flex; align-items: center; margin-bottom: 15px;",
                p("This table shows the raw motif scores for all cells. Use the filters and search functionality to explore the data."),
                div(style = "margin-left: auto;",
                    tipify(
                      selectInput("motifTable_motif", "Select Motif:", 
                                choices = list.files("motif_data", pattern = "*.csv") %>% 
                                         sub(".csv$", "", .),
                                selected = list.files("motif_data", pattern = "*.csv")[1] %>% 
                                         sub(".csv$", "", .),
                                width = "250px"),
                      "Change the displayed motif data"
                    )
                )
              ),
              dataTableOutput("motifTable"),
              
              # Add download buttons for the table
              div(
                style = "margin-top: 20px;",
                downloadButton("downloadMotifCSV", "Download CSV", class = "btn-primary"),
                downloadButton("downloadMotifExcel", "Download Excel", class = "btn-primary", style = "margin-left: 10px;")
              )
          )
        )
      )
  )
),
    
    # Help and documentation tab
    tabPanel(
      "Help",
      value = "help",
      div(style = "padding-top: 60px;",
          # Breadcrumb navigation
          div(
            class = "breadcrumb",
            tags$span(class = "breadcrumb-item", icon("home"), "Home"),
            tags$span(class = "breadcrumb-item active", "Help & Documentation")
          ),
          
          fluidRow(
            column(12,
                  div(class = "control-card",
                      h3("Help & Documentation", style = "color: #1e3a5f; margin-top: 0;"),
                      p("This application provides interactive visualization and analysis of single-cell RNA sequencing data. Below you'll find guidance on how to use each section of the app."),
                      
                      # Tabs for different help sections
                      tabsetPanel(
                        tabPanel("Getting Started",
                                h4("Overview", class = "section-title"),
                                p("ShinyCell is an interactive visualization tool for exploring single-cell RNA sequencing data. The app is organized into several main sections:"),
                                tags$ul(
                                  tags$li(strong("Home"), " - Main landing page with navigation to different visualizations"),
                                  tags$li(strong("Cell Info / Gene Expr"), " - Visualize cell information and gene expression on reduced dimensions"),
                                  tags$li(strong("Gene Coexpression"), " - Explore relationships between pairs of genes"),
                                  tags$li(strong("Statistics"), " - Generate statistical plots including violin plots, boxplots, and heatmaps"),
                                  tags$li(strong("Chromatin Accessibility"), " - Analyze motif and ChIp-seq scores")
                                ),
                                h4("Dataset Selection", class = "section-title"),
                                p("Use the dropdown menu in the top navigation bar to switch between different datasets:"),
                                tags$ul(
                                  tags$li(strong("Gene Expression Data"), " - Explore gene expression patterns across cells"),
                                  tags$li(strong("Chromatin Accessibility"), " - Analyze Motif and ChIp-seq scores")
                                )
                        ),
                        tabPanel("More",
                                h4("Optimization Tips", class = "section-title"),
                                tags$ul(
                                  tags$li(strong("Save Sessions"), " - Use the Save/Load Session buttons to preserve your analysis state"),
                                  tags$li(strong("Plot Downloads"), " - Download plots as PDF for publication-quality vector graphics or PNG for quick sharing"),
                                  tags$li(strong("Plot Customization"), " - Use the toggle controls to access additional customization options"),
                                ),
                                h4("FAQ", class = "section-title"),
                                tags$dl(
                                  tags$dt("How do I filter cells?"),
                                  tags$dd("Use the 'Toggle to subset cells' button in each visualization section to show the subsetting controls."),
                                  tags$dt("Can I modify the color schemes?"),
                                  tags$dd("Yes, in each visualization section there are plot control toggles that allow you to change color schemes."),
                                  tags$dt("How do I find a specific gene?"),
                                  tags$dd("Gene dropdowns are searchable - start typing the gene name to filter the options.")
                                )
                        ),
                        tabPanel("About",
                                h4("Application Information", class = "section-title"),
                                p("ShinyCell v1.0"),
                                p("Developed by: Tanush Swaminathan, University of Pittsburgh, Singh Lab"),
                                p("If you use ShinyCell in your research, please cite:"),
                                tags$blockquote(" Ouyang et al. ShinyCell: Simple and sharable visualisation of single-cell gene expression data. Bioinformatics, doi:10.1093/bioinformatics/btab209"),
                                h4("Contact", class = "section-title"),
                                p("For questions or support, please contact: tanush.swaminathan@pitt.edu")
                        )
                      )
                  )
            )
          )
      )
    )
  )
)