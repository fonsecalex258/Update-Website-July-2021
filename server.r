library(ggplot2)
library(ggiraph)
library(DT)
library(huxtable)
library(dplyr)
library(data.table)
library(formattable)
library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(rsvg)
library(rio)
library(DiagrammeRsvg)
library(DiagrammeR)
library(shinyjs)
library(tesseract)
library(magick)
library(gridExtra)
library(grid)
library(patchwork)
library(cowplot)



template <- read.csv("www/PRISMA.csv",stringsAsFactors = FALSE)
template[5,8] <- des[11,1]
#template[11,8] <- as.numeric(des[26,1])#des[11,1]-des[26,1]
#eo <- as.numeric(des[11,1])- as.numeric(des[26,1])
#template[14,8] <- 30334
#template[14,8] <- des[31,1]
template[14,8] <- as.numeric(des[11,1])-  as.numeric(des[26,1])
template[15,8] <- as.numeric(des[38,1])
template[16,8] <- des[49,1]
template[17,8] <- des[56,1]
template[20,8] <- des[65,1]
template[24,8] <- 0
#template <- distiller1
PRISMA_flowdiagram <- function (data,
                                interactive = FALSE,
                                previous = TRUE,
                                other = T,
                                fontsize = 12,
                                font = 'Helvetica',
                                title_colour = 'Goldenrod1',
                                greybox_colour = 'Gainsboro',
                                main_colour = 'Black',
                                arrow_colour = 'Black',
                                arrow_head = 'normal',
                                arrow_tail = 'none') {
  
  #wrap exclusion reasons
  dbr_excluded[,1] <- stringr::str_wrap(dbr_excluded[,1], 
                                        width = 35)
  other_excluded[,1] <- stringr::str_wrap(other_excluded[,1], 
                                          width = 35)
  
  if(stringr::str_count(paste(dbr_excluded[,1], collapse = "\n"), "\n") > 3){
    dbr_excludedh <- 3.5 - ((stringr::str_count(paste(dbr_excluded[,1], collapse = "\n"), "\n")-4)/9)
  } else {
    dbr_excludedh <- 3.5
  }
  if(nrow(other_excluded) > 3){
    other_excludedh <- 3.5 - ((nrow(other_excluded)-4)/9)
  } else {
    other_excludedh <- 3.5
  }
  
  #remove previous box if both values are zero
  if (is.na(previous_studies) == TRUE && is.na(previous_reports) == TRUE) {
    previous <- FALSE
  }
  
  if(previous == TRUE){
    xstart <- 0
    ystart <- 0
    A <- paste0("A [label = '', pos='",xstart+1,",",ystart+0,"!', tooltip = '']")
    Aedge <- paste0("subgraph cluster0 {
                  edge [color = White, 
                      arrowhead = none, 
                      arrowtail = none]
                  1->2;
                  edge [color = ", arrow_colour, ", 
                      arrowhead = none, 
                      arrowtail = ", arrow_tail, "]
                  2->A; 
                  edge [color = ", arrow_colour, ", 
                      arrowhead = ", arrow_head, ", 
                      arrowtail = none,
                      constraint = FALSE]
                  A->19;
                }")
    bottomedge <- paste0("edge [color = ", arrow_colour, ", 
  arrowhead = ", arrow_head, ", 
  arrowtail = ", arrow_tail, "]
              12->19;\n")
    h_adj1 <- 0
    h_adj2 <- 0
    
    #conditional studies and reports - empty text if blank
    if(is.na(previous_studies) == TRUE) {
      cond_prevstud <- ''
    } else {
      cond_prevstud <- stringr::str_wrap(paste0(previous_studies_text,
                                                " (n = ",
                                                previous_studies, 
                                                ")"), 
                                         width = 40)
    }
    if(is.na(previous_reports) == TRUE) {
      cond_prevrep <- ''
    } else {
      cond_prevrep <- paste0(stringr::str_wrap(previous_reports_text, 
                                               width = 40),
                             "\n(n = ",
                             previous_reports,
                             ')')
    }
    if (is.na(previous_studies) == TRUE || is.na(previous_reports) == TRUE) {
      dbl_br <- ''
    } else {
      dbl_br <- "\n\n"
    }
    
    previous_nodes <- paste0("node [shape = box,
          fontsize = ", fontsize,",
          fontname = ", font, ",
          color = ", greybox_colour, "]
    1 [label = '", previous_text, "', style = 'rounded,filled', width = 3.5, height = 0.5, pos='",xstart+1,",",ystart+8.25,"!', tooltip = '", tooltips[1], "']
    
    node [shape = box,
          fontname = ", font, ",
          color = ", greybox_colour, "]
    2 [label = '",paste0(cond_prevstud,
                         dbl_br,
                         cond_prevrep), 
                         "', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+1,",",ystart+7,"!', tooltip = '", tooltips[2], "']")
    finalnode <- paste0("
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  19 [label = '",paste0(stringr::str_wrap(paste0(total_studies_text,
                                                 " (n = ",
                                                 total_studies, 
                                                 ")"), 
                                          width = 33),
                        "\n",
                        stringr::str_wrap(paste0(total_reports_text,
                                                 " (n = ",
                                                 total_reports,
                                                 ')'), 
                                          width = 33)),  
                        "', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+5,",",ystart+0,"!', tooltip = '", tooltips[19], "']")
    prev_rank1 <- "{rank = same; A; 19}"
    prevnode1 <- "1; "
    prevnode2 <- "2; "
    
  } else {
    xstart <- -3.5
    ystart <- 0
    A <- ""
    Aedge <- ""
    bottomedge <- ""
    previous_nodes <- ""
    finalnode <- ""
    h_adj1 <- 0.63
    h_adj2 <- 1.4
    prev_rank1 <- ""
    prevnode1 <- ""
    prevnode2 <- ""
    
  }
  
  if (is.na(website_results) == TRUE && is.na(organisation_results) == TRUE && is.na(citations_results) == TRUE) {
    other <- FALSE
  }
  
  if(other == TRUE){
    if (any(!grepl("\\D", other_excluded)) == FALSE){
      other_excluded_data <- paste0(':',
                                    paste(paste('\n', 
                                                other_excluded[,1], 
                                                ' (n = ', 
                                                other_excluded[,2], 
                                                ')', 
                                                sep = ''), 
                                          collapse = ''))
    } else {
      other_excluded_data <- paste0('\n', '(n = ', other_excluded, ')')
    }
    B <- paste0("B [label = '', pos='",xstart+13,",",ystart+1.5,"!', tooltip = '']")
    
    if (is.na(website_results) == FALSE) {
      cond_websites <- paste0(website_results_text,
                              " (n = ",
                              website_results,
                              ')\n')
    } else {
      cond_websites <- ''
    }
    if (is.na(organisation_results) == FALSE) {
      cond_organisation <- paste0(organisation_results_text,
                                  " (n = ",
                                  organisation_results,
                                  ')\n')
    } else {
      cond_organisation <- ''
    }
    if (is.na(citations_results) == FALSE) {
      cond_citation <- paste0(citations_results_text,
                              " (n = ",
                              citations_results,
                              ')')
    } else {
      cond_citation <- ''
    }
    
    cluster2 <- paste0("subgraph cluster2 {
    edge [color = White, 
          arrowhead = none, 
          arrowtail = none]
    13->14;
    edge [color = ", arrow_colour, ", 
        arrowhead = ", arrow_head, ", 
        arrowtail = ", arrow_tail, "]
    14->15; 15->16;
    15->17; 17->18;
    edge [color = ", arrow_colour, ", 
        arrowhead = none, 
        arrowtail = ", arrow_tail, "]
    17->B; 
    edge [color = ", arrow_colour, ", 
        arrowhead = ", arrow_head, ", 
        arrowtail = none,
        constraint = FALSE]
    B->12;
  }")
    othernodes <- paste0("node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  13 [label = '", other_text, "', style = 'rounded,filled', width = 7.5, height = 0.5, pos='",xstart+15,",",ystart+8.25,"!', tooltip = '", tooltips[5], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  14 [label = '", paste0('Records identified from:\n',
                         cond_websites,
                         cond_organisation,
                         cond_citation),
                         "', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+13,",",ystart+7,"!', tooltip = '", tooltips[6], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  15 [label = '", paste0(other_sought_reports_text,
                         '\n(n = ',
                         other_sought_reports,
                         ')'), "', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+13,",",ystart+4.5,"!', tooltip = '", tooltips[12], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  16 [label = '", paste0(other_notretrieved_reports_text,'\n(n = ',
                         other_notretrieved_reports,
                         ')'), "', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+17,",",ystart+4.5,"!', tooltip = '", tooltips[13], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  17 [label = '", paste0(other_assessed_text,
                         '\n(n = ',
                         other_assessed,
                         ')'),"', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+13,",",ystart+3.5,"!', tooltip = '", tooltips[16], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", greybox_colour, "]
  18 [label = '", paste0(other_excluded_text,
                         other_excluded_data), "', style = 'filled', width = 3.5, height = 0.5, pos='",xstart+17,",",ystart+other_excludedh,"!', tooltip = '", tooltips[17], "']\n
                       ")
    extraedges <- "16->18;"
    othernode13 <- "; 13"
    othernode14 <- "; 14"
    othernode1516 <- "; 15; 16"
    othernode1718 <- "; 17; 18"
    othernodeB <- "; B"
    
  } else {
    B <- ""
    cluster2 <- ""
    othernodes <- ""
    extraedges <- ""
    optnodesother <- ""
    othernode13 <- ""
    othernode14 <- ""
    othernode1516 <- ""
    othernode1718 <- ""
    othernodeB <- ""
    
  }
  
  if (any(!grepl("\\D", dbr_excluded)) == FALSE){
    dbr_excluded_data <- paste0(':',
                                paste(paste('\n', 
                                            dbr_excluded[,1], 
                                            ' (n = ', 
                                            dbr_excluded[,2], 
                                            ')', 
                                            sep = ''), 
                                      collapse = ''))
  } else {
    dbr_excluded_data <- paste0('\n', '(n = ', dbr_excluded, ')')
  }
  
  if (is.na(database_results) == FALSE) {
    cond_database <- paste0(database_results_text, 
                            ' (n = ',
                            database_results,
                            ')\n')
  } else {
    cond_database <- ''
  }
  if (is.na(register_results) == FALSE) {
    cond_register <- paste0(register_results_text, 
                            ' (n = ',
                            register_results,
                            ')')
  } else {
    cond_register <- ''
  }
  
  if (is.na(duplicates) == FALSE) {
    cond_duplicates <- paste0(stringr::str_wrap(paste0(duplicates_text,
                                                       ' (n = ',
                                                       duplicates,
                                                       ')'),
                                                width = 42),
                              '\n')
  } else {
    cond_duplicates <- ''
  }
  if (is.na(excluded_automatic) == FALSE) {
    cond_automatic <- paste0(stringr::str_wrap(paste0(excluded_automatic_text,
                                                      ' (n = ',
                                                      excluded_automatic,
                                                      ')'),
                                               width = 42),
                             '\n')
  } else {
    cond_automatic <- ''
  }
  if (is.na(excluded_other) == FALSE) {
    cond_exclother <- paste0(stringr::str_wrap(paste0(excluded_other_text, 
                                                      ' (n = ',
                                                      excluded_other,
                                                      ')'),
                                               width = 42))
  } else {
    cond_exclother <- ''
  }
  if (is.na(duplicates) == TRUE && is.na(excluded_automatic) == TRUE && is.na(excluded_other) == TRUE) {
    cond_duplicates <- paste0('(n = 0)')
  }
  
  if(is.na(new_studies) == FALSE) {
    cond_newstud <- paste0(stringr::str_wrap(new_studies_text, width = 40),
                           '\n(n = ',
                           new_studies,
                           ')\n')
  } else {
    cond_newstud <- ''
  }
  if(is.na(new_reports) == FALSE) {
    cond_newreports <- paste0(stringr::str_wrap(new_reports_text, width = 40),
                              '\n(n = ',
                              new_reports,
                              ')')
  } else {
    cond_newreports <- ''
  }
  
  x <- DiagrammeR::grViz(
    paste0("digraph TD {
  
  graph[splines=ortho, layout=neato, tooltip = 'Click the boxes for further information', outputorder=edgesfirst]
  
  ",
           previous_nodes,"
  node [shape = box,
        fontsize = ", fontsize,",
        fontname = ", font, ",
        color = ", title_colour, "]
  3 [label = '", newstud_text, "', style = 'rounded,filled', width = 7.5, height = 0.5, pos='",xstart+7,",",ystart+8.25,"!', tooltip = '", tooltips[3], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  4 [label = '", paste0('Records identified from:\n', 
                        cond_database, 
                        cond_register), "', width = 3.5, height = 0.5, height = 0.5, pos='",xstart+5,",",ystart+7,"!', tooltip = '", tooltips[4], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  5 [label = '", paste0('Records removed before screening:\n', 
                        cond_duplicates,
                        cond_automatic, 
                        cond_exclother),
           "', width = 3.5, height = 0.5, pos='",xstart+9,",",ystart+7,"!', tooltip = '", tooltips[7], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  6 [label = '", paste0(records_screened_text,
                        '\n(n = ',
                        records_screened,
                        ')'), "', width = 3.5, height = 0.5, height = 0.5, pos='",xstart+5,",",ystart+5.5,"!', tooltip = '", tooltips[8], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  7 [label = '", paste0(records_excluded_text,
                        '\n(n = ',
                        records_excluded,
                        ')'), "', width = 3.5, height = 0.5, pos='",xstart+9,",",ystart+5.5,"!', tooltip = '", tooltips[9], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  8 [label = '", paste0(dbr_sought_reports_text,
                        '\n(n = ',
                        dbr_sought_reports,
                        ')'), "', width = 3.5, height = 0.5, height = 0.5, pos='",xstart+5,",",ystart+4.5,"!', tooltip = '", tooltips[10], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  9 [label = '", paste0(dbr_notretrieved_reports_text,
                        '\n(n = ',
                        dbr_notretrieved_reports,
                        ')'), "', width = 3.5, height = 0.5, pos='",xstart+9,",",ystart+4.5,"!', tooltip = '", tooltips[11], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, "]
  10 [label = '", paste0(dbr_assessed_text,
                         '\n(n = ',
                         dbr_assessed,
                         ')'), "', width = 3.5, height = 0.5, pos='",xstart+5,",",ystart+3.5,"!', tooltip = '", tooltips[14], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, ", 
        fillcolor = White,
        style = filled]
  11 [label = '", paste0(dbr_excluded_text,
                         dbr_excluded_data), "', width = 3.5, height = 0.5, pos='",xstart+9,",",ystart+dbr_excludedh,"!', tooltip = '", tooltips[15], "']
  
  node [shape = box,
        fontname = ", font, ",
        color = ", main_colour, ", fillcolor = '', style = solid]
  12 [label = '", paste0(cond_newstud,
                         cond_newreports), "', width = 3.5, height = 0.5, pos='",xstart+5,",",ystart+1.5,"!', tooltip = '", tooltips[18], "']
  
  ",othernodes,
           
           finalnode,"
  
  node [shape = square, width = 0, color=White]\n",
           A,"
  ",B,"
  
  ",
           Aedge,"
  
  node [shape = square, width = 0, style=invis]
  C [label = '', width = 3.5, height = 0.5, pos='",xstart+9,",",ystart+3.5,"!', tooltip = '']
  
  subgraph cluster1 {
    edge [style = invis]
    3->4; 3->5;
    
    edge [color = ", arrow_colour, ", 
        arrowhead = ", arrow_head, ", 
        arrowtail = ", arrow_tail, ", 
        style = filled]
    4->5;
    4->6; 6->7;
    6->8; 8->9;
    8->10; 10->C;
    10->12;
    edge [style = invis]
    5->7;
    7->9;
    9->11;
    ",extraedges,"
  }
  
  ",cluster2,"
  
  ",
           bottomedge,"\n\n",
           prev_rank1,"\n",
           "{rank = same; ",prevnode1,"3",othernode13,"} 
  {rank = same; ",prevnode2,"4; 5",othernode14,"} 
  {rank = same; 6; 7} 
  {rank = same; 8; 9",othernode1516,"} 
  {rank = same; 10; 11",othernode1718,"} 
  {rank = same; 12",othernodeB,"} 
  
  }
  ")
  )
  
  # Append in vertical text on blue bars
  if (paste0(previous,  other) == 'TRUETRUE'){
    insertJS <- function(plot){
      javascript <- htmltools::HTML('
var theDiv = document.getElementById("node1");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'537\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Identification</text>";
var theDiv = document.getElementById("node2");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'356\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Screening</text>";
var theDiv = document.getElementById("node3");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'95\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Included</text>";
                              ')
      htmlwidgets::appendContent(plot, htmlwidgets::onStaticRenderComplete(javascript))
    }
    x <- insertJS(x)
  } else if (paste0(previous,  other) == 'FALSETRUE'){
    insertJS <- function(plot){
      javascript <- htmltools::HTML('
var theDiv = document.getElementById("node1");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'497\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Identification</text>";
var theDiv = document.getElementById("node2");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'315\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Screening</text>";
var theDiv = document.getElementById("node3");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'100\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Included</text>";
                              ')
      htmlwidgets::appendContent(plot, htmlwidgets::onStaticRenderComplete(javascript))
    }
    x <- insertJS(x)
  } else if (paste0(previous,  other) == 'TRUEFALSE'){
    insertJS <- function(plot){
      javascript <- htmltools::HTML('
var theDiv = document.getElementById("node1");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'536\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Identification</text>";
var theDiv = document.getElementById("node2");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'357\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Screening</text>";
var theDiv = document.getElementById("node3");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'95\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Included</text>";
                              ')
      htmlwidgets::appendContent(plot, htmlwidgets::onStaticRenderComplete(javascript))
    }
    x <- insertJS(x)
  } else {
    insertJS <- function(plot){
      javascript <- htmltools::HTML('
var theDiv = document.getElementById("node1");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'497\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Identification</text>";
var theDiv = document.getElementById("node2");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'315\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Screening</text>";
var theDiv = document.getElementById("node3");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'100\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Included</text>";
                              ')
      htmlwidgets::appendContent(plot, htmlwidgets::onStaticRenderComplete(javascript))
    }
    x <- insertJS(x)
  }
  
  if (interactive == TRUE) {
    x <- sr_flow_interactive(x, urls, previous = previous, other = other)
  }
  
  return(x)
}


#' Read in PRISMA flow diagram data
#' 
#' @description Read in a template CSV containing data for the flow diagram
#' @param data File to read in.
#' @return A list of objects needed to plot the flow diagram
#' @examples 
#' \dontrun{
#' data <- read.csv(file.choose());
#' data <- read_PRISMAdata(data);
#' attach(data);
#' }
#' @export
read_PRISMAdata <- function(data){
  
  #Set parameters
  previous_studies <- scales::comma(as.numeric(data[grep('previous_studies', data[,1]),]$n))
  previous_reports <- scales::comma(as.numeric(data[grep('previous_reports', data[,1]),]$n))
  register_results <- scales::comma(as.numeric(data[grep('register_results', data[,1]),]$n))
  database_results <- scales::comma(as.numeric(data[grep('database_results', data[,1]),]$n))
  website_results <- scales::comma(as.numeric(data[grep('website_results', data[,1]),]$n))
  organisation_results <- scales::comma(as.numeric(data[grep('organisation_results', data[,1]),]$n))
  citations_results <- scales::comma(as.numeric(data[grep('citations_results', data[,1]),]$n))
  duplicates <- scales::comma(as.numeric(data[grep('duplicates', data[,1]),]$n))
  excluded_automatic <- scales::comma(as.numeric(data[grep('excluded_automatic', data[,1]),]$n))
  excluded_other <- scales::comma(as.numeric(data[grep('excluded_other', data[,1]),]$n))
  records_screened <- scales::comma(as.numeric(data[grep('records_screened', data[,1]),]$n))
  records_excluded <- scales::comma(as.numeric(data[grep('records_excluded', data[,1]),]$n))
  dbr_sought_reports <- scales::comma(as.numeric(data[grep('dbr_sought_reports', data[,1]),]$n))
  dbr_notretrieved_reports <- scales::comma(as.numeric(data[grep('dbr_notretrieved_reports', data[,1]),]$n))
  other_sought_reports <- scales::comma(as.numeric(data[grep('other_sought_reports', data[,1]),]$n))
  other_notretrieved_reports <- scales::comma(as.numeric(data[grep('other_notretrieved_reports', data[,1]),]$n))
  dbr_assessed <- scales::comma(as.numeric(data[grep('dbr_assessed', data[,1]),]$n))
  dbr_excluded <- data.frame(reason = gsub(",.*$", "", unlist(strsplit(data[grep('dbr_excluded', data[,1]),]$n, split = '; '))), 
                             n = gsub(".*,", "", unlist(strsplit(data[grep('dbr_excluded', data[,1]),]$n, split = '; '))))
  other_assessed <- scales::comma(as.numeric(data[grep('other_assessed', data[,1]),]$n))
  other_excluded <- data.frame(reason = gsub(",.*$", "", unlist(strsplit(data[grep('other_excluded', data[,1]),]$n, split = '; '))), 
                               n = gsub(".*,", "", unlist(strsplit(data[grep('other_excluded', data[,1]),]$n, split = '; '))))
  new_studies <- scales::comma(as.numeric(data[grep('new_studies', data[,1]),]$n))
  new_reports <- scales::comma(as.numeric(data[grep('new_reports', data[,1]),]$n))
  total_studies <- scales::comma(as.numeric(data[grep('total_studies', data[,1]),]$n))
  total_reports <- scales::comma(as.numeric(data[grep('total_reports', data[,1]),]$n))
  tooltips <- stats::na.omit(data$tooltips)
  urls <- data.frame(box = data[!duplicated(data$box), ]$box, url = data[!duplicated(data$box), ]$url)
  
  #set text - if text >33 characters, 
  previous_text <- data[grep('prevstud', data[,3]),]$boxtext
  newstud_text <- data[grep('newstud', data[,3]),]$boxtext
  other_text <- data[grep('othstud', data[,3]),]$boxtext
  previous_studies_text <- data[grep('previous_studies', data[,1]),]$boxtext
  previous_reports_text <- data[grep('previous_reports', data[,1]),]$boxtext
  register_results_text <- data[grep('register_results', data[,1]),]$boxtext
  database_results_text <- data[grep('database_results', data[,1]),]$boxtext
  website_results_text <- data[grep('website_results', data[,1]),]$boxtext
  organisation_results_text <- data[grep('organisation_results', data[,1]),]$boxtext
  citations_results_text <- data[grep('citations_results', data[,1]),]$boxtext
  duplicates_text <- data[grep('duplicates', data[,1]),]$boxtext
  excluded_automatic_text <- data[grep('excluded_automatic', data[,1]),]$boxtext
  excluded_other_text <- data[grep('excluded_other', data[,1]),]$boxtext
  records_screened_text <- data[grep('records_screened', data[,1]),]$boxtext
  records_excluded_text <- data[grep('records_excluded', data[,1]),]$boxtext
  dbr_sought_reports_text <- data[grep('dbr_sought_reports', data[,1]),]$boxtext
  dbr_notretrieved_reports_text <- data[grep('dbr_notretrieved_reports', data[,1]),]$boxtext
  other_sought_reports_text <- data[grep('other_sought_reports', data[,1]),]$boxtext
  other_notretrieved_reports_text <- data[grep('other_notretrieved_reports', data[,1]),]$boxtext
  dbr_assessed_text <- data[grep('dbr_assessed', data[,1]),]$boxtext
  dbr_excluded_text <- data[grep('dbr_excluded', data[,1]),]$boxtext
  other_assessed_text <- data[grep('other_assessed', data[,1]),]$boxtext
  other_excluded_text <- data[grep('other_excluded', data[,1]),]$boxtext
  new_studies_text <- data[grep('new_studies', data[,1]),]$boxtext
  new_reports_text <- data[grep('new_reports', data[,1]),]$boxtext
  total_studies_text <- data[grep('total_studies', data[,1]),]$boxtext
  total_reports_text <- data[grep('total_reports', data[,1]),]$boxtext
  
  x <- list(previous_studies = previous_studies,
            previous_reports = previous_reports,
            register_results = register_results,
            database_results = database_results,
            website_results = website_results,
            organisation_results = organisation_results,
            citations_results = citations_results,
            duplicates = duplicates,
            excluded_automatic = excluded_automatic,
            excluded_other = excluded_other,
            records_screened = records_screened,
            records_excluded = records_excluded,
            dbr_sought_reports = dbr_sought_reports,
            dbr_notretrieved_reports = dbr_notretrieved_reports,
            other_sought_reports = other_sought_reports,
            other_notretrieved_reports = other_notretrieved_reports,
            dbr_assessed = dbr_assessed,
            dbr_excluded = dbr_excluded,
            other_assessed = other_assessed,
            other_excluded = other_excluded,
            new_studies = new_studies,
            new_reports = new_reports,
            total_studies = total_studies,
            total_reports = total_reports,
            previous_text = previous_text,
            newstud_text = newstud_text,
            other_text = other_text,
            previous_studies_text = previous_studies_text,
            previous_reports_text = previous_reports_text,
            register_results_text = register_results_text,
            database_results_text = database_results_text,
            website_results_text = website_results_text,
            organisation_results_text = organisation_results_text,
            citations_results_text = citations_results_text,
            duplicates_text = duplicates_text,
            excluded_automatic_text = excluded_automatic_text,
            excluded_other_text = excluded_other_text,
            records_screened_text = records_screened_text,
            records_excluded_text = records_excluded_text,
            dbr_sought_reports_text = dbr_sought_reports_text,
            dbr_notretrieved_reports_text = dbr_notretrieved_reports_text,
            other_sought_reports_text = other_sought_reports_text,
            other_notretrieved_reports_text = other_notretrieved_reports_text,
            dbr_assessed_text = dbr_assessed_text,
            dbr_excluded_text = dbr_excluded_text,
            other_assessed_text = other_assessed_text,
            other_excluded_text = other_excluded_text,
            new_studies_text = new_studies_text,
            new_reports_text = new_reports_text,
            total_studies_text = total_studies_text,
            total_reports_text = total_reports_text,
            tooltips = tooltips,
            urls = urls)
  
  return(x)
  
}

sr_flow_interactive <- function(plot, 
                                urls,
                                previous,
                                other) {
  
  if(paste0(previous, other) == 'TRUETRUE'){
    link <- data.frame(boxname = c('identification', 'screening', 'included', 'prevstud', 'box1', 'newstud', 'box2', 'box3', 'box4', 'box5', 'box6', 'box7', 
                                   'box8', 'box9', 'box10', 'othstud', 'box11', 'box12', 'box13', 'box14', 'box15', 'box16', 'A', 'B'), 
                       node = paste0('node', seq(1, 24)))
    target <- c('node1', 'node2', 'node3', 'node4', 'node5', 'node23', 'node6', 'node7', 'node8', 'node9', 'node10', 'node11', 'node12', 'node13', 'node14', 
                'node15', 'node22', 'node16', 'node17', 'node18', 'node19', 'node20', 'node21', 'node24')
  } else if(paste0(previous, other) == 'FALSETRUE'){
    link <- data.frame(boxname = c('identification', 'screening', 'included', 'newstud', 'box2', 'box3', 'box4', 'box5', 'box6', 'box7', 
                                   'box8', 'box9', 'box10', 'othstud', 'box11', 'box12', 'box13', 'box14', 'box15', 'B'), 
                       node = paste0('node', seq(1, 20)))
    target <- c('node1', 'node2', 'node3', 'node4', 'node5', 'node6', 'node7', 'node8', 'node9', 'node10', 'node11', 'node12', 'node13', 'node14', 'node15', 
                'node16', 'node17', 'node18', 'node19', 'node20')
  }
  else if(paste0(previous, other) == 'TRUEFALSE'){
    link <- data.frame(boxname = c('identification', 'screening', 'included', 'prevstud', 'box1', 'newstud', 'box2', 'box3', 'box4', 'box5', 'box6', 'box7', 
                                   'box8', 'box9', 'box10', 'box16', 'A'), 
                       node = paste0('node', seq(1, 17)))
    target <- c('node1', 'node2', 'node3', 'node4', 'node5', 'node6', 'node7', 'node8', 'node9', 'node10', 'node11', 'node12', 'node13', 'node14', 'node15', 
                'node16', 'node17')
  }
  else {
    link <- data.frame(boxname = c('identification', 'screening', 'included', 'newstud', 'box2', 'box3', 'box4', 'box5', 'box6', 'box7', 
                                   'box8', 'box9', 'box10'), 
                       node = paste0('node', seq(1, 13)))
    target <- c('node1', 'node2', 'node3', 'node4', 'node5', 'node6', 'node7', 'node8', 'node9', 'node10', 'node11', 'node12', 'node13')
  }
  
  
  link <- merge(link, urls, by.x = 'boxname', by.y = 'box', all.x = TRUE)
  link <- link[match(target, link$node),]
  node <- link$node
  url <- link$url
  
  #the following function produces three lines of JavaScript per node to add a specified hyperlink for the node, pulled in from nodes.csv
  myfun <- function(node, 
                    url){
    t <- paste0('const ', node, ' = document.getElementById("', node, '");
  var link', node, ' = "<a href=\'', url, '\' target=\'_blank\'>" + ', node, '.innerHTML + "</a>";
  ', node, '.innerHTML = link', node, ';
  ')
  }
  #the following code adds the location link for the new window
  javascript <- htmltools::HTML(paste(mapply(myfun, 
                                             node, 
                                             url), 
                                      collapse = '\n'))  
  htmlwidgets::prependContent(plot, 
                              htmlwidgets::onStaticRenderComplete(javascript))
}



prisma_pdf <- function(x, filename = "prisma.pdf") {
  utils::capture.output({
    rsvg::rsvg_pdf(svg = charToRaw(DiagrammeRsvg::export_svg(x)),
                   file = filename)
  })
  invisible()
}
prisma_png <- function(x, filename = "prisma.png") {
  utils::capture.output({
    rsvg::rsvg_png(svg = charToRaw(DiagrammeRsvg::export_svg(x)),
                   file = filename)
  })
  invisible()
}



dat <- data.frame(
  country = c('us', 'nl', 'de')
  #flag = c(
  #   '<img src = "colombia.jpg" height="52" ></img>',
  #       '<img src = "united.jpg" height="52" ></img>'
)

#dat$flag1 <- sprintf ('<img src = "http://flagpedia.net/data/flags/mini/%s.png" height="52" ></img>', dat$country)
dat$flag1 <- sprintf ('<img src = "https://flagpedia.net/data/flags/w2560/%s.png" height="52" ></img>', dat$country)

#My APA-format theme
apatheme=theme_bw(base_size = 23)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        strip.text.y.left = element_text(size = 27,angle = 90),
        axis.text.y = element_text(hjust = 23),
        axis.title.y = element_text(face = "bold", size=25),
        text=element_text(family='Times', size=29),
        legend.position='none')

bg.picker <- function(z){
  if(is.na(z)){return("black")}
  else if(z == "Low"){return("forestgreen")}
  else if(z == "Moderate"){return("lightgreen")}
  else if(z == "High"){return("pink")}
  else if(z == "Serious"){return("salmon")}
  else if(z == "Critical"){return("red")}
  else if(z == "Uncertain"){return("wheat")}
  #else if( z > 20 & z <= 80){return("yellow")}
  #else {return("pink")}
}

##### color for new domains
bg.picker1 <- function(z){
  if(is.na(z)){return("black")}
  else if(z == "Definitely yes (low risk of bias)"){return("forestgreen")}
  else if(z == "Probably yes"){return("lightgreen")}
  else if(z == "Probably no"){return("pink")}
  else if(z == "Definitely no (high risk of bias)"){return("red")}
  else if(z == "Unclear risk of bias for one or more key domains."){return("wheat")}
  else if(z == "Low risk of bias for all key domains."){return("forestgreen")}
  else if(z == "High risk of bias for one or more key domains."){return("red")}
  #else if( z > 20 & z <= 80){return("yellow")}
  #else {return("pink")}
}

#forest <- forest %>% mutate(inter = ifelse(is.na(lowerci), yi, paste(forest$yi,"[",forest$lowerci, ",", forest$upperci,"]")), Reference = paste(forest$id, ".", forest$study))
#forest123 <- forest123 %>% mutate( Reference = paste(forest123$id, ".", forest123$study))
#forest123 <- forest123 %>% mutate( Reference = paste(forest123$study, "(", forest123$id,")"))
shinyServer(function(input, output){ 
  
  #source("reactive.R", local = TRUE)
  #source("forestROBPlot.R",  local = TRUE)
  #source("forestROBPlotExperiment.R", local = TRUE)
  #  observeEvent(input$chk, {
  #    if (input$chk) hide('mytable1234') else show('mytable1234')
  #  })
  
  
  ## descriptive plots ####
  ## * switch text ####
  output$eda_text <- renderUI({
    switch(
      input$eda_btn,
      "sp" = p("Most articles were published in North America and some in Europe."),
      "ts" = p("This plot shows the date of publication of studies included in the review"),
      "coef" = p("The following table shows the number of reported outcomes grouped in broad categories (i.e lower and upper respiratory tracts, MRSA etc) 
                    for the relevant studies included in the last review. We observe that nearly 500 outcomes were extracted from the 16 publications and 10 study populations. For several study populations, numerous correlated outcomes were compared with
numerous correlated exposures. This approach increases
the potential to discover important associations and also
increases the potential for identification of false associations due to random error (increased type 1 error)."),
      "tabl" = p("The following list contains the titles of the articles included in this review.")
    )
  })
  ## * switch plot ####
  output$eda_plot <- renderUI({
    switch(
      input$eda_btn,
      "sp" = fluidRow(
        column(width = 6, leafletOutput("map") %>% withSpinner()),
        #column(width = 6, plotlyOutput("geobar"),dataTableOutput('mytable1234') %>% withSpinner())#,
        column(width = 6, plotlyOutput("geobar") %>% withSpinner())),
      "ts" = timevisOutput("timeline"),
      "coef" = plotOutput("measure_all", hover = "plot_hover") %>% withSpinner()#,
    #"tabl" = DT::dataTableOutput("mytable1234")
    )
  })
  
  ##
  
  
  
  ## * geographic distribution ####
  output$map <- renderLeaflet({
    
    leaflet(cafoo) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%  
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
      addMarkers(lng = cafoo$long, lat = cafoo$lat,
                 #radius = log(cafoo$`Number of Studies`)*8,
                 popup = ifelse(cafoo$Country=="United States", paste("Country:",cafoo$Country,"<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/23111006/'>Wing et al. 2013</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/7620910/'>Schiffman et al. 1995</a></b>","<br>",
                                                                      "<b><a href='https://link.springer.com/article/10.1007/s10745-005-1653-3'>Bullers 2005</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/21228696/'>Schinasi et al. 2011</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/19890165/'>Horton et al. 2009</a></b>","<br>",
                                                                      "<b><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4517575/'>Mirabelli et al. 2006</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/15866765/'>Schiffman et al. 2005</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/16075904/'>Avery et al. 2004</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/24958086/'>Schinasi et al. 2014</a></b>","<br>"
                 ), ifelse(cafoo$Country=="Germany", paste("Country:",cafoo$Country,"<br>","<b><a href='https://pubmed.ncbi.nlm.nih.gov/21864103/'>Schulze et al. 2011</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17435437/'>Radon et al. 2007</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17039438/'>Hoopmann et al. 2006</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/16379061/'>Radon et al. 2005</a></b>","<br>"
                 ),
                 paste("Country:",cafoo$Country,"<br>",
                       "<b><a href='https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0038843'>Smit et al. 2012</a></b>","<br>",
                       "<b><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3810935/'>Feingold et al. 2012</a></b>","<br>",
                       "<b><a href='https://oem.bmj.com/content/71/2/134.long'>Smit et al. 2014</a></b>","<br>"
                 ) )))
    
    
    
    
  })
  
  #####
  output$geobar <- renderPlotly({
    gg <- cafo2 %>% distinct() %>%
      group_by(Country) %>% summarise(Count = n()) %>% 
      mutate(Country = forcats::fct_reorder(factor(Country), Count)) %>%
      ggplot(aes(x = Country, y = Count)) + 
      geom_bar(aes(fill = Country), stat = "identity") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(gg, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)
  })
  ## * timeline ####
  output$timeline <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% select(paperInfo, paperYear) %>% distinct() %>% 
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period 
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ## Insert into a dataframe 
    datt1 <- data.frame(
      ## make it reactive
      id = 1:nrow(testtimeline),   
      content = testtimeline$weblink,
      start = testtimeline$year,
      end = NA
    )
    timevis(datt1, showZoom = F)
  })
  
  
  ## * outcome ####
  output$measure_all <- renderPlot({
    colnames(dataset)[which(names(dataset) == "Categorized.class")] <- "Broad Outcome Category"
    dataset %>% ggplot(aes(x = paperInfo ) ) +
      geom_bar(aes(fill = `Broad Outcome Category`)) + coord_flip() + 
      scale_fill_brewer(palette = "Set3") +
      #labs(x = "", fill = "Health Outcome Group") +
      labs(x = "", fill = "Broad Outcome Category") +
      xlab("Paper") +
      ylab("Number of Reported Outcomes") + 
      theme(plot.background = element_rect(fill = "#BFD5E3"),
            panel.background = element_rect(fill = "white"),
            axis.line.x = element_line(color = "grey"))
    #ggplotly(gg, tooltip = c('Broad Outcome Category', 'count')) %>% config(displayModeBar = T)
    
    
  })
  
  ## forest fitlers ####
  selected_class <- reactive({
    case_when(
      grepl("low_rsp", input$sidebar) ~ "Lower Respiratory",
      grepl("up_rsp", input$sidebar) ~ "Upper Respiratory",
      grepl("ar_rsp", input$sidebar) ~ "Antimicrobial resistance",
      grepl("gi_rsp", input$sidebar) ~ "Gastrointestinal diseases",
      grepl("Neur_rsp", input$sidebar) ~ "Neurologic",
      TRUE ~ "Other"
    )
  }) 
  selected_id <- reactive({
    dataset %>% filter(Categorized.class==selected_class()) %>% 
      pull(Refid) %>% unique()
  })
  
  
  ##### only measure of association
  output$expo_var_1 <- renderUI({
    choices1 <- forest$effect_z
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_b",
                "Effect size",
                choices = unique(choices1), 
                selected = choices1[1])
    
  })
  
  ### up
  
  output$expo_var_1_up <- renderUI({
    #choices1 <- forest_sabado$effect_measure
    choices1 <- forest_joint$mm
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_b_up",
                "Effect size",
                choices = unique(choices1), 
                selected = choices1[1])
    
  })
  
  
  
  output$expo_var_1_up_state <- renderUI({
    choices1 <- up_forest_state1$mm
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_b_up_state",
                "Effect size",
                choices = unique(choices1), 
                selected = choices1[1])
    
  })
  ### AR
  
  output$expo_var_1_ar <- renderUI({
    choices1 <- forest$effect_z
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_b_ar",
                "Effect size",
                choices = unique(choices1), 
                selected = choices1[1])
    
  })
  
  ### GI
  
  output$expo_var_1_gi <- renderUI({
    choices1 <- up_forest_melo$effect_z
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_b_gi",
                "Effect size",
                choices = unique(choices1), 
                selected = choices1[1])
    
  })
  
  ### Neuro
  
  output$expo_var_1_Neur <- renderUI({
    choices1 <- forest$effect_z
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_b_Neur",
                "Effect size",
                choices = unique(choices1), 
                selected = choices1[1])
    
  })
  
  output$expo_var_2 <- renderUI({
    choices2 <- forest$t_expo
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_c",
                "Type of Exposure",
                choices = unique(choices2),
                
                selected = choices2[1])
  })
  
  #### Upper R
  
  output$expo_var_2_up <- renderUI({
    choices2 <- forest$t_expo
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_c_up",
                "Type of Exposure",
                choices = unique(choices2),
                
                selected = choices2[1])
  })
  
  #### AR
  
  output$expo_var_2_ar <- renderUI({
    choices2 <- forest$t_expo
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_c_ar",
                "Type of Exposure",
                choices = unique(choices2),
                
                selected = choices2[1])
  })
  
  #### Gastro
  
  output$expo_var_2_gi <- renderUI({
    choices2 <- forest$t_expo
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_c_gi",
                "Type of Exposure",
                choices = unique(choices2),
                
                selected = choices2[1])
  })
  
  #### Neuro
  
  output$expo_var_2_Neur <- renderUI({
    choices2 <- forest$t_expo
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_c_Neur",
                "Type of Exposure",
                choices = unique(choices2),
                
                selected = choices2[1])
  })
  
  
  
  
  ## lower respiratory ####
  ## * intro #####
  output$low_res_intro_text <- renderUI({
    switch(
      input$low_res_btn,
      "sp" = p("Articles related to lower respiratory disease were published in Germany, United States and Netherlands."),
      "ts" = p("This timeline shows the date of publication of references in which outcome related to lower respiratory tract were studied. This category was the most analyzed with 9 out 16 relevant references included."),
      "coef" = p("The following table shows all the outcomes that were categorized as lower respiratory tract. Asthma and wheeze related conditions were the most common outcomes grouped in this category.")
    )
  })
  
  #####copying for upper
  
  output$up_res_intro_text <- renderUI({
    switch(
      input$up_res_btn,
      "sp" = p("Most articles related to upper respiratory disease were published in United States, Netherlands and Germany."),
      "ts" = p("This timeline shows the date of publication of references in which outcome related to upper respiratory tract were studied. This category was analyzed by 6 out 16 relevant references included."),
      "coef" = p("The following table shows all the outcomes that were categorized as upper respiratory tract. Allergic rhinitis and nasal irritation were the most common variables grouped in this category.")
    )
  })
  
  #####copying for AMR
  output$ar_res_intro_text <- renderUI({
    switch(
      input$ar_res_btn,
      "sp" = p("Most articles related to Antimicrobial resistance were published in United States and Netherlands"),
      "ts" = p("This timeline shows the date of publication of references in which outcome related to Antimicrobial resistance were studied. This category was analyzed by 2 out 16 relevant references included."),
      "coef" = p("The following table shows all the outcomes that were categorized as Antimicrobial resistance outcomes.")
    )
  })
  
  #####copying for GI
  
  output$gi_res_intro_text <- renderUI({
    switch(
      input$gi_res_btn,
      "sp" = p("Articles related to Gastrointestinal diseases were published only in United States."),
      "ts" = p("This timeline shows the date of publication of references in which outcome related to gastrointestinal tract were studied. This category was analyzed by 2 out 16 relevant references included."),
      "coef" = p("The following table shows all the outcomes that were categorized as gastrointestinal tract. Diarhea, nausea and poor appetite were the most common outcomes grouped in this category.")
    )
  })
  
  output$Neur_res_intro_text <- renderUI({
    switch(
      input$Neur_res_btn,
      "sp" = p("Articles related to Neurologic diseases were published only in United States."),
      "ts" = p("This timeline shows the date of publication of references in which outcome related to Neurologic diseases were studied. This category was analyzed by 3 out 16 relevant references included."),
      "coef" = p("The following table shows all the outcomes that were categorized as Neurologic diseases. Headache and dizziness related conditions were the most common outcomes grouped in this category.")
    )
  })
  
  output$low_res_intro_plot <- renderUI({
    switch(
      input$low_res_btn,
      "sp" = fluidRow(
        column(width = 6, leafletOutput("map_low_res") %>% withSpinner()),
        column(width = 6, plotlyOutput("geobar_low_res") %>% withSpinner())),
      "ts" = timevisOutput("timeline_low_res"),
      #"coef" = plotlyOutput("measure_all_low_res") %>% withSpinner()
      "coef" = dataTableOutput("measure_all_low_res") %>% withSpinner()
    )
  })
  
  #######copying for uppper
  
  output$up_res_intro_plot <- renderUI({
    switch(
      input$up_res_btn,
      "sp" = fluidRow(
        column(width = 6, leafletOutput("map_up_res") %>% withSpinner()) ,
        column(width = 6, plotlyOutput("geobar_up_res") %>% withSpinner())),
      "ts" = timevisOutput("timeline_up_res"),
      #"coef" = plotlyOutput("measure_all_low_res") %>% withSpinner()
      "coef" = dataTableOutput("measure_all_up_res") %>% withSpinner()
    )
  })
  
  #######copying for AMR
  
  output$ar_res_intro_plot <- renderUI({
    switch(
      input$ar_res_btn,
      "sp" = fluidRow(
        column(width = 6, leafletOutput("map_ar_res") %>% withSpinner()) ,
        column(width = 6, plotlyOutput("geobar_ar_res") %>% withSpinner())),
      "ts" = timevisOutput("timeline_ar_res"),
      #"coef" = plotlyOutput("measure_all_low_res") %>% withSpinner()
      "coef" = dataTableOutput("measure_all_ar_res") %>% withSpinner()
    )
  })
  
  #######copying for GI
  
  output$gi_res_intro_plot <- renderUI({
    switch(
      input$gi_res_btn,
      "sp" = fluidRow(
        column(width = 6, leafletOutput("map_gi_res") %>% withSpinner()) ,
        column(width = 6, plotlyOutput("geobar_gi_res") %>% withSpinner())),
      "ts" = timevisOutput("timeline_gi_res"),
      #"coef" = plotlyOutput("measure_all_low_res") %>% withSpinner()
      "coef" = dataTableOutput("measure_all_gi_res") %>% withSpinner()
    )
  })
  
  output$Neur_res_intro_plot <- renderUI({
    switch(
      input$Neur_res_btn,
      "sp" = fluidRow(
        column(width = 6, leafletOutput("map_Neur_res") %>% withSpinner()),
        column(width = 6, plotlyOutput("geobar_Neur_res") %>% withSpinner())),
      "ts" = timevisOutput("timeline_Neur_res"),
      #"coef" = plotlyOutput("measure_all_low_res") %>% withSpinner()
      "coef" = dataTableOutput("measure_all_Neur_res") %>% withSpinner()
    )
  })
  
  ## ** geographic distribution ####
  output$map_low_res <- renderLeaflet({
    leaflet(cafoo) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%  
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
      addMarkers(lng = cafoo$long, lat = cafoo$lat,
                 #radius = log(cafoo$`Number of Studies`)*8,
                 popup = ifelse(cafoo$Country=="United States", paste("Country:",cafoo$Country,"<br>",
                                                                      
                                                                      
                                                                      
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/21228696/'>Schinasi et al. 2011</a></b>","<br>",
                                                                      
                                                                      "<b><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4517575/'>Mirabelli et al. 2006</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/15866765/'>Schiffman et al. 2005</a></b>","<br>"
                                                                      
                                                                      
                 ), ifelse(cafoo$Country=="Germany", paste("Country:",cafoo$Country,"<br>","<b><a href='https://pubmed.ncbi.nlm.nih.gov/21864103/'>Schulze et al. 2011</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17435437/'>Radon et al. 2007</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17039438/'>Hoopmann et al. 2006</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/16379061/'>Radon et al. 2005</a></b>","<br>"
                 ),
                 paste("Country:",cafoo$Country,"<br>",
                       "<b><a href='https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0038843'>Smit et al. 2012</a></b>","<br>",
                       
                       "<b><a href='https://oem.bmj.com/content/71/2/134.long'>Smit et al. 2014</a></b>","<br>"
                 ) )))
  })
  
  #####copying for upper
  
  output$map_up_res <- renderLeaflet({
    leaflet(cafoo) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%  
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
      addMarkers(lng = cafoo$long, lat = cafoo$lat,
                 #radius = log(cafoo$`Number of Studies`)*8,
                 popup = ifelse(cafoo$Country=="United States", paste("Country:",cafoo$Country,"<br>",
                                                                      
                                                                      
                                                                      
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/21228696/'>Schinasi et al. 2011</a></b>","<br>",
                                                                      
                                                                      
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/15866765/'>Schiffman et al. 2005</a></b>","<br>"
                                                                      
                                                                      
                 ), ifelse(cafoo$Country=="Germany", paste("Country:",cafoo$Country,"<br>","<b><a href='https://pubmed.ncbi.nlm.nih.gov/21864103/'>Schulze et al. 2011</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17435437/'>Radon et al. 2007</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17039438/'>Hoopmann et al. 2006</a></b>","<br>"
                 ),
                 paste("Country:",cafoo$Country,"<br>",
                       
                       
                       "<b><a href='https://oem.bmj.com/content/71/2/134.long'>Smit et al. 2014</a></b>","<br>"
                 ) )))
  })
  
  #####copying for AMR
  
  output$map_ar_res <- renderLeaflet({
    leaflet(cafoo) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%  
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
      addMarkers(lng = cafoo$long, lat = cafoo$lat,
                 #radius = log(cafoo$`Number of Studies`)*8,
                 popup = ifelse(cafoo$Country=="United States", paste("Country:",cafoo$Country,"<br>",
                                                                      
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/24958086/'>Schinasi et al. 2014</a></b>","<br>"
                                                                      
                 ), ifelse(cafoo$Country=="Germany", paste("Country:",cafoo$Country,"<br>",
                                                           "No Records"
                                                                            ),
                 paste("Country:",cafoo$Country,"<br>",
                       
                       
                       "<b><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3810935/'>Feingold et al. 2012</a></b>","<br>"
                 ) )))
  })
  
  ####copying for GI
  
  output$map_gi_res <- renderLeaflet({
    leaflet(cafoo) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%  
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
      addMarkers(lng = cafoo$long, lat = cafoo$lat,
                 #radius = log(cafoo$`Number of Studies`)*8,
                 popup = ifelse(cafoo$Country=="United States", paste("Country:",cafoo$Country,"<br>",
                                                                      
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/15866765/'>Schiffman et al. 2005</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/21228696/'>Schinasi et al. 2011</a></b>","<br>"
                                                                      
                 ), ifelse(cafoo$Country=="Germany", paste("Country:",cafoo$Country,"<br>",
                                                           "No Records"
                 ),
                 paste("Country:",cafoo$Country,"<br>",
                       
                       
                       "No Records"
                 ) )))
  })
  
  ####copying for Neurologic
  
  output$map_Neur_res <- renderLeaflet({
    leaflet(cafoo) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%  
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
      addMarkers(lng = cafoo$long, lat = cafoo$lat,
                 #radius = log(cafoo$`Number of Studies`)*8,
                 popup = ifelse(cafoo$Country=="United States", paste("Country:",cafoo$Country,"<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/19890165/'>Horton et al. 2009</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/15866765/'>Schiffman et al. 2005</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/21228696/'>Schinasi et al. 2011</a></b>","<br>"
                                                                      
                 ), ifelse(cafoo$Country=="Germany", paste("Country:",cafoo$Country,"<br>",
                                                           "No Records"
                 ),
                 paste("Country:",cafoo$Country,"<br>",
                       
                       
                       "No Records"
                 ) )))
  })
  
  output$geobar_low_res <- renderPlotly({
    gg <- cafo2 %>% filter(Refid %in% selected_id()) %>% distinct() %>%
      group_by(Country) %>% summarise(Count = n()) %>%
      mutate(Country = forcats::fct_reorder(factor(Country), Count)) %>%
      ggplot(aes(x = Country, y = Count)) +
      geom_bar(aes(fill = Country), stat = "identity") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(gg, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)
  })
  
  ########copying for up
  
  output$geobar_up_res <- renderPlotly({
    gg <- cafo2 %>% filter(Refid %in% selected_id()) %>% distinct() %>%
      group_by(Country) %>% summarise(Count = n()) %>%
      mutate(Country = forcats::fct_reorder(factor(Country), Count)) %>%
      ggplot(aes(x = Country, y = Count)) +
      geom_bar(aes(fill = Country), stat = "identity") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(gg, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)
  })
  
  ########copying for AMR
  
  output$geobar_ar_res <- renderPlotly({
    gg <- cafo2 %>% filter(Refid %in% selected_id()) %>% distinct() %>%
      group_by(Country) %>% summarise(Count = n()) %>%
      mutate(Country = forcats::fct_reorder(factor(Country), Count)) %>%
      ggplot(aes(x = Country, y = Count)) +
      geom_bar(aes(fill = Country), stat = "identity") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(gg, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)
  })
  
  ########copying for GI
  
  output$geobar_gi_res <- renderPlotly({
    gg <- cafo2 %>% filter(Refid %in% selected_id()) %>% distinct() %>%
      group_by(Country) %>% summarise(Count = n()) %>%
      mutate(Country = forcats::fct_reorder(factor(Country), Count)) %>%
      ggplot(aes(x = Country, y = Count)) +
      geom_bar(aes(fill = Country), stat = "identity") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(gg, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)
  })
  
  ########copying for Neurologic
  
  output$geobar_Neur_res <- renderPlotly({
    gg <- cafo2 %>% filter(Refid %in% selected_id()) %>% distinct() %>%
      group_by(Country) %>% summarise(Count = n()) %>%
      mutate(Country = forcats::fct_reorder(factor(Country), Count)) %>%
      ggplot(aes(x = Country, y = Count)) +
      geom_bar(aes(fill = Country), stat = "identity") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(gg, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)
  })
  ## ** timeline ####
  output$timeline_low_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(Refid, paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ###
    timedata2 <- testtimeline %>% filter (ids %in% timedata$Refid)%>% 
      select(weblink, year, ids) %>% distinct() 
    ## Insert into a dataframe 
    datt2 <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),   
      content = timedata2$weblink,
      start = timedata2$year,
      end = NA
    )
    timevis(datt2, showZoom = F)
  })
  
  #######copying for up
  
  output$timeline_up_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(Refid, paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ###
    timedata2 <- testtimeline %>% filter (ids %in% timedata$Refid)%>% 
      select(weblink, year, ids) %>% distinct() 
    ## Insert into a dataframe 
    datt2 <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),   
      content = timedata2$weblink,
      start = timedata2$year,
      end = NA
    )
    timevis(datt2, showZoom = F)
  })
  
  #######copying for AMR
  
  output$timeline_ar_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(Refid, paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ###
    timedata2 <- testtimeline %>% filter (ids %in% timedata$Refid)%>% 
      select(weblink, year, ids) %>% distinct() 
    ## Insert into a dataframe 
    datt2 <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),   
      content = timedata2$weblink,
      start = timedata2$year,
      end = NA
    )
    timevis(datt2, showZoom = F)
  })
  
  ##### copying for GI
  
  output$timeline_gi_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(Refid, paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ###
    timedata2 <- testtimeline %>% filter (ids %in% timedata$Refid)%>% 
      select(weblink, year, ids) %>% distinct() 
    ## Insert into a dataframe 
    datt2 <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),   
      content = timedata2$weblink,
      start = timedata2$year,
      end = NA
    )
    timevis(datt2, showZoom = F)
  })
  
  #######copying for Neurologic
  
  output$timeline_Neur_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(Refid, paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ###
    timedata2 <- testtimeline %>% filter (ids %in% timedata$Refid)%>% 
      select(weblink, year, ids) %>% distinct() 
    ## Insert into a dataframe 
    datt2 <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),   
      content = timedata2$weblink,
      start = timedata2$year,
      end = NA
    )
    timevis(datt2, showZoom = F)
  })
  
  
  ##### table outcomes Lower R
  
  output$measure_all_low_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    names(sat)[1] <- "Outcome category"
    names(sat)[2] <- "Outcome variable analyzed"
    names(sat)[3] <- "Number of times the variable was analyzed"
    
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=T, pageLength = 50))
    #subset(cafoo, cafoo$`Country` == "Germany")
    #broadfilter <- subset(dataset, Categorized.class == selected_class())
  })
  
  #######copying for up
  
  output$measure_all_up_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    names(sat)[1] <- "Outcome category"
    names(sat)[2] <- "Outcome variable analyzed"
    names(sat)[3] <- "Number of times the variable was analyzed"
    
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=T, pageLength = 50))
    #subset(cafoo, cafoo$`Country` == "Germany")
    #broadfilter <- subset(dataset, Categorized.class == selected_class())
  })
  
  #######copying for AMR
  
  output$measure_all_ar_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    names(sat)[1] <- "Outcome category"
    names(sat)[2] <- "Outcome variable analyzed"
    names(sat)[3] <- "Number of times the variable was analyzed"
    
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=T, pageLength = 50))
    #subset(cafoo, cafoo$`Country` == "Germany")
    #broadfilter <- subset(dataset, Categorized.class == selected_class())
  })
  
  #######copying for GI
  
  output$measure_all_gi_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    
    names(sat)[1] <- "Outcome category"
    names(sat)[2] <- "Outcome variable analyzed"
    names(sat)[3] <- "Number of times the variable was analyzed"
    
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=T, pageLength = 50))
    #subset(cafoo, cafoo$`Country` == "Germany")
    #broadfilter <- subset(dataset, Categorized.class == selected_class())
  })
  
  #######copying for Neurologic
  
  output$measure_all_Neur_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    
    names(sat)[1] <- "Outcome category"
    names(sat)[2] <- "Outcome variable analyzed"
    names(sat)[3] <- "Number of times the variable was analyzed"
    
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=T, pageLength = 50))
    #subset(cafoo, cafoo$`Country` == "Germany")
    #broadfilter <- subset(dataset, Categorized.class == selected_class())
  })
  
  ## ** outcome ####
  # output$measure_all_low_res <- renderPlotly({
  # browser()
  #   gg_low_res <- dataset %>%
  #    filter(`Categorized.class` == selected_class()) %>% 
  #   ggplot(aes(x = paperInfo)) +
  #     geom_bar(aes(fill = Outcome.variable)) + coord_flip() +
  #     scale_fill_brewer(palette = "Set3") +
  #     labs(x = "", fill = "Health Outcome Group") +
  #    xlab("Study") +
  #     ylab("Number of Reported Outcomes") + 
  #     theme(plot.background = element_rect(fill = "#BFD5E3"),
  #          panel.background = element_rect(fill = "white"),
  #          axis.line.x = element_line(color = "grey"))
  #  ggplotly(gg_low_res) %>% layout(showlegend = FALSE)
  # })
  
  
  #####################
  ## * forest plot ####
  #####################
  selected_state <- reactive({
    input$plot_low_selected
  })
  
  ##### For Upper
  
  selected_state_up <- reactive({
    input$plot_up_selected
  })
  
  ##### For AR
  
  selected_state_ar <- reactive({
    input$plot_ar_selected
  })
  
  ##### For Gastro
  
  selected_state_gi <- reactive({
    input$plot_gi_selected
  })
  
  ##### For Neuro
  
  selected_state_Neur <- reactive({
    input$plot_Neur_selected
  })
  
  #####
  
  forest_data <- reactive({
   
    forest_data <- forest %>% filter(
      effect_z == input$expo_b &  t_expo == input$expo_c)
  })
  
  
  #output$low_rsp_dt <- DT::renderDataTable({
  #  forest_data()
  #newdatas <- forest_data()[c(1,2)]
  #})
  
  output$out1 <- renderInfoBox({
    infoBox("How to use a forest plot",  
            a("Click here to go to the tutorial", onclick = "openTab('tutorial')", href="#", style = "font-size: 90%;"),
            icon = icon("chalkboard-teacher"), color = "blue"
    )
  }) 
 #### for Upper R 
  output$out12 <- renderInfoBox({
    infoBox("How to use a forest plot",  
            a("Click here to go to the tutorial", onclick = "openTab('tutorial')", href="#", style = "font-size: 90%;"),
            icon = icon("chalkboard-teacher"), color = "blue"
    )
  })
  
  #### for AR 
  output$out12_ar <- renderInfoBox({
    infoBox("How to use a forest plot",  
            a("Click here to go to the tutorial", onclick = "openTab('tutorial')", href="#", style = "font-size: 90%;"),
            icon = icon("chalkboard-teacher"), color = "blue"
    )
  })
  
  #### for GI 
  output$out12_gi <- renderInfoBox({
    infoBox("How to use a forest plot",  
            a("Click here to go to the tutorial", onclick = "openTab('tutorial')", href="#", style = "font-size: 90%;"),
            icon = icon("chalkboard-teacher"), color = "blue"
    )
  })
  
  #### for Neuro 
  output$out12_Neur <- renderInfoBox({
    infoBox("How to use a forest plot",  
            a("Click here to go to the tutorial", onclick = "openTab('tutorial')", href="#", style = "font-size: 90%;"),
            icon = icon("chalkboard-teacher"), color = "blue"
    )
  })
  
  
  output$plot_low <- renderGirafe({
    low_forest <- forest_data() %>% filter(Categorized.class==selected_class())
    
    
       validate(
      need(low_forest$id != "", "There are no records for the selected combination of effect size (ES) and Type of exposure, please make another selection.")
    )
    
    x <- girafe(code = print(
      
      
      ggplot(low_forest, aes(y=Reference, x=yi, col=study))+
        #Add data points and color them black
        
        geom_point_interactive(
              aes( data_id = Reference, tooltip = inter), size = 7, shape=18,position=position_dodge(5)) +
        #Add 'special' points for the summary estimates, by making them diamond shaped
        #geom_point(data=subset(forest, h=='Summary'), color='black', shape=18, size=4)+
        #add the CI error bars
        geom_errorbarh(aes(xmin= lowerci, xmax=upperci),height=.4, position=position_dodge(5) )+
        coord_cartesian(xlim= c(min(low_forest$yi),max(low_forest$yi)))+
        #coord_cartesian(xlim= c(lowerci,upperci))+
        #geom_jitter(position=position_dodge(3))+
        #coord_cartesian(xlim= if (input$expo_b == "OR") {c(min(forest$lowerci),max(forest$yi))} else {c(-5,5)})+
        #Specify the limits of the x-axis and relabel it to something more meaningful
        scale_x_continuous( if (input$expo_b == "Odds Ratio (OR)") {name = "Odds Ratio (95% confidence interval)"}
                            else if (input$expo_b == "beta coefficient of the variable"){name = "beta coefficient (95% CI)"}
                            else if (input$expo_b == "Mean difference"){name = "Mean difference"}
                            else if (input$expo_b == "Prevalence Ratio (PR)"){name = "Prevalence ratio (95% CI)"}
                            else if (input$expo_b == "p value of the Odds Ratio"){name = "p value of the Odds Ratio"}
                            else if (input$expo_b == "p value of the beta coefficient of the variable"){name = "p value of the beta coefficient"}
        )+
        scale_y_discrete('References',position="right")+
        #Give y-axis a meaningful label
        #ylab('Reference')+ 
        #Add a vertical dashed line indicating an effect size of zero, for reference
        geom_vline(xintercept= if (input$expo_b == "Odds Ratio (OR)"|input$expo_b == "Prevalence Ratio (PR)"|input$expo_b == "p value of the odds ratio") {1} else {0}, color='black', linetype='dashed')+
        #Create sub-plots (i.e., facets) based on levels of setting
        #And allow them to have their own unique axes (so authors don't redundantly repeat)
        facet_grid(narrow ~., scales= 'free', space='free', switch="y")+
        #annotate("segment", x = UCL_l, xend = if (input$expo_b == "OR") {3.3} else {5.4}, y = study, yend = study, alpha=0.6, arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = if(input$expo_b == "OR" & is.na(LCL_l)){lowerci} else {LCL_l-yi}, xend = if (input$expo_b == "OR") {-3.3} else {-5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = UCL_l , xend = if (input$expo_b == "OR") {3.3} else {5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        apatheme
    ),
    
    width_svg = 11, height_svg = if(input$expo_b == "beta coefficient of the variable"){28} 
    else if (input$expo_b == "Odds Ratio (OR)") {25}
    else if (input$expo_b == "Prevalence Ratio (PR)") {25} else {10}
    ,
    #width_svg = 12, height_svg = 12,
    options = list(
      opts_selection(
        type = "multiple", css = "fill:#0c0000;stroke:black;"),
      opts_hover(css = "fill:#0c0000;stroke:black;cursor:pointer;")
    ))
    #x
  })
  
  observeEvent(input$reset, {
    session$sendCustomMessage(type = 'plot_low', message = character(0))
  })
  

  
  
  ##### For Upper R
  forest_data_up <- reactive({
    
    forest_data_up <- forest_joint %>% filter(
      effect_z == input$expo_b_up )
  })
  
  output$plot_up <- renderGirafe({
    up_forest <- forest_data_up() %>% filter(Categorized.class==selected_class())
    g <- nrow(up_forest)
    up_forest[g+1,]<- NA
    up_forest <-  up_forest%>% mutate(id2 = c(1:(g+1)))
    
    #up_forest[g+1,3] <- "OUTCOME"
    up_forest[g+1,13] <- "EXPOSURE"
    up_forest[g+1,7] <- "SUBCATEGORY"
    #up_forest[g+1,6] <- "POINT ESTIMATE(95% CI)"
    up_forest[g+1,4] <- "CATEGORY"
    up_forest[g+1,11] <- "OUTCOME"
    
   
    
   
    validate(
      need(up_forest$id != "", "There are no records for the selected combination of effect size (ES) and Type of exposure, please make another selection.")
    )
    
    
    ###
    #g <- nrow(up_forest)
    #up_forest[gunter+1,]<- NA
    
    #up_forest$id2 <- c(1:(g+1))
    #up_forest$id2 <- seq(1,(g+1))
    
    #up_forest[nrow(up_forest)+1,]<- "NEA"
    ##
   
    
    
    
    ###
    #up_forest[nrow(up_forest),3] <- "OutcomeNEA"
    
    ##
    #up_forest[g+1,6] <- "POINT ESTIMATE(95% CI)"
    
    
    
    fonseca <- ggplot(up_forest,aes(yi,id2, col=IDD_2)) + 
      geom_point_interactive(
        aes(data_id = IDD_2, tooltip = yi), size = 7, shape=18
      
      )+
      #geom_point_interactive(size=5, shape=18) +
      geom_errorbarh(aes(xmax = upperci, xmin = lowerci), height = 0.15) +
      #coord_cartesian(xlim= c(min(up_forest$yi),max(up_forest$yi)))+
      
      coord_cartesian(xlim= c(min(up_forest$yi),max(up_forest$yi)))+
      geom_vline(xintercept = 1, linetype = "longdash") +
      #scale_x_continuous(breaks = seq(-3,10,1))+
      scale_y_continuous(breaks = seq(1,g,3)) +
      #labs(x="Adjusted Odds Ratio", y="")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            
            legend.position = "none"
            )
   
    
    
    
    fonseca3 <- ggplot(data = up_forest, aes(y = id2)) +
      #geom_hline(aes(yintercept = Outcome.variable), size = 7) +
      geom_text_interactive(aes(x = 1,label = short, tooltip = Outcome.variable),size= 4.5,hjust = 0) + 
      #geom_text(aes(x = 2.2, label = Outcome.variable), size= 3.4)+
      geom_text_interactive(aes(x = 2, label = shortexpo,tooltip = Exposure.measure),size= 4.5,hjust = 1) +
      #geom_text(aes(x = 4, label = Subcategory), size= 3.4,hjust = 1)+
      #geom_text(aes(x = 5, label = inter1), size= 3.4,hjust = 1)+
      geom_hline(aes(yintercept=c(g+0.5)))+
      # +
      #geom_text(aes(x = 3, label = Subcategory), hjust = 1)
      scale_colour_identity() +
      theme_void()
    
    up_forest[g+1,15] <- "POINT ESTIMATE(95% CI)"
    
    
    
    fonseca2 <- ggplot(data = up_forest, aes(y = id2)) +
      #geom_hline(aes(yintercept = Outcome.variable), size = 7) +
      geom_text(aes(x = 1,label = IDD_2),size= 4.5,hjust = 0)+
     geom_text(aes(x = 2.2, label = Subcategory),size= 4.5, hjust = 1) +
      geom_text(aes(x = 3, label = inter_95), size= 4.5,hjust = 1)+
      geom_hline(aes(yintercept=c(g+0.5)))+
      # +
      #geom_text(aes(x = 3, label = Subcategory), hjust = 1)
      scale_colour_identity() +
     theme_void()
    
    
    girafe( code = print(fonseca3 + fonseca + fonseca2), width_svg = 22, height_svg = 10,
            options = list(
              opts_selection(
                type = "single", css = "fill:#0c0000;stroke:black;"),
              opts_hover(css = "fill:#0c0000;stroke:black;cursor:pointer;")
            )
            
            )
    
    
  })   
  
  
  ##### For Upper R STATES
  forest_data_up_state <- reactive({
    
    forest_data_up_state <- up_forest_state1 %>% filter(
      effect_z_state == input$expo_b_up_state )
  })
  
  output$plot_up_state <- renderGirafe({
    up_forest_state1 <- forest_data_up_state() %>% filter(Categorized.class==selected_class())
    g <- nrow(up_forest_state1)
    up_forest_state1[g+1,]<- NA
    up_forest_state1 <-  up_forest_state1%>% mutate(id2 = c(1:(g+1)))
    
    #up_forest[g+1,3] <- "OUTCOME"
    up_forest_state1[g+1,13] <- "EXPOSURE"
    up_forest_state1[g+1,7] <- "SUBCATEGORY"
    #up_forest[g+1,6] <- "POINT ESTIMATE(95% CI)"
    up_forest_state1[g+1,4] <- "CATEGORY"
    up_forest_state1[g+1,11] <- "OUTCOME"
    
    
    
    
    validate(
      need(up_forest_state1$id != "", "There are no records for the selected combination of effect size (ES) and Type of exposure, please make another selection.")
    )
    
    
    ###
    #g <- nrow(up_forest)
    #up_forest[gunter+1,]<- NA
    
    #up_forest$id2 <- c(1:(g+1))
    #up_forest$id2 <- seq(1,(g+1))
    
    #up_forest[nrow(up_forest)+1,]<- "NEA"
    ##
    
    
    
    
    ###
    #up_forest[nrow(up_forest),3] <- "OutcomeNEA"
    
    ##
    #up_forest[g+1,6] <- "POINT ESTIMATE(95% CI)"
    
    
    
    fonseca_state <- ggplot(up_forest_state1,aes(yi,id2, col=IDD_2)) + 
      geom_point_interactive(
        aes(data_id = IDD_2, tooltip = yi), size = 7, shape=18
        
      )+
      #geom_point_interactive(size=5, shape=18) +
      geom_errorbarh(aes(xmax = upperci, xmin = lowerci), height = 0.15) +
      #coord_cartesian(xlim= c(min(up_forest$yi),max(up_forest$yi)))+
      
      coord_cartesian(xlim= c(min(up_forest_state1$yi),max(up_forest_state1$yi)))+
      geom_vline(xintercept = 1, linetype = "longdash") +
      #scale_x_continuous(breaks = seq(-3,10,1))+
      scale_y_continuous(breaks = seq(1,g,3)) +
      #labs(x="Adjusted Odds Ratio", y="")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            
            legend.position = "none"
      )
    
    fonseca3_state <- ggplot(data = up_forest_state1, aes(y = id2)) +
      #geom_hline(aes(yintercept = Outcome.variable), size = 7) +
      geom_text_interactive(aes(x = 1,label = short, tooltip = Outcome.variable),size= 4.5,hjust = 0) + 
      #geom_text(aes(x = 2.2, label = Outcome.variable), size= 3.4)+
      geom_text_interactive(aes(x = 2, label = shortexpo,tooltip = Exposure.measure),size= 4.5,hjust = 1) +
      #geom_text(aes(x = 4, label = Subcategory), size= 3.4,hjust = 1)+
      #geom_text(aes(x = 5, label = inter1), size= 3.4,hjust = 1)+
      geom_hline(aes(yintercept=c(g+0.5)))+
      # +
      #geom_text(aes(x = 3, label = Subcategory), hjust = 1)
      scale_colour_identity() +
      theme_void()
    
    up_forest_state1[g+1,15] <- "POINT ESTIMATE(95% CI)"
    
    fonseca2_state <- ggplot(data = up_forest_state1, aes(y = id2)) +
      #geom_hline(aes(yintercept = Outcome.variable), size = 7) +
      geom_text(aes(x = 1,label = Categorized.class),size= 4.5,hjust = 0)+
      geom_text(aes(x = 2.2, label = Subcategory),size= 4.5, hjust = 1) +
      geom_text(aes(x = 3, label = inter_95), size= 4.5,hjust = 1)+
      geom_hline(aes(yintercept=c(g+0.5)))+
      # +
      #geom_text(aes(x = 3, label = Subcategory), hjust = 1)
      scale_colour_identity() +
      theme_void()
    
    
    
    
    
    
    girafe( code = print(fonseca3_state + fonseca_state + fonseca2_state), width_svg = 22, height_svg = 10,
            options = list(
              opts_selection(
                type = "single", css = "fill:#0c0000;stroke:black;"),
              opts_hover(css = "fill:#0c0000;stroke:black;cursor:pointer;")
            )
            
    )
    
    
  })
  
  #### For AR
  
  forest_data_ar <- reactive({
    
    forest_data_ar <- forest %>% filter(
      effect_z == input$expo_b_ar &  t_expo == input$expo_c_ar)
  })
  
  output$plot_ar <- renderGirafe({
    ar_forest <- forest_data_ar() %>% filter(Categorized.class==selected_class())
    
    
    validate(
      need(ar_forest$id != "", "There are no records for the selected combination of effect size (ES) and Type of exposure, please make another selection.")
    )
    
    j <- girafe(code = print(
      
      
      ggplot(ar_forest, aes(y=Reference, x=yi, col=study))+
        #Add data points and color them black
        geom_point_interactive(
          aes( data_id = Reference, tooltip = inter), size = 7, shape=18, position=position_dodge(5)) +
        #Add 'special' points for the summary estimates, by making them diamond shaped
        #geom_point(data=subset(forest, h=='Summary'), color='black', shape=18, size=4)+
        #add the CI error bars
        geom_errorbarh(aes(xmin= lowerci, xmax=upperci),height=.4, position=position_dodge(5) )+
        coord_cartesian(xlim= c(min(ar_forest$yi),max(ar_forest$yi)))+
        #coord_cartesian(xlim= c(lowerci,upperci))+
        #geom_jitter(position=position_dodge(3))+
        #coord_cartesian(xlim= if (input$expo_b == "OR") {c(min(forest$lowerci),max(forest$yi))} else {c(-5,5)})+
        #Specify the limits of the x-axis and relabel it to something more meaningful
        scale_x_continuous( if (input$expo_b_ar == "Odds Ratio (OR)") {name = "Odds Ratio (95% confidence interval)"}
                            else if (input$expo_b_ar == "beta coefficient of the variable"){name = "beta coefficient (95% CI)"}
                            else if (input$expo_b_ar == "Mean difference"){name = "Mean difference"}
                            else if (input$expo_b_ar == "Prevalence Ratio (PR)"){name = "Prevalence ratio (95% CI)"}
                            else if (input$expo_b_ar == "p value of the Odds Ratio"){name = "p value of the Odds Ratio"}
                            else if (input$expo_b_ar == "p value of the beta coefficient of the variable"){name = "p value of the beta coefficient"}
        )+
        scale_y_discrete('References',position="right")+
        #Give y-axis a meaningful label
        #ylab('Reference')+ 
        #Add a vertical dashed line indicating an effect size of zero, for reference
        geom_vline(xintercept= if (input$expo_b_ar == "Odds Ratio (OR)"|input$expo_b_ar == "Prevalence Ratio (PR)"|input$expo_b_ar == "p value of the odds ratio") {1} else {0}, color='black', linetype='dashed')+
        #Create sub-plots (i.e., facets) based on levels of setting
        #And allow them to have their own unique axes (so authors don't redundantly repeat)
        facet_grid(narrow ~., scales= 'free', space='free', switch="y")+
        #annotate("segment", x = UCL_l, xend = if (input$expo_b == "OR") {3.3} else {5.4}, y = study, yend = study, alpha=0.6, arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = if(input$expo_b == "OR" & is.na(LCL_l)){lowerci} else {LCL_l-yi}, xend = if (input$expo_b == "OR") {-3.3} else {-5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = UCL_l , xend = if (input$expo_b == "OR") {3.3} else {5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        apatheme
    ),
    
    width_svg = 11, height_svg = if(input$expo_b_ar == "beta coefficient of the variable"){28} 
    else if (input$expo_b_ar == "Odds Ratio (OR)") {15}
    else if (input$expo_b_ar == "Prevalence Ratio (PR)") {25} else {10}
    ,
    #width_svg = 12, height_svg = 12,
    options = list(
      opts_selection(
        type = "multiple", css = "fill:#0c0000;stroke:black;"),
      opts_hover(css = "fill:#0c0000;stroke:black;cursor:pointer;")
    ))
    #x
  })
  
  #### For Gastro
  
  forest_data_gi <- reactive({
    
    forest_data_gi <- forest %>% filter(
      effect_z == input$expo_b_gi &  t_expo == input$expo_c_gi)
  })
  
  output$plot_gi <- renderGirafe({
    gi_forest <- forest_data_gi() %>% filter(Categorized.class==selected_class())
    
    
    validate(
      need(gi_forest$id != "", "There are no records for the selected combination of effect size (ES) and Type of exposure, please make another selection.")
    )
    
    j <- girafe(code = print(
      
      
      ggplot(gi_forest, aes(y=Reference, x=yi, col=study))+
        #Add data points and color them black
        geom_point_interactive(
          aes( data_id = Reference, tooltip = inter), size = 7, shape=18, position=position_dodge(5)) +
        #Add 'special' points for the summary estimates, by making them diamond shaped
        #geom_point(data=subset(forest, h=='Summary'), color='black', shape=18, size=4)+
        #add the CI error bars
        geom_errorbarh(aes(xmin= lowerci, xmax=upperci),height=.4, position=position_dodge(5) )+
        coord_cartesian(xlim= c(min(gi_forest$yi),max(gi_forest$yi)))+
        #coord_cartesian(xlim= c(lowerci,upperci))+
        #geom_jitter(position=position_dodge(3))+
        #coord_cartesian(xlim= if (input$expo_b == "OR") {c(min(forest$lowerci),max(forest$yi))} else {c(-5,5)})+
        #Specify the limits of the x-axis and relabel it to something more meaningful
        scale_x_continuous( if (input$expo_b_gi == "Odds Ratio (OR)") {name = "Odds Ratio (95% confidence interval)"}
                            else if (input$expo_b_gi == "beta coefficient of the variable"){name = "beta coefficient (95% CI)"}
                            else if (input$expo_b_gi == "Mean difference"){name = "Mean difference"}
                            else if (input$expo_b_gi == "Prevalence Ratio (PR)"){name = "Prevalence ratio (95% CI)"}
                            else if (input$expo_b_gi == "p value of the Odds Ratio"){name = "p value of the Odds Ratio"}
                            else if (input$expo_b_gi == "p value of the beta coefficient of the variable"){name = "p value of the beta coefficient"}
        )+
        scale_y_discrete('References',position="right")+
        #Give y-axis a meaningful label
        #ylab('Reference')+ 
        #Add a vertical dashed line indicating an effect size of zero, for reference
        geom_vline(xintercept= if (input$expo_b_gi == "Odds Ratio (OR)"|input$expo_b_gi == "Prevalence Ratio (PR)"|input$expo_b_gi == "p value of the odds ratio") {1} else {0}, color='black', linetype='dashed')+
        #Create sub-plots (i.e., facets) based on levels of setting
        #And allow them to have their own unique axes (so authors don't redundantly repeat)
        facet_grid(narrow ~., scales= 'free', space='free', switch="y")+
        #annotate("segment", x = UCL_l, xend = if (input$expo_b == "OR") {3.3} else {5.4}, y = study, yend = study, alpha=0.6, arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = if(input$expo_b == "OR" & is.na(LCL_l)){lowerci} else {LCL_l-yi}, xend = if (input$expo_b == "OR") {-3.3} else {-5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = UCL_l , xend = if (input$expo_b == "OR") {3.3} else {5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        apatheme
    ),
    
    width_svg = 11, height_svg = if(input$expo_b_gi == "beta coefficient of the variable"){28} 
    else if (input$expo_b_gi == "Odds Ratio (OR)") {15}
    else if (input$expo_b_gi == "Prevalence Ratio (PR)") {25} else {10}
    ,
    #width_svg = 12, height_svg = 12,
    options = list(
      opts_selection(
        type = "multiple", css = "fill:#0c0000;stroke:black;"),
      opts_hover(css = "fill:#0c0000;stroke:black;cursor:pointer;")
    ))
    #x
  })
  
  #### For Neuro
  
  forest_data_Neur <- reactive({
    
    forest_data_Neur <- forest %>% filter(
      effect_z == input$expo_b_Neur &  t_expo == input$expo_c_Neur)
  })
  
  output$plot_Neur <- renderGirafe({
    Neur_forest <- forest_data_Neur() %>% filter(Categorized.class==selected_class())
    
    
    validate(
      need(Neur_forest$id != "", "There are no records for the selected combination of effect size (ES) and Type of exposure, please make another selection.")
    )
    
    j <- girafe(code = print(
      
      
      ggplot(Neur_forest, aes(y=Reference, x=yi, col=study))+
        #Add data points and color them black
        geom_point_interactive(
          aes( data_id = Reference, tooltip = inter), size = 7, shape=18, position=position_dodge(5)) +
        #Add 'special' points for the summary estimates, by making them diamond shaped
        #geom_point(data=subset(forest, h=='Summary'), color='black', shape=18, size=4)+
        #add the CI error bars
        geom_errorbarh(aes(xmin= lowerci, xmax=upperci),height=.4, position=position_dodge(5) )+
        coord_cartesian(xlim= c(min(Neur_forest$yi),max(Neur_forest$yi)))+
        #coord_cartesian(xlim= c(lowerci,upperci))+
        #geom_jitter(position=position_dodge(3))+
        #coord_cartesian(xlim= if (input$expo_b == "OR") {c(min(forest$lowerci),max(forest$yi))} else {c(-5,5)})+
        #Specify the limits of the x-axis and relabel it to something more meaningful
        scale_x_continuous( if (input$expo_b_Neur == "Odds Ratio (OR)") {name = "Odds Ratio (95% confidence interval)"}
                            else if (input$expo_b_Neur == "beta coefficient of the variable"){name = "beta coefficient (95% CI)"}
                            else if (input$expo_b_Neur == "Mean difference"){name = "Mean difference"}
                            else if (input$expo_b_Neur == "Prevalence Ratio (PR)"){name = "Prevalence ratio (95% CI)"}
                            else if (input$expo_b_Neur == "p value of the Odds Ratio"){name = "p value of the Odds Ratio"}
                            else if (input$expo_b_Neur == "p value of the beta coefficient of the variable"){name = "p value of the beta coefficient"}
        )+
        scale_y_discrete('References',position="right")+
        #Give y-axis a meaningful label
        #ylab('Reference')+ 
        #Add a vertical dashed line indicating an effect size of zero, for reference
        geom_vline(xintercept= if (input$expo_b_Neur == "Odds Ratio (OR)"|input$expo_b_Neur == "Prevalence Ratio (PR)"|input$expo_b_Neur == "p value of the odds ratio") {1} else {0}, color='black', linetype='dashed')+
        #Create sub-plots (i.e., facets) based on levels of setting
        #And allow them to have their own unique axes (so authors don't redundantly repeat)
        facet_grid(narrow ~., scales= 'free', space='free', switch="y")+
        #annotate("segment", x = UCL_l, xend = if (input$expo_b == "OR") {3.3} else {5.4}, y = study, yend = study, alpha=0.6, arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = if(input$expo_b == "OR" & is.na(LCL_l)){lowerci} else {LCL_l-yi}, xend = if (input$expo_b == "OR") {-3.3} else {-5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = UCL_l , xend = if (input$expo_b == "OR") {3.3} else {5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        apatheme
    ),
    
    width_svg = 11, height_svg = if(input$expo_b_Neur == "beta coefficient of the variable"){28} 
    else if (input$expo_b_Neur == "Odds Ratio (OR)") {15}
    else if (input$expo_b_Neur == "Prevalence Ratio (PR)") {25} else {10}
    ,
    #width_svg = 12, height_svg = 12,
    options = list(
      opts_selection(
        type = "multiple", css = "fill:#0c0000;stroke:black;"),
      opts_hover(css = "fill:#0c0000;stroke:black;cursor:pointer;")
    ))
    #x
  })
  
  
  
  
  #
  #forest1 <- forest[c(-1,-2,-3, -4, -5, -6, -7, -8, -9, -10, -11, -12)]
  #The dt output code
  output$my_table <- renderDataTable({
    
    out <- forest[forest$Reference %in% selected_state(), ]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    #datatable(out ,selection='single')
    #out
    
    #newdata <- out[c(-1,-3, -4, -5,-6,-7,-15, -16, -17, -18)]
    newdata <- out[c(1, 10, 11, 12, 13, 14,15, 16, 17)]
    tablereac <- as.datatable(formattable(newdata,align = c("l", rep("c", NCOL(newdata) - 1)), list(
      #CPOE = color_tile("white", "green"),
      #VERBAL = color_tile("white", "red"),
      #WRITTEN = color_tile("white", "red"),
      Confounding = formatter("span",
                              style = x ~ style(display  = "block",
                                                "border-radius" = "50%",
                                                height="30px",
                                                margin="auto",
                                                width="30px",
                                                "font-size"="9px", 
                                                "line-height"="30px",
                                                "text-align"="center",
                                                "padding-right" = "4px",
                                                color = "black",
                                                "background-color" = sapply(x,bg.picker))
                              
      ),
      "Selection of participants" = formatter("span",
                                              style = x ~ style(display  = "block",
                                                                "border-radius" = "50%",
                                                                height="30px",
                                                                margin="auto",
                                                                width="30px",
                                                                "font-size"="9px", 
                                                                "line-height"="30px",
                                                                "text-align"="center",
                                                                "padding-right" = "4px",
                                                                color = "black",
                                                                "background-color" = sapply(x,bg.picker))),
      
      "Measurement of exposure" = formatter("span",
                                            style = x ~ style(display  = "block",
                                                              "border-radius" = "50%",
                                                              height="30px",
                                                              margin="auto",
                                                              width="30px",
                                                              "font-size"="9px", 
                                                              "line-height"="30px",
                                                              "text-align"="center",
                                                              "padding-right" = "4px",
                                                              color = "black",
                                                              "background-color" = sapply(x,bg.picker))),
      "Missing Data" = formatter("span",
                                 style = x ~ style(display  = "block",
                                                   "border-radius" = "50%",
                                                   height="30px",
                                                   margin="auto",
                                                   width="30px",
                                                   "font-size"="9px", 
                                                   "line-height"="30px",
                                                   "text-align"="center",
                                                   "padding-right" = "4px",
                                                   color = "black",
                                                   "background-color" = sapply(x,bg.picker))),
      "Measurement of outcome" = formatter("span",
                                           style = x ~ style(display  = "block",
                                                             "border-radius" = "50%",
                                                             height="30px",
                                                             margin="auto",
                                                             width="30px",
                                                             "font-size"="10px", 
                                                             "line-height"="30px",
                                                             "text-align"="center",
                                                             "padding-right" = "4px",
                                                             color = "black",
                                                             "background-color" = sapply(x,bg.picker))),
      "Selection of reported results" = formatter("span",
                                                  style = x ~ style(display  = "block",
                                                                    "border-radius" = "50%",
                                                                    height="30px",
                                                                    margin="auto",
                                                                    width="30px",
                                                                    "font-size"="9px", 
                                                                    "line-height"="30px",
                                                                    "text-align"="center",
                                                                    "padding-right" = "4px",
                                                                    color = "black",
                                                                    "background-color" = sapply(x,bg.picker))),
      "Measurement of interventions" = formatter("span",
                                                 style = x ~ style(display  = "block",
                                                                   "border-radius" = "50%",
                                                                   height="30px",
                                                                   margin="auto",
                                                                   width="30px",
                                                                   "font-size"="9px", 
                                                                   "line-height"="30px",
                                                                   "text-align"="center",
                                                                   "padding-right" = "4px",
                                                                   color = "black",
                                                                   "background-color" = sapply(x,bg.picker))),
      
      "Overall" = formatter("span",
                            style = x ~ style(display  = "block",
                                              "border-radius" = "50%",
                                              height="30px",
                                              margin="auto",
                                              width="30px",
                                              "font-size"="9px", 
                                              "line-height"="30px",
                                              "text-align"="center",
                                              "padding-right" = "4px",
                                              color = "black",
                                              "background-color" = sapply(x,bg.picker)))
      
      
    )), rownames = FALSE, escape = F, selection = c("single"),class="compact",options = list(ordering=F, bFilter=F))
    
    
  })
  
  #### For upper R
  
  
  
  
  #### For AR
  
  output$my_table_ar <- renderDataTable({
    
    out23ar <- forest[forest$Reference %in% selected_state_ar(), ]
    if( nrow(out23ar) < 1 ) return(NULL)
    row.names(out23ar) <- NULL
    #datatable(out ,selection='single')
    #out
    
    #newdata <- out[c(-1,-3, -4, -5,-6,-7,-15, -16, -17, -18)]
    newdata23ar <- out23ar[c(1, 10, 11, 12, 13, 14,15, 16, 17)]
    tablereac23ar <- as.datatable(formattable(newdata23ar,align = c("l", rep("c", NCOL(newdata23ar) - 1)), list(
      #CPOE = color_tile("white", "green"),
      #VERBAL = color_tile("white", "red"),
      #WRITTEN = color_tile("white", "red"),
      Confounding = formatter("span",
                              style = j ~ style(display  = "block",
                                                "border-radius" = "50%",
                                                height="30px",
                                                margin="auto",
                                                width="30px",
                                                "font-size"="9px", 
                                                "line-height"="30px",
                                                "text-align"="center",
                                                "padding-right" = "4px",
                                                color = "black",
                                                "background-color" = sapply(j,bg.picker))
                              
      ),
      "Selection of participants" = formatter("span",
                                              style = j ~ style(display  = "block",
                                                                "border-radius" = "50%",
                                                                height="30px",
                                                                margin="auto",
                                                                width="30px",
                                                                "font-size"="9px", 
                                                                "line-height"="30px",
                                                                "text-align"="center",
                                                                "padding-right" = "4px",
                                                                color = "black",
                                                                "background-color" = sapply(j,bg.picker))),
      
      "Measurement of exposure" = formatter("span",
                                            style = j ~ style(display  = "block",
                                                              "border-radius" = "50%",
                                                              height="30px",
                                                              margin="auto",
                                                              width="30px",
                                                              "font-size"="9px", 
                                                              "line-height"="30px",
                                                              "text-align"="center",
                                                              "padding-right" = "4px",
                                                              color = "black",
                                                              "background-color" = sapply(j,bg.picker))),
      "Missing Data" = formatter("span",
                                 style = j ~ style(display  = "block",
                                                   "border-radius" = "50%",
                                                   height="30px",
                                                   margin="auto",
                                                   width="30px",
                                                   "font-size"="9px", 
                                                   "line-height"="30px",
                                                   "text-align"="center",
                                                   "padding-right" = "4px",
                                                   color = "black",
                                                   "background-color" = sapply(j,bg.picker))),
      "Measurement of outcome" = formatter("span",
                                           style = j ~ style(display  = "block",
                                                             "border-radius" = "50%",
                                                             height="30px",
                                                             margin="auto",
                                                             width="30px",
                                                             "font-size"="10px", 
                                                             "line-height"="30px",
                                                             "text-align"="center",
                                                             "padding-right" = "4px",
                                                             color = "black",
                                                             "background-color" = sapply(j,bg.picker))),
      "Selection of reported results" = formatter("span",
                                                  style = j ~ style(display  = "block",
                                                                    "border-radius" = "50%",
                                                                    height="30px",
                                                                    margin="auto",
                                                                    width="30px",
                                                                    "font-size"="9px", 
                                                                    "line-height"="30px",
                                                                    "text-align"="center",
                                                                    "padding-right" = "4px",
                                                                    color = "black",
                                                                    "background-color" = sapply(j,bg.picker))),
      "Measurement of interventions" = formatter("span",
                                                 style = j ~ style(display  = "block",
                                                                   "border-radius" = "50%",
                                                                   height="30px",
                                                                   margin="auto",
                                                                   width="30px",
                                                                   "font-size"="9px", 
                                                                   "line-height"="30px",
                                                                   "text-align"="center",
                                                                   "padding-right" = "4px",
                                                                   color = "black",
                                                                   "background-color" = sapply(j,bg.picker))),
      
      "Overall" = formatter("span",
                            style = j ~ style(display  = "block",
                                              "border-radius" = "50%",
                                              height="30px",
                                              margin="auto",
                                              width="30px",
                                              "font-size"="9px", 
                                              "line-height"="30px",
                                              "text-align"="center",
                                              "padding-right" = "4px",
                                              color = "black",
                                              "background-color" = sapply(j,bg.picker)))
      
      
    )), rownames = FALSE, escape = F, selection = c("single"),class="compact",options = list(ordering=F, bFilter=F))
    
    
  })
  
  #### For GI
  
  output$my_table_gi <- renderDataTable({
    
    out23gi <- forest[forest$Reference %in% selected_state_gi(), ]
    if( nrow(out23gi) < 1 ) return(NULL)
    row.names(out23gi) <- NULL
    #datatable(out ,selection='single')
    #out
    
    #newdata <- out[c(-1,-3, -4, -5,-6,-7,-15, -16, -17, -18)]
    newdata23gi <- out23gi[c(1, 10, 11, 12, 13, 14,15, 16, 17)]
    tablereac23gi <- as.datatable(formattable(newdata23gi,align = c("l", rep("c", NCOL(newdata23gi) - 1)), list(
      #CPOE = color_tile("white", "green"),
      #VERBAL = color_tile("white", "red"),
      #WRITTEN = color_tile("white", "red"),
      Confounding = formatter("span",
                              style = j ~ style(display  = "block",
                                                "border-radius" = "50%",
                                                height="30px",
                                                margin="auto",
                                                width="30px",
                                                "font-size"="9px", 
                                                "line-height"="30px",
                                                "text-align"="center",
                                                "padding-right" = "4px",
                                                color = "black",
                                                "background-color" = sapply(j,bg.picker))
                              
      ),
      "Selection of participants" = formatter("span",
                                              style = j ~ style(display  = "block",
                                                                "border-radius" = "50%",
                                                                height="30px",
                                                                margin="auto",
                                                                width="30px",
                                                                "font-size"="9px", 
                                                                "line-height"="30px",
                                                                "text-align"="center",
                                                                "padding-right" = "4px",
                                                                color = "black",
                                                                "background-color" = sapply(j,bg.picker))),
      
      "Measurement of exposure" = formatter("span",
                                            style = j ~ style(display  = "block",
                                                              "border-radius" = "50%",
                                                              height="30px",
                                                              margin="auto",
                                                              width="30px",
                                                              "font-size"="9px", 
                                                              "line-height"="30px",
                                                              "text-align"="center",
                                                              "padding-right" = "4px",
                                                              color = "black",
                                                              "background-color" = sapply(j,bg.picker))),
      "Missing Data" = formatter("span",
                                 style = j ~ style(display  = "block",
                                                   "border-radius" = "50%",
                                                   height="30px",
                                                   margin="auto",
                                                   width="30px",
                                                   "font-size"="9px", 
                                                   "line-height"="30px",
                                                   "text-align"="center",
                                                   "padding-right" = "4px",
                                                   color = "black",
                                                   "background-color" = sapply(j,bg.picker))),
      "Measurement of outcome" = formatter("span",
                                           style = j ~ style(display  = "block",
                                                             "border-radius" = "50%",
                                                             height="30px",
                                                             margin="auto",
                                                             width="30px",
                                                             "font-size"="10px", 
                                                             "line-height"="30px",
                                                             "text-align"="center",
                                                             "padding-right" = "4px",
                                                             color = "black",
                                                             "background-color" = sapply(j,bg.picker))),
      "Selection of reported results" = formatter("span",
                                                  style = j ~ style(display  = "block",
                                                                    "border-radius" = "50%",
                                                                    height="30px",
                                                                    margin="auto",
                                                                    width="30px",
                                                                    "font-size"="9px", 
                                                                    "line-height"="30px",
                                                                    "text-align"="center",
                                                                    "padding-right" = "4px",
                                                                    color = "black",
                                                                    "background-color" = sapply(j,bg.picker))),
      "Measurement of interventions" = formatter("span",
                                                 style = j ~ style(display  = "block",
                                                                   "border-radius" = "50%",
                                                                   height="30px",
                                                                   margin="auto",
                                                                   width="30px",
                                                                   "font-size"="9px", 
                                                                   "line-height"="30px",
                                                                   "text-align"="center",
                                                                   "padding-right" = "4px",
                                                                   color = "black",
                                                                   "background-color" = sapply(j,bg.picker))),
      
      "Overall" = formatter("span",
                            style = j ~ style(display  = "block",
                                              "border-radius" = "50%",
                                              height="30px",
                                              margin="auto",
                                              width="30px",
                                              "font-size"="9px", 
                                              "line-height"="30px",
                                              "text-align"="center",
                                              "padding-right" = "4px",
                                              color = "black",
                                              "background-color" = sapply(j,bg.picker)))
      
      
    )), rownames = FALSE, escape = F, selection = c("single"),class="compact",options = list(ordering=F, bFilter=F))
    
    
  })
  
  #### For Neuro
  
  output$my_table_Neur <- renderDataTable({
    
    out23Neur <- forest[forest$Reference %in% selected_state_Neur(), ]
    if( nrow(out23Neur) < 1 ) return(NULL)
    row.names(out23Neur) <- NULL
    #datatable(out ,selection='single')
    #out
    
    #newdata <- out[c(-1,-3, -4, -5,-6,-7,-15, -16, -17, -18)]
    newdata23Neur <- out23Neur[c(1, 10, 11, 12, 13, 14,15, 16, 17)]
    tablereac23Neur <- as.datatable(formattable(newdata23Neur,align = c("l", rep("c", NCOL(newdata23Neur) - 1)), list(
      #CPOE = color_tile("white", "green"),
      #VERBAL = color_tile("white", "red"),
      #WRITTEN = color_tile("white", "red"),
      Confounding = formatter("span",
                              style = j ~ style(display  = "block",
                                                "border-radius" = "50%",
                                                height="30px",
                                                margin="auto",
                                                width="30px",
                                                "font-size"="9px", 
                                                "line-height"="30px",
                                                "text-align"="center",
                                                "padding-right" = "4px",
                                                color = "black",
                                                "background-color" = sapply(j,bg.picker))
                              
      ),
      "Selection of participants" = formatter("span",
                                              style = j ~ style(display  = "block",
                                                                "border-radius" = "50%",
                                                                height="30px",
                                                                margin="auto",
                                                                width="30px",
                                                                "font-size"="9px", 
                                                                "line-height"="30px",
                                                                "text-align"="center",
                                                                "padding-right" = "4px",
                                                                color = "black",
                                                                "background-color" = sapply(j,bg.picker))),
      
      "Measurement of exposure" = formatter("span",
                                            style = j ~ style(display  = "block",
                                                              "border-radius" = "50%",
                                                              height="30px",
                                                              margin="auto",
                                                              width="30px",
                                                              "font-size"="9px", 
                                                              "line-height"="30px",
                                                              "text-align"="center",
                                                              "padding-right" = "4px",
                                                              color = "black",
                                                              "background-color" = sapply(j,bg.picker))),
      "Missing Data" = formatter("span",
                                 style = j ~ style(display  = "block",
                                                   "border-radius" = "50%",
                                                   height="30px",
                                                   margin="auto",
                                                   width="30px",
                                                   "font-size"="9px", 
                                                   "line-height"="30px",
                                                   "text-align"="center",
                                                   "padding-right" = "4px",
                                                   color = "black",
                                                   "background-color" = sapply(j,bg.picker))),
      "Measurement of outcome" = formatter("span",
                                           style = j ~ style(display  = "block",
                                                             "border-radius" = "50%",
                                                             height="30px",
                                                             margin="auto",
                                                             width="30px",
                                                             "font-size"="10px", 
                                                             "line-height"="30px",
                                                             "text-align"="center",
                                                             "padding-right" = "4px",
                                                             color = "black",
                                                             "background-color" = sapply(j,bg.picker))),
      "Selection of reported results" = formatter("span",
                                                  style = j ~ style(display  = "block",
                                                                    "border-radius" = "50%",
                                                                    height="30px",
                                                                    margin="auto",
                                                                    width="30px",
                                                                    "font-size"="9px", 
                                                                    "line-height"="30px",
                                                                    "text-align"="center",
                                                                    "padding-right" = "4px",
                                                                    color = "black",
                                                                    "background-color" = sapply(j,bg.picker))),
      "Measurement of interventions" = formatter("span",
                                                 style = j ~ style(display  = "block",
                                                                   "border-radius" = "50%",
                                                                   height="30px",
                                                                   margin="auto",
                                                                   width="30px",
                                                                   "font-size"="9px", 
                                                                   "line-height"="30px",
                                                                   "text-align"="center",
                                                                   "padding-right" = "4px",
                                                                   color = "black",
                                                                   "background-color" = sapply(j,bg.picker))),
      
      "Overall" = formatter("span",
                            style = j ~ style(display  = "block",
                                              "border-radius" = "50%",
                                              height="30px",
                                              margin="auto",
                                              width="30px",
                                              "font-size"="9px", 
                                              "line-height"="30px",
                                              "text-align"="center",
                                              "padding-right" = "4px",
                                              color = "black",
                                              "background-color" = sapply(j,bg.picker)))
      
      
    )), rownames = FALSE, escape = F, selection = c("single"),class="compact",options = list(ordering=F, bFilter=F))
    
    
  })
  
 
  
  #reactive table based on the selected row 
  tbl_reactive <- reactive({
    #t(input$filter(forest123, study == selected_state()))
    gew <- forest123[forest123$Reference %in% selected_state(), ]
    gew<- gew[c(12, 4, 5, 6, 7, 8, 9, 10, 11)]
    t(gew[as.numeric(input$my_table_rows_selected[1]),])
    #t(forest123[as.character(input$my_table_rows_selected[1]),])
    #t(forest123[input$my_table_rows_selected %in% selected_state())
    #t(forest123[selected_state(), ])
  })

  
  
  ##### For upper R
  
#  tbl_reactive_up <- reactive({
    #t(input$filter(forest123, study == selected_state()))
#    gew1 <- forest_sabado[forest_sabado$IDD %in% selected_state_up(), ]
#    gew1<- gew1[c(25, 26,27)]
#    t(gew1[1,])
    #t(forest123[as.character(input$my_table_rows_selected[1]),])
    #t(forest123[input$my_table_rows_selected %in% selected_state())
    #t(forest123[selected_state(), ])
    
#  })
  
  tbl_reactive_up <- reactive({
          gew1 <- ROB_joint[ROB_joint$IDD_2 %in% selected_state_up(), ]
      gew1<- gew1[c(1,2,3,4,5,6,7,8,9,10,11)]
      t(gew1[1,])
   
  })
  
  ##### For AR
  
  tbl_reactive_ar <- reactive({
    #t(input$filter(forest123, study == selected_state()))
    gew1ar <- forest123[forest123$Reference %in% selected_state_ar(), ]
    gew1ar<- gew1ar[c(12, 4, 5, 6, 7, 8, 9, 10, 11)]
    t(gew1ar[as.numeric(input$my_table_ar_rows_selected[1]),])
    #t(forest123[as.character(input$my_table_rows_selected[1]),])
    #t(forest123[input$my_table_rows_selected %in% selected_state())
    #t(forest123[selected_state(), ])
    
  })
  
  ##### For GI
  
  tbl_reactive_gi <- reactive({
    #t(input$filter(forest123, study == selected_state()))
    gew1gi <- forest123[forest123$Reference %in% selected_state_gi(), ]
    gew1gi<- gew1gi[c(12, 4, 5, 6, 7, 8, 9, 10, 11)]
    t(gew1gi[as.numeric(input$my_table_gi_rows_selected[1]),])
    #t(forest123[as.character(input$my_table_rows_selected[1]),])
    #t(forest123[input$my_table_rows_selected %in% selected_state())
    #t(forest123[selected_state(), ])
    
  })
  ##test
  output$alexander <- renderDataTable({
    
    out23gi <- forest[forest$Reference %in% selected_state_gi(), ]
    if( nrow(out23gi) < 1 ) return(NULL)
    row.names(out23gi) <- NULL
    #datatable(out ,selection='single')
    #out
    
    #newdata <- out[c(-1,-3, -4, -5,-6,-7,-15, -16, -17, -18)]
    newdata23gi <- out23gi[c(10, 11, 12, 13, 14,15, 16, 17)]
    tablereac23gi <- as.datatable(formattable(newdata23gi,align = c("l", rep("c", NCOL(newdata23gi) - 1)), list(
      #CPOE = color_tile("white", "green"),
      #VERBAL = color_tile("white", "red"),
      #WRITTEN = color_tile("white", "red"),
      Confounding = formatter("span",
                              style = j ~ style(display  = "block",
                                                "border-radius" = "80%",
                                                height="50px",
                                                margin="auto",
                                                width="50px",
                                                "font-size"="10px", 
                                                "line-height"="50px",
                                                "text-align"="center",
                                                "padding-right" = "4px",
                                                color = "black",
                                                "background-color" = sapply(j,bg.picker))
                              
      ),
      "Selection of participants" = formatter("span",
                                              style = j ~ style(display  = "block",
                                                                "border-radius" = "50%",
                                                                height="50px",
                                                                margin="auto",
                                                                width="50px",
                                                                "font-size"="9px", 
                                                                "line-height"="50px",
                                                                "text-align"="center",
                                                                "padding-right" = "4px",
                                                                color = "black",
                                                                "background-color" = sapply(j,bg.picker))),
      
      "Measurement of exposure" = formatter("span",
                                            style = j ~ style(display  = "block",
                                                              "border-radius" = "50%",
                                                              height="50px",
                                                              margin="auto",
                                                              width="50px",
                                                              "font-size"="9px", 
                                                              "line-height"="50px",
                                                              "text-align"="center",
                                                              "padding-right" = "4px",
                                                              color = "black",
                                                              "background-color" = sapply(j,bg.picker))),
      "Missing Data" = formatter("span",
                                 style = j ~ style(display  = "block",
                                                   "border-radius" = "50%",
                                                   height="50px",
                                                   margin="auto",
                                                   width="50px",
                                                   "font-size"="9px", 
                                                   "line-height"="50px",
                                                   "text-align"="center",
                                                   "padding-right" = "4px",
                                                   color = "black",
                                                   "background-color" = sapply(j,bg.picker))),
      "Measurement of outcome" = formatter("span",
                                           style = j ~ style(display  = "block",
                                                             "border-radius" = "50%",
                                                             height="50px",
                                                             margin="auto",
                                                             width="50px",
                                                             "font-size"="10px", 
                                                             "line-height"="50px",
                                                             "text-align"="center",
                                                             "padding-right" = "4px",
                                                             color = "black",
                                                             "background-color" = sapply(j,bg.picker))),
      "Selection of reported results" = formatter("span",
                                                  style = j ~ style(display  = "block",
                                                                    "border-radius" = "50%",
                                                                    height="50px",
                                                                    margin="auto",
                                                                    width="50px",
                                                                    "font-size"="9px", 
                                                                    "line-height"="50px",
                                                                    "text-align"="center",
                                                                    "padding-right" = "4px",
                                                                    color = "black",
                                                                    "background-color" = sapply(j,bg.picker))),
      "Measurement of interventions" = formatter("span",
                                                 style = j ~ style(display  = "block",
                                                                   "border-radius" = "50%",
                                                                   height="50px",
                                                                   margin="auto",
                                                                   width="50px",
                                                                   "font-size"="9px", 
                                                                   "line-height"="50px",
                                                                   "text-align"="center",
                                                                   "padding-right" = "4px",
                                                                   color = "black",
                                                                   "background-color" = sapply(j,bg.picker))),
      
      "Overall" = formatter("span",
                            style = j ~ style(display  = "block",
                                              "border-radius" = "50%",
                                              height="50px",
                                              margin="auto",
                                              width="50px",
                                              "font-size"="9px", 
                                              "line-height"="50px",
                                              "text-align"="center",
                                              "padding-right" = "4px",
                                              color = "black",
                                              "background-color" = sapply(j,bg.picker)))
      
      
    )), rownames = FALSE, escape = F, selection = c("single"),class="compact",options = list(ordering=F, bFilter=F))
    
    
  })
  
  
  ##### to include in the new version of forest plot for upper respiratory
  output$alexander1 <- renderDataTable({
    
    
    out23up <- ROB_joint_tl[ROB_joint_tl$IDD_2 %in% selected_state_up(), ]
    if( nrow(out23up) < 1 ) return(NULL)
    row.names(out23up) <- NULL
    #datatable(out ,selection='single')
    #out
    
    #newdata <- out[c(-1,-3, -4, -5,-6,-7,-15, -16, -17, -18)]
    #newdata23up <- out23up[c(10, 11, 12, 13, 14,15, 16, 17)]
    tablereac23up <- as.datatable(formattable(out23up,align = c("l", rep("c", NCOL(out23up) - 1)), list(
      #CPOE = color_tile("white", "green"),
      #VERBAL = color_tile("white", "red"),
      #WRITTEN = color_tile("white", "red"),
      "Differential information" = formatter("span",
                              style = j ~ style(display  = "block",
                                                "border-radius" = "80%",
                                                height="80px",
                                                margin="auto",
                                                width="80px",
                                                "font-size"="10px", 
                                                "line-height"="50px",
                                                "text-align"="center",
                                                "padding-right" = "4px",
                                                color = "black",
                                                "background-color" = sapply(j,bg.picker1))
                              
      ),
      "Differential information 2" = formatter("span",
                                              style = j ~ style(display  = "block",
                                                                "border-radius" = "70%",
                                                                height="80px",
                                                                #margin="auto",
                                                                width="80px",
                                                                "font-size"="10px", 
                                                                "line-height"="70px",
                                                                "text-align"="center",
                                                                "padding-right" = "4px",
                                                                color = "black",
                                                                "background-color" = sapply(j,bg.picker1))),
      
      "Selection bias" = formatter("span",
                                            style = j ~ style(display  = "block",
                                                              "border-radius" = "50%",
                                                              height="80px",
                                                              margin="auto",
                                                              width="80px",
                                                              "font-size"="9px", 
                                                              "line-height"="50px",
                                                              "text-align"="center",
                                                              "padding-right" = "4px",
                                                              color = "black",
                                                              "background-color" = sapply(j,bg.picker1))),
      "Selection_bias 2" = formatter("span",
                                 style = j ~ style(display  = "block",
                                                   "border-radius" = "50%",
                                                   height="80px",
                                                   margin="auto",
                                                   width="80px",
                                                   "font-size"="9px", 
                                                   "line-height"="50px",
                                                   "text-align"="center",
                                                   "padding-right" = "4px",
                                                   color = "black",
                                                   "background-color" = sapply(j,bg.picker1))),
      "Confounding" = formatter("span",
                                           style = j ~ style(display  = "block",
                                                             "border-radius" = "50%",
                                                             height="80px",
                                                             margin="auto",
                                                             width="80px",
                                                             "font-size"="10px", 
                                                             "line-height"="50px",
                                                             "text-align"="center",
                                                             "padding-right" = "4px",
                                                             color = "black",
                                                             "background-color" = sapply(j,bg.picker1))),
      "Overall bias" = formatter("span",
                                                  style = j ~ style(display  = "block",
                                                                    "border-radius" = "50%",
                                                                    height="50px",
                                                                    margin="auto",
                                                                    width="50px",
                                                                    "font-size"="9px", 
                                                                    "line-height"="50px",
                                                                    "text-align"="center",
                                                                    "padding-right" = "4px",
                                                                    color = "black",
                                                                    "background-color" = sapply(j,bg.picker1))),
      "Differential information 3" = formatter("span",
                                                 style = j ~ style(display  = "block",
                                                                   "border-radius" = "50%",
                                                                   height="50px",
                                                                   margin="auto",
                                                                   width="50px",
                                                                   "font-size"="9px", 
                                                                   "line-height"="50px",
                                                                   "text-align"="center",
                                                                   "padding-right" = "4px",
                                                                   color = "black",
                                                                   "background-color" = sapply(j,bg.picker1))),
      
      "Differential information 4" = formatter("span",
                            style = j ~ style(display  = "block",
                                              "border-radius" = "50%",
                                              height="50px",
                                              margin="auto",
                                              width="50px",
                                              "font-size"="9px", 
                                              "line-height"="50px",
                                              "text-align"="center",
                                              "padding-right" = "4px",
                                              color = "black",
                                              "background-color" = sapply(j,bg.picker1)))
      
      
    )), rownames = FALSE, escape = F, selection = c("single"),class="compact",options = list(ordering=F, bFilter=F))
    
    
  })
  
  
  ####
  
  ##### For Neuro
  
  tbl_reactive_Neur <- reactive({
    #t(input$filter(forest123, study == selected_state()))
    gew1Neur <- forest123[forest123$Reference %in% selected_state_Neur(), ]
    gew1Neur<- gew1Neur[c(12, 4, 5, 6, 7, 8, 9, 10, 11)]
    t(gew1Neur[as.numeric(input$my_table_Neur_rows_selected[1]),])
    #t(forest123[as.character(input$my_table_rows_selected[1]),])
    #t(forest123[input$my_table_rows_selected %in% selected_state())
    #t(forest123[selected_state(), ])
    
    
    
  })
  
  #here's the table displayed in our modal
  output$modal_table <- DT::renderDataTable({
    tbl_reactive()
    
  })
  
  ### for Upper R
  
  output$modal_table_up <- DT::renderDataTable({
    tbl_reactive_up()
  })
  
  ### for AR
  
  output$modal_table_ar <- DT::renderDataTable({
    tbl_reactive_ar()
  })
  
  ### for GI
  
  output$modal_table_gi <- DT::renderDataTable({
    tbl_reactive_gi()
  })
  
  ### for Neuro
  
  output$modal_table_Neur <- DT::renderDataTable({
    tbl_reactive_Neur()
  })
  
  #our modal dialog box
  myModal <- function(failed=FALSE){
    modalDialog(
      div(dataTableOutput('modal_table'), style = "font-size:85%"),
      width= "fit-content",
      easyClose = TRUE
      
    )
  }
  
  #### for Upper R
  
  myModal_up <- function(failed=FALSE){
    modalDialog(
      div(dataTableOutput('alexander1'), style = "font-size:95%"),
      hr(),
      div(dataTableOutput('modal_table_up'), style = "font-size:95%"),
      #width= "fit-content",
      size = c("l"),
      easyClose = TRUE
      
    )
  }
  
  #### for AR
  
  myModal_ar <- function(failed=FALSE){
    modalDialog(
      div(dataTableOutput('modal_table_ar'), style = "font-size:85%"),
      width= "fit-content",
      easyClose = TRUE
      
    )
  }
  
  #### for GI
  
  myModal_gi <- function(failed=FALSE){
    modalDialog(
      div(dataTableOutput('alexander'), style = "font-size:65%"),
      div(dataTableOutput('modal_table_gi'), style = "font-size:95%"),
      width= "fit-content",
      easyClose = TRUE
      
    )
  }
  
  #### for Neuro
  
  myModal_Neur <- function(failed=FALSE){
    modalDialog(
      div(dataTableOutput('modal_table_Neur'), style = "font-size:85%"),
      width= "fit-content",
      easyClose = TRUE
      
    )
  }
  
  #event to trigger the modal box to appear
  observeEvent(input$my_table_rows_selected,{
    #observeEvent(input$selected_state,{
    showModal(myModal())
    
  }) 
  
  ### For upper R
  observeEvent(selected_state_up(),{
    #observeEvent(input$selected_state,{
    showModal(myModal_up())
    
  }) 
  
  ### For AR
  observeEvent(input$my_table_ar_rows_selected,{
    #observeEvent(input$selected_state,{
    showModal(myModal_ar())
    
  }) 
  
  ### For GI
  observeEvent(input$my_table_gi_rows_selected,{
    #observeEvent(input$selected_state,{
    showModal(myModal_gi())
    
  }) 
  
  ### For Neuro
  observeEvent(input$my_table_Neur_rows_selected,{
    #observeEvent(input$selected_state,{
    showModal(myModal_Neur())
    
  })
  
  
  output$mytable1234 <- DT::renderDataTable({
    #datatable(df())
    #DT::datatable(ROB_joint_tl, escape = FALSE, options = list(ordering=F, bFilter=F, pageLength = 20))
    DT::datatable(cafo3, escape = FALSE, options = list(ordering=F, bFilter=F, pageLength = 20))
  })
  
  output$mytable12345 <- DT::renderDataTable({
    #datatable(df())
    #DT::datatable(ROB_joint_tl, escape = FALSE, options = list(ordering=F, bFilter=F, pageLength = 20))
    DT::datatable(cafo4, escape = FALSE, options = list(ordering=F, bFilter=F, pageLength = 20))
  })
  
  
  
  ## * risk of bias ####
  output$bias <- renderPlotly({
    #gg <- r22 %>% filter(Refid %in% selected_id()) %>% 
    gg <- r22 %>% filter(Categorized.class==selected_class()) %>% 
      ggplot(aes(x = `Type of Bias`, fill = Bias)) + 
      geom_bar(position = "fill") + coord_flip() + 
      scale_fill_manual(values = color_table$Color) + 
      scale_x_discrete(labels = rev(c("Overall", "Confounding", "Measurement of Exposure", 
                                      "Measurement of Outcome", "Measurement of Interventions","Missing Data", "Selection of Participants",
                                      "Selection of Reported Result"))) +
      ylab("Proportion")
    ggplotly(gg)
  })
  
  output$bias_up <- renderPlotly({
    #gg <- r22 %>% filter(Refid %in% selected_id()) %>% 
    gg <- r22 %>% filter(Categorized.class==selected_class()) %>% 
      ggplot(aes(x = `Type of Bias`, fill = Bias)) + 
      geom_bar(position = "fill") + coord_flip() + 
      scale_fill_manual(values = color_table$Color) + 
      scale_x_discrete(labels = rev(c("Overall", "Confounding", "Measurement of Exposure", 
                                      "Measurement of Outcome", "Measurement of Interventions","Missing Data", "Selection of Participants",
                                      "Selection of Reported Result"))) +
      ylab("Proportion")
    ggplotly(gg)
  })
  
  output$bias_ar <- renderPlotly({
    #gg <- r22 %>% filter(Refid %in% selected_id()) %>% 
    gg <- r22 %>% filter(Categorized.class==selected_class()) %>% 
      ggplot(aes(x = `Type of Bias`, fill = Bias)) + 
      geom_bar(position = "fill") + coord_flip() + 
      scale_fill_manual(values = color_table$Color) + 
      scale_x_discrete(labels = rev(c("Overall", "Confounding", "Measurement of Exposure", 
                                      "Measurement of Outcome", "Measurement of Interventions","Missing Data", "Selection of Participants",
                                      "Selection of Reported Result"))) +
      ylab("Proportion")
    ggplotly(gg)
  })
  
  output$bias_gi <- renderPlotly({
    #gg <- r22 %>% filter(Refid %in% selected_id()) %>% 
    gg <- r22 %>% filter(Categorized.class==selected_class()) %>% 
      ggplot(aes(x = `Type of Bias`, fill = Bias)) + 
      geom_bar(position = "fill") + coord_flip() + 
      scale_fill_manual(values = color_table$Color) + 
      scale_x_discrete(labels = rev(c("Overall", "Confounding", "Measurement of Exposure", 
                                      "Measurement of Outcome", "Measurement of Interventions","Missing Data", "Selection of Participants",
                                      "Selection of Reported Result"))) +
      ylab("Proportion")
    ggplotly(gg)
  })
  
  output$bias_Neur <- renderPlotly({
    #gg <- r22 %>% filter(Refid %in% selected_id()) %>% 
    gg <- r22 %>% filter(Categorized.class==selected_class()) %>% 
      ggplot(aes(x = `Type of Bias`, fill = Bias)) + 
      geom_bar(position = "fill") + coord_flip() + 
      scale_fill_manual(values = color_table$Color) + 
      scale_x_discrete(labels = rev(c("Overall", "Confounding", "Measurement of Exposure", 
                                      "Measurement of Outcome", "Measurement of Interventions","Missing Data", "Selection of Participants",
                                      "Selection of Reported Result"))) +
      ylab("Proportion")
    ggplotly(gg)
  })
  
  
  # Define reactive values
  rv <- reactiveValues()
  
  # Data Handling ----
  
  # Use template data to populate editable table
  observe({
    if (is.null(input$data_upload)) {
      rv$data <- template
    } else {
      rv$data <- read.csv(input$data_upload$datapath)
    }
  })
  
  
  
  
  # Text box
  observeEvent(input$update,{
    rv$data[which(rv$data$data == "previous_studies"), "n"] <- gsub(',', '', input$previous_studies)
    rv$data[which(rv$data$data == "previous_reports"), "n"] <- gsub(',', '', input$previous_reports)
    rv$data[which(rv$data$data == "register_results"), "n"] <- gsub(',', '', input$register_results)
    rv$data[which(rv$data$data == "database_results"), "n"] <- gsub(',', '', input$database_results)
    rv$data[which(rv$data$data == "website_results"), "n"] <- gsub(',', '', input$website_results)
    rv$data[which(rv$data$data == "organisation_results"), "n"] <- gsub(',', '', input$organisation_results)
    rv$data[which(rv$data$data == "citations_results"), "n"] <- gsub(',', '', input$citations_results)
    rv$data[which(rv$data$data == "duplicates"), "n"] <- gsub(',', '', input$duplicates)
    rv$data[which(rv$data$data == "excluded_automatic"), "n"] <- gsub(',', '', input$excluded_automatic)
    rv$data[which(rv$data$data == "excluded_other"), "n"] <- gsub(',', '', input$excluded_other)
    rv$data[which(rv$data$data == "records_screened"), "n"] <- gsub(',', '', input$records_screened)
    rv$data[which(rv$data$data == "records_excluded"), "n"] <- gsub(',', '', input$records_excluded)
    rv$data[which(rv$data$data == "dbr_sought_reports"), "n"] <- gsub(',', '', input$dbr_sought_reports)
    rv$data[which(rv$data$data == "dbr_notretrieved_reports"), "n"] <- gsub(',', '', input$dbr_notretrieved_reports)
    rv$data[which(rv$data$data == "other_sought_reports"), "n"] <- gsub(',', '', input$other_sought_reports)
    rv$data[which(rv$data$data == "other_notretrieved_reports"), "n"] <- gsub(',', '', input$other_notretrieved_reports)
    rv$data[which(rv$data$data == "dbr_assessed"), "n"] <- gsub(',', '', input$dbr_assessed)
    rv$data[which(rv$data$data == "dbr_excluded"), "n"] <- input$dbr_excluded
    rv$data[which(rv$data$data == "other_assessed"), "n"] <- gsub(',', '', input$other_assessed)
    rv$data[which(rv$data$data == "other_excluded"), "n"] <- input$other_excluded
    rv$data[which(rv$data$data == "new_studies"), "n"] <- gsub(',', '', input$new_studies)
    rv$data[which(rv$data$data == "new_reports"), "n"] <- gsub(',', '', input$new_reports)
    rv$data[which(rv$data$data == "total_studies"), "n"] <- gsub(',', '', input$total_studies)
    rv$data[which(rv$data$data == "total_reports"), "n"] <- gsub(',', '', input$total_reports)
  })
  
  # Define table proxy
  proxy = dataTableProxy('mytable')
  
  # Update reactive dataset on cell edit
  observeEvent(input$mytable_cell_edit, {
    info <- input$mytable_cell_edit
    # Define edited row
    i <- info$row 
    # Define edited column (column index offset by 4, because you are hiding
    # the rownames column and the first 3 columns of the data)
    j <- info$col + 4L
    # Define value of edit
    v <- info$value
    
    # Pass edited value to appropriate cell of data stored in rv$data
    rv$data[i, j] <- coerceValue(v, rv$data[i, j])
    
    # Replace data in table with updated data stored in rv$data
    replaceData(proxy,
                rv$data,
                resetPaging = FALSE,
                rownames = FALSE)  # important
  })
  
  
  # Reactive plot ----
  
  # Create plot
  plot <- reactive({
    data <- read_PRISMAdata(rv$data)
    attach(data)
    
   
    
    
    plot <- PRISMA_flowdiagram(data,
                               interactive = FALSE,
                               #previous = include_previous,
                               other = F
                               )
  })
  
  
  # Display plot
  output$plot1 <- DiagrammeR::renderDiagrammeR({
    plot <- plot()
  })
  
  
  # Handle downloads ----
  output$PRISMAflowdiagramPDF <- downloadHandler(
    filename = "prisma.pdf",
    content = function(file){
      prisma_pdf(plot(), 
                 file)
    }
  )
  output$PRISMAflowdiagramPNG <- downloadHandler(
    filename = "prisma.png",
    content = function(file){
      prisma_png(plot(), 
                 file)
    }
  )
  
})


  
 
  
  


