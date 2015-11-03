# ui.R

library(shiny)



shinyUI(fluidPage(
  
  title = "Hodgkin Huxley App",
  selectInput("whatClamp", "What do you want to clamp:", choices = c("current", "voltage")),
  
  
  hr(),
  
  
  tabsetPanel(
    tabPanel("Input",
             
             fluidRow(
               column(3, offset=0.5,
                      
                      numericInput("BaselineDuration", "Pre-Clamp Duration(ms):", 10),
                      numericInput("ClampDuration", "Clamp Duration(ms):", 50),
                      numericInput("EndDuration", " Post-Clamp Duration(ms):", 10),
                      numericInput("dt", "Time steps(ms):", 0.025),
                      # Decimal interval with step value
                      sliderInput("Iapplied", "Applied current (0-4 uAmp):", min = 0, max = 4, value = 0.3, step= 0.0001),
                     # numericInput("Iapplied", "Applied current (0-4 uAmp):", 4),    
                     sliderInput("Vapplied", "Applied voltage(-10-50 mV):", min = -10, max = 50, value = 40, step= 0.0001),                     
                     #numericInput("Vapplied", "Applied voltage(-10-50 mV):", 40),
                      numericInput("C", "Membrane Capacitance ( mF):", 0.01),
                     # downloadLink('downloadData', 'Download'),
                     downloadButton('downloadData', 'Download'),
                      submitButton("Update")
                      
                      
                      
                      
                      
               ),
               column(3, offset=0.5,
                      
                      sliderInput("InNa", "Concentration of Sodium Inside (0-1 M)", min = 0, max = 1, value = 0.01, step= 0.0001),
                      
#                      numericInput("InNa","Concentration of Sodium Inside (0-1 M)",0.010),
                      
                      sliderInput("OutNa", "Concentration of Sodium outside (0-1 M):", min = 0, max = 1, value = 0.1, step= 0.0001),
                      
 #                     numericInput("OutNa","Concentration of Sodium outside (0-1 M)",0.100),
                      
                      sliderInput("InK", "Concentration of Potassium Inside(0-1 M):", min = 0, max = 1, value = 0.1, step= 0.0001),
                      
#                      numericInput("InK","Concentration of Potassium Inside(0-1 M)",0.100),
                      
                      sliderInput("OutK", "Concentration of Potassium outside(0-1 M):", min = 0, max = 1, value = 0.01, step= 0.0001),
                      
 #                     numericInput("OutK","Concentration of Potassium outside(0-1 M)",0.010),
                      
                      sliderInput("InCl", "Concentration of Chlorine Inside(0-1 M):", min = 0, max = 1, value = 0.01, step= 0.0001),
                      
  #                    numericInput("InCl","Concentration of Chlorine Inside(0-1 M)",0.010),
                      
                      sliderInput("OutCl", "Concentration of Chlorine outside(0-1 M):", min = 0, max = 1, value = 0.1, step= 0.0001),
                      
  #                    numericInput("OutCl","Concentration of Chlorine outside(0-1 M)",0.100),
                      
                      sliderInput("InCa", "Concentration of Calcium Inside(0-0.00001 M):", min = 0, max = 0.00001, value = 0.00001, step= 0.000001),
                      
   #                   numericInput("InCa","Concentration of Calcium Inside(0-0.00001 M)",0.00001),
                      
                      sliderInput("OutCa", "Concentration of Calcium outside(0-0.0001 M):", min = 0, max = 0.0001, value = 0.0001, step= 0.000001)
                      
  #                    numericInput("OutCa","Concentration of Calcium outside(0-0.0001 M)",0.0001)
                      
               ),
               column(3, offset=0.5,
                      
                      
                      sliderInput("Gna", "Maximum Conductance of Sodium channel (0-2 Si ):", min = 0, max = 2, value = 1, step= 0.0001),
                      
  #                    numericInput("Gna", "Maximum Conductance of Sodium channel (0-2 Si ):", 1.20),
                      
                      sliderInput("Gk", "Maximum Conductance of Potassium channel (0-2 Si):", min = 0, max = 2, value = 0.5, step= 0.0001),
                      
  #                    numericInput("Gk", "Maximum Conductance of Potassium channel (0-2 Si):", 0.36),
                      
                      sliderInput("Gcl", "Maximum Conductance of Chlorine channel(0-2 Si):", min = 0, max = 2, value = 0, step= 0.0001),
                      
  #                    numericInput("Gcl", "Maximum Conductance of Chlorine channel(0-2 Si):", 0),
                      
                      sliderInput("Ga", "Maximum Conductance of A channel (0-0.1 Si ):", min = 0, max = 0.1, value = 0, step= 0.0001),
                      
  #                    numericInput("Ga", "Maximum Conductance of A channel (0-0.1 Si ):", 0 ),
                      
                      sliderInput("Gm", "Maximum Conductance of M channel (0-0.1 Si):", min = 0, max = 0.1, value = 0, step= 0.0001),
                      
  #                    numericInput("Gm", "Maximum Conductance of M channel (0-0.1 Si):", 0 ),
                      
                      sliderInput("Gahp", "Maximum Conductance of AHP channel(0 - 0.1 Si):", min = 0, max = 0.1, value = 0, step= 0.0001),
                      
  #                    numericInput("Gahp", "Maximum Conductance of AHP channel(0 - 0.1 Si):", 0),
                      
                      sliderInput("Gkca", "Maximum Conductance of Kca channel(0-0.1 Si):", min = 0, max = 0.1, value = 0, step= 0.0001),
                      
  #                    numericInput("Gkca", "Maximum Conductance of Kca channel(0-0.1 Si):", 0 ),
                      
                      sliderInput("Gt", "Maximum Permeability of CaT channel (0-1 ):", min = 0, max = 1, value = 0, step= 0.0001),
                      
  #                    numericInput("Gt", "Maximum Permeability of CaT channel (0-1 ):", 0 ),                    
                      
                      sliderInput("Gcal", "Maximum Permeability of CaL channel(0-1 ):", min = 0, max = 1, value = 0, step= 0.0001),
                      
  #                    numericInput("Gcal", "Maximum Permeability of CaL channel(0-1 ):", 0 ),
                      
                      sliderInput("GKleak", "Maximum Conductance of Sodium Leak channel (0-0.01 Si):", min = 0, max = 0.01, value = 0, step= 0.0001),
                      
  #                    numericInput("GKleak", "Maximum Conductance of Sodium Leak channel (0-0.01 Si):", 0.003),
                      
                      sliderInput("GNaleak", "Maximum Conductance of Potassium Leak channel ( 0-0.01 Si):", min = 0, max = 0.01, value = 0, step= 0.0001)
                      
  #                    numericInput("GNaleak", "Maximum Conductance of Potassium Leak channel ( 0-0.01 Si):", 0.003)
                      
                      
                      #  numericInput("Gm", "Conductance due to magnesium channel:", 0),
                      
               )
             )
             #   plotOutput("threeDPlot")
    ),
    
    
    tabPanel('Help',
             
             h5("In 1952 Hodgkin and Huxley published a series of papers, describing the basic processes underlying the nervous mechanisms of control and the communication between nerve cells"),
             h5("For this they received the Nobel prize in physiology and medicine, together with John Eccles in 1963."),
             h5("Their research was based on electrophysiological experiments carried out in the late 1940s and early 1950 on a giant squid axon to understand how action potentials in neurons are initiated and propagated."),
             h5("Change the parameters below and have fun playing ver 7 !!! "),
             
             h3(" Now Try answering these questions"),
             h5("Why does Sodium go down while Potassium go up ?"),
             h5("Why is the leakage current exist ? "),
             h5(" Why does the Sodium current have the peculiar spike in the beginning ? "),
             h5("Try answering these questions"),
             h5("Try answering these questions"),
             h5("Try answering these questions"),
             h5("Try answering these questions"),
             h5("Try answering these questions")),
    
    
    tabPanel('About Us',
             h5("Developed at University of Oklahoma"),
             h5("By Anuj Guruacharya and Dr. Michael Markham"),
             h5("On 2014"),
             h5("For educational purposes"),
             h5("Contact us for more features or any ideas"),
             h5("Funded by NSF"),
             h5("Norman,Oklahoma, USA"))
    
    
  ),
  
  fluidRow(
    tags$div(
      HTML("<strong>The Colors used in the graphs are as follows: </strong> ")
      
    ),
    tags$div(
      HTML("<TABLE BORDER=4 CELLSPACING=4 CELLPADDING=4><TR>
           <TD BGCOLOR='#00FF00' width=10%>Sodium channel
           <TD BGCOLOR='#0000FF' width=10%>Potassium channel
           <TD BGCOLOR='#FF0000' width=10%>Chlorine Channel
           <TD BGCOLOR='#FF00FF' width=10%>AHP channel
           <TD BGCOLOR='#660000' width=10%>M channel                     
           <TD BGCOLOR='#00FFFF' width=10%>Kca channel
           <TD BGCOLOR='#FFFF00' width=10%>A channel
           <TD BGCOLOR='#006600' width=10%>CaT channel
           <TD BGCOLOR='#000066' width=10%>CaL Channel
           <TD BGCOLOR='#660066' width=10%>K Leak Channel
           <TD BGCOLOR='#666600' width=10%>Na Leak Channel
           </TD>
           </TR>
           </TABLE>     ")
      
      )
    
    ),
  
  plotOutput("VnetPlot"),
  plotOutput("allIPlot"),
  plotOutput("allGPlot")

  
))




