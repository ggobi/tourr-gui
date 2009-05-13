library(colorspace)
library(gWidgets)
options("guiToolkit"="RGtk2")

gui_xy = function(data = flea, ...) {
  w = gwindow("2D Tour plot example")
  g = ggroup(cont = w, horizontal = FALSE)

  if(options("guiToolkit") == "RGtk2") {
     cont = w
   } else {
     cont = g
   }

  vbox = glayout(cont=w)

  num <- sapply(data, is.numeric)

  #==================Handlers=======================
  displayTour <- function(h, ...) {
    var_selected <- svalue(Variables)
    if (length(var_selected) < 3) {
      gmessage("Please select at least three variables",  icon = "warning")
    }
    
    cat_selected <- data[svalue(Class)]
    if (length(cat_selected) > 0) {
      # collapse to single variable if multiple selected
      int <- interaction(cat_selected, drop = TRUE)
      pal <- rainbow_hcl(length(levels(int)))
      col <- pal[as.numeric(int)]
    } else {
      col <- "black"
    }

    tour <- switch(svalue(TourType),
      "Grand" = grand_tour(), 
      "Little" = little_tour(), 
      "Guided(holes)" = guided_tour(holes), 
      "Guided(cm)" = guided_tour(cm), 
      "Guided(lda_pp)" = guided_tour(), 
      "Local" = local_tour()
    )
    
    aps <- svalue(sl)
    axes_location <- svalue(dl)

    animate(data[var_selected], tour, 
      display_xy(data, axes = axes_location, center = TRUE, col = col), 
      aps = aps)
  }
  # ==================Controls==========================

  # Variable selection column
  vbox[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox[2, 1] <- Variables <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE, cont=vbox)
  vbox[3, 1, anchor = c(-1, 0)] <- "Class Selection"
  vbox[4, 1, anchor = c(-1, 0)] <- Class <- gtable(names(data)[!num], 
    multiple = TRUE, cont = vbox)

  # Tour selection column
  vbox[1, 3, anchor=c(-1,0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox[2, 3] <- TourType <- gradio(tour_types, cont=vbox, handler = NULL)

  # speed slider control
  vbox[5,1, anchor=c(-1,0)] <- "Speed"
  vbox[6,1, expand=T] <- sl <- gslider(from = 0, to= 20, by=1, value = 1, 
    cont = vbox)

  #axes control
  vbox[3,3, anchor=c(-1,0)] <- "Axes Locations"
  locations <- c("center", "bottomleft", "off")
  vbox[4,3, anchor=c(-1,0)] <- dl <- gradio(locations, cont = vbox)


  # buttons control

  buttonGroup = ggroup(horizontal = F, cont=vbox)
  
  pausePushed <<- FALSE
  pauseButton = gcheckbox("Pause",cont=buttonGroup, handler = function(h,...){
    pausePushed <<- svalue(h$obj)
  }) 
  addSpace(buttonGroup,10)

  okButton = gbutton("Apply", cont=buttonGroup)
  addhandlerclicked(okButton, handler = function(h,...) {
    svalue(pauseButton) <- FALSE  
    displayTour()
   } )
  addSpace(buttonGroup,10)

  quitPushed <<- FALSE
  quitButton = gbutton("Quit",cont=buttonGroup)
  addHandlerClicked(quitButton, handler= function(h,...){
    quitPushed <<- TRUE
    dispose(w)
    dev.off()
  })

  vbox[4,4, anchor=c(0,1)] = buttonGroup
  
  if(length(strsplit(R.Version()$os,"darwin")[[1]]) > 1){
    require(Cairo)
    library(Cairo)
    CairoX11()
  }
}
