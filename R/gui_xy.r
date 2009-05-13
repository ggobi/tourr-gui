library(colorspace)
library(gWidgets)
library(RGtk2)
options("guiToolkit"="RGtk2")

gui_xy <- function(data = flea, ...) {
  num <- sapply(data, is.numeric)

  create_tour <- function(h, ...) {
    var_selected <- svalue(Variables)
    if (length(var_selected) < 3) {
      gmessage("Please select at least three variables", icon = "warning")
      return()
    }
    
    # Work out point colours
    cat_selected <- data[svalue(Class)]
    if (length(cat_selected) > 0) {
      # collapse to single variable if multiple selected
      int <- interaction(cat_selected, drop = TRUE)
      pal <- rainbow_hcl(length(levels(int)))
      col <- pal[as.numeric(int)]
    } else {
      col <- "black"
    }
    axes_location <- svalue(dl)
    display <- display_xy(data, axes = axes_location, center = TRUE, 
      col = col)

    # Work out which type of tour to use
    tour <- switch(svalue(TourType),
      "Grand" = grand_tour(), 
      "Little" = little_tour(), 
      "Guided(holes)" = guided_tour(holes), 
      "Guided(cm)" = guided_tour(cm), 
      "Guided(lda_pp)" = guided_tour(), 
      "Local" = local_tour()
    )
    
    aps <- svalue(sl)
        
    list(
      data = rescale(data[var_selected]),
      tour_path = tour,
      display = display,
      aps = aps
    )
  }
  tour <- NULL
  tour_anim <- NULL
  update_tour <- function(...) {
    tour <<- create_tour(...)
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }
  
  draw_frame <- function(...) {
    if (is.null(tour)) return(TRUE)  # if there's no tour, exit
    if (svalue(pauseButton)) return(TRUE) # if the tour is paused, exit

    tour_step <- tour_anim$step()
    tour$display$render_transition()
    with(tour_step, tour$display$render_data(tour$data, proj, target))
    Sys.sleep(1/33)
    
    TRUE
  }
  
  

  # ==================Controls==========================
  w = gwindow("2D Tour plot example")
  g = ggroup(cont = w, horizontal = FALSE)

  if(options("guiToolkit") == "RGtk2") {
     cont = w
   } else {
     cont = g
   }

  vbox = glayout(cont=w)

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
    update_tour()
  })
  addSpace(buttonGroup,10)

  # Add animation listener.  This runs draw_frame every 1/33 of a second
  anim_id <- gIdleAdd(draw_frame)

  quitPushed <<- FALSE
  quitButton = gbutton("Quit",cont=buttonGroup)
  addHandlerClicked(quitButton, handler= function(h,...){
    quitPushed <<- TRUE
    dispose(w)
    gtkIdleRemove(anim_id)
  })

  vbox[4,4, anchor=c(0,1)] = buttonGroup
  
  
  # If on a mac, open a Cairo device, if there's not already one open
  # The cairo device has a much better refresh rate than Quartz
  if (find_platform()$os == "mac" && names(dev.cur()) != "Cairo") {
    require(Cairo)
    CairoX11()
  }
  
  
}
