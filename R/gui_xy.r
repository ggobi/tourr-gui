library(colorspace)
library(RGtk2)
library(gWidgets)

gui_xy <- function(data = flea, ...) {
  os <- find_platform()$os
  num <- sapply(data, is.numeric)
  
  tour <- NULL
  tour_anim <- NULL
  update_tour <- function(...) {
    tour <<- create_tour(data,
      var_selected = svalue(Variables), 
      cat_selected = svalue(Class), 
      axes_location = svalue(dl),
      tour_type = svalue(TourType),
      aps = svalue(sl)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }
  
  draw_frame <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)  

    tour_step <- tour_anim$step2(svalue(sl) / 33)
    if (is.null(tour_step$proj)) return(FALSE)
    
    if (os == "win") {
      tour$display$render_frame()
    } else {
      tour$display$render_transition()      
    }
    with(tour_step, tour$display$render_data(tour$data, proj, target))
    Sys.sleep(1/33)
    
    TRUE
  }
  
  
  # ==================Controls==========================
  w <- gwindow("2D Tour plot example", visible = FALSE)
  vbox <- glayout(cont = w)

  # Variable selection column
  vbox[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox[2, 1] <- Variables <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)
  vbox[3, 1, anchor = c(-1, 0)] <- "Class Selection"
  vbox[4, 1, anchor = c(-1, 0)] <- Class <- gtable(names(data)[!num], 
    multiple = TRUE)

  # Tour selection column
  vbox[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox[2, 3] <- TourType <- gradio(tour_types)

  # speed and pause
  vbox[5,1, anchor = c(-1, 0)] <- "Speed"
  vbox[6,1, expand = TRUE] <- sl <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox[6, 3] <- chk_pause <- gcheckbox("Pause", 
    handler = function(h, ...) pause(svalue(h$obj)))

  # axes control
  vbox[3,3, anchor=c(-1,0)] <- "Axes Locations"
  locations <- c("center", "bottomleft", "off")
  vbox[4,3, anchor=c(-1,0)] <- dl <- gradio(locations)

  # buttons control
  pause <- function(paused) {
    svalue(chk_pause) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <<- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }
  buttonGroup <- ggroup(horizontal = F, cont=vbox)  
  
  # addSpace(buttonGroup,10)
  gbutton("Apply", cont = buttonGroup, handler = function(...) {
    pause(FALSE)
    update_tour()
  })
  
  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup, handler = function(...) {
    pause(TRUE)
    dispose(w)
  })

  vbox[4:6, 4, anchor = c(0, 1)] <- buttonGroup
  
  # If on a mac, open a Cairo device, if there's not already one open
  # The cairo device has a much better refresh rate than Quartz
  if (find_platform()$os == "mac" && names(dev.cur()) != "Cairo") {
    require(Cairo)
    CairoX11()
  }

  # Turn off display list to maximise speed
  dev.control(displaylist = "inhibit")
  
  update_tour()
  pause(FALSE)
  visible(w) <- TRUE
  
  invisible()
}


create_tour <- function(data, var_selected, cat_selected, axes_location, tour_type, aps) {
  if (length(var_selected) < 3) {
    gmessage("Please select at least three variables", icon = "warning")
    return()
  }
  
  # Work out point colours
  cat <- data[cat_selected]
  if (length(cat_selected) > 0) {
    # collapse to single variable if multiple selected
    int <- interaction(cat, drop = TRUE)
    pal <- rainbow_hcl(length(levels(int)))
    col <- pal[as.numeric(int)]
  } else {
    col <- "black"
  }
  
  display <- display_xy(data, axes = axes_location, center = TRUE, 
    col = col)

  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(), 
    "Little" = little_tour(), 
    "Guided(holes)" = guided_tour(holes), 
    "Guided(cm)" = guided_tour(cm), 
    "Guided(lda_pp)" = guided_tour(lda_pp(data[,cat_selected])),
    "Local" = local_tour()
  )
  
  
  sel <- data[var_selected]
  # Sphere the data if we're using a guided tour
  if (length(grep(tour_type, "Guided")) > 0) {
    sel <- sphere(sel)
  }
  
  list(
    data = rescale(sel),
    tour_path = tour,
    display = display,
    aps = aps
  )
}
