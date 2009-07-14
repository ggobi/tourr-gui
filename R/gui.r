library(colorspace)
library(RGtk2)
library(gWidgets)
library(ash)

gui<- function(data = flea, ...) {
  os <- find_platform()$os
  num <- sapply(data, is.numeric)
  tour <- NULL
  tour_anim <- NULL

  w <- gwindow("2D Tour plot example", visible = FALSE)
  mw = gnotebook(cont=w)
  g1 = ggroup(cont = mw, horizontal = FALSE,label="gui_xy")
  g2 = ggroup(cont = mw, horizontal = FALSE,label="gui_density")

# =============================== Gui_density ==========================================
  update_tour_density <- function(...) {
    tour <<- create_1d_tour(data,
      var_selected = svalue(Variables_density), 
      method_selected = svalue(MethodType),
      center_selected = svalue(CenterType),
      tour_type = svalue(TourType_density),
      aps = svalue(sl_density)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }
    
  
  # ==================Controls==========================
  vbox_density <- glayout(cont = g2)

  # Variable selection column
  vbox_density[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_density[2, 1] <- Variables_density <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)
  



  # Tour selection column
  vbox_density[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Local")
  vbox_density[2, 3] <- TourType_density <- gradio(tour_types)

  # speed and pause
  vbox_density[5,1, anchor = c(-1, 0)] <- "Speed"
  vbox_density[6,1, expand = TRUE] <- sl_density <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox_density[6, 3] <- chk_pause_density <- gcheckbox("Pause", 
    handler = function(h, ...) pause_density(svalue(h$obj)))

  # method control
  vbox_density[3, 1, anchor = c(-1, 0)] <- "Method Type"
  method_types <- c("density","hist","ash")
  vbox_density[4, 1, anchor = c(-1, 0)] <- MethodType <- gradio(method_types)
    
  # center control
  vbox_density[3,3, anchor=c(-1,0)] <- "Center or Not"
  center_types <- c("TRUE", "FALSE")
  vbox_density[4,3, anchor=c(-1,0)] <- CenterType <- gradio(center_types)

  # buttons control
  anim_id <- NULL
  pause_density <- function(paused) {
    svalue(chk_pause_density) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <<- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }

  buttonGroup_density <- ggroup(horizontal = F, cont=vbox_density
)  
  
  # addSpace(buttonGroup_density,10)
  gbutton("Apply", cont = buttonGroup_density, handler = function(...) {
    pause_density(FALSE)
    update_tour_density()
  })
  
  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup_density, handler = function(...) {
    pause_density(TRUE)
    dispose(w)
  })

  vbox_density[4:6, 4, anchor = c(0, 1)] <- buttonGroup_density
  

# ============================ End of Gui_density ==========================================
  
# =============================== Gui_xy ===============================================
  
  update_tour_xy <- function(...) {
    tour <<- create_tour(data,
      var_selected = svalue(Variables_xy), 
      cat_selected = svalue(Class_xy), 
      axes_location = svalue(dl_xy),
      tour_type = svalue(TourType_xy),
      aps = svalue(sl_xy)
    )
    tour_anim <<- with(tour, tourer(data, tour_path, velocity = aps / 33))
    
    tour$display$init(tour$data)
    tour$display$render_frame()
    
    TRUE
  }
  
  draw_frame <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)  

    tour_step <- tour_anim$step2(svalue(sl_xy) / 33)
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


  vbox_xy <- glayout(cont = g1)

  # Variable selection column
  vbox_xy[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_xy[2, 1] <- Variables_xy <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE)
  vbox_xy[3, 1, anchor = c(-1, 0)] <- "Class Selection"
  vbox_xy[4, 1, anchor = c(-1, 0)] <- Class_xy <- gtable(names(data)[!num], 
    multiple = TRUE)

  # Tour selection column
  vbox_xy[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox_xy[2, 3] <- TourType_xy <- gradio(tour_types)

  # speed and pause
  vbox_xy[5,1, anchor = c(-1, 0)] <- "Speed"
  vbox_xy[6,1, expand = TRUE] <- sl_xy <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox_xy[6, 3] <- chk_pause_xy <- gcheckbox("Pause", 
    handler = function(h, ...) pause_xy(svalue(h$obj)))

  # axes control
  vbox_xy[3,3, anchor=c(-1,0)] <- "Axes Locations"
  locations <- c("center", "bottomleft", "off")
  vbox_xy[4,3, anchor=c(-1,0)] <- dl_xy <- gradio(locations)

  # buttons control
  anim_id <- NULL
  pause_xy <- function(paused) {
    svalue(chk_pause_xy) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <<- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }
  
  buttonGroup_xy <- ggroup(horizontal = F, cont=vbox_xy)  
  
  # addSpace(buttonGroup_xy,10)
  gbutton("Apply", cont = buttonGroup_xy, handler = function(...) {
    pause_xy(FALSE)
    update_tour_xy()
  })
  
  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup_xy, handler = function(...) {
    pause_xy(TRUE)
    dispose(w)
  })

  # addSpace(buttonGroup,10)
  gbutton("Help",cont=buttonGroup_xy, handler = function(...) {
gmessage("GUI_xy allows user to control a dynamic plot by using a checkbox, a ratiobox, a table, a slider and some bottons. And it could easily be extended. 
It's much more convenient for users to just click on this simple GUI instead of trying to figure out how to write the proper auguments for their desirable graphics.", 
title="gui_help",icon="info")
  })


  vbox_xy[4:6, 4, anchor = c(0, 1)] <- buttonGroup_xy
  
  # If on a mac, open a Cairo device, if there's not already one open
  # The cairo device has a much better refresh rate than Quartz
  if (find_platform()$os == "mac" && names(dev.cur()) != "Cairo") {
    require(Cairo)
    CairoX11()
  } else if (length(dev.list()) == 0) {
    # Open new display if necessary
    dev.new()
    # Turn off display list to maximise speed
    dev.control(displaylist = "inhibit")
  }
# =================================== End of Gui_xy ================================================
  
  pause_xy(FALSE)
  pause_density(FALSE)
  visible(w) <- TRUE
  invisible()
}



# ==========================================================================================================================

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

create_1d_tour <- function(data, var_selected, method_selected, center_selected, tour_type, aps) {
  if (length(var_selected) < 2) {
    gmessage("Please select at least two variables", icon = "warning")
    return()
  }
   
  display <- display_dist(data, method = method_selected, center = center_selected) 

  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(), 
    "Little" = little_tour(), 
    "Guided(holes)" = guided_tour(holes), 
    "Guided(cm)" = guided_tour(cm), 
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




gui(flea)