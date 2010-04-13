#' Scatmat Tour GUI                                   
#' Displays an Scatmat Tour GUI                       
#'
#'This GUI allows users to control the Scatter matrix tour by simply moving and clicking their mouses.
#' 
#' @param data matrix, or data frame containing numeric columns, defaults to flea dataset
#' @param ... other arguments passed on to \code{\link{animate}} and \code{\link{display_xy}}
#' @author Bei Huang\email{beihuang@@iastate.edu}, Di Cook \email{dicook@@iastate.edu}, and Hadley Wickham \email{hadley@@rice.edu} 
#' @keywords display_scatmat
#' @examples
#' \dontrun{gui_scatmat(flea)}
gui_scatmat <- function(data = flea, ...) {
  require(tourr)
  require(gWidgets)
  require(RGtk2)
  options("guiToolkit"="RGtk2")

  os <- find_platform()$os
  num <- sapply(data, is.numeric)
  
  tour <- NULL
  tour_anim <- NULL
  update_tour <- function(...) {
    tour <<- .create_mat_tour(data,
      var_selected = svalue(Variables),
      projdim_selected = svalue(Projections), 
      tour_type = svalue(TourType),
      aps = svalue(sl)
    )
    tour_anim <<- with(tour, new_tour(data, tour_path))
    
    tour$display$init(tour$data)
 #  tour$display$render_frame()
    
    TRUE
  }
  
  draw_frame <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(TRUE)  

    tour_step <- tour_anim(svalue(sl) / 33)
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

  # Tour selection column
  vbox[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox[2, 3] <- TourType <- gradio(tour_types)

  # dimension control
  vbox[3, 1, anchor = c(-1, 0)] <- "Choose Projection Dimension"
  projections <- c(3:length(data[num]))
  vbox[4, 1, anchor = c(-1, 0)] <- Projections <- gradio(projections)
	

  # speed and pause
  vbox[3,3, anchor = c(-1, 0)] <- "Speed"
  vbox[4,3, expand = TRUE] <- sl <- gslider(from = 0, to = 5, by = 0.1, value = 1)
  
  vbox[4, 4] <- chk_pause <- gcheckbox("Pause", 
    handler = function(h, ...) pause(svalue(h$obj)))

  # buttons control
  anim_id <- NULL
  pause <- function(paused) {
    svalue(chk_pause) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame)
    }
  }
  buttonGroup <- ggroup(horizontal = FALSE, cont=vbox)  
  
  # addSpace(buttonGroup,10)
  gbutton("Apply", cont = buttonGroup, handler = function(...){
    pause(FALSE)
    update_tour()
  })

  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup, handler = function(...) {
    pause(TRUE)
    dispose(w)
  })

  vbox[2:3, 4, anchor = c(0, 1)] <- buttonGroup
  
  # If on a mac, open a Cairo device, if there's not already one open
  # The cairo device has a much better refresh rate than Quartz
  if (find_platform()$os == "mac" && names(dev.cur()) != "Cairo") {
    require(Cairo)
    CairoX11()
  }
  
  update_tour()
  pause(FALSE)
  visible(w) <- TRUE
  
  invisible()
}

#' Scatmat Tour Plotting
#' Plots the Scatmat Tour
#'
#' @keywords internal
#' @author Bei Huang\email{beihuang@@iastate.edu}, Di Cook \email{dicook@@iastate.edu}, and Hadley Wickham \email{hadley@@rice.edu}

.create_mat_tour <- function(data, var_selected, projdim_selected, tour_type, aps) {
  if (length(var_selected) < 3) {
    gmessage("Please select at least three variables", icon = "warning")
    return()
  }


  # display <- display_scatmat(data,tour_path=tour_type)
  # cat("names:\n");print(names(data))
  # cat("\n\ndimnames:\n");print(dimnames(data))
  # display <- display_scatmat(data, labels = var_selected[1:as.numeric(projdim_selected)])
  display <- display_scatmat(data)  

  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(as.numeric(projdim_selected)), 
    "Little" = little_tour(as.numeric(projdim_selected)), 
    "Guided(holes)" = guided_tour(holes,as.numeric(projdim_selected)), 
    "Guided(cm)" = guided_tour(cm,as.numeric(projdim_selected)), 
    "Guided(lda_pp)" = guided_tour(lda_pp(data[,cat_selected]),as.numeric(projdim_selected)),
    "Local" = local_tour()
  )
    
  sel <- data[,var_selected]
  # browser()
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