#' PCP Tour Plotting
#' Plots the PCP Tour in tab g7
#'
#' @keywords internal
#' @author Bei Huang\email{beihuang@@iastate.edu}, Di Cook \email{dicook@@iastate.edu}, and Hadley Wickham \email{hadley@@rice.edu} 
# =============================== Gui_pcp ====================================
.interface_pcp = function(g7,data, w){

  # ================= Function: update_tour_pcp ==================
  update_tour_pcp <- function(...) {
    tour <<- .create_pcp_tour(data,
      var_selected = svalue(Variables_pcp),
      dim_selected = svalue(Dimensions_pcp),
      tour_type = svalue(TourType_pcp),
      aps = svalue(sl_pcp)
    )
    tour_anim <<- with(tour, new_tour(data, tour_path))

    tour$display$init(tour$data)
    tour$display$render_frame()

    TRUE
  }
  # -------------------- End of update_tour_pcp -----------------
  
  
  # ================= Function: draw_frame_pcp ==================
  draw_frame_pcp <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)

    tour_step <- tour_anim(svalue(sl_pcp) / 33)
    if (is.null(tour_step$proj)) return(FALSE)

    if (find_platform()$os == "win") {
      tour$display$render_frame()
    } else {
      tour$display$render_transition()
    }
    with(tour_step, tour$display$render_data(tour$data, proj, target))
    Sys.sleep(1/33)

    TRUE
  }
  # -------------------- End of draw_frame_pcp -----------------

  num <- sapply(data, is.numeric)
  # ================== Controls ==========================
  vbox_pcp <- glayout(cont = g7)

  # Variable selection column
  vbox_pcp[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_pcp[2, 1] <- Variables_pcp <- gcheckboxgroup(names(data[num]),
    checked = TRUE, horizontal = FALSE)

  # Tour selection column
  vbox_pcp[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox_pcp[2, 3] <- TourType_pcp <- gradio(tour_types)

  # dimension control
  vbox_pcp[3, 1, anchor = c(-1, 0)] <- "Choose Dimension"
  dimensions <- c(2:length(data[num]))
  vbox_pcp[4, 1, anchor = c(-1, 0)] <- Dimensions_pcp <- gradio(dimensions)

  # speed and pause
  vbox_pcp[3,3, anchor = c(-1, 0)] <- "Speed"
  vbox_pcp[4,3, expand = TRUE] <- sl_pcp <- gslider(from = 0, to = 5, by = 0.1, value = 1)

  vbox_pcp[4, 4] <- chk_pause_pcp <- gcheckbox("Pause",
    handler = function(h, ...) pause_pcp(svalue(h$obj)))

  # buttons control
  anim_id <- NULL
  pause_pcp <- function(paused) {
    svalue(chk_pause_pcp) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame_pcp)
    }
  }
  buttonGroup_pcp <- ggroup(horizontal = FALSE, cont=vbox_pcp)

  # addSpace(buttonGroup,10)
  gbutton("Apply", cont = buttonGroup_pcp, handler = function(...){
    print("apply from gui_pcp")
    opar <- par(mfrow = c(1,1))
    pause_pcp(FALSE)
    update_tour_pcp()
  })

  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup_pcp, handler = function(...) {
    pause_pcp(TRUE)
    dispose(w)
  })

  vbox_pcp[2:3, 4, anchor = c(0, 1)] <- buttonGroup_pcp
  
}
# ------------------------------ End of Gui_pcp ---------------------------------