#' Stars Tour Plotting
#' Plots the Stars Tour in tab g4
#'
#' @keywords internal
#' @author Bei Huang\email{beihuang@@iastate.edu}, Di Cook \email{dicook@@iastate.edu}, and Hadley Wickham \email{hadley@@rice.edu} 
# =============================== Gui_stars==============================
.interface_stars = function(g4,data, w){
  # =============== Function: update_tour_stars ==================
  update_tour_stars <- function(...) {
    tour <<- .create_stars_tour(data,
      var_selected = svalue(Variables_stars),
      dim_selected = svalue(Dimensions_stars),
      tour_type = svalue(TourType_stars),
      aps = svalue(sl_stars)
    )
    tour_anim <<- with(tour, new_tour(data, tour_path))

    tour$display$init(tour$data)
    tour$display$render_frame()

    TRUE
  }
  # --------------------- End of update_tour_stars ----------------
  
  # ================= Function: draw_frame_stars ==================
  draw_frame_stars <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)

    tour_step <- tour_anim(svalue(sl_stars) / 33)
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
  # -------------------- End of draw_frame_stars -----------------
  
  num <- sapply(data, is.numeric)
  # ================== Controls ==========================
  vbox_stars <- glayout(cont = g4)

  # Variable selection column
  vbox_stars[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_stars[2, 1] <- Variables_stars <- gcheckboxgroup(names(data[num]),
    checked = TRUE, horizontal = FALSE)

  # Tour selection column
  vbox_stars[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox_stars[2, 3] <- TourType_stars <- gradio(tour_types)

  # dimension control
  vbox_stars[3, 1, anchor = c(-1, 0)] <- "Choose Dimension"
  dimensions <- c(2:length(data[num]))
  vbox_stars[4, 1, anchor = c(-1, 0)] <- Dimensions_stars <- gradio(dimensions)

  # speed and pause
  vbox_stars[3,3, anchor = c(-1, 0)] <- "Speed"
  vbox_stars[4,3, expand = TRUE] <- sl_stars <- gslider(from = 0, to = 5, by = 0.1, value = 1)

  vbox_stars[4, 4] <- chk_pause_stars <- gcheckbox("Pause",
    handler = function(h, ...) pause_stars(svalue(h$obj)))

  # buttons control
  anim_id <- NULL
  pause_stars <- function(paused) {
    svalue(chk_pause_stars) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame_stars)
    }
  }
  buttonGroup_stars <- ggroup(horizontal = FALSE, cont=vbox_stars)

  # addSpace(buttonGroup,10)
  gbutton("Apply", cont = buttonGroup_stars, handler = function(...){
    print("apply from gui_stars")
    opar <- par(mfrow = c(1,1))
    pause_stars(FALSE)
    update_tour_stars()
  })

  # addSpace(buttonGroup,10)
  gbutton("Quit",cont=buttonGroup_stars, handler = function(...) {
    pause_stars(TRUE)
    dispose(w)
  })

  vbox_stars[2:3, 4, anchor = c(0, 1)] <- buttonGroup_stars
  
}
# ------------------------- End of Gui_stars ----------------------------