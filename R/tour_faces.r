#' Faces Tour Plotting
#' Plots the Faces Tour in tab g3
#'
#' @keywords internal
#' @author Bei Huang\email{beihuang@@iastate.edu}, Di Cook \email{dicook@@iastate.edu}, and Hadley Wickham \email{hadley@@rice.edu} 
# =============================== Gui_faces ====================================
.interface_faces = function(g3, data, w){

  # =============== Function: update_tour_faces ==================
  update_tour_faces <- function(...) {
    tour <<- .create_faces_tour(data,
      var_selected = svalue(Variables_faces),
      VarIndex = svalue(Variables_faces, index = TRUE),
      dim_selected = svalue(Dimensions_faces),
      tour_type = svalue(TourType_faces),
      aps = svalue(sl_faces)
    )
    tour_anim <<- with(tour, new_tour(data, tour_path))

    tour$display$init(tour$data)
    tour$display$render_frame()

    TRUE
  }
  # --------------------- End of update_tour_xy ------------------
  
  # ================= Function: draw_frame =======================
  draw_frame_faces <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)

    tour_step <- tour_anim(svalue(sl_faces) / 33)
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
  # ---------------------- End of draw_frame ---------------------
  
  num <- sapply(data, is.numeric)
  # ==================Controls==========================
  vbox_faces <- glayout(cont = g3)
    # Variable selection column
    vbox_faces[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
    vbox_faces[2, 1] <- Variables_faces <- gcheckboxgroup(names(data[num]),
      checked = TRUE, horizontal = FALSE)

    # Tour selection column
    vbox_faces[1, 3, anchor=c(-1, 0)] <- "Tour Type"
    tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
    vbox_faces[2, 3] <- TourType_faces <- gradio(tour_types)

    # speed and pause
    vbox_faces[3,3, anchor = c(-1, 0)] <- "Speed"
    vbox_faces[4,3, expand = TRUE] <- sl_faces <- gslider(from = 0, to = 5, by = 0.1, value = 1)

    vbox_faces[4, 4] <- chk_pause_faces <- gcheckbox("Pause",
      handler = function(h, ...) pause_faces(svalue(h$obj)))

    # dimension control
    vbox_faces[3, 1, anchor = c(-1, 0)] <- "Choose Dimension"
    dimensions <- c(2:length(data[num]))
    vbox_faces[4, 1, anchor = c(-1, 0)] <- Dimensions_faces <- gradio(dimensions)

    # buttons control
    anim_id <- NULL
    pause_faces <- function(paused) {
      svalue(chk_pause_faces) <- paused
      if (paused) {
        gtkIdleRemove(anim_id)
        anim_id <<- NULL
      } else {
        if (!is.null(anim_id)) gtkIdleRemove(anim_id)
        anim_id <<- gIdleAdd(draw_frame_faces)
      }
    }
    buttonGroup_faces <- ggroup(horizontal = FALSE, cont=vbox_faces)

    # addSpace(buttonGroup,10)
    gbutton("Apply", cont = buttonGroup_faces, handler = function(...){
      print("apply from gui_faces")
      pause_faces(FALSE)
      update_tour_faces()
    })

    # addSpace(buttonGroup,10)
    gbutton("Quit",cont=buttonGroup_faces, handler = function(...) {
      pause_faces(TRUE)
      dispose(w)
    })

    vbox_faces[3, 4, anchor = c(0, 1)] <- buttonGroup_faces
}
# -------------------------- End of Gui_faces ----------------------------------