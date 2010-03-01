
interface_xy = function (g1,data,w) {

  # ================= Function: update_tour_xy =======================
  update_tour_xy <- function(...) {
    tour <<- .create_xy_tour(data,
    var_selected = svalue(Variables_xy),
    cat_selected = svalue(Class_xy),
    axes_location = svalue(dl_xy),
    tour_type = svalue(TourType_xy),
    aps = svalue(sl_xy)
    )
    tour_anim <<- with(tour, new_tour(data, tour_path))

    tour$display$init(tour$data)
    tour$display$render_frame()
    TRUE
  }
  # ---------------------- End of update_tour_xy ---------------------


  # ================= Function: draw_frame_xy =======================
  draw_frame_xy <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) return(FALSE)

    tour_step <- tour_anim(svalue(sl_xy) / 33)
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
  # ---------------------- End of draw_frame_xy ---------------------

  os <- find_platform()$os
  num <- sapply(data, is.numeric)
  # ================== Controls for tour_xy ==========================
  vbox_xy1 <- glayout(cont = g1)
  # Variable selection column
  vbox_xy1[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox_xy1[2, 1] <- Variables_xy <- gcheckboxgroup(names(data[num]),
    checked = TRUE, horizontal = FALSE)
  vbox_xy1[3, 1, anchor = c(-1, 0)] <- "Class Selection"
  vbox_xy1[4, 1, anchor = c(-1, 0)] <- Class_xy <- gtable(names(data)[!num],
    multiple = TRUE)

  # Tour selection column
  vbox_xy1[1, 3, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox_xy1[2, 3] <- TourType_xy <- gradio(tour_types)

  # Speed and pause
  vbox_xy1[5,1, anchor = c(-1, 0)] <- "Speed"
  vbox_xy1[6,1, expand = TRUE] <- sl_xy <- gslider(from = 0, to = 5, by = 0.1, value = 1)

  vbox_xy1[6, 3] <- chk_pause_xy <- gcheckbox("Pause",
    handler = function(h, ...) pause_xy(svalue(h$obj)))

  # Axes control
  vbox_xy1[3,3, anchor=c(-1,0)] <- "Axes Locations"
  locations <- c("center", "bottomleft", "off")
  vbox_xy1[4,3, anchor=c(-1,0)] <- dl_xy <- gradio(locations)

  # Buttons control
  anim_id <- NULL
  pause_xy <- function(paused) {
    svalue(chk_pause_xy) <- paused
    if (paused) {
      gtkIdleRemove(anim_id)
      anim_id <<- NULL
    } else {
      if (!is.null(anim_id)) gtkIdleRemove(anim_id)
      anim_id <<- gIdleAdd(draw_frame_xy)
    }
  }

  buttonGroup_xy <- ggroup(horizontal = FALSE, cont=vbox_xy1)

  # Apply button & handler
  gbutton("Apply", cont = buttonGroup_xy, handler = function(...) {

    print("apply from gui_xy")
    if(is.null(anim_id))
    	pause_xy(FALSE)
    opar <- par(mfrow = c(1,1))
    update_tour_xy()
  })


  gbutton("Quit",cont=buttonGroup_xy, handler = function(...) {
    pause_xy(TRUE)
    dispose(w)
  })

  gbutton("Help",cont=buttonGroup_xy, handler = function(...) {
gmessage("GUI_xy allows user to control a dynamic plot by using a checkbox, a ratiobox, a table, a slider and some bottons. And it could easily be extended.
It's much more convenient for users to just click on this simple GUI instead of trying to figure out how to write the proper auguments for their desirable graphics.",
title="gui_help",icon="info")
  })

  vbox_xy1[4:6, 4, anchor = c(0, 1)] <- buttonGroup_xy

}