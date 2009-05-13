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

  type = "Grand"
  VarIndex=c(1,2)
  speed_aps = 1
  ClIndex <<- c(1:length(data))
  Class = 1
  axes_location="center"
  ColType = 0

  #=====================split data set===================================
  num <- sapply(data, is.numeric)
  x1 <- data[num]
  x2 <- data[!num]

  #==================Handlers=======================
  defHandler = function(h,...) print(svalue(h$obj))

  # Handler of Control 1
  getVariables = function (h,...)
  {
    VarIndex <<-svalue(Variables, index = T)
    if (length(VarIndex)<3) print("At least three variables is needed")
  }

  # Handler of Class
  getClass = function (h,...)
  {
  }


  tour_types <- c(
    "Grand" = grand_tour(), 
    "Little" = little_tour(), 
    "Guided(holes)" = guided_tour(holes), 
    "Guided(cm)" = guided_tour(cm), 
    "Guided(lda_pp)" = guided_tour(), 
    "Local" = local_tour()
  )

  displayTour <- function(h, ...) {
    if (length(VarIndex) < 3) {
      gmessage(message = "At least three variables are required", 
        icon = "warning")
    }
    
    tour <- tour_types[[svalue(TourType)]]

    cat_selected <- data[svalue(Class)]
    if (length(cat_selected) > 0) {
      # collapse to single variable if multiple selected
      int <- interaction(cat_selected, drop = TRUE)
      pal <- rainbow_hcl(length(levels(int)))
      col <- pal[as.numeric(int)]
    } else {
      col <- "black"
    }
    
    #2D tour control 
    #' @param aps target angular velocity (in radians per second)
    #' @param fps target frames per second (defaults to 30)
    animate(x1[VarIndex], tour, 
      display_xy(data, axes = axes_location, center = TRUE, col = col), 
      aps = speed_aps)
      
    # if (type == "Grand" & length(ClIndex) != length(x))    
    #   animate_xy(x1[VarIndex],grand_tour(), center=T,aps = speed_aps, axes = axes_location, col=ColType[as.numeric(x2[,ClIndex])])
  }
  # ==================Controls==========================

  # Control: gcheckboxgroup 
  VarIndex <- c(1:length(x1))

  vbox[2,1] <- (Variables<-gcheckboxgroup(names(data[num]), checked=TRUE, horizontal=FALSE, cont=vbox, handler = defHandler))
  addHandlerChanged(Variables,handler = getVariables)


  # Control: title
  vbox[1,1,anchor=c(-1,0)] <- "Variable Selection"
  vbox[3,1,anchor=c(-1,0)] <- "Class Selection"

  # Control: gtable
  vbox[4,1,anchor=c(-1,0)] <- (Class<-gtable(names(data)[!num], multiple = T, cont=vbox,handler = defHandler))

  #====================================================================

  short = c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")


  # Gradio box control
  vbox[1,3, anchor=c(-1,0)] <- "Tour Type"

  TourType = gradio(short, cont=vbox, handler = NULL)

  vbox[2,3] <- TourType


  # speed slider control
  vbox[5,1, anchor=c(-1,0)] <- "Speed"
  vbox[6,1, expand=T] <- (sl <- gslider(from = 0, to= 20, by=1, value = 1, 
    cont = vbox, handler = function(h,...){speed_aps <<- svalue(h$obj)}))


  #axes control
  vbox[3,3, anchor=c(-1,0)] <- "Axes Locations"
  Location=c("center", "bottomleft", "off")
  vbox[4,3, anchor=c(-1,0)]<-(dl<-gradio(Location,cont=vbox,
    handler=function(h,...){axes_location <<-svalue(h$obj)}))


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
