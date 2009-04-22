#===================================================
gui_xy = function(x = flea, ...) {
  # browser()
  library(gWidgets)
  options("guiToolkit"="RGtk2")
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
  ClIndex <<- c(1:length(x))
  Class = 1
  axes_location="center"
  ColType = 0

  #=====================split data set===================================
  idx <- sapply(1:(dim(x)[2]), 
     function(i) return(is.factor(x[,i]) | is.character(x[,i])))
  x1 <- x[,which(idx==FALSE)]
  x2 <- x[c(idx)]
  variablename1<<-colnames(x1)
  variablename2<<-colnames(x2)

  #==================Handlers=======================
  defHandler = function(h,...) print(svalue(h$obj))

  # Handler of Control 1
  getVariables = function (h,...)
  {
    VarIndex <<-svalue(Variables, index = T)
    if (length(VarIndex)<3) print("At least three variables is needed")
  }

  # Handler of Control 2
  getTourType = function (h,...)
  {
    type <<-svalue(TourType)
  }


  # Handler of Class
  getClass = function (h,...)
  {
    ClIndex <<-svalue(h$obj, index = T)
    require(colorspace)
    ColType <<- rainbow_hcl(length(unique(x2[,ClIndex])))
  }




  #2D tour control 
  #' @param aps target angular velocity (in radians per second)
  #' @param fps target frames per second (defaults to 30)


  displayTour = function (h,...)
  {
    if (length(VarIndex)<3) gmessage(message="At least three variables is needed",icon="warning")
    if (type == "Grand" & length(ClIndex) == length(x))
       animate_xy(x1[VarIndex],grand_tour(), center=T,aps = speed_aps, axes = axes_location)
    if (type == "Grand" & length(ClIndex) != length(x))    
      animate_xy(x1[VarIndex],grand_tour(), center=T,aps = speed_aps, axes = axes_location, col=ColType[as.numeric(x2[,ClIndex])])
    if (type == "Little" & length(ClIndex) == length(x))
       animate_xy(x1[VarIndex],little_tour(), center=T,aps = speed_aps, axes = axes_location)
    if (type == "Little"& length(ClIndex) != length(x))
      animate_xy(x1[VarIndex],little_tour(), aps = speed_aps, axes = axes_location, col=ColType[as.numeric(x2[,ClIndex])])
    if (type == "Guided(holes)" & length(ClIndex) == length(x))
      animate_xy(x1[VarIndex],guided_tour(holes), aps = speed_aps, axes = axes_location)
    if (type == "Guided(holes)" & length(ClIndex) != length(x))
      animate_xy(x1[VarIndex],guided_tour(holes), aps = speed_aps, axes = axes_location, col=ColType[as.numeric(x2[,ClIndex])])
    if (type == "Guided(cm)" & length(ClIndex) == length(x))
      animate_xy(x1[VarIndex],guided_tour(cm), aps = speed_aps, axes = axes_location)
    if (type == "Guided(cm)" & length(ClIndex) != length(x))
      animate_xy(x1[VarIndex],guided_tour(cm), aps = speed_aps, axes = axes_location, col=ColType[as.numeric(x2[,ClIndex])])
    if (type == "Guided(lda_pp)" & length(ClIndex) == length(x)) 
      animate_xy(x1[VarIndex],guided_tour(lda_pp,cl=cl), aps = speed_aps, axes = axes_location)
    if (type == "Guided(lda_pp)" & length(ClIndex) != length(x)) 
      animate_xy(x1[VarIndex],guided_tour(lda_pp,cl=cl), aps = speed_aps, axes = axes_location,col=ColType[as.numeric(x2[,ClIndex])])
    if (type == "Local" & length(ClIndex) == length(x))
       animate_xy(x1[VarIndex],local_tour(basis_init(length(VarIndex), 2)), center=T,aps = speed_aps, axes = axes_location)
    if (type == "Local"& length(ClIndex) != length(x))
      animate_xy(x1[VarIndex],local_tour(basis_init(length(VarIndex), 2)), aps = speed_aps, axes = axes_location, col=ColType[as.numeric(x2[,ClIndex])])  
  }
  #===============================================

  # ==================Controls==========================

  # Control: gcheckboxgroup 
  VarIndex <- c(1:length(x1))

  vbox[2,1] <- (Variables<-gcheckboxgroup(variablename1, checked=TRUE, horizontal=FALSE, cont=vbox, handler = defHandler))
  addHandlerChanged(Variables,handler = getVariables)


  # Control: title
  vbox[1,1,anchor=c(-1,0)] <- "Variable Selection"
  vbox[3,1,anchor=c(-1,0)] <- "Class Selection"

  # Control: gtable
  vbox[4,1,anchor=c(-1,0)] <- (Class<-gtable(variablename2, multiple = T, cont=vbox,handler = defHandler))
  addHandlerChanged(Class, handler = getClass)

  #====================================================================

  short = c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")


  # Gradio box control
  vbox[1,3, anchor=c(-1,0)] <- "Tour Type"

  TourType = gradio(short, cont=vbox, handler = NULL)
  addHandlerChanged(TourType,handler = getTourType)

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
