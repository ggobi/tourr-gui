#===================================================
gui_xy = function(x,...) {
# browser()

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
	# cl <<- length(unique(as.numeric(x[ClIndex])))
}

#2D tour control 
#' @param aps target angular velocity (in radians per second)
#' @param fps target frames per second (defaults to 30)


displayTour = function (h,...)
{
	if (type == "Grand" & length(ClIndex) == length(x))
 		animate_xy(x1[VarIndex],grand_tour(), center=T,aps = speed_aps, axes = axes_location)
	if (type == "Grand" & length(ClIndex) != length(x))
		animate_xy(x1[VarIndex],grand_tour(), center=T,aps = speed_aps, axes = axes_location, col=as.numeric(as.factor(t(x2[ClIndex])))+3)	
	if (type == "Little" & length(ClIndex) == length(x))
 		animate_xy(x1[VarIndex],little_tour(), center=T,aps = speed_aps, axes = axes_location)
	if (type == "Little"& length(ClIndex) != length(x))
		animate_xy(x[VarIndex],little_tour(), aps = speed_aps, axes = axes_location, col=as.numeric(x2[,ClIndex])+3)
	if (type == "Guided(holes)" & length(ClIndex) == length(x))
		animate_xy(x[VarIndex],guided_tour(holes), aps = speed_aps, axes = axes_location)
	if (type == "Guided(holes)" & length(ClIndex) != length(x))
		animate_xy(x[VarIndex],guided_tour(holes), aps = speed_aps, axes = axes_location, col=as.numeric(x2[,ClIndex])+3)
	if (type == "Guided(cm)" & length(ClIndex) == length(x))
		animate_xy(x[VarIndex],guided_tour(cm), aps = speed_aps, axes = axes_location)
	if (type == "Guided(cm)" & length(ClIndex) != length(x))
		animate_xy(x[VarIndex],guided_tour(cm), aps = speed_aps, axes = axes_location, col=as.numeric(x2[,ClIndex])+3)
	if (type == "Guided(lda_pp)" & length(ClIndex) == length(x)) 
		animate_xy(x[VarIndex],guided_tour(lda_pp,cl=cl), aps = speed_aps, axes = axes_location)
	if (type == "Guided(lda_pp)" & length(ClIndex) != length(x)) 
		animate_xy(x[VarIndex],guided_tour(lda_pp,cl=cl), aps = speed_aps, axes = axes_location,col=as.numeric(x2[,ClIndex])+3)       	
}
#===============================================

# ==================Controls==========================

# Control: gcheckboxgroup 
VarIndex <- c(1:length(x1))

vbox[1,2] <- (Variables<-gcheckboxgroup(variablename1, checked=TRUE, horizontal=FALSE, cont=vbox, handler = defHandler))
addHandlerChanged(Variables,handler = getVariables)


# Control: title
vbox[1,1,anchor=c(-1,0)] <- "Variable Select"
vbox[2,1,anchor=c(-1,0)] <-"Select Class for Color"

# Control: gtable
vbox[2,2,anchor=c(-1,1)] <- (Class<-gtable(variablename2, multiple = T, cont=vbox,handler = defHandler))
addHandlerChanged(Class, handler = getClass)
size(Class) <- c(60,100)
      
             
# menu control
mbl = list(
   File = list(
#     openFile = list(handler=handler1, icon="open"),
     close = list(handler=function(h,...) dispose(w), icon="cancel")
     ),
   Edit = list(
     paste = list(handler=defHandler),
     copy =  list(handler=defHandler)
     )
   )

# Tab control
tbl = list(
   new = list(handler = defHandler, icon="new"),
   quit = list(handler= function(h,...) dispose(w), icon="quit"),
   save = list(handler= defHandler, icon="save"),
   ok = list(handler=function(h,...) animate_xy(x), icon="ok")
   )

mb = gmenu(mbl, cont=cont) 
tb = gtoolbar(tbl, cont=cont)


#====================================================================

short = c("Grand","Little","Guided(holes)","Guided(cm)","Guided(lda_pp)")



# Gradio box control
vbox[1,3, anchor=c(-1,0)] <- "Select Tour "

TourType=gradio(short, cont=vbox, handler = NULL)
addHandlerChanged(TourType,handler = getTourType)

vbox[1,4] <- TourType





# speed slider control
vbox[3,1] <- "Adjust tourr speed"
vbox[3,2, expand=T] <- (sl <- gslider(from = 0, to= 10, by=0.1, value = 1, 
	cont = vbox, handler = function(h,...){speed_aps <<- svalue(h$obj)}))

#axes countrol
vbox[2,3] <- "Locations of axes"
Location=c("center", "bottomleft", "off")
vbox[2,4, anchor=c(-1,0)]<-(dl<-gradio(Location,cont=vbox,
			handler=function(h,...){axes_location <<-svalue(h$obj)}))



# buttons control
g= ggroup(horizontal = FALSE, cont=w)
vbox[1,5, anchor=c(-1,1)] = ggroup(cont=g, expand=F, horizontal = F)
buttonGroup = ggroup(horizontal = F, cont=g)
   helpButton = gbutton("help", cont=buttonGroup)
   addSpring(buttonGroup)
   pauseButton = gbutton("Pause",cont=buttonGroup)
   addSpace(buttonGroup, 10)

   okButton = gbutton("ok", cont=buttonGroup)
   addHandlerClicked(okButton, handler = displayTour)
   addSpace(buttonGroup, 10)
   cancelButton = gbutton("cancel", cont=buttonGroup)
}
