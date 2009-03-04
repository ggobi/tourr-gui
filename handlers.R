# 2.3
# Updated: colour


source(file.choose())
library(gWidgets)
options("guiToolkit"="RGtk2")
#===================================================
w = gwindow("2D Tour plot example")
g = ggroup(cont = w, horizontal = FALSE)


x=c(1,2)
type = "Grand"
VarIndex=c(1,2)
Variables = 1
speed_aps = 1
ClIndex=c(1,2)
Class = 1
axes_location="center"

#==================Handlers=======================
defHandler = function(h,...) print(svalue(h$obj))

handler1 = function (h,...) 
{
	x <<- read.csv(file.choose())
	VarIndex <<- c(1:length(x))
	variablename<<-colnames(x)
# gcheckboxgroup control
	Variables <<- gcheckboxgroup(variablename, checked=TRUE, horizontal=FALSE, cont=vbox, handler
   = defHandler)
	addHandlerChanged(Variables,handler = getVariables)
	vbox[1,2] <- Variables
	
	vbox[2,4,anchor=c(0,1)] <-"Select Class"
	Class<<-gtable(variablename, multiple = T, cont=vbox,handler = defHandler)
	addHandlerChanged(Class, handler = getClass)
	vbox[2,5,anchor=c(-1,1)] <- Class
	size(Class) <- c(100,200)
	ClIndex <<- c(1:length(x))
	
	       
#	svalue(Variables,index=TRUE) <- VarIndex
}

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
	cl <<- length(unique(x[ClIndex]))
}

#2D tour control 
#' @param aps target angular velocity (in radians per second)
#' @param fps target frames per second (defaults to 30)


displayTour = function (h,...)
{
	if (type == "Grand")
		animate_xy(x[VarIndex],grand_tour(), center=T,aps = speed_aps, axes = axes_location,col=t(x[ClIndex])+3)
 									
	if (type == "Little")
		animate_xy(x[VarIndex],little_tour(), aps = speed_aps, axes = axes_location, col=t(x[ClIndex])+3)
	if (type == "Guided(holes)")
		animate_xy(x[VarIndex],guided_tour(holes), aps = speed_aps, axes = axes_location,col=t(x[ClIndex])+3)
	if (type == "Guided(cm)")
		animate_xy(x[VarIndex],guided_tour(cm), aps = speed_aps, axes = axes_location, col=t(x[ClIndex])+3)
	if (type == "Guided(lda_pp)") 
		animate_xy(x[VarIndex],guided_tour(lda_pp,cl=cl), aps = speed_aps, axes = axes_location,col=t(x[ClIndex])+3)
        	
}
      
             
# menu control
mbl = list(
   File = list(
     openFile = list(handler=handler1, icon="open"),
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



 
if(options("guiToolkit") == "RGtk2") {
   cont = w
 } else {
   cont = g
 }

 mb = gmenu(mbl, cont=cont) 
 tb = gtoolbar(tbl, cont=cont)

vbox = glayout(cont=w)

#====================================================================

short = c("Grand","Little","Guided(holes)","Guided(cm)","Guided(lda_pp)")

# checkbox group control
vbox[1,1, anchor=c(-1,0)] <- "Variable select"



# Gradio box control
vbox[1,3, anchor=c(-1,0)] <- "Select Tour "

TourType=gradio(short, cont=vbox, handler = NULL)
addHandlerChanged(TourType,handler = getTourType)

vbox[1,4] <- TourType





# speed slider control
vbox[2,1] <- "Adjust tourr speed"
vbox[2,2, expand=T] <- (sl <- gslider(from = 0, to= 10, by=0.1, value = 1, 
	cont = vbox, handler = function(h,...){speed_aps <<- svalue(h$obj)}))

#axes countrol
vbox[3,1] <- "Locations of axes"
Location=c("center", "bottomleft", "off")
vbox[3,2, anchor=c(-1,0)]<-(dl<-gradio(Location,cont=vbox,
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


#########################################################
#We need to double clike on the gtable variables, 
#or it will not print the current object, what's more, 
#it will not give us the ClIndex.
###########################################################
