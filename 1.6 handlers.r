# 1.6
# Updated: different tour type
# Using Global variables

library(gWidgets)
options("guiToolkit"="RGtk2")
###########################################################
w = gwindow("2D Tour plot example")
 g = ggroup(cont = w, horizontal = FALSE)
defHandler = function(h,...) print("hi")

x=c(1,2)
type = "Grand"
handler1 = function (h,...) 
{
	x <<- read.csv(file.choose())
	variablename<<-colnames(x)
	Variables = gcheckboxgroup(variablename, horizontal=FALSE, cont=vbox, handler
   = defHandler)
	vbox[1,2] <- Variables
}

getTourType = function (h,...)
{
	type <<-svalue(TourType)
}

displayTour = function (h,...)
{
	if (type == "Grand")
		animate_xy(x,grand_tour())
	if (type == "Little")
		animate_xy(x,little_tour())
	if (type == "Guided")
		animate_xy(x,guided_tour(holes))
}

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
defHandler = function(h,...) print(svalue(h$obj))
short = c("Grand","Guided","Little")

#checkbox group
vbox[1,1, anchor=c(-1,0)] <- "Variable select"


# Gradio box
vbox[1,3, anchor=c(-1,0)] <- "Select Tour "

TourType=gradio(short, cont=vbox, handler = defHandler)
addHandlerChanged(TourType,handler = getTourType)

vbox[1,4] <- TourType


vbox[2,1] <- "Choose tourr speed"
vbox[2,2] <- (sb <- gspinbutton(from=0,to=100,by=1,value=10, container=vbox,
    handler=defHandler))
vbox[3,1] <- "Adjust tourr speed"
vbox[3,2, expand=TRUE] <- (sl <- gslider(from = 0, to= 100, by=1, value = 10,
    cont = vbox, handler = defHandler))


g= ggroup(horizontal = FALSE, cont=w)
vbox[4,1, anchor=c(-1,0)] = ggroup(cont=g, expand=FALSE, horizontal = FALSE)
buttonGroup = ggroup(horizontal = FALSE, cont=g)
   helpButton = gbutton("help", cont=buttonGroup)
   addSpring(buttonGroup)
   pauseButton = gbutton("Pause",cont=buttonGroup)
   addSpace(buttonGroup, 10)

   okButton = gbutton("ok", cont=buttonGroup)
   addHandlerClicked(okButton, handler = displayTour)
	
   addSpace(buttonGroup, 10)
   
