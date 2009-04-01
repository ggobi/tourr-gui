#===================================================
gui_image = function(x,...) {

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
speed_aps = 1


# Handler of Control 2
getTourType = function (h,...)
{
	type <<-svalue(TourType)
}


displayTour = function (h,...)
{
	if (type == "Grand")
 		animate_image(x, grand_tour(1),aps = speed_aps)
	if (type == "Little")
 		animate_image(x,little_tour(),aps = speed_aps)
	if (type == "Guided(holes)")
		animate_image(x,guided_tour(holes),aps = speed_aps)
	if (type == "Guided(cm)") 
		animate_image(x,guided_tour(cm),aps = speed_aps)
	if (type == "Guided(lda_pp)") 
		animate_image(x,guided_tour(lda_pp,cl=cl),aps = speed_aps)
	if (type == "Local") 
 		animate_image(x,local_tour(basis_init(length(VarIndex), 2)),aps = speed_aps)
}
#===============================================

# ==================Controls==========================

short = c("Grand","Little","Guided(holes)","Guided(cm)","Guided(lda_pp)","Local")


# Gradio box control
vbox[1,1, anchor=c(-1,0)] <- "Tour Type"

TourType = gradio(short, cont=vbox, handler = NULL)
addHandlerChanged(TourType,handler = getTourType)

vbox[2,1] <- TourType

# speed slider control
vbox[3,1, anchor=c(-1,0)] <- "Speed"
vbox[4,1, expand=T] <- (sl <- gslider(from = 0, to= 10, by=0.1, value = 1, 
  	cont = vbox, handler = function(h,...){speed_aps <<- svalue(h$obj)}))

# buttons control

buttonGroup = ggroup(horizontal = F, cont=vbox)
   pauseButton = gbutton("Pause",cont=buttonGroup)
   addSpace(buttonGroup,10)

   quitButton = gbutton("Quit",cont=buttonGroup)
   addHandlerClicked(quitButton, handler= function(h,...) dispose(w))
   addSpace(buttonGroup,10)

   okButton = gbutton("ok", cont=buttonGroup)
   addHandlerClicked(okButton, handler = displayTour)

vbox[2,2, anchor=c(0,1)] = buttonGroup

}


