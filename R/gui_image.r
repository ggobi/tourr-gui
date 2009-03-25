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


# Handler of Control 2
getTourType = function (h,...)
{
	type <<-svalue(TourType)
}


displayTour = function (h,...)
{
	if (type == "Grand")
 		animate_image(x, grand_tour(1))
	if (type == "Little")
 		animate_image(x,little_tour())
	if (type == "Guided(holes)")
		animate_image(x,guided_tour(holes))
	if (type == "Guided(cm)") 
		animate_image(x,guided_tour(cm))
	if (type == "Guided(lda_pp)") 
		animate_image(x,guided_tour(lda_pp,cl=cl))
	if (type == "Local") 
 		animate_image(x,local_tour(basis_init(length(VarIndex), 2)))
}
#===============================================

# ==================Controls==========================

short = c("Grand","Little","Guided(holes)","Guided(cm)","Guided(lda_pp)","Local")


# Gradio box control
vbox[1,1, anchor=c(-1,0)] <- "Tour Type"

TourType = gradio(short, cont=vbox, handler = NULL)
addHandlerChanged(TourType,handler = getTourType)

vbox[2,1] <- TourType


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


