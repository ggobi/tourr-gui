#===================================================
gui_density = function(x,...) {

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
MethodType="density"
ClIndex <<- c(1:length(x))


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


#2D tour control 
#' @param aps target angular velocity (in radians per second)
#' @param fps target frames per second (defaults to 30)


displayTour = function (h,...)
{
	if (type == "Grand")
 		animate_dist(x1[VarIndex],grand_tour(), center=T,method=MethodType)
	if (type == "Little")
 		animate_dist(x1[VarIndex],little_tour(),method=MethodType)
	if (type == "Guided(holes)")
		animate_dist(x1[VarIndex],guided_tour(holes),method=MethodType)
	if (type == "Guided(cm)") 
		animate_dist(x1[VarIndex],guided_tour(cm),method=MethodType)
	if (type == "Guided(lda_pp)") 
		animate_dist(x1[VarIndex],guided_tour(lda_pp,cl=cl),method=MethodType)
	if (type == "Local") 
 		animate_dist(x1[VarIndex],local_tour(basis_init(length(VarIndex), 2)),method=MethodType)
}
#===============================================

# ==================Controls==========================

# Control: gcheckboxgroup 
VarIndex <- c(1:length(x1))

vbox[2,1] <- (Variables<-gcheckboxgroup(variablename1, checked=TRUE, horizontal=FALSE, cont=vbox, handler = defHandler))
addHandlerChanged(Variables,handler = getVariables)


# Control: title
vbox[1,1,anchor=c(-1,0)] <- "Variable Selection"

#====================================================================

short = c("Grand","Little","Guided(holes)","Guided(cm)","Guided(lda_pp)","Local")


# Gradio box control
vbox[1,3, anchor=c(-1,0)] <- "Tour Type"

TourType = gradio(short, cont=vbox, handler = NULL)
addHandlerChanged(TourType,handler = getTourType)

vbox[2,3] <- TourType

#Method type control
vbox[3,1, anchor=c(-1,0)] <- "Method Type"
Methodtype=c("density", "hist")
vbox[4,1, anchor=c(-1,0)]<-(dl<-gradio(Methodtype,cont=vbox,
			handler=function(h,...){MethodType <<-svalue(h$obj)}))

# buttons control

buttonGroup = ggroup(horizontal = F, cont=vbox)
   pauseButton = gbutton("Pause",cont=buttonGroup)
   addSpace(buttonGroup,10)

   quitButton = gbutton("Quit",cont=buttonGroup)
   addHandlerClicked(quitButton, handler= function(h,...) dispose(w))
   addSpace(buttonGroup,10)

   okButton = gbutton("ok", cont=buttonGroup)
   addHandlerClicked(okButton, handler = displayTour)

vbox[4,4, anchor=c(0,1)] = buttonGroup

}


