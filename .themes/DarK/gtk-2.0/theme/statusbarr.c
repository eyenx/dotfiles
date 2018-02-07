style"statusbar"{
xthickness=2
ythickness=2
GtkStatusbar::shadow-type=GTK_SHADOW_NONE
engine"pixmap"{
image{
function=RESIZE_GRIP
detail="statusbar"
file="images/none.xpm"
stretch=FALSE}}}
class"GtkStatusbar"style"statusbar"

style"statusbarlabel"{
font_name="9"}
widget_class"*.<GtkStatusbar>*<GtkFrame>*<GtkLabel>"style"statusbarlabel"

widget_class"*.<GtkStatusbar>*<GtkImage>"style"mistrender"
widget_class"*.<GtkStatusbar>*<GtkLabel>"style"mistrender"
