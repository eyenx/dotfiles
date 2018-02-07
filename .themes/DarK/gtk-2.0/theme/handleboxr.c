style"handlebox"{
xthickness=0
ythickness=0
bg[NORMAL]=shade(0.9,@bg_color)
bg[PRELIGHT]=shade(0.9,@bg_color)
bg[SELECTED]=shade(0.9,@bg_color)
bg[ACTIVE]=shade(0.9,@bg_color)
bg[INSENSITIVE]=shade(0.9,@bg_color)
engine"pixmap"{
image{
function=BOX
file="images/bg.xpm"
stretch=TRUE}
image{
function=FLAT_BOX
file="images/bg.xpm"
stretch=TRUE}
image{
function=SHADOW
shadow=OUT
file="images/none.xpm"
stretch=TRUE}}}
widget_class "*<GtkHandleBox>*"style"handlebox"

