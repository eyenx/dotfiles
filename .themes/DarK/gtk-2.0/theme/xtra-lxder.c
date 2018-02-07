style"pcmanfm"{
base[NORMAL]=@bg_color}
widget_class"*.FmTabPage*"style"pcmanfm"
widget_class"*.FmSidePane*"style"pcmanfm"
widget_class"*.FmPlacesView*"style"pcmanfm"

style"pcmanfmentry"{
base[NORMAL]=shade(0.75,@bg_color)}
widget_class"*.FmPathEntry*"style"pcmanfmentry"

style "pcmanfmshortcut"{
engine"pixmap"{
image{
function=FLAT_BOX
file="images/bg.xpm"
state=NORMAL
stretch=TRUE}
image{
function=FLAT_BOX
file="images/selection.xpm"
state=SELECTED
stretch=TRUE}}}
widget_class"*.FmSidePane*"style"pcmanfmshortcut"
widget_class"*.FmPlacesView*"style"pcmanfmshortcut"

style "pcmanfmshortcut"{
font_name="bold"}
widget_class"*.FmPlacesView*"style"pcmanfmshortcut"
