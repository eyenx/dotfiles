style"gtktreeviewheader"{
xthickness=1
ythickness=1
GtkButton::inner-border={4,4,1,4}
bg[NORMAL]=shade(0.85,@bg_color)
bg[SELECTED]=shade(0.85,@bg_color)
bg[PRELIGHT]=shade(0.85,@bg_color)
bg[ACTIVE]=@selected_bg_color
bg[INSENSITIVE]=shade(0.85,@bg_color)
engine"pixmap"{
image{
function=SHADOW
file="images/treeviewheader.png"
border={0,1,0,1}
stretch=TRUE}}}
widget_class"*.<GtkTreeView><GtkButton>"style"gtktreeviewheader"
widget_class"*.<GtkCTree><GtkButton>"style"gtktreeviewheader"
widget_class"*.<GtkList><GtkButton>"style"gtktreeviewheader"
widget_class"*.<GtkCList><GtkButton>"style"gtktreeviewheader"

widget_class"*.<GtkTreeView><GtkButton>*<GtkLabel>"style"mistrenderlabelbold"

