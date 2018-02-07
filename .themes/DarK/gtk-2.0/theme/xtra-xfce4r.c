style"xfceheaders"{
xthickness=0
ythickness=0
base[NORMAL]=shade(0.7, @bg_color)
base[PRELIGHT]=shade(0.7, @bg_color)
base[ACTIVE]=shade(0.7, @bg_color)
base[SELECTED]=shade(0.7, @bg_color)
base[INSENSITIVE]=shade(0.7, @bg_color)}
widget"*.XfceHeading*"style"xfceheaders"

style"xfdesktop"{
font_name="bold"
XfdesktopIconView::shadow-blur-radius=2
XfdesktopIconView::label-alpha=0
XfdesktopIconView::selected-label-alpha=255
XfdesktopIconView::ellipsize-icon-labels=1
XfdesktopIconView::shadow-x-offset=0
XfdesktopIconView::shadow-y-offset=1
XfdesktopIconView::selected-shadow-x-offset=0
XfdesktopIconView::selected-shadow-y-offset=0
XfdesktopIconView::tooltip_size=48
XfdesktopIconView::cell-spacing=1
XfdesktopIconView::cell-padding=1
XfdesktopIconView::cell-text-width-proportion=2
XfdesktopIconView::shadow-color=@bg_color
XfdesktopIconView::selected-shadow-color=@selected_bg_color
fg[NORMAL]=@fg_color
fg[SELECTED]=@selected_fg_color
fg[ACTIVE]=@selected_fg_color
base[ACTIVE]=@selected_bg_color}
class"XfdesktopIconView"style"xfdesktop"

style "exoicons"{
xthickness=0
ythickness=0
GtkWidget::focus_padding=4
fg[NORMAL]=@selected_bg_color
text[SELECTED]=@selected_fg_color}
widget_class "*ExoIconView*"style "exoicons"
widget_class "*ThunarIconView*ExoIconView"style "exoicons"

style "exoicons2"{
base[PRELIGHT]=@fg_color
base[SELECTED]=@selected_bg_color
base[ACTIVE]=@selected_bg_color
text[PRELIGHT]=@selected_fg_color
text[ACTIVE]=@selected_fg_color
text[SELECTED]=@selected_fg_color}
widget_class "*XfceSettingsManagerDialog*ExoIconView*"style "exoicons2"

style "thunarshortcutspane"{
engine"pixmap"{
image{
function=SHADOW
file="images/none.xpm"
stretch=TRUE}}}
widget_class "*.ThunarShortcutsPane"style "thunarshortcutspane"

style "thunarshortcutsview"{
font_name="bold"
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
widget_class "*.ThunarShortcutsView"style "thunarshortcutsview"

style "thunariconview"{
base[NORMAL]=@fg_color
engine"pixmap"{
image{
function=SHADOW
file="images/none.xpm"
stretch=TRUE}}}
widget_class "*ThunarIconView"style "thunariconview"

style "thunariconviewexo"{
base[NORMAL]=@bg_color}
widget_class "*ThunarIconView*ExoIconView"style "thunariconviewexo"

style"thunarnotebook"{
xthickness=1
ythickness=1
GtkNotebook::tab-curvature=1
GtkNotebook::tab-overlap=2
engine"pixmap"{
image{
function=EXTENSION
state=ACTIVE
file="images/tabtopunselected.png"
border={3,3,3,3}
stretch=TRUE
gap_side=BOTTOM}
image{
function=EXTENSION
file="images/tabtopunselected.png"
border={3,3,3,3}
stretch=TRUE
gap_side=BOTTOM
overlay_file="images/tabtop.xpm"
overlay_border={3,3,3,1}
overlay_stretch=TRUE
gap_side=BOTTOM}}}
widget_class "*ThunarWindow*GtkHPaned*GtkTable*GtkNotebook"style "thunarnotebook"

style"notebooklabelbold"{
fg[NORMAL]=@fg_color
fg[ACTIVE]=shade(0.5,@fg_color)
engine"mist"{}}
widget_class"*ThunarWindow*GtkHPaned*GtkTable*GtkNotebook*GtkLabel"style"mistrender"
widget_class "*XfburnNotebookTab*" style "mistrender"

style"thunarlocationbutton"{
xthickness=0
ythickness=0}
widget_class"*ThunarLocationButtons"style"thunarlocationbutton"
widget_class"*ThunarLocationButton"style"thunarlocationbutton"

style"thunarlocationentry"{
base[NORMAL]=shade(0.75,@bg_color)
base[PRELIGHT]=shade(0.75,@bg_color)
base[SELECTED]=@selected_bg_color
base[ACTIVE]=@selected_bg_color
base[INSENSITIVE]=@bg_color}
widget_class"*ThunarPathEntry"style"thunarlocationentry"

widget_class"*ThunarRenamerDialog.GtkVBox.GtkToolbar"style"dialogtoolbar"

widget_class"*XfcePanelWindow*Button"style"toolbutton"

style"xfcetaskbar"{
fg[NORMAL]=shade(0.7,@fg_color)}
widget_class"*XfceTasklist*Button*GtkLabel"style"xfcetaskbar"

widget_class"*XfceTasklist*Button"style"toolbutton"

class"XfrunDialog"style"dialogs"
class"ThunarUcaChooser"style"dialogs"

style "xfcenotifyd"{
XfceNotifyWindow::summary-bold=1
XfceNotifyWindow::border-color=@bg_color
XfceNotifyWindow::border-color-hover=@bg_color
XfceNotifyWindow::border-radius=0.0
XfceNotifyWindow::border-width=1
XfceNotifyWindow::border-width-hover=1
bg[NORMAL]=shade(0.85,@bg_color)}
class "XfceNotifyWindow" style "xfcenotifyd"
