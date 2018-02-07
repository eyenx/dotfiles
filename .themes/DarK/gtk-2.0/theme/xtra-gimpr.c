style"gimp"{
GtkPaned::handle-size=5
GimpDockWindow::default-height=240
GimpDock::font-scale=1
GimpMenuDock::minimal-width=200
GimpDockWindow::menu-preview-size=menu
GimpToolPalette::tool-icon-size=menu
GimpToolPalette::button-relief=none
GimpDockbook::tab-border=5
GimpDockbook::tab-icon-size=menu
GimpColorNotebook::tab-border=0
GimpColorNotebook::tab-icon-size=menu
GimpDeviceEditor::handle-size=12
GimpDockable::content-border=1
GimpEditor::content-spacing=1
GimpEditor::button-spacing=1
GimpEditor::button-icon-size=menu
GimpDataEditor::minimal-height=64
GimpFrame::label-spacing=5
GtkDialog::content-area-border=0
GtkDialog::button-spacing=6
GtkDialog::action-area-border=12
GimpUnitComboBox::appears-as-list=0}
class"GimpImageWindow"style"gimp"

widget_class"*GimpSpinScale"style"gtkspinbutton"

style"gimpspinscale"{
base[NORMAL]="#323131"
base[SELECTED]=@selected_bg_color
base[ACTIVE]=@selected_bg_color
text[NORMAL]=shade(0.5,@fg_color)
base[SELECTED]="#323131"
base[ACTIVE]="#323131"}
widget_class"*.GimpSpinScale"style"gimpspinscale"

widget_class"*GimpDockable*GtkButton"style"toolbutton"
