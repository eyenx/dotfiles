widget"GeanyMainWindow.GtkVBox.GtkVPaned.GtkHPaned.GtkNotebook.GtkEventBox.GtkHBox.GtkLabel"style"notebooklabel"

style"geany-close-tab-button"{
xthickness=0
ythickness=0
bg[NORMAL]=@bg_color
bg[PRELIGHT]=@bg_color
bg[SELECTED]=@bg_color
bg[ACTIVE]=@bg_color
bg[INSENSITIVE]=@bg_color
engine""{}}
widget"*.geany-close-tab-button"style"geany-close-tab-button"

style "geany-document-status-changed-style"{
font_name="bold"}
widget "*.geany-document-status-changed" style:theme "geany-document-status-changed-style"
widget "*.geany-terminal-dirty" style "geany-document-status-changed-style"
