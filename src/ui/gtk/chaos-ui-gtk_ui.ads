private with Gtk.Widget;
private with Gtk.Builder;

package Chaos.UI.Gtk_UI is

   function Create return Chaos_UI;

private

   UI_Definition : Gtk.Builder.Gtk_Builder;

   type Root_Gtk_Model is
     abstract new Root_UI_Model with
      record
         Top    : Gtk.Widget.Gtk_Widget;
         Expand : Boolean := False;
      end record;

   procedure Show (Model : in out Root_Gtk_Model) is null;

   type Gtk_UI_Model is access all Root_Gtk_Model'Class;

   procedure Create_Model
     (Model : in out Root_Gtk_Model'Class;
      Top   : Gtk.Widget.Gtk_Widget);

   procedure Hide_Widget
     (W : not null access Gtk.Widget.Gtk_Widget_Record'Class);

end Chaos.UI.Gtk_UI;
