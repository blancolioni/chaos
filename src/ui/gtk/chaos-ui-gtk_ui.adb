with Ada.Text_IO;

with Glib;
with Glib.Error;

with Gtk.Handlers;

with Gtk.Box;
with Gtk.Container;
with Gtk.Frame;
with Gtk.Main;
with Gtk.Style_Provider;
with Gtk.Window;

with Gtkada.Style;

with Chaos.Paths;

with Chaos.UI.Gtk_UI.Start_Model;

package body Chaos.UI.Gtk_UI is

   type Root_Gtk_UI is
     new Root_Chaos_UI with
      record
         Main_Window     : Gtk.Window.Gtk_Window;
         Top_Layout      : Gtk.Box.Gtk_Box;
         Main_Layout     : Gtk.Box.Gtk_Box;
         Current_Model   : Gtk_UI_Model;
      end record;

   overriding procedure Start (UI : in out Root_Gtk_UI);
   overriding procedure Stop (UI : in out Root_Gtk_UI);

   overriding procedure Show_Model
     (UI    : in out Root_Gtk_UI;
      Model : not null access Root_UI_Model'Class);

   type Gtk_UI is access all Root_Gtk_UI'Class;

   Local_UI : Gtk_UI;

   package Main_Window_Callback is
     new Gtk.Handlers.Callback (Gtk.Window.Gtk_Window_Record);

   procedure Destroy_Handler (W : access Gtk.Window.Gtk_Window_Record'Class);

   ------------
   -- Create --
   ------------

   function Create return Chaos_UI is
      Builder : Gtk.Builder.Gtk_Builder;
      UI_Path : constant String :=
                  Chaos.Paths.Config_File ("ui/chaos.ui");
      Result  : constant Gtk_UI := new Root_Gtk_UI;
   begin

      Local_UI := Result;
      Local_Current_UI := Chaos_UI (Result);

      Gtk.Main.Init;

      declare
         Path  : constant String :=
                   Chaos.Paths.Config_File ("ui/chaos.css");

         procedure Error (Message : String);

         -----------
         -- Error --
         -----------

         procedure Error (Message : String) is
         begin
            raise Program_Error with "Could not load " & Path & ": " & Message;
         end Error;

      begin
         Gtkada.Style.Load_Css_File
           (Path     => Path,
            Error    => Error'Access,
            Priority => Gtk.Style_Provider.Priority_Application);
      end;

      Gtk.Builder.Gtk_New (Builder);
      UI_Definition := Builder;

      Ada.Text_IO.Put_Line ("Loading: " & UI_Path);

      declare
         use type Glib.Guint;
         Error : aliased Glib.Error.GError;
         Result : constant Glib.Guint :=
                    Builder.Add_From_File
                      (Filename => UI_Path,
                       Error    => Error'Access);
      begin
         if Result = 0 then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Error opening GUI definition: " & UI_Path
               & ": " & Glib.Error.Get_Message (Error));
            return null;
         end if;
      end;

      Ada.Text_IO.Put_Line ("done");

      Result.Main_Window :=
        Gtk.Window.Gtk_Window
          (Builder.Get_Object ("Chaos_Application"));

      Main_Window_Callback.Connect
        (Result.Main_Window,
         "destroy",
         Main_Window_Callback.To_Marshaller (Destroy_Handler'Access));

      Result.Top_Layout :=
        Gtk.Box.Gtk_Box
          (Builder.Get_Object ("Top_Layout"));

      Result.Main_Layout :=
        Gtk.Box.Gtk_Box
          (Builder.Get_Object ("Main_Layout"));

      Result.Main_Window.Show_All;

      Result.Show_Model (Chaos.UI.Gtk_UI.Start_Model.Model);

      return Chaos_UI (Result);

   end Create;

   ------------------
   -- Create_Model --
   ------------------

   procedure Create_Model
     (Model : in out Root_Gtk_Model'Class;
      Top   : Gtk.Widget.Gtk_Widget)
   is
      Frame       : Gtk.Frame.Gtk_Frame;
   begin
      Gtk.Frame.Gtk_New (Frame);
      Top.Ref;
      Gtk.Container.Gtk_Container_Record'Class
        (Top.Get_Parent.all).Remove (Top);
      Frame.Add (Top);
      Top.Unref;
      Model.Top := Gtk.Widget.Gtk_Widget (Frame);
   end Create_Model;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (W : access Gtk.Window.Gtk_Window_Record'Class)
   is
      pragma Unreferenced (W);
   begin
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   -----------------
   -- Hide_Widget --
   -----------------

   procedure Hide_Widget
     (W : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      W.Set_Visible (False);
   end Hide_Widget;

   ----------------
   -- Show_Model --
   ----------------

   overriding procedure Show_Model
     (UI    : in out Root_Gtk_UI;
      Model : not null access Root_UI_Model'Class)
   is
      use type Glib.Gint;
      Current_Model_Expand : constant Boolean :=
                               (if UI.Current_Model /= null
                                then UI.Current_Model.Expand
                                else False);
   begin
      if UI.Current_Model /= null then
         UI.Main_Layout.Remove (UI.Current_Model.Top);
      end if;

      UI.Current_Model := Gtk_UI_Model (Model);
      Local_Current_Model := UI_Model (Model);

      if Current_Model_Expand /= UI.Current_Model.Expand then
         UI.Top_Layout.Remove (UI.Main_Layout);
         UI.Top_Layout.Pack_Start
           (Child   => UI.Main_Layout,
            Expand  => True,
            Fill    => UI.Current_Model.Expand,
            Padding => 0);
      end if;

      Local_UI.Main_Layout.Pack_Start
        (Child   => UI.Current_Model.Top,
         Expand  => True,
         Fill    => UI.Current_Model.Expand,
         Padding => 0);
      UI.Current_Model.Top.Show_All;
      UI.Current_Model.Show;
   end Show_Model;

   -----------
   -- Start --
   -----------

   overriding procedure Start (UI : in out Root_Gtk_UI) is
      pragma Unreferenced (UI);
   begin
      Gtk.Main.Main;
   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop (UI : in out Root_Gtk_UI) is
      pragma Unreferenced (UI);
   begin
      Gtk.Main.Main_Quit;
   end Stop;

end Chaos.UI.Gtk_UI;
