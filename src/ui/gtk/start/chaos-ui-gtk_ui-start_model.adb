with Gtk.Button;

--  with Chaos.UI.Gtk_UI.Character_Gen_Model;
with Chaos.UI.Gtk_UI.Main_Model;

with Chaos.Creatures.Quick;
with Chaos.Classes;
with Chaos.Races;

with Chaos.Areas.Import;

package body Chaos.UI.Gtk_UI.Start_Model is

   type Root_Start_Model is
     new Root_Gtk_Model with null record;

   type Start_Model_Access is access all Root_Start_Model'Class;

   Local_Start_Model : Start_Model_Access;

   procedure Create_Model;

   procedure On_New_Game_Click
     (Self : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Resume_Game_Click
     (Self : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Load_Game_Click
     (Self : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Exit_Game_Click
     (Self : access Gtk.Button.Gtk_Button_Record'Class);

   ------------------
   -- Create_Model --
   ------------------

   procedure Create_Model is
      Load_Game    : constant Gtk.Button.Gtk_Button :=
                       Gtk.Button.Gtk_Button
                         (UI_Definition.Get_Object
                            ("Button_Load_Game"));
      Resume_Game    : constant Gtk.Button.Gtk_Button :=
                         Gtk.Button.Gtk_Button
                           (UI_Definition.Get_Object
                              ("Button_Resume_Game"));
      New_Game    : constant Gtk.Button.Gtk_Button :=
                      Gtk.Button.Gtk_Button
                        (UI_Definition.Get_Object
                           ("Button_New_Game"));
      Exit_Game   : constant Gtk.Button.Gtk_Button :=
                      Gtk.Button.Gtk_Button
                        (UI_Definition.Get_Object
                           ("Button_Quit"));
      Top         : constant Gtk.Widget.Gtk_Widget :=
                      Gtk.Widget.Gtk_Widget
                        (UI_Definition.Get_Object ("Start_Model"));
   begin
      New_Game.On_Clicked
        (On_New_Game_Click'Access);
      Resume_Game.On_Clicked
        (On_Resume_Game_Click'Access);
      Load_Game.On_Clicked
        (On_Load_Game_Click'Access);
      Exit_Game.On_Clicked
        (On_Exit_Game_Click'Access);
      Local_Start_Model := new Root_Start_Model;
      Local_Start_Model.Create_Model (Top);
   end Create_Model;

   -----------
   -- Model --
   -----------

   function Model return Gtk_UI_Model is
   begin
      if Local_Start_Model = null then
         Create_Model;
      end if;
      return Gtk_UI_Model (Local_Start_Model);
   end Model;

   ------------------------
   -- On_Exit_Game_Click --
   ------------------------

   procedure On_Exit_Game_Click
     (Self : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Current_UI.Stop;
   end On_Exit_Game_Click;

   ------------------------
   -- On_Load_Game_Click --
   ------------------------

   procedure On_Load_Game_Click
     (Self : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Self);
      Creature : constant Chaos.Creatures.Chaos_Creature :=
                   Chaos.Creatures.Quick.Quick_Creature
                     ("Aramael Musitello",
                      Chaos.Races.Get ("human"),
                      Chaos.Classes.Get ("fighter"));
      Area     : constant Chaos.Areas.Chaos_Area :=
                   Chaos.Areas.Import.Import_Area
                     ("AR2602");
   begin
      Current_UI.Party.Clear;
      Current_UI.Party.Add_Party_Member
        (Chaos.Actors.Create_Actor (Creature, Area, (10, 10)));
      Current_UI.Show_Model
        (Main_Model.Model);
   end On_Load_Game_Click;

   -----------------------
   -- On_New_Game_Click --
   -----------------------

   procedure On_New_Game_Click
     (Self : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      null;
--        Current_UI.Show_Model
--          (Character_Gen_Model.Model);
   end On_New_Game_Click;

   --------------------------
   -- On_Resume_Game_Click --
   --------------------------

   procedure On_Resume_Game_Click
     (Self : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Current_UI.Show_Model
        (Main_Model.Model);
   end On_Resume_Game_Click;

end Chaos.UI.Gtk_UI.Start_Model;
