with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash_Case_Insensitive;
with Ada.Strings.Fixed.Equal_Case_Insensitive;

with Glib;

with Gtk.Box;
with Gtk.Button;
with Gtk.Container;
with Gtk.GEntry;
with Gtk.Grid;
with Gtk.Label;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Text_View;
with Gtk.Toggle_Button;

with Chaos.Abilities;
with Chaos.Alignment;
with Chaos.Creatures;
with Chaos.Classes;
with Chaos.Genders;
with Chaos.Races;

with Chaos.Localisation;               use Chaos.Localisation;

with Chaos.Creatures.Quick;

with Chaos.UI.Gtk_UI.Main_Model;

package body Chaos.UI.Gtk_UI.Character_Gen_Model is

   All_Steps_On : constant Boolean := False;

   type Choice_Button_Record is
     new Gtk.Toggle_Button.Gtk_Toggle_Button_Record with
      record
         Tag : access String;
      end record;

   type Choice_Button is access all Choice_Button_Record'Class;

   type Choice_Application_Procedure is access
     procedure (Id : String);

   type Creation_State is
      record
         State_Button : Gtk.Button.Gtk_Button;
         Next_Button  : Gtk.Button.Gtk_Button;
         Apply_Choice : Choice_Application_Procedure;
      end record;

   package Choice_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (String, Creation_State,
        Ada.Strings.Fixed.Hash_Case_Insensitive,
        Ada.Strings.Fixed.Equal_Case_Insensitive);

   Choice_Map : Choice_Maps.Map;

   function Create_Choice_Button
     (Tag       : String;
      On_Choose : Choice_Application_Procedure)
      return Gtk.Widget.Gtk_Widget;

   procedure Init_Step_Button
     (Step_Name      : String;
      Next_Step_Name : String);

   type Ability_Widgets_Record is
      record
         Current_Score : Chaos.Abilities.Ability_Score_Range;
         Score_Label   : Gtk.Label.Gtk_Label;
         Bonus_Label   : Gtk.Label.Gtk_Label;
         Plus_Button   : Gtk.Button.Gtk_Button;
         Minus_Button  : Gtk.Button.Gtk_Button;
      end record;

   type Ability_Widget_Arrays is
     array (Chaos.Abilities.Ability) of Ability_Widgets_Record;

   type Ability_Change_Button_Record is
     new Gtk.Button.Gtk_Button_Record with
      record
         Ability     : Chaos.Abilities.Ability;
         Change      : Integer;
         Score_Label : Gtk.Label.Gtk_Label;
         Bonus_Label : Gtk.Label.Gtk_Label;
      end record;

   type Ability_Change_Button is access all Ability_Change_Button_Record'Class;

   function New_Ability_Change_Button
     (Label : String;
      Ability     : Chaos.Abilities.Ability;
      Change      : Integer;
      Score_Label : Gtk.Label.Gtk_Label;
      Bonus_Label : Gtk.Label.Gtk_Label)
      return Gtk.Button.Gtk_Button;

   procedure On_Ability_Change_Click
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   type Root_Character_Gen_Model is
     new Root_Gtk_Model with
      record
         Step_Buttons      : Gtk.Container.Gtk_Container;
         Step_Screens      : Gtk.Container.Gtk_Container;
         Choose_Gender_Box : Gtk.Widget.Gtk_Widget;
         Creature          : Chaos.Creatures.Chaos_Creature;
         Gender            : Chaos.Genders.Chaos_Gender;
         Alignment         : Chaos.Alignment.Chaos_Alignment;
         Race              : Chaos.Races.Chaos_Race;
         Class             : Chaos.Classes.Chaos_Class;
         Abilities         : Ability_Widget_Arrays;
         Points_Left_Label : Gtk.Label.Gtk_Label;
         Points_Left       : Natural;
         Description_Text  : Gtk.Text_View.Gtk_Text_View;
         Character_Name    : Gtk.GEntry.Gtk_Entry;
         Current_Step      : Gtk.Button.Gtk_Button;
         Next_Button       : Gtk.Button.Gtk_Button;
      end record;

   overriding procedure Show (Model : in out Root_Character_Gen_Model);

   type Character_Gen_Model_Access is
     access all Root_Character_Gen_Model'Class;

   Local_Character_Gen_Model : Character_Gen_Model_Access;

   procedure Create_Model;

   procedure Choose (Tag : String);

   procedure Show_Step (Tag : String);

   procedure Create_Gender_Model;

   procedure Choose_Gender (Gender_Tag : String);

   procedure Create_Race_Model;

   procedure Choose_Race (Race_Tag : String);

   procedure Create_Class_Model;

   procedure Choose_Class (Class_Tag : String);

   procedure Create_Alignment_Model;

   procedure Choose_Alignment (Alignment_Tag : String);

   procedure Create_Name_Model;

   procedure Create_Abilities_Model;

   procedure Show_Abilities;

   procedure Choose_Ability_Change (Tag : String);
   pragma Unreferenced (Choose_Ability_Change);

   procedure On_Step_Choice
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Choice
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Next
     (Next_Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure Text_Name_Changed
     (Self   : access Gtk.GEntry.Gtk_Entry_Record'Class;
      String : Glib.UTF8_String);

   procedure Initially_Enabled_Step
     (Button : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Next_Step
     (Current : String);

   procedure Show_Current_Summary;

   procedure Finish_Model
     (Name : String);

   ------------
   -- Choose --
   ------------

   procedure Choose (Tag : String) is
   begin
      if Choice_Map.Contains (Tag) then
         Choice_Map.Element (Tag).Apply_Choice (Tag);
         Local_Character_Gen_Model.Description_Text.Get_Buffer.Set_Text
           (Localisation.Capitalised_Local_Text (Tag & "-description"));
      else
         raise Constraint_Error with
           "unknown choice: " & Tag;
      end if;
   end Choose;

   ---------------------------
   -- Choose_Ability_Change --
   ---------------------------

   procedure Choose_Ability_Change (Tag : String) is
      pragma Unreferenced (Tag);
   begin
      null;
   end Choose_Ability_Change;

   ----------------------
   -- Choose_Alignment --
   ----------------------

   procedure Choose_Alignment (Alignment_Tag : String) is
      Alignment : constant Chaos.Alignment.Chaos_Alignment :=
                    Chaos.Alignment.Chaos_Alignment'Value (Alignment_Tag);
   begin
      Local_Character_Gen_Model.Alignment := Alignment;
   end Choose_Alignment;

   ------------------
   -- Choose_Class --
   ------------------

   procedure Choose_Class (Class_Tag : String) is
      Class : constant Chaos.Classes.Chaos_Class :=
                Chaos.Classes.Get (Class_Tag);
   begin
      Local_Character_Gen_Model.Class := Class;
      Local_Character_Gen_Model.Creature :=
        Chaos.Creatures.Quick.Quick_Creature
          ("CHARNAME",
           Local_Character_Gen_Model.Race,
           Local_Character_Gen_Model.Class);
   end Choose_Class;

   -------------------
   -- Choose_Gender --
   -------------------

   procedure Choose_Gender (Gender_Tag : String) is
      Gender : constant Chaos.Genders.Chaos_Gender :=
                 Chaos.Genders.Chaos_Gender'Value (Gender_Tag);
   begin
      Local_Character_Gen_Model.Gender := Gender;
   end Choose_Gender;

   -----------------
   -- Choose_Race --
   -----------------

   procedure Choose_Race (Race_Tag : String) is
      Race : constant Chaos.Races.Chaos_Race :=
               Chaos.Races.Get (Race_Tag);
   begin

      Local_Character_Gen_Model.Race := Race;
      Show_Abilities;

   end Choose_Race;

   ----------------------------
   -- Create_Abilities_Model --
   ----------------------------

   procedure Create_Abilities_Model is
      use Glib;
      use Gtk.Label;
      Grid : constant Gtk.Grid.Gtk_Grid :=
               Gtk.Grid.Gtk_Grid
                 (UI_Definition.Get_Object ("Container_Abilities"));
      Points_Left_Label : constant Gtk_Label :=
                            Gtk_Label_New
                              (Capitalised_Local_Text
                                 ("ability-points-left"));
      Points_Left_Value : constant Gtk_Label :=
                            Gtk_Label_New;

      Row_Index : Gint := 0;
   begin
      Grid.Attach (Points_Left_Label, 0, 0, 3, 1);
      Grid.Attach (Points_Left_Value, 3, 0, 2, 1);

      for Ability in Chaos.Abilities.Ability loop
         declare
            Ability_Name : constant Gtk_Label :=
                             Gtk_Label_New
                               (Capitalised_Local_Text
                                  (Chaos.Abilities.Ability'Image (Ability)));
            Ability_Score : constant Gtk_Label :=
                              Gtk_Label_New;
            Ability_Bonus : constant Gtk_Label :=
                              Gtk_Label_New;
            Ability_Plus  : constant Gtk.Button.Gtk_Button :=
                              New_Ability_Change_Button
                                (Label       => "+",
                                 Ability     => Ability,
                                 Change      => 1,
                                 Score_Label => Ability_Score,
                                 Bonus_Label => Ability_Bonus);
            Ability_Minus  : constant Gtk.Button.Gtk_Button :=
                              New_Ability_Change_Button
                                (Label       => "-",
                                 Ability     => Ability,
                                 Change      => -1,
                                 Score_Label => Ability_Score,
                                 Bonus_Label => Ability_Bonus);
         begin
            Row_Index := Row_Index + 1;
            Grid.Attach (Ability_Name, 0, Row_Index, 1, 1);
            Grid.Attach (Ability_Score, 1, Row_Index, 1, 1);
            Grid.Attach (Ability_Bonus, 2, Row_Index, 1, 1);
            Grid.Attach (Ability_Plus, 3, Row_Index, 1, 1);
            Grid.Attach (Ability_Minus, 4, Row_Index, 1, 1);
            Local_Character_Gen_Model.Abilities (Ability) :=
              (8, Ability_Score, Ability_Bonus,
               Ability_Plus, Ability_Minus);
         end;
      end loop;

      Local_Character_Gen_Model.Points_Left_Label := Points_Left_Value;

      Init_Step_Button ("Abilities", "Name");

   end Create_Abilities_Model;

   ----------------------------
   -- Create_Alignment_Model --
   ----------------------------

   procedure Create_Alignment_Model is
      use type Gtk.Widget.Gtk_Widget;
      Box : constant Gtk.Box.Gtk_Box :=
              Gtk.Box.Gtk_Box
                (UI_Definition.Get_Object
                   ("Container_Alignment"));
   begin
      while Box.Get_Child (0) /= null loop
         Box.Remove (Box.Get_Child (0));
      end loop;

      for Alignment in Chaos.Alignment.Chaos_Alignment loop
         Box.Add
           (Create_Choice_Button
              (Chaos.Alignment.Chaos_Alignment'Image (Alignment),
               Choose_Alignment'Access));
      end loop;

      Init_Step_Button ("Alignment", "Abilities");

   end Create_Alignment_Model;

   --------------------------
   -- Create_Choice_Button --
   --------------------------

   function Create_Choice_Button
     (Tag       : String;
      On_Choose : Choice_Application_Procedure)
      return Gtk.Widget.Gtk_Widget
   is
      Button : constant Choice_Button := new Choice_Button_Record;
   begin
      Gtk.Button.Initialize (Button, Capitalised_Local_Text (Tag));
      Button.Tag := new String'(Tag);
      Choice_Map.Insert
        (Tag, (Gtk.Button.Gtk_Button (Button), null, On_Choose));
      Button.On_Clicked (On_Choice'Access);
      return Gtk.Widget.Gtk_Widget (Button);
   end Create_Choice_Button;

   ------------------------
   -- Create_Class_Model --
   ------------------------

   procedure Create_Class_Model is
      use type Gtk.Widget.Gtk_Widget;
      Box : constant Gtk.Box.Gtk_Box :=
              Gtk.Box.Gtk_Box
                (UI_Definition.Get_Object
                   ("Container_Class"));
   begin
      while Box.Get_Child (0) /= null loop
         Box.Remove (Box.Get_Child (0));
      end loop;

      declare
         procedure Add_Button (Class : Chaos.Classes.Chaos_Class);

         ----------------
         -- Add_Button --
         ----------------

         procedure Add_Button (Class : Chaos.Classes.Chaos_Class) is
            Button : constant Gtk.Widget.Gtk_Widget :=
                       Create_Choice_Button
                         (Class.Identifier,
                          Choose_Class'Access);
         begin
            Box.Add (Button);
         end Add_Button;

      begin
         Chaos.Classes.Scan (Add_Button'Access);

      end;

      Init_Step_Button ("Class", "Alignment");

   end Create_Class_Model;

   -------------------------
   -- Create_Gender_Model --
   -------------------------

   procedure Create_Gender_Model is
      use type Gtk.Widget.Gtk_Widget;
      Box : constant Gtk.Box.Gtk_Box :=
              Gtk.Box.Gtk_Box
                (UI_Definition.Get_Object
                   ("Container_Gender"));
   begin
      while Box.Get_Child (0) /= null loop
         Box.Remove (Box.Get_Child (0));
      end loop;

      Box.Add (Create_Choice_Button ("female", Choose_Gender'Access));
      Box.Add (Create_Choice_Button ("male", Choose_Gender'Access));

      Init_Step_Button ("Gender", "Race");
   end Create_Gender_Model;

   ------------------
   -- Create_Model --
   ------------------

   procedure Create_Model is
      Top         : constant Gtk.Widget.Gtk_Widget :=
                      Gtk.Widget.Gtk_Widget
                        (UI_Definition.Get_Object ("Character_Gen_Model"));
   begin

      Local_Character_Gen_Model := new Root_Character_Gen_Model;

      Create_Gender_Model;
      Create_Race_Model;
      Create_Class_Model;
      Create_Alignment_Model;
      Create_Abilities_Model;
      Create_Name_Model;

      Local_Character_Gen_Model.Step_Screens :=
        Gtk.Container.Gtk_Container
          (UI_Definition.Get_Object ("Container_Step_Screens"));
      Local_Character_Gen_Model.Step_Buttons :=
        Gtk.Container.Gtk_Container
          (UI_Definition.Get_Object ("Container_Step_Buttons"));
      Local_Character_Gen_Model.Description_Text :=
        Gtk.Text_View.Gtk_Text_View
          (UI_Definition.Get_Object ("Text_Current_Choice_Description"));

      declare
         Button : constant Gtk.Button.Gtk_Button :=
                    Gtk.Button.Gtk_Button
                      (UI_Definition.Get_Object ("Button_Next_Finish"));
      begin
         Button.On_Clicked (On_Next'Access);
         Local_Character_Gen_Model.Next_Button := Button;
      end;

      Local_Character_Gen_Model.Create_Model (Top);
   end Create_Model;

   -----------------------
   -- Create_Name_Model --
   -----------------------

   procedure Create_Name_Model is
      use type Gtk.Widget.Gtk_Widget;
      Name_Text : constant Gtk.GEntry.Gtk_Entry :=
                    Gtk.GEntry.Gtk_Entry
                      (UI_Definition.Get_Object
                         ("Text_Character_Name"));
   begin
      Name_Text.On_Insert_At_Cursor
        (Text_Name_Changed'Access);
      Local_Character_Gen_Model.Character_Name := Name_Text;

      Init_Step_Button ("Name", "");
   end Create_Name_Model;

   -----------------------
   -- Create_Race_Model --
   -----------------------

   procedure Create_Race_Model is
      use type Gtk.Widget.Gtk_Widget;
      Box : constant Gtk.Box.Gtk_Box :=
              Gtk.Box.Gtk_Box
                (UI_Definition.Get_Object
                   ("Container_Race"));
   begin
      while Box.Get_Child (0) /= null loop
         Box.Remove (Box.Get_Child (0));
      end loop;

      for Race of Chaos.Db.Race.Select_By_Tag loop
         declare
            Button : constant Gtk.Widget.Gtk_Widget :=
                       Create_Choice_Button
                         (Race.Tag, Choose_Race'Access);
         begin
            Box.Add (Button);
         end;
      end loop;

      Init_Step_Button ("Race", "Class");

   end Create_Race_Model;

   ------------------
   -- Finish_Model --
   ------------------

   procedure Finish_Model
     (Name : String)
   is
      Actor : Chaos.Db.Actor.Actor_Type :=
                Chaos.Db.Actor.Get (Local_Character_Gen_Model.Actor);
   begin
      Actor.Set_Tag ("CHARNAME");
      Chaos.Configure.World.Start
        (Actor, Name, 1);
      Current_UI.Show_Model
        (Chaos.UI.Gtk_UI.Main_Model.Model);
   end Finish_Model;

   ----------------------
   -- Init_Step_Button --
   ----------------------

   procedure Init_Step_Button
     (Step_Name      : String;
      Next_Step_Name : String)
   is
      Button : constant Gtk.Button.Gtk_Button :=
                 Gtk.Button.Gtk_Button
                   (UI_Definition.Get_Object ("Button_" & Step_Name));
      Next_Button : constant Gtk.Button.Gtk_Button :=
                 Gtk.Button.Gtk_Button
                   (UI_Definition.Get_Object ("Button_" & Next_Step_Name));
   begin
      Button.Set_Name (Step_Name);
      Button.On_Clicked (On_Step_Choice'Access);
      Choice_Map.Insert
        (Step_Name, (Button, Next_Button, Show_Step'Access));
   end Init_Step_Button;

   ----------------------------
   -- Initially_Enabled_Step --
   ----------------------------

   procedure Initially_Enabled_Step
     (Button : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      if Button.Get_Name = "Gender" then
         Button.Set_Sensitive (True);
      else
         Button.Set_Sensitive (All_Steps_On);
      end if;
   end Initially_Enabled_Step;

   -----------
   -- Model --
   -----------

   function Model return Gtk_UI_Model is
   begin
      if Local_Character_Gen_Model = null then
         Create_Model;
      end if;
      return Gtk_UI_Model (Local_Character_Gen_Model);
   end Model;

   -------------------------------
   -- New_Ability_Change_Button --
   -------------------------------

   function New_Ability_Change_Button
     (Label : String;
      Ability     : Chaos.Abilities.Ability;
      Change      : Integer;
      Score_Label : Gtk.Label.Gtk_Label;
      Bonus_Label : Gtk.Label.Gtk_Label)
      return Gtk.Button.Gtk_Button
   is
      Result : constant Ability_Change_Button :=
                 new Ability_Change_Button_Record;
   begin
      Result.Initialize (Label);
      Result.Ability := Ability;
      Result.Change := Change;
      Result.Score_Label := Score_Label;
      Result.Bonus_Label := Bonus_Label;
      Result.On_Clicked (On_Ability_Change_Click'Access);
      return Gtk.Button.Gtk_Button (Result);
   end New_Ability_Change_Button;

   ---------------
   -- Next_Step --
   ---------------

   procedure Next_Step
     (Current : String)
   is
      use type Gtk.Button.Gtk_Button;
   begin
      Local_Character_Gen_Model.Step_Screens.Foreach
        (Hide_Widget'Access);
      Choice_Map.Element (Current).State_Button.Set_Sensitive (False);

      if Choice_Map.Element (Current).Next_Button = null then
         Finish_Model
           (Local_Character_Gen_Model.Character_Name.Get_Text);
      else
         Choice_Map.Element (Current).Next_Button.Set_Sensitive (True);
         Local_Character_Gen_Model.Current_Step :=
           Choice_Map.Element (Current).Next_Button;
         Show_Current_Summary;
      end if;
   end Next_Step;

   -----------------------------
   -- On_Ability_Change_Click --
   -----------------------------

   procedure On_Ability_Change_Click
     (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Change_Button : constant Ability_Change_Button :=
                        Ability_Change_Button (Button);
      Actor : Chaos.Db.Actor.Actor_Type :=
                        Chaos.Db.Actor.Get (Local_Character_Gen_Model.Actor);
   begin
      declare
         Score : Chaos.Db.Ability_Score.Ability_Score_Type :=
                   Chaos.Db.Ability_Score.Get_By_Ability_Score
                     (Actor.Reference,
                      Change_Button.Ability);
      begin
         Local_Character_Gen_Model.Points_Left :=
           Local_Character_Gen_Model.Points_Left
             - Chaos.Abilities.Change_Cost (Score.Value, Change_Button.Change);
         Score.Set_Value (Score.Value + Change_Button.Change);
      end;
      Show_Abilities;
   end On_Ability_Change_Click;

   ---------------
   -- On_Choice --
   ---------------

   procedure On_Choice
     (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Choose (Choice_Button (Button).Tag.all);
   end On_Choice;

   -------------
   -- On_Next --
   -------------

   procedure On_Next
     (Next_Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Next_Button);
   begin
      Next_Step (Local_Character_Gen_Model.Current_Step.Get_Name);
   end On_Next;

   --------------------
   -- On_Step_Choice --
   --------------------

   procedure On_Step_Choice
     (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Button.Set_Sensitive (False);
      Local_Character_Gen_Model.Current_Step := Gtk.Button.Gtk_Button (Button);
      Choose (Button.Get_Name);
   end On_Step_Choice;

   ----------
   -- Show --
   ----------

   overriding procedure Show (Model : in out Root_Character_Gen_Model) is
   begin
      Model.Step_Screens.Foreach
        (Hide_Widget'Access);
      Model.Step_Buttons.Foreach
        (Initially_Enabled_Step'Access);
      Model.Actor := Create_Empty_Actor;
      Model.Points_Left := 27;
      Show_Abilities;
      Show_Current_Summary;
   end Show;

   --------------------
   -- Show_Abilities --
   --------------------

   procedure Show_Abilities is
   begin
      for Ability in Chaos.Abilities.Ability loop
         declare
            use Chaos.Abilities;
            Widgets : Ability_Widgets_Record renames
                        Local_Character_Gen_Model.Abilities (Ability);

            Score   : constant Ability_Score_Range :=
                        Local_Character_Gen_Model.Creature.Ability_Score
                          (Ability);
            Mod_Score : constant Positive :=
                          (if Actor.Race = Chaos.Db.Null_Race_Reference
                           then Score
                           else Score +
                             Chaos.Races.Ability_Bonus
                               (Actor.Race, Ability.Reference));
         begin
            Widgets.Score_Label.Set_Label
              (Positive'Image (Mod_Score));
            Widgets.Bonus_Label.Set_Label
              (Integer'Image (Chaos.Abilities.Ability_Bonus (Mod_Score)));
            Widgets.Plus_Button.Set_Sensitive
              (Score < 15
               and then Chaos.Abilities.Change_Cost (Score, 1)
               <= Local_Character_Gen_Model.Points_Left);
            Widgets.Minus_Button.Set_Sensitive (Score > 8);
         end;
      end loop;
      Local_Character_Gen_Model.Points_Left_Label.Set_Label
        (Natural'Image (Local_Character_Gen_Model.Points_Left));
   end Show_Abilities;

   --------------------------
   -- Show_Current_Summary --
   --------------------------

   procedure Show_Current_Summary is
      use type Gtk.Button.Gtk_Button;
      M : Character_Gen_Model_Access renames Local_Character_Gen_Model;

      ---------
      -- Get --
      ---------

      function Get (Tag : String) return String
                    renames Localisation.Capitalised_Local_Text;
      B : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
            M.Description_Text.Get_Buffer;

      Actor : Chaos.Db.Actor.Actor_Type := Chaos.Db.Actor.Get (M.Actor);

      procedure Add (Name_Tag : String;
                     Value_Tag : String);

      ---------
      -- Add --
      ---------

      procedure Add (Name_Tag : String;
                     Value_Tag : String)
      is
         Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      begin
         B.Get_End_Iter (Iter);
         B.Insert (Iter,
                   Get (Name_Tag) & ": " & Get (Value_Tag)
                   & Character'Val (10));
      end Add;

   begin

      if Local_Character_Gen_Model.Current_Step = null then
         B.Set_Text (Get ("character-creation-start"));
         return;
      end if;

      B.Set_Text ("");

      declare
         Name : constant String := M.Current_Step.Get_Name;
      begin
         Add ("gender", Chaos.Db.Gender_Type'Image (Actor.Gender));
         if Name = "Race" then
            return;
         end if;

         Add ("race", Chaos.Db.Race.Get (Actor.Race).Tag);

         if Name = "Class" then
            return;
         end if;

         Add ("class", Chaos.Db.Class.Get (Actor.Class).Tag);

         if Name = "Alignment" then
            return;
         end if;

         Add ("alignment", Chaos.Db.Alignment_Type'Image (Actor.Alignment));

         if Name = "Abilities" then
            return;
         end if;

      end;

   end Show_Current_Summary;

   ---------------
   -- Show_Step --
   ---------------

   procedure Show_Step (Tag : String) is
      Container_Tag : constant String :=
                        "Container_" & Tag;
      Container     : constant Gtk.Widget.Gtk_Widget :=
                        Gtk.Widget.Gtk_Widget
                          (UI_Definition.Get_Object
                             (Container_Tag));
   begin
      Container.Show_All;
      Local_Character_Gen_Model.Description_Text.Get_Buffer.Set_Text
        (Chaos.Localisation.Local_Text (Tag & "-description"));
   end Show_Step;

   -----------------------
   -- Text_Name_Changed --
   -----------------------

   procedure Text_Name_Changed
     (Self   : access Gtk.GEntry.Gtk_Entry_Record'Class;
      String : Glib.UTF8_String)
   is
      pragma Unreferenced (Self);
   begin
      Local_Character_Gen_Model.Next_Button.Set_Label
        (Localisation.Capitalised_Local_Text ("finish"));
      Local_Character_Gen_Model.Next_Button.Set_Sensitive
        (String /= "");
   end Text_Name_Changed;

end Chaos.UI.Gtk_UI.Character_Gen_Model;
