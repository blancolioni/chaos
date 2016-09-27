with Ada.Strings.Fixed;

with Chaos.Db.Ability;
with Chaos.Db.Ability_Score;
with Chaos.Db.Actor;

with Chaos.UI.Elements.Buttons;
with Chaos.UI.Elements.Containers;
with Chaos.UI.Elements.Labels;
with Chaos.UI.Elements.Tables;

package body Chaos.UI.Models.Character_Gen is

   use Chaos.UI.Elements;

   type Creation_Step is
     (Choose_Gender,
      Choose_Race,
      Choose_Class,
      Choose_Scores);

   type Step_Button is
     new Chaos.UI.Elements.Buttons.Root_UI_Button with
      record
         Step    : Creation_Step;
         Step_UI : UI_Element;
      end record;

   function Create_Step
     (Step      : Creation_Step;
      Label     : String;
      Configure : access Root_UI_Element'Class)
      return UI_Element;

   type Array_Of_Step_Buttons is
     array (Creation_Step) of UI_Element;

   type Choose_Gender_Button is
     new Chaos.UI.Elements.Buttons.Root_UI_Button with
      record
         Gender : Chaos.Db.Gender_Type;
      end record;

   function Create_Gender_Button
     (Gender    : Chaos.Db.Gender_Type;
      Label     : String)
      return UI_Element;

   type Ability_Change_Button is
     new Chaos.UI.Elements.Buttons.Root_UI_Button with
      record
         Ability : Chaos.Db.Ability_Reference;
         Change  : Integer;
         Label   : Chaos.UI.Elements.UI_Element;
      end record;

   type Root_Character_Gen_Model is new Root_UI_Model with
      record
         Stages_Box : Chaos.UI.Elements.UI_Element;
         Steps      : Array_Of_Step_Buttons;
         Actor      : Chaos.Db.Actor_Reference;
      end record;

   overriding function Top_Element
     (Model : Root_Character_Gen_Model)
      return Chaos.UI.Elements.UI_Element
   is (Model.Stages_Box);

   Have_Model : Boolean := False;
   Single_Character_Gen_Model : aliased Root_Character_Gen_Model;

   procedure Create_Model;

   procedure Create_Step_Buttons
     (Gender_UI : access Root_UI_Element'Class;
      Race_UI   : access Root_UI_Element'Class;
      Class_UI  : access Root_UI_Element'Class;
      Scores_UI : access Root_UI_Element'Class);

   procedure On_Step_Clicked
     (Button : not null access Buttons.Root_UI_Button'Class);

   procedure On_Ability_Change
     (Button : not null access Chaos.UI.Elements.Buttons.Root_UI_Button'Class);

   -------------------------
   -- Character_Gen_Model --
   -------------------------

   function Character_Gen_Model return UI_Model is
      use Chaos.Db;
      Index : Positive := 1;
      Actor : Chaos.Db.Actor_Reference :=
                Chaos.Db.Actor.Get_Reference_By_Tag
                  ("PC" & Integer'Image (-Index));
   begin
      if not Have_Model then
         Create_Model;
      end if;

      loop
         Actor :=
           Chaos.Db.Actor.Get_Reference_By_Tag
             ("PC" & Integer'Image (-Index));
         exit when Actor = Null_Actor_Reference;
         Index := Index + 1;
      end loop;

      declare
         Actor_Rec : Chaos.Db.Actor.Actor_Type :=
                       Chaos.Db.Actor.Create;
      begin
         Actor_Rec.Set_Tag ("PC" & Integer'Image (-Index));
         Actor := Actor_Rec.Reference;
         for Ability of Chaos.Db.Ability.Select_By_Tag loop
            Chaos.Db.Ability_Score.Create
              (Actor_Rec.Reference, Ability.Reference, 10);
         end loop;
      end;

      Single_Character_Gen_Model.Actor := Actor;

      return Single_Character_Gen_Model'Access;
   end Character_Gen_Model;

   --------------------------
   -- Create_Gender_Button --
   --------------------------

   function Create_Gender_Button
     (Gender    : Chaos.Db.Gender_Type;
      Label     : String)
      return UI_Element
   is
      Button : Choose_Gender_Button;
      Result : UI_Element;
   begin
      Button.New_Button
        (Chaos.Db.Gender_Type'Image (Gender) & "_CHOOSE",
         Label, null);
      Result := new Choose_Gender_Button'(Button);
      return Result;
   end Create_Gender_Button;

   ------------------
   -- Create_Model --
   ------------------

   procedure Create_Model is
      use Chaos.UI.Elements.Containers;
      use Chaos.UI.Elements.Tables;
      M : Root_Character_Gen_Model renames Single_Character_Gen_Model;
      Top_Box : constant UI_Container :=
                  New_Container ("character-gen-top-box");
      Main_Table : constant UI_Table :=
                     New_Table ("main-table");
      Gender_Table  : constant UI_Table :=
                        New_Table ("gender-table");
      Ability_Table : constant UI_Table :=
                        New_Table ("ability-table");
      Step_Table    : constant UI_Table :=
                        New_Table ("step-button-table");
   begin

      Top_Box.Add (Main_Table);

      Gender_Table.Set_Visible (False);
      declare
         Row  : UI_Table_Row;
         Cell : UI_Table_Cell;
      begin
         Row  := Gender_Table.New_Row;
         Cell := Row.New_Cell;
         Cell.Add (Create_Gender_Button (Chaos.Db.Female, "Female"));
         Row  := Gender_Table.New_Row;
         Cell := Row.New_Cell;
         Cell.Add (Create_Gender_Button (Chaos.Db.Male, "Male"));
      end;

      Ability_Table.Set_Visible (False);
      for Ability of Chaos.Db.Ability.Select_By_Top_Record loop
         declare
            use Chaos.UI.Elements.Labels;
            use Chaos.UI.Elements.Buttons;
            Row : constant UI_Table_Row := Ability_Table.New_Row;
            Ability_Label : constant UI_Element :=
                             New_Label ("ability-label-" & Ability.Tag,
                                        Ability.Tag);
            Ability_Score : constant UI_Element :=
                             New_Label ("ability-score-" & Ability.Tag,
                                        "10");
            Ability_Up, Ability_Down : Ability_Change_Button;
            Cell                     : UI_Table_Cell;

         begin
            Ability_Up.New_Button ("ability-up-" & Ability.Tag,
                                   "+",
                                   On_Ability_Change'Access);
            Ability_Up.Ability := Ability.Reference;
            Ability_Up.Change := 1;
            Ability_Up.Label := Ability_Score;

            Ability_Down.New_Button ("ability-down-" & Ability.Tag,
                                     "-",
                                     On_Ability_Change'Access);
            Ability_Down.Ability := Ability.Reference;
            Ability_Down.Change := -1;
            Ability_Down.Label := Ability_Score;

            Cell := Row.New_Cell;
            Cell.Add (Ability_Label);

            Cell := Row.New_Cell;
            Cell.Add (Ability_Score);

            declare
               Up : constant Chaos.UI.Elements.UI_Element :=
                      new Ability_Change_Button'(Ability_Up);
               Down : constant Chaos.UI.Elements.UI_Element :=
                        new Ability_Change_Button'(Ability_Down);
            begin
               Cell := Row.New_Cell;
               Cell.Add (Up);

               Cell := Row.New_Cell;
               Cell.Add (Down);
            end;

         end;
      end loop;

      Create_Step_Buttons
        (Gender_UI => Gender_Table,
         Race_UI   => null,
         Class_UI  => null,
         Scores_UI => Ability_Table);

      for B of M.Steps loop
         declare
            Row  : constant UI_Table_Row := Step_Table.New_Row;
            Cell : constant UI_Table_Cell := Row.New_Cell;
         begin
            Cell.Add (B);
         end;
      end loop;

      M.Steps (M.Steps'First).Set_Enabled (True);

      declare
         Row  : constant UI_Table_Row := Main_Table.New_Row;
         Cell : UI_Table_Cell;
      begin
         Cell := Row.New_Cell;
         Cell.Add (Step_Table);
         Cell := Row.New_Cell;
         Cell.Add (Gender_Table);
         Cell.Add (Ability_Table);
      end;

      M.Init_Model ("character-gen-model");
      M.Stages_Box := Chaos.UI.Elements.UI_Element (Top_Box);
      --           Single_Character_Gen_Model :=
      --             new Root_Character_Gen_Model'(M);
      Top_Box.Set_Parent (Single_Character_Gen_Model'Access);
      Single_Character_Gen_Model.Load_Style_Rules;
      Single_Character_Gen_Model.Load_Style_Rules ("hover");
      Single_Character_Gen_Model.Load_Style_Rules ("active");

      Have_Model := True;

   end Create_Model;

   -----------------
   -- Create_Step --
   -----------------

   function Create_Step
     (Step      : Creation_Step;
      Label     : String;
      Configure : access Root_UI_Element'Class)
      return UI_Element
   is
      Result : Step_Button;
   begin
      Result.New_Button
        (Creation_Step'Image (Step) & "-BUTTON",
         Label, On_Step_Clicked'Access);
      Result.Step_UI := UI_Element (Configure);
      Result.Set_Enabled (False);
      return new Step_Button'(Result);
   end Create_Step;

   -------------------------
   -- Create_Step_Buttons --
   -------------------------

   procedure Create_Step_Buttons
     (Gender_UI : access Root_UI_Element'Class;
      Race_UI   : access Root_UI_Element'Class;
      Class_UI  : access Root_UI_Element'Class;
      Scores_UI : access Root_UI_Element'Class)
   is
      M : Root_Character_Gen_Model renames Single_Character_Gen_Model;
   begin
      M.Steps (Choose_Gender) :=
        Create_Step (Choose_Gender, "Gender", Gender_UI);
      M.Steps (Choose_Race) :=
        Create_Step (Choose_Gender, "Race", Race_UI);
      M.Steps (Choose_Class) :=
        Create_Step (Choose_Gender, "Class", Class_UI);
      M.Steps (Choose_Scores) :=
        Create_Step (Choose_Gender, "Abilities", Scores_UI);
   end Create_Step_Buttons;

   -----------------------
   -- On_Ability_Change --
   -----------------------

   procedure On_Ability_Change
     (Button : not null access Chaos.UI.Elements.Buttons.Root_UI_Button'Class)
   is
      Change_Button : Ability_Change_Button renames
                        Ability_Change_Button (Button.all);
      Actor         : Chaos.Db.Actor.Actor_Type :=
                        Chaos.Db.Actor.Get (Single_Character_Gen_Model.Actor);
      Score         : Chaos.Db.Ability_Score.Ability_Score_Type :=
                        Chaos.Db.Ability_Score.Get_By_Ability_Score
                          (Actor.Reference, Change_Button.Ability);
   begin
      Score.Set_Value
        (Score.Value + Change_Button.Change);
      Change_Button.Label.Set_Text
        (Ada.Strings.Fixed.Trim
           (Integer'Image (Score.Value),
            Ada.Strings.Left));
   end On_Ability_Change;

   ---------------------
   -- On_Step_Clicked --
   ---------------------

   procedure On_Step_Clicked
     (Button : not null access Buttons.Root_UI_Button'Class)
   is
      Step : Step_Button renames Step_Button (Button.all);
   begin
      Step.Step_UI.Set_Visible (True);
   end On_Step_Clicked;

end Chaos.UI.Models.Character_Gen;
