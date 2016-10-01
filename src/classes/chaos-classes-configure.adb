with Ada.Directories;

with Chaos.Logging;
with Chaos.Paths;
with Chaos.Settings;

with Chaos.Parser;
with Chaos.Expressions;

with Chaos.Expressions.Environments;
with Chaos.Expressions.Maps;
with Chaos.Expressions.Numbers;
with Chaos.Expressions.Vectors;

with Chaos.Powers.Configure;

with Chaos.Classes.Db;

package body Chaos.Classes.Configure is

   package Class_Settings is
     new Chaos.Settings (Chaos_Class_Record);

   procedure Create_Class
     (Path : String);

   procedure Set_Base_Hit_Points
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Defences
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Identity
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Key_Abilities
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Level_Hit_Points
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Power_Source
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Role
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   ------------------
   -- Create_Class --
   ------------------

   procedure Create_Class
     (Path : String)
   is
      procedure Create (Class : in out Chaos_Class_Record'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Class : in out Chaos_Class_Record'Class) is
         use Ada.Directories;

         procedure Add_Power (Path : String);

         ---------------
         -- Add_Power --
         ---------------

         procedure Add_Power (Path : String) is
         begin
            Class.Add_Power (Chaos.Powers.Configure.Load_Power (Path));
         end Add_Power;

      begin
         Class_Settings.Load_Object
           (Class, Path);

         declare
            Code : constant String := Class.Identifier;
         begin
            Class.Animation_Code := Code (Code'First);
         end;

         declare
            Power_Directory : constant String :=
                                Compose
                                  (Containing_Directory (Path),
                                   Class.Identifier & "-powers");
         begin
            if Exists (Power_Directory) then
               Chaos.Parser.Load_Directory
                 (Power_Directory, "power", Add_Power'Access);
            else
               Chaos.Logging.Log
                 ("CLASS", Class.Identifier & " has no powers");
            end if;
         end;

      end Create;

   begin
      Db.Create (Create'Access);
   end Create_Class;

   -----------------
   -- Read_Config --
   -----------------

   procedure Read_Config is
   begin
      Class_Settings.Setting ("abilities",
                              Set_Key_Abilities'Access);
      Class_Settings.Setting ("base-hit-points", Set_Base_Hit_Points'Access);
      Class_Settings.Setting ("defences", Set_Defences'Access);
      Class_Settings.Setting ("identity", Set_Identity'Access);
      Class_Settings.Setting ("hit-points-per-level",
                              Set_Level_Hit_Points'Access);
      Class_Settings.Setting ("power-source", Set_Power_Source'Access);
      Class_Settings.Setting ("role", Set_Role'Access);

      Chaos.Parser.Load_Directory
        (Chaos.Paths.Config_File ("classes"), "class",
         Create_Class'Access);

      declare
         Class_Map : constant Chaos.Expressions.Chaos_Expression :=
                       Chaos.Expressions.Maps.Map_Expression;

         procedure Add_Class (Class : Chaos_Class);

         ---------------
         -- Add_Class --
         ---------------

         procedure Add_Class (Class : Chaos_Class) is
         begin
            Chaos.Expressions.Maps.Set
              (Class_Map, Class.Identifier, Class.To_Expression);
         end Add_Class;

      begin
         Db.Scan (Add_Class'Access);

         Chaos.Expressions.Environments.Add_Standard_Value
           ("classes", Class_Map);
      end;

   end Read_Config;

   -------------------------
   -- Set_Base_Hit_Points --
   -------------------------

   procedure Set_Base_Hit_Points
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Class.Base_Hit_Points := Chaos.Expressions.Numbers.To_Integer (Value);
   end Set_Base_Hit_Points;

   ------------------
   -- Set_Defences --
   ------------------

   procedure Set_Defences
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
      use Chaos.Expressions.Maps;
      use Chaos.Expressions.Numbers;
   begin
      for D in Chaos.Defences.Defence loop
         declare
            X : constant Chaos.Expressions.Chaos_Expression :=
                  Get (Value, D'Img);
         begin
            if Is_Number (X) then
               Class.Defences (D) :=
                 Chaos.Defences.Defence_Score_Change
                   (To_Integer (X));
            end if;
         end;
      end loop;
   end Set_Defences;

   ------------------
   -- Set_Identity --
   ------------------

   procedure Set_Identity
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Class.Initialize (Chaos.Expressions.To_String (Value));
      Chaos.Logging.Log
        ("CONFIG", "new class: " & Class.Identifier);
   end Set_Identity;

   procedure Set_Key_Abilities
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
      Is_Set : array (Chaos.Abilities.Ability) of Boolean :=
                 (others => False);
      Index  : Natural := 0;
   begin
      for I in 1 .. Chaos.Expressions.Vectors.Length (Value) loop
         declare
            Img : constant String :=
                    Chaos.Expressions.To_String
                      (Chaos.Expressions.Vectors.Get (Value, I));
            A   : Chaos.Abilities.Ability;
         begin
            A := Chaos.Abilities.Ability'Value (Img);
            if Is_Set (A) then
               Chaos.Logging.Log
                 ("CONFIG",
                  "warning: key ability " & Img
                  & " appears more than once");
            else
               Index := Index + 1;
               Class.Key_Abilities (Index) := A;
               Is_Set (A) := True;
            end if;
         exception
            when Constraint_Error =>
               Chaos.Logging.Log
                 ("CONFIG",
                  "error: expected an ability but found " & Img);
         end;
      end loop;

      while Index < 6 loop
         for I in Is_Set'Range loop
            if not Is_Set (I) then
               Index := Index + 1;
               Class.Key_Abilities (Index) := I;
               Is_Set (I) := True;
               exit;
            end if;
         end loop;
      end loop;

   end Set_Key_Abilities;

   --------------------------
   -- Set_Level_Hit_Points --
   --------------------------

   procedure Set_Level_Hit_Points
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Class.Level_Hit_Points :=
        Chaos.Expressions.Numbers.To_Integer (Value);
   end Set_Level_Hit_Points;

   ----------------------
   -- Set_Power_Source --
   ----------------------

   procedure Set_Power_Source
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Class.Power_Source :=
        Power_Source_Expressions.To_Enum (Value);
   end Set_Power_Source;

   --------------
   -- Set_Role --
   --------------

   procedure Set_Role
     (Class : in out Chaos_Class_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Class.Role :=
        Role_Expressions.To_Enum (Value);
   end Set_Role;

end Chaos.Classes.Configure;
