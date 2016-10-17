with Ada.Directories;

with Lith.Objects;

with Chaos.Logging;
with Chaos.Paths;
with Chaos.Settings;

with Chaos.Parser;
with Chaos.Expressions;

with Chaos.Expressions.Maps;
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
      Value : Lith.Objects.Object);

   procedure Set_Defences
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Healing_Surges
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Identity
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Key_Abilities
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Level_Hit_Points
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Power_Source
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Role
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object);

   ------------------
   -- Create_Class --
   ------------------

   procedure Create_Class
     (Path : String)
   is
      procedure Create (Class : in out Chaos_Class_Record'Class) is null;

      procedure Configure
        (Class : in out Chaos_Class_Record'Class);

      ------------
      -- Create --
      ------------

      procedure Configure
        (Class : in out Chaos_Class_Record'Class)
      is

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

      end Configure;

      New_Class : constant Chaos_Class :=
                    Db.Create (Create'Access);
   begin
      New_Class.Save_Object;
      Db.Update (New_Class.Reference, Configure'Access);
      New_Class.Define_Object;
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
      Class_Settings.Setting ("healing-surges-per-day",
                              Set_Healing_Surges'Access);
      Class_Settings.Setting ("identity", Set_Identity'Access);
      Class_Settings.Setting ("hit-points-per-level",
                              Set_Level_Hit_Points'Access);
      Class_Settings.Setting ("power-source", Set_Power_Source'Access);
      Class_Settings.Setting ("role", Set_Role'Access);

      Chaos.Parser.Load_Directory
        (Chaos.Paths.Config_File ("classes"), "class",
         Create_Class'Access);

   end Read_Config;

   -------------------------
   -- Set_Base_Hit_Points --
   -------------------------

   procedure Set_Base_Hit_Points
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object)
   is
   begin
      Class.Base_Hit_Points := Lith.Objects.To_Integer (Value);
   end Set_Base_Hit_Points;

   ------------------
   -- Set_Defences --
   ------------------

   procedure Set_Defences
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object)
   is
      use Chaos.Expressions.Maps;
   begin
      for D in Chaos.Defences.Defence loop
         declare
            X : constant Lith.Objects.Object :=
                  Get (Value, D'Img);
         begin
            if Lith.Objects.Is_Integer (X) then
               Class.Defences (D) :=
                 Chaos.Defences.Defence_Score_Change
                   (Lith.Objects.To_Integer (X));
            end if;
         end;
      end loop;
   end Set_Defences;

   ------------------------
   -- Set_Healing_Surges --
   ------------------------

   procedure Set_Healing_Surges
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object)
   is
   begin
      Class.Healing_Surges := Lith.Objects.To_Integer (Value);
   end Set_Healing_Surges;

   ------------------
   -- Set_Identity --
   ------------------

   procedure Set_Identity
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object)
   is
   begin
      Class.Initialize
        (Chaos.Expressions.Store.Show (Value));
      Chaos.Logging.Log
        ("CONFIG", "new class: " & Class.Identifier);
   end Set_Identity;

   -----------------------
   -- Set_Key_Abilities --
   -----------------------

   procedure Set_Key_Abilities
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object)
   is
      Is_Set : array (Chaos.Abilities.Ability) of Boolean :=
                 (others => False);
      Index  : Natural := 0;
   begin
      for I in 1 .. Chaos.Expressions.Vectors.Length (Value) loop
         declare
            Img : constant String :=
                    Chaos.Expressions.Store.Show
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
      Value : Lith.Objects.Object)
   is
   begin
      Class.Level_Hit_Points :=
        Lith.Objects.To_Integer (Value);
   end Set_Level_Hit_Points;

   ----------------------
   -- Set_Power_Source --
   ----------------------

   procedure Set_Power_Source
     (Class : in out Chaos_Class_Record'Class;
      Value : Lith.Objects.Object)
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
      Value : Lith.Objects.Object)
   is
   begin
      Class.Role :=
        Role_Expressions.To_Enum (Value);
   end Set_Role;

end Chaos.Classes.Configure;
