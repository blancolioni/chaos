with WL.String_Maps;

with Chaos.Logging;
with Chaos.Paths;

with Chaos.Parser;

with Chaos.Items.Weapons.Db;

with Chaos.Expressions.Enumerated;
with Chaos.Expressions.Numbers;
with Chaos.Expressions.Vectors;

package body Chaos.Items.Weapons.Configure is

   package Weapon_Category_Expressions is
     new Chaos.Expressions.Enumerated (Weapon_Category);

   package Weapon_Group_Expressions is
     new Chaos.Expressions.Enumerated (Weapon_Group);

   package Weapon_Property_Expressions is
     new Chaos.Expressions.Enumerated (Weapon_Property);

   type Configure_Weapon_Handler is access
     procedure (Weapon : in out Chaos_Weapon_Record'Class;
                Value : Chaos.Expressions.Chaos_Expression);

   package Configure_Weapon_Maps is
     new WL.String_Maps (Configure_Weapon_Handler);

   Configure_Map : Configure_Weapon_Maps.Map;

   procedure Create_Weapon (Path : String);

   procedure Setting
     (Name    : String;
      Handler : Configure_Weapon_Handler);

   procedure Set_Identity
     (Item   : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression);

   procedure Set_Price
     (Item   : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression);

   procedure Set_Weight
     (Item   : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression);

   procedure Set_Category
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Damage
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression);

   procedure Set_Group
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Proficiency_Bonus
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression);

   procedure Set_Properties
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression);

   procedure Set_Range
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression);

   procedure Set_Two_Handed
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression);

   ----------------
   -- Load_Weapon --
   ----------------

   procedure Create_Weapon
     (Path : String)
   is
      procedure Create
        (Weapon : in out Chaos_Weapon_Record'Class);

      ------------
      -- Create --
      ------------

      procedure Create
        (Weapon : in out Chaos_Weapon_Record'Class)
      is

         procedure Set_Value
           (Name : String;
            Value : Chaos.Expressions.Chaos_Expression);

         ---------------
         -- Set_Value --
         ---------------

         procedure Set_Value
           (Name  : String;
            Value : Chaos.Expressions.Chaos_Expression)
         is
         begin
            if Configure_Map.Contains (Name) then
               Configure_Map.Element (Name) (Weapon, Value);
            else
               Chaos.Logging.Log
                 ("CONFIG", "unknown Weapon setting: " & Name
                  & " = " & Chaos.Expressions.To_String (Value));
            end if;
         end Set_Value;

      begin
         Chaos.Parser.Load_Configuration
           (Path, Set_Value'Access);
      end Create;

   begin
      Chaos.Items.Weapons.Db.Create (Create'Access);
   end Create_Weapon;

   -----------------
   -- Read_Config --
   -----------------

   procedure Read_Config is

   begin
      Setting ("identity", Set_Identity'Access);
      Setting ("price", Set_Price'Access);
      Setting ("weight", Set_Weight'Access);

      Setting ("category", Set_Category'Access);
      Setting ("damage", Set_Damage'Access);
      Setting ("group", Set_Group'Access);
      Setting ("proficiency", Set_Proficiency_Bonus'Access);
      Setting ("properties", Set_Properties'Access);
      Setting ("range", Set_Range'Access);
      Setting ("two-handed", Set_Two_Handed'Access);

      Chaos.Parser.Load_Directory
        (Chaos.Paths.Config_File ("items/weapons"),
         "weapon",
         Create_Weapon'Access);
   end Read_Config;

   ------------------
   -- Set_Category --
   ------------------

   procedure Set_Category
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Weapon.Category := Weapon_Category_Expressions.To_Enum (Value);
   end Set_Category;

   ----------------
   -- Set_Damage --
   ----------------

   procedure Set_Damage
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Weapon.Damage :=
        Chaos.Dice.Parse_Die_Roll
          (Chaos.Expressions.To_String (Value));
   end Set_Damage;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Weapon.Group := Weapon_Group_Expressions.To_Enum (Value);
   end Set_Group;

   ------------------
   -- Set_Identity --
   ------------------

   procedure Set_Identity
     (Item   : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Item.Initialize
        (Chaos.Expressions.To_String (Value));
      Item.Log ("new weapon: " & Item.Identifier);
   end Set_Identity;

   ---------------
   -- Set_Price --
   ---------------

   procedure Set_Price
     (Item   : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Item.Price :=
        Chaos.Coins.To_Coins
          (Chaos.Expressions.To_String (Value));
   end Set_Price;

   ---------------------------
   -- Set_Proficiency_Bonus --
   ---------------------------

   procedure Set_Proficiency_Bonus
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Weapon.Proficiency := Chaos.Expressions.Numbers.To_Integer (Value);
   end Set_Proficiency_Bonus;

   --------------------
   -- Set_Properties --
   --------------------

   procedure Set_Properties
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression)
   is
   begin
      for I in 1 .. Chaos.Expressions.Vectors.Length (Value) loop
         declare
            Property : constant Weapon_Property :=
                         Weapon_Property_Expressions.To_Enum
                           (Chaos.Expressions.Vectors.Get (Value, I));
         begin
            Weapon.Properties (Property) := True;
         end;
      end loop;
   end Set_Properties;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression)
   is
      R : constant String :=
            Chaos.Expressions.To_String (Value);
   begin
      for I in R'Range loop
         if R (I) = '-' then
            Weapon.Short_Range :=
              Natural'Value (R (R'First .. I - 1));
            Weapon.Long_Range :=
              Natural'Value (R (I + 1 .. R'Last));
            return;
         end if;
      end loop;
      Weapon.Log ("warning: invalid range setting: " & R);
   exception
      when Constraint_Error =>
         Weapon.Log ("warning: invalid range setting: " & R);
   end Set_Range;

   --------------------
   -- Set_Two_Handed --
   --------------------

   procedure Set_Two_Handed
     (Weapon : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression)
   is
      pragma Unreferenced (Value);
   begin
      Weapon.Two_Handed := True;
   end Set_Two_Handed;

   ----------------
   -- Set_Weight --
   ----------------

   procedure Set_Weight
     (Item   : in out Chaos_Weapon_Record'Class;
      Value  : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Item.Weight :=
        Chaos.Weight.Chaos_Weight'Value
          (Chaos.Expressions.To_String (Value));
   end Set_Weight;

   -------------
   -- Setting --
   -------------

   procedure Setting (Name    : String;
                      Handler : Configure_Weapon_Handler)
   is
   begin
      Configure_Map.Insert (Name, Handler);
   end Setting;

end Chaos.Items.Weapons.Configure;
