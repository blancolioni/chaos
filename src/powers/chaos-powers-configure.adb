with WL.String_Maps;

with Chaos.Logging;
with Chaos.Paths;

with Chaos.Parser;

with Chaos.Powers.Db;

with Chaos.Actions;

with Chaos.Expressions.Enumerated;

package body Chaos.Powers.Configure is

   package Power_Source_Expressions is
     new Chaos.Expressions.Enumerated (Power_Source);

   package Power_Use_Expressions is
     new Chaos.Expressions.Enumerated (Power_Use_Class);

   type Configure_Power_Handler is access
     procedure (Power : in out Chaos_Power_Record'Class;
                Value : Chaos.Expressions.Chaos_Expression);

   package Configure_Power_Maps is
     new WL.String_Maps (Configure_Power_Handler);

   Configure_Map : Configure_Power_Maps.Map;

   procedure Setting
     (Name    : String;
      Handler : Configure_Power_Handler);

   procedure Set_Attack
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Action
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Defence
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Hit_Damage
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Identity
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Miss_Damage
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Source
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   procedure Set_Use
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression);

   ----------------
   -- Load_Power --
   ----------------

   function Load_Power
     (Path : String)
      return Chaos_Power
   is
      procedure Create
        (Power : in out Chaos_Power_Record'Class);

      ------------
      -- Create --
      ------------

      procedure Create
        (Power : in out Chaos_Power_Record'Class)
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
               Configure_Map.Element (Name) (Power, Value);
            else
               Chaos.Logging.Log
                 ("CONFIG", "unknown power setting: " & Name
                  & " = " & Chaos.Expressions.To_String (Value));
            end if;
         end Set_Value;

      begin
         Chaos.Parser.Load_Configuration
           (Path, Set_Value'Access);
      end Create;

   begin
      return Chaos.Powers.Db.Create (Create'Access);
   end Load_Power;

   -----------------
   -- Read_Config --
   -----------------

   procedure Read_Config is

   begin
      Setting ("action", Set_Action'Access);
      Setting ("attack", Set_Attack'Access);
      Setting ("defence", Set_Defence'Access);
      Setting ("hit-damage", Set_Hit_Damage'Access);
      Setting ("miss-damage", Set_Miss_Damage'Access);
      Setting ("identity", Set_Identity'Access);
      Setting ("source", Set_Source'Access);
      Setting ("use", Set_Use'Access);

      declare
         Basic_Melee_Attack : constant Chaos_Power :=
                                Load_Power
                                  (Chaos.Paths.Config_File
                                     ("basic-melee-attack.txt"));
         pragma Unreferenced (Basic_Melee_Attack);
      begin
         null;
      end;

   end Read_Config;

   ----------------
   -- Set_Action --
   ----------------

   procedure Set_Action
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Power.Action := Chaos.Actions.To_Action (Value);
   end Set_Action;

   ----------------
   -- Set_Attack --
   ----------------

   procedure Set_Attack
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Power.Attack := Value;
   end Set_Attack;

   -----------------
   -- Set_Defence --
   -----------------

   procedure Set_Defence
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Power.Defence := Value;
   end Set_Defence;

   --------------------
   -- Set_Hit_Damage --
   --------------------

   procedure Set_Hit_Damage
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Power.Log ("hit: " & Chaos.Expressions.To_String (Value));
      Power.Hit.Append ((Chaos.Expressions.Always, Value));
   end Set_Hit_Damage;

   ------------------
   -- Set_Identity --
   ------------------

   procedure Set_Identity
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Power.Initialize (Chaos.Expressions.To_String (Value));
      Chaos.Logging.Log
        ("CONFIG", "new power: " & Power.Identifier);
   end Set_Identity;

   ---------------------
   -- Set_Miss_Damage --
   ---------------------

   procedure Set_Miss_Damage
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Power.Log ("miss: " & Chaos.Expressions.To_String (Value));
      Power.Miss.Append ((Chaos.Expressions.Always, Value));
   end Set_Miss_Damage;

   ----------------
   -- Set_Source --
   ----------------

   procedure Set_Source
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Power.Source := Power_Source_Expressions.To_Enum (Value);
   end Set_Source;

   -------------
   -- Set_Use --
   -------------

   procedure Set_Use
     (Power : in out Chaos_Power_Record'Class;
      Value : Chaos.Expressions.Chaos_Expression)
   is
   begin
      Power.Use_Class := Power_Use_Expressions.To_Enum (Value);
   end Set_Use;

   -------------
   -- Setting --
   -------------

   procedure Setting (Name    : String;
                      Handler : Configure_Power_Handler)
   is
   begin
      Configure_Map.Insert (Name, Handler);
   end Setting;

end Chaos.Powers.Configure;
