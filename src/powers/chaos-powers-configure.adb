with WL.String_Maps;

with Lith.Objects.Symbols;

with Chaos.Logging;
with Chaos.Paths;

with Chaos.Parser;

with Chaos.Powers.Db;

with Chaos.Actions;

with Chaos.Expressions.Maps;

package body Chaos.Powers.Configure is

   type Configure_Power_Handler is access
     procedure (Power : in out Chaos_Power_Record'Class;
                Value : Lith.Objects.Object);

   package Configure_Power_Maps is
     new WL.String_Maps (Configure_Power_Handler);

   Configure_Map : Configure_Power_Maps.Map;

   procedure Setting
     (Name    : String;
      Handler : Configure_Power_Handler);

   procedure Set_Attack
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Action
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Defence
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Effects
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Hit
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Hit_Effect
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Identity
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Implement
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Miss
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Miss_Effect
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Source
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Target
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Target_Size
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   procedure Set_Use
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object);

   ----------------
   -- Load_Power --
   ----------------

   function Load_Power
     (Path : String)
      return Chaos_Power
   is
      procedure Create
        (Power : in out Chaos_Power_Record'Class) is null;

      procedure Configure
        (Power : in out Chaos_Power_Record'Class);

      ---------------
      -- Configure --
      ---------------

      procedure Configure
        (Power : in out Chaos_Power_Record'Class)
      is

         procedure Set_Value
           (Name  : String;
            Value : Lith.Objects.Object);

         ---------------
         -- Set_Value --
         ---------------

         procedure Set_Value
           (Name  : String;
            Value : Lith.Objects.Object)
         is
         begin
            if Configure_Map.Contains (Name) then
               Configure_Map.Element (Name) (Power, Value);
            else
               Chaos.Logging.Log
                 ("CONFIG", "unknown power setting: " & Name
                  & " = " & Chaos.Expressions.Store.Show (Value));
            end if;
         end Set_Value;

      begin
         Chaos.Parser.Load_Configuration
           (Path, Set_Value'Access);
      end Configure;

      New_Power : constant Chaos_Power :=
                    Db.Create (Create'Access);
   begin
      New_Power.Save_Object;
      Db.Update (New_Power.Reference, Configure'Access);
      return New_Power;
   end Load_Power;

   -----------------
   -- Read_Config --
   -----------------

   procedure Read_Config is

   begin
      Setting ("action", Set_Action'Access);
      Setting ("attack", Set_Attack'Access);
      Setting ("defence", Set_Defence'Access);
      Setting ("effect", Set_Effects'Access);
      Setting ("hit-effect", Set_Hit'Access);
      Setting ("hit", Set_Hit'Access);
      Setting ("hit-damage", Set_Hit_Effect'Access);
      Setting ("miss-effect", Set_Miss'Access);
      Setting ("miss", Set_Miss'Access);
      Setting ("miss-damage", Set_Miss_Effect'Access);
      Setting ("identity", Set_Identity'Access);
      Setting ("implement", Set_Implement'Access);
      Setting ("source", Set_Source'Access);
      Setting ("target", Set_Target'Access);
      Setting ("target-size", Set_Target_Size'Access);
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
      Value : Lith.Objects.Object)
   is
   begin
      Power.Action := Chaos.Actions.To_Action (Value);
   end Set_Action;

   ----------------
   -- Set_Attack --
   ----------------

   procedure Set_Attack
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
   is
   begin
      Chaos.Expressions.Store.Define_Top_Level
        (Lith.Objects.Symbols.Get_Symbol (Power.Attack_Setting), Value);
   end Set_Attack;

   -----------------
   -- Set_Defence --
   -----------------

   procedure Set_Defence
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
   is
   begin
      Chaos.Expressions.Store.Define_Top_Level
        (Lith.Objects.Symbols.Get_Symbol (Power.Defence_Setting), Value);
   end Set_Defence;

   -----------------
   -- Set_Effects --
   -----------------

   procedure Set_Effects
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
   is
   begin
      Power.Effects := Value;
   end Set_Effects;

   -------------
   -- Set_Hit --
   -------------

   procedure Set_Hit
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
   is
      use Chaos.Expressions, Chaos.Expressions.Maps;
   begin
      Power.Log ("hit: " & Store.Show (Value));
      if Is_Map (Value) then
         declare
            use type Lith.Objects.Object;
            Damage : constant Lith.Objects.Object :=
                       Get (Value, "damage");
            Effects : constant Lith.Objects.Object :=
                        Get (Value, "effect");
         begin
            if Effects /= Lith.Objects.Undefined then
               Power.Hit_Effects := Effects;
            end if;
            if Damage /= Lith.Objects.Undefined then
               Set_Hit_Effect (Power, Damage);
            end if;
         end;
      end if;
   end Set_Hit;

   --------------------
   -- Set_Hit_Damage --
   --------------------

   procedure Set_Hit_Effect
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
   is
      use Chaos.Expressions;
   begin
      Store.Push (Value);
      Store.Push (Power.Hit_Effects);
      Store.Cons;
      Power.Hit_Effects := Store.Pop;
   end Set_Hit_Effect;

   ------------------
   -- Set_Identity --
   ------------------

   procedure Set_Identity
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
   is
   begin
      Power.Initialize (Chaos.Expressions.Store.Show (Value));
      Chaos.Logging.Log
        ("CONFIG", "new power: " & Power.Identifier);
   end Set_Identity;

   -------------------
   -- Set_Implement --
   -------------------

   procedure Set_Implement
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
   is
   begin
      Power.Implement := Power_Implement_Expressions.To_Enum (Value);
   end Set_Implement;

   --------------
   -- Set_Miss --
   --------------

   procedure Set_Miss
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
   is
      use Chaos.Expressions, Chaos.Expressions.Maps;
   begin
      Power.Log ("miss: " & Store.Show (Value));
      if Is_Map (Value) then
         declare
            use type Lith.Objects.Object;
            Damage  : constant Lith.Objects.Object :=
                        Get (Value, "damage");
            Effects : constant Lith.Objects.Object :=
                        Get (Value, "effect");
         begin
            if Effects /= Lith.Objects.Undefined then
               Power.Miss_Effects := Effects;
            end if;
            if Damage /= Lith.Objects.Undefined then
               Set_Miss_Effect (Power, Damage);
            end if;
         end;
      end if;
   end Set_Miss;

   ---------------------
   -- Set_Miss_Damage --
   ---------------------

   procedure Set_Miss_Effect
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
   is
      use Chaos.Expressions;
   begin
      Store.Push (Value);
      Store.Push (Power.Miss_Effects);
      Store.Cons;
      Power.Miss_Effects := Store.Pop;
   end Set_Miss_Effect;

   ----------------
   -- Set_Source --
   ----------------

   procedure Set_Source
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
   is
   begin
      Power.Source := Power_Source_Expressions.To_Enum (Value);
   end Set_Source;

   ----------------
   -- Set_Target --
   ----------------

   procedure Set_Target
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
   is
   begin
      Power.Target := Power_Target_Expressions.To_Enum (Value);
   end Set_Target;

   ---------------------
   -- Set_Target_Size --
   ---------------------

   procedure Set_Target_Size
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
   is
   begin
      Power.Target_Size := Lith.Objects.To_Integer (Value);
   end Set_Target_Size;

   -------------
   -- Set_Use --
   -------------

   procedure Set_Use
     (Power : in out Chaos_Power_Record'Class;
      Value : Lith.Objects.Object)
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
