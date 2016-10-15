with Ada.Calendar;
with Ada.Characters.Handling;

with WL.String_Maps;

with Lith.Environment;
with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.Objects;

with Chaos.Expressions.Maps;

with Chaos.Dice;

package body Chaos.Expressions.Primitives is

   package Timeout_Maps is new
     WL.String_Maps (Ada.Calendar.Time, Ada.Calendar."=");

   Timeouts : Timeout_Maps.Map;

   function Timeout_Key
     (Timeout_Target : Lith.Objects.Object;
      Timeout_Name   : Lith.Objects.Object)
      return String;

   function Evaluate_Chaos_Get
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Set
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Roll_Dice
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Set_Timer
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Timer_Expired
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   -----------------------
   -- Create_Primitives --
   -----------------------

   procedure Create_Primitives is
      use Lith.Objects.Interfaces;
   begin
      Define_Function ("chaos-set-property", 3, Evaluate_Chaos_Set'Access);
      Define_Function ("chaos-get-property", 2, Evaluate_Chaos_Get'Access);
      Define_Function ("chaos-roll-dice", 3, Evaluate_Roll_Dice'Access);
      Define_Function ("chaos-set-timer", 3, Evaluate_Set_Timer'Access);
      Define_Function ("chaos-timer-expired", 2,
                       Evaluate_Timer_Expired'Access);
   end Create_Primitives;

   ------------------------
   -- Evaluate_Chaos_Get --
   ------------------------

   function Evaluate_Chaos_Get
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects, Lith.Objects.Symbols;

      function Get
        (Map  : Object;
         Name : String)
         return Object;

      ---------
      -- Get --
      ---------

      function Get
        (Map  : Object;
         Name : String)
         return Object
      is
      begin
         if Chaos.Objects.Is_Object (Map) then
            return Chaos.Objects.To_Object (Map).Property (Name);
         elsif Chaos.Expressions.Maps.Is_Map (Map) then
            return Chaos.Expressions.Maps.Get (Map, Name);
         else
            raise Constraint_Error with
              "chaos-get-property: expected a map, but found "
              & Store.Show (Map);
         end if;
      end Get;

   begin
      if Store.Argument_Count = 1 then
         declare
            Full_Name : constant String :=
                          Get_Name (To_Symbol (Store.Argument (1)));
            Map_Name  : constant String :=
                          Ada.Characters.Handling.To_Lower
                            (Full_Name
                               (Full_Name'First .. Full_Name'First + 5));
            Prop_Name : constant String :=
                          Full_Name (Full_Name'First + 6 .. Full_Name'Last);
            Map       : constant Lith.Objects.Object :=
                          Lith.Environment.Get (Map_Name);
         begin
            return Get (Map, Prop_Name);
         end;
      else
         return Get (Store.Argument (1),
                     Lith.Objects.Symbols.Get_Name
                       (Lith.Objects.To_Symbol (Store.Argument (2))));
      end if;
   end Evaluate_Chaos_Get;

   ------------------------
   -- Evaluate_Chaos_Set --
   ------------------------

   function Evaluate_Chaos_Set
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Map : constant Lith.Objects.Object :=
              Store.Argument (1);
      Name : constant String :=
               Lith.Objects.Symbols.Get_Name
                 (Lith.Objects.To_Symbol (Store.Argument (2)));
      Value : constant Lith.Objects.Object :=
                Store.Argument (3);
   begin
      if not Chaos.Expressions.Maps.Is_Map (Map) then
         raise Constraint_Error with
           "chaos-setg-property!: expected a map, but found "
           & Store.Show (Map);
      end if;
      Chaos.Expressions.Maps.Set
        (Map, Name, Value);
      return Value;
   end Evaluate_Chaos_Set;

   ------------------------
   -- Evaluate_Roll_Dice --
   ------------------------

   function Evaluate_Roll_Dice
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Count : constant Natural := To_Integer (Store.Argument (1));
      Die   : constant Natural := To_Integer (Store.Argument (2));
      Plus  : constant Integer := To_Integer (Store.Argument (3));
      Dice : constant Chaos.Dice.Die_Roll :=
                Chaos.Dice.Create (Count, Die, Plus);
   begin
      return To_Object (Chaos.Dice.Roll (Dice));
   end Evaluate_Roll_Dice;

   ------------------------
   -- Evaluate_Set_Timer --
   ------------------------

   function Evaluate_Set_Timer
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use type Ada.Calendar.Time;
      Target   : constant Lith.Objects.Object := Store.Argument (1);
      Timer_Id : constant Lith.Objects.Object := Store.Argument (2);
      Timeout  : constant Natural :=
                   Lith.Objects.To_Integer (Store.Argument (3));
      Key      : constant String :=
                   Timeout_Key (Target, Timer_Id);
   begin
      if Timeouts.Contains (Key) then
         Timeouts.Delete (Key);
      end if;

      Timeouts.Insert
        (Key,
         Ada.Calendar.Clock + Duration (Timeout));
      return Timer_Id;
   end Evaluate_Set_Timer;

   ----------------------------
   -- Evaluate_Timer_Expired --
   ----------------------------

   function Evaluate_Timer_Expired
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use type Ada.Calendar.Time;
      Target   : constant Lith.Objects.Object := Store.Argument (1);
      Timer_Id : constant Lith.Objects.Object := Store.Argument (2);
      Key      : constant String :=
                   Timeout_Key (Target, Timer_Id);
   begin
      if not Timeouts.Contains (Key) then
         return Lith.Objects.True_Value;
      elsif Ada.Calendar.Clock >= Timeouts.Element (Key) then
         Timeouts.Delete (Key);
         return Lith.Objects.True_Value;
      else
         return Lith.Objects.False_Value;
      end if;
   end Evaluate_Timer_Expired;

   -----------------
   -- Timeout_Key --
   -----------------

   function Timeout_Key
     (Timeout_Target : Lith.Objects.Object;
      Timeout_Name   : Lith.Objects.Object)
      return String
   is
   begin
      if Chaos.Objects.Is_Object (Timeout_Target) then
         declare
            Obj : constant Chaos.Objects.Chaos_Object :=
                    Chaos.Objects.To_Object (Timeout_Target);
         begin
            return Obj.Global_Setting_Name (Store.Show (Timeout_Name));
         end;
      else
         return Store.Show (Timeout_Target)
           & "-" & Store.Show (Timeout_Name);
      end if;
   end Timeout_Key;

end Chaos.Expressions.Primitives;
