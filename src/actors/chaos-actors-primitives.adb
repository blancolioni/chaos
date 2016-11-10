with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.Identifiers;

with Chaos.Actors.Visibility;

with Chaos.Game;

package body Chaos.Actors.Primitives is

   function Evaluate_Chaos_Can_See
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Start_Dialog
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Set_Allegiance
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Is_Creature (Value : Lith.Objects.Object) return Boolean
   is (Chaos.Objects.Is_Object (Value)
       and then Chaos.Objects.To_Object (Value).all in
         Chaos.Creatures.Chaos_Creature_Record'Class);

   --------------------
   -- Add_Primitives --
   --------------------

   procedure Add_Primitives is
      use Lith.Objects.Interfaces;

      Actor_Type : constant Function_Argument_Type :=
                     Custom_Argument (Is_Creature'Access);
      Integer_Or_Symol : constant Function_Argument_Type :=
                           Integer_Argument or Symbol_Argument;
   begin
      Define_Function ("chaos-can-see",
                       Evaluate_Chaos_Can_See'Access);
      Define_Function ("chaos-start-dialog",
                       Evaluate_Chaos_Start_Dialog'Access);
      Define_Function ("chaos-set-allegiance",
                       (Actor_Type, Integer_Or_Symol),
                       Evaluate_Chaos_Set_Allegiance'Access);
   end Add_Primitives;

   ----------------------------
   -- Evaluate_Chaos_Can_See --
   ----------------------------

   function Evaluate_Chaos_Can_See
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use type Lith.Objects.Object;
      use type Chaos.Objects.Chaos_Object;
      Actor : constant Chaos.Actors.Chaos_Actor :=
                Get_Actor (Store.Argument (1));
      Target : constant Chaos.Objects.Chaos_Object :=
                 (if Store.Argument (2) = Lith.Objects.Nil
                  then null
                  else Chaos.Objects.To_Object (Store.Argument (2)));
      Can_See : constant Boolean :=
                  (if Target = null then False
                   else Chaos.Actors.Visibility.Can_See (Actor, Target));
   begin
      return Lith.Objects.To_Object (Can_See);
   end Evaluate_Chaos_Can_See;

   -----------------------------------
   -- Evaluate_Chaos_Set_Allegiance --
   -----------------------------------

   function Evaluate_Chaos_Set_Allegiance
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Creature : constant Chaos.Creatures.Chaos_Creature :=
                   Chaos.Creatures.Chaos_Creature
                     (Chaos.Objects.To_Object (Store.Argument (1)));
      New_Value : Natural;

      procedure Set_Allegiance
        (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class);

      --------------------
      -- Set_Allegiance --
      --------------------

      procedure Set_Allegiance
        (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class)
      is
      begin
         Creature.Set_Enemy_Ally (New_Value);
      end Set_Allegiance;

   begin
      if Is_Symbol (Store.Argument (2)) then
         New_Value :=
           Chaos.Identifiers.Value
             ("ea", Symbols.Get_Name (To_Symbol (Store.Argument (2))));
      else
         New_Value := To_Integer (Store.Argument (2));
      end if;
      Creature.Update (Set_Allegiance'Access);
      return No_Value;
   end Evaluate_Chaos_Set_Allegiance;

   ---------------------------------
   -- Evaluate_Chaos_Start_Dialog --
   ---------------------------------

   function Evaluate_Chaos_Start_Dialog
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Actor  : constant Chaos.Actors.Chaos_Actor :=
                 Get_Actor (Store.Argument (1));
      Target : constant Chaos.Actors.Chaos_Actor :=
                 Get_Actor (Store.Argument (2));
   begin
      Chaos.Game.Current_Game.Start_Dialog (Actor, Target);
      return Lith.Objects.Nil;
   end Evaluate_Chaos_Start_Dialog;

end Chaos.Actors.Primitives;
