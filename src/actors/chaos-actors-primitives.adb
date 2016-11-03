with Lith.Objects.Interfaces;

with Chaos.Actors.Visibility;

with Chaos.Game;

package body Chaos.Actors.Primitives is

   function Evaluate_Chaos_Can_See
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Start_Dialog
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   --------------------
   -- Add_Primitives --
   --------------------

   procedure Add_Primitives is
      use Lith.Objects.Interfaces;
   begin
      Define_Function ("chaos-can-see", 2,
                       Evaluate_Chaos_Can_See'Access);
      Define_Function ("chaos-start-dialog", 2,
                       Evaluate_Chaos_Start_Dialog'Access);
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
