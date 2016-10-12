with Chaos.Expressions.Enumerated;

package body Chaos.Vision is

   package Vision_Expressions is
     new Chaos.Expressions.Enumerated (Chaos_Vision);

   Visibility : constant array (Chaos_Vision, Chaos_Vision) of Natural :=
                  (Normal => (Normal => 20, Low_Light => 10, Dark => 2),
                   Low_Light => (Normal => 20, Low_Light => 15, Dark => 2),
                   Dark      => (Normal => 20, Low_Light => 18, Dark => 15));

   ---------------
   -- Is_Vision --
   ---------------

   function Is_Vision
     (Expression : Lith.Objects.Object)
      return Boolean
   is
   begin
      return Vision_Expressions.Is_Enum (Expression);
   end Is_Vision;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression
     (Vision : Chaos_Vision)
      return Lith.Objects.Object
   is
   begin
      return Vision_Expressions.To_Object (Vision);
   end To_Expression;

   -------------------
   -- Visible_Range --
   -------------------

   function Visible_Range
     (Looker     : Chaos_Vision_Interface'Class;
      Conditions : Chaos_Vision)
      return Natural
   is
   begin
      return Visibility (Looker.Vision, Conditions);
   end Visible_Range;

end Chaos.Vision;
