with Chaos.Expressions;

package Chaos.Vision is

   type Chaos_Vision is (Normal, Low_Light, Dark);

   function To_Expression
     (Vision : Chaos_Vision)
      return Chaos.Expressions.Chaos_Expression;

   function Is_Vision
     (Expression : Chaos.Expressions.Chaos_Expression)
      return Boolean;

   type Chaos_Vision_Interface is limited interface;

   function Vision (Item : Chaos_Vision_Interface) return Chaos_Vision
                    is abstract;

   function Visible_Range
     (Looker     : Chaos_Vision_Interface'Class;
      Conditions : Chaos_Vision)
      return Natural;

   function Vision
     (Item : Chaos_Vision_Interface'class)
      return Chaos.Expressions.Chaos_Expression
   is (To_Expression (Item.Vision));

end Chaos.Vision;
