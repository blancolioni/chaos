with Lith.Objects;

package Chaos.Vision is

   type Chaos_Vision is (Normal, Low_Light, Dark);

   function To_Expression
     (Vision : Chaos_Vision)
      return Lith.Objects.Object;

   function Is_Vision
     (Expression : Lith.Objects.Object)
      return Boolean;

   type Chaos_Vision_Interface is limited interface;

   function Vision (Entity : Chaos_Vision_Interface) return Chaos_Vision
                    is abstract;

   function Visible_Range
     (Looker     : Chaos_Vision_Interface'Class;
      Conditions : Chaos_Vision)
      return Natural;

   function Vision
     (Entity : Chaos_Vision_Interface'class)
      return Lith.Objects.Object
   is (To_Expression (Entity.Vision));

end Chaos.Vision;
