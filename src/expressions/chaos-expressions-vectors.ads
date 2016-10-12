with Lith.Objects;

package Chaos.Expressions.Vectors is

   function Create
     return Lith.Objects.Object;

   function Is_Vector
     (Value : Lith.Objects.Object)
      return Boolean;

   procedure Append
     (To_Vector : Lith.Objects.Object;
      Value     : Lith.Objects.Object);

   function Length
     (Vector    : Lith.Objects.Object)
      return Natural;

   function Get
     (Vector : Lith.Objects.Object;
      Index     : Positive)
      return Lith.Objects.Object
     with Pre => Index <= Length (Vector);

   procedure Set
     (Vector    : Lith.Objects.Object;
      Index     : Positive;
      Value     : Lith.Objects.Object)
     with Pre => Index <= Length (Vector),
     Post => Lith.Objects."=" (Get (Vector, Index), Value);

end Chaos.Expressions.Vectors;
