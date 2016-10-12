with Lith.Objects;

generic
   type Enum is (<>);
package Chaos.Expressions.Enumerated is

   function To_Object (Value : Enum) return Lith.Objects.Object;
   function Is_Enum (Value : Lith.Objects.Object) return Boolean;
   function To_Enum (Value : Lith.Objects.Object) return Enum
     with Pre => Is_Enum (Value);

end Chaos.Expressions.Enumerated;
