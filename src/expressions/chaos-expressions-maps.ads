with Lith.Objects;

package Chaos.Expressions.Maps is

   function Create return Lith.Objects.Object;

   function Is_Map
     (Value : Lith.Objects.Object)
      return Boolean;

   function Contains
     (Map    : Lith.Objects.Object;
      Key    : String)
      return Boolean
     with Pre => Is_Map (Map);

   function Get
     (Map    : Lith.Objects.Object;
      Key    : String)
      return Lith.Objects.Object
     with Pre => Is_Map (Map);

   procedure Set
     (Map : Lith.Objects.Object;
      Key    : String;
      Value  : Lith.Objects.Object)
     with Pre => Is_Map (Map),
     Post => Lith.Objects."=" (Get (Map, Key), Value);

end Chaos.Expressions.Maps;
