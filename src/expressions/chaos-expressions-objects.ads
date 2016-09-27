generic
   type Object_Data_Type is private;
   with function To_String (Object : Object_Data_Type) return String;
   with procedure Create_Virtual_Table (VT : in out Chaos_Environment);
   --   with function "=" (Left, Right : Value_Type) return Boolean is <>;
package Chaos.Expressions.Objects is

   function To_Expression (Value : Object_Data_Type) return Chaos_Expression;
   function Is_Object (Expression : Chaos_Expression) return Boolean;
   function To_Object (Expression : Chaos_Expression) return Object_Data_Type;
   procedure Update_Object
     (Expression : Chaos_Expression;
      Update     : not null access
        procedure (Value : in out Object_Data_Type))
     with Pre => Is_Object (Expression);

end Chaos.Expressions.Objects;
