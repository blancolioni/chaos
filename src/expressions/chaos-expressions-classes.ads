generic
   type Class_Data_Type is private;
   with function To_String (Class : Class_Data_Type) return String;
   with function Get_Environment (Class : Class_Data_Type)
            return Chaos_Environment;
package Chaos.Expressions.Classes is

   function To_Expression (Value : Class_Data_Type) return Chaos_Expression;
   function Is_Class (Expression : Chaos_Expression) return Boolean;
   function To_Class (Expression : Chaos_Expression) return Class_Data_Type;
   procedure Update_Class
     (Expression : Chaos_Expression;
      Update     : not null access
        procedure (Value : in out Class_Data_Type))
     with Pre => Is_Class (Expression);

end Chaos.Expressions.Classes;
