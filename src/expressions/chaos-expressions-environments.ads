package Chaos.Expressions.Environments is

   function Standard_Environment return Chaos_Environment;
   function New_Environment return Chaos_Environment;

   function Global
     (Name : String)
      return Chaos_Expression;

   procedure Set_Global
     (Name  : String;
      Value : Chaos_Expression);

   function Global_Object return Chaos_Expression;

   procedure Add_Standard_Value
     (Name  : String;
      Value : Chaos_Expression);

end Chaos.Expressions.Environments;
