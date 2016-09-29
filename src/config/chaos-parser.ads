with Chaos.Expressions;

package Chaos.Parser is

   procedure Load_Configuration
     (Path : String;
      On_Setting : not null access
        procedure (Name  : String;
                   Value : Chaos.Expressions.Chaos_Expression));

   procedure Load_Directory
     (Path      : String;
      Extension : String;
      Loader    : not null access
        procedure (Path : String));

   function Load_Script
     (Path : String)
      return Chaos.Expressions.Chaos_Expression;

   function Parse_Expression
     (Text : String)
      return Chaos.Expressions.Chaos_Expression;

end Chaos.Parser;
