with Lith.Objects;

package Chaos.Parser is

   procedure Load_Configuration
     (Path       : String;
      On_Setting : not null access
        procedure (Name  : String;
                   Value : Lith.Objects.Object));

   procedure Load_Directory
     (Path      : String;
      Extension : String;
      Loader    : not null access
        procedure (Path : String));

   function Load_Script
     (Path  : String)
      return Lith.Objects.Object;

   procedure Parse_Expression
     (Text  : String);

   function Parse_Expression
     (Text  : String)
      return Lith.Objects.Object;

   function Parse_Trigger (Text : String) return Lith.Objects.Object;

end Chaos.Parser;
