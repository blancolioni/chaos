with Lith.Objects;

package Chaos.Parser is

   procedure Load_Configuration
     (Path       : String;
      Store      : in out Lith.Objects.Object_Store'Class;
      On_Setting : not null access
        procedure (Name  : String;
                   Store : in out Lith.Objects.Object_Store'Class;
                   Value : Lith.Objects.Object));

   procedure Load_Directory
     (Path      : String;
      Extension : String;
      Loader    : not null access
        procedure (Path : String));

   function Load_Script
     (Path  : String;
      Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   procedure Parse_Expression
     (Text  : String;
      Store : in out Lith.Objects.Object_Store'Class);

   function Parse_Expression
     (Text  : String;
      Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

end Chaos.Parser;
