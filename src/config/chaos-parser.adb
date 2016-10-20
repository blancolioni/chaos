with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;

with Lith.Objects.Symbols;

with Chaos.Parser.Tokens;              use Chaos.Parser.Tokens;
with Chaos.Parser.Lexical;             use Chaos.Parser.Lexical;

with Chaos.Dice;

with Chaos.Expressions.Maps;
with Chaos.Expressions.Vectors;

with Chaos.Expressions.Import.Actions;
with Chaos.Expressions.Import.Triggers;

with Chaos.Logging;

package body Chaos.Parser is

   use Set_Of_Tokens;

   type Precedence_Level is range 1 .. 9;

   type Precedence_Table is array (Token) of Precedence_Level;
   type Name_Table is array (Token) of String (1 .. 6);

   Operator_Precedence : constant Precedence_Table :=
                 (Tok_Asterisk => 2, Tok_Forward_Slash => 2,
                  Tok_Plus     => 3, Tok_Minus => 3,
                  Tok_Less     => 4, Tok_Less_Equal => 4,
                  Tok_Greater  => 4, Tok_Greater_Equal => 4,
                  Tok_Equal    => 5, Tok_Not_Equal => 5,
                  Tok_And      => 6, Tok_Or => 7,
                  Tok_Arrow    => 8,
                  others       => 9);

   Operator_Name : constant Name_Table :=
                     (Tok_Asterisk => "*     ",
                      Tok_Forward_Slash => "/     ",
                      Tok_Plus          => "+     ",
                      Tok_Minus         => "-     ",
                      Tok_Less          => "<     ",
                      Tok_Less_Equal    => "<=    ",
                      Tok_Greater       => ">     ",
                      Tok_Greater_Equal => ">=    ",
                      Tok_Equal         => "eq?   ",
                      Tok_Not_Equal     => "neq?  ",
                      Tok_And           => "and   ",
                      Tok_Or            => "or    ",
                      others            => "      ");

   procedure Parse_Atomic_Expression;

   procedure Parse_Lambda_Expression;

   procedure Parse_Expression;

   procedure Parse_Operator_Expression
     (Precedence : Precedence_Level);

   procedure Parse_Function_Call_Expression;

   function Import_Inline_Function_Call
     return Chaos.Expressions.Import.Function_Call;

   function At_Expression return Boolean
   is (Tok = Tok_Left_Paren or else Tok = Tok_Left_Brace
       or else Tok = Tok_Left_Bracket or else Tok = Tok_Identifier
       or else Tok = Tok_Lambda or else Tok = Tok_If
       or else Tok = Tok_Quote);

   function At_Operator return Boolean is
     (Tok <= +(Tok_Asterisk, Tok_Forward_Slash, Tok_Plus, Tok_Minus,
               Tok_Less, Tok_Less_Equal, Tok_Greater, Tok_Greater_Equal,
               Tok_Equal, Tok_Not_Equal, Tok_Arrow,
               Tok_And, Tok_Or));

   function Is_Number
     (Text : String)
      return Boolean;

   ---------------------------------
   -- Import_Inline_Function_Call --
   ---------------------------------

   function Import_Inline_Function_Call
     return Chaos.Expressions.Import.Function_Call
   is
      use Chaos.Expressions.Import;
      Call : Function_Call;
      Negated : Boolean := False;
   begin
      if Tok = Tok_Exclam then
         Scan;
         Negated := True;
      end if;

      Create_Function_Call (Call, Tok_Text, Negated);
      Scan;

      if Tok = Tok_Left_Paren then
         Scan;
      else
         Error ("missing '('");
         Skip_To (Tok_Identifier);
         return Call;
      end if;

      while Tok /= Tok_Right_Paren loop
         if Tok = Tok_Identifier then
            if Is_Number (Tok_Text) then
               Add_Integer_Argument (Call, Integer'Value (Tok_Text));
            else
               Add_Identifier_Argument (Call, Tok_Text);
            end if;
            Scan;
            if Tok = Tok_Left_Paren then
               Scan;
               if Tok = Tok_Right_Paren then
                  Scan;
               else
                  Warning ("function calls not implemented");
                  Skip_To (Tok_Right_Paren);
                  Scan;
               end if;
            end if;
         elsif Tok = Tok_String_Constant then
            Add_String_Argument (Call, Tok_Text);
            Scan;
         elsif Tok = Tok_Left_Bracket then
            Scan;
            declare
               X, Y : Integer := 0;
            begin
               if Tok = Tok_Identifier and then Is_Number (Tok_Text) then
                  X := Integer'Value (Tok_Text);
                  Scan;
               else
                  Error ("missing X coordinate");
                  Skip_To
                    (Tok_Dot, Tok_Right_Bracket, Tok_Comma,
                     Tok_Right_Paren);
               end if;
               if Tok = Tok_Dot then
                  Scan;
               else
                  Error ("missing '.'");
               end if;
               if Tok = Tok_Identifier and then Is_Number (Tok_Text) then
                  Y := Integer'Value (Tok_Text);
                  Scan;
               else
                  Error ("missing Y coordinate");
                  Skip_To
                    (Tok_Right_Bracket, Tok_Comma,
                     Tok_Right_Paren, Tok_Identifier);
               end if;
               if Tok = Tok_Right_Bracket then
                  Scan;
               else
                  Error ("missing ']'");
                  Skip_To (Tok_Right_Bracket, Tok_Right_Paren, Tok_Comma);
               end if;
               Add_Coordinate_Argument (Call, X, Y);
            end;
         else
            Error ("unexpected '" & Tok_Text & "'");
            Skip_To (Tok_Identifier, Tok_Right_Paren);
            if Tok = Tok_Right_Paren then
               Scan;
            end if;
            return Call;
         end if;

         if Tok = Tok_Comma then
            Scan;
         elsif Tok /= Tok_Right_Paren then
            if Tok = Tok_Identifier
              or else Tok = Tok_String_Constant
              or else Tok = Tok_Left_Bracket
            then
               Warning ("inserted ','");
            else
               Error ("syntax error");
               Skip_To (Tok_Identifier, Tok_Right_Paren);
            end if;
         end if;
      end loop;

      if Tok = Tok_Right_Paren then
         Scan;
      else
         Error ("missing ')'");
      end if;

      return Call;
   end Import_Inline_Function_Call;

   ---------------
   -- Is_Number --
   ---------------

   function Is_Number
     (Text : String)
      return Boolean
   is
      use Ada.Characters.Handling;
      Start : Boolean := True;
   begin
      for I in Text'Range loop
         declare
            Ch : constant Character := Text (I);
         begin
            if Ch = '-' or else Ch = '+' then
               if not Start then
                  return False;
               end if;
            elsif not Is_Digit (Ch) then
               return False;
            end if;
         end;
         Start := False;
      end loop;
      return True;
   end Is_Number;

   ------------------------
   -- Load_Configuration --
   ------------------------

   procedure Load_Configuration
     (Path       : String;
      On_Setting : not null access
        procedure (Name  : String;
                   Value : Lith.Objects.Object))
   is
      use Chaos.Expressions;
   begin
      Open (Path);
      while Tok /= Tok_End_Of_File loop
         if Tok = Tok_Identifier then
            declare
               Name  : constant String := Tok_Text;
            begin
               Scan;
               if Tok = Tok_Colon then
                  Scan;
                  Parse_Expression;
                  if Tok = Tok_Semicolon then
                     Scan;
                  end if;
                  On_Setting (Name, Store.Pop);
               else
                  Error ("missing ':'");
                  while Tok /= Tok_End_Of_File
                    and then Next_Tok /= Tok_Colon
                  loop
                     Scan;
                  end loop;
               end if;
            end;
         else
            Error ("missing identifier at " & Token'Image (Tok));
            while Tok /= Tok_End_Of_File
              and then Next_Tok /= Tok_Colon
            loop
               Scan;
            end loop;
         end if;
      end loop;
      Close;
   end Load_Configuration;

   --------------------
   -- Load_Directory --
   --------------------

   procedure Load_Directory
     (Path      : String;
      Extension : String;
      Loader    : not null access
        procedure (Path : String))
   is
      Ordinary_File : Ada.Directories.File_Kind renames
                        Ada.Directories.Ordinary_File;

      procedure Process
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      -------------
      -- Process --
      -------------

      procedure Process
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
      begin
         Loader (Ada.Directories.Full_Name (Directory_Entry));
      end Process;

   begin
      Ada.Directories.Search
        (Directory => Path,
         Pattern   => "*." & Extension,
         Filter    => (Ordinary_File => True, others => False),
         Process   => Process'Access);
   end Load_Directory;

   -----------------
   -- Load_Script --
   -----------------

   function Load_Script
     (Path  : String)
      return Lith.Objects.Object
   is
      use Chaos.Expressions;
      Count : Natural := 0;
   begin
      Open (Path);
      Store.Push (Lith.Objects.Symbols.Begin_Symbol);
      while Tok /= Tok_End_Of_File loop
         Parse_Expression;
         Count := Count + 1;
      end loop;

      Store.Create_List (Count + 1);

      return Result : constant Lith.Objects.Object := Store.Pop do
         Close;
      end return;

   end Load_Script;

   ------------------
   -- Parse_Action --
   ------------------

   function Parse_Action (Text : String) return Lith.Objects.Object is
      Clean_Text : String (1 .. Text'Length);
      Length     : Natural := 0;
      Count      : Positive := 1;
   begin
      for Ch of Text loop
         if Character'Pos (Ch) not in 32 .. 126 then
            null;
         else
            Length := Length + 1;
            Clean_Text (Length) := Ch;
         end if;
      end loop;

      Open_String (Clean_Text (1 .. Length));

      Chaos.Expressions.Store.Push
        (Lith.Objects.Symbols.Get_Symbol ("begin"));

      while Tok = Tok_Identifier loop
         declare
            Call : constant Chaos.Expressions.Import.Function_Call :=
                     Import_Inline_Function_Call;
         begin
            Chaos.Logging.Log
              ("ACTION", Clean_Text (1 .. Length));
            Chaos.Expressions.Import.Actions.Import_Action (Call);
         end;

         Count := Count + 1;
      end loop;

      Close;

      if Count = 1 then
         Chaos.Expressions.Store.Drop;
         Chaos.Expressions.Store.Push (Lith.Objects.False_Value);
      elsif Count = 2 then
         Chaos.Expressions.Store.Swap;
         Chaos.Expressions.Store.Drop;
      else
         Chaos.Expressions.Store.Create_List (Count);
      end if;

      return Chaos.Expressions.Store.Pop;
   end Parse_Action;

   -----------------------------
   -- Parse_Atomic_Expression --
   -----------------------------

   procedure Parse_Atomic_Expression is
      use Chaos.Expressions;
   begin
      if Tok_Line > 0 then
         Store.Set_Context (Tok_File_Name, Tok_Line - 1);
      end if;

      if Tok = Tok_Identifier then
         if Tok_Text = "true" then
            Store.Push (Lith.Objects.True_Value);
         elsif Tok_Text = "false" then
            Store.Push (Lith.Objects.False_Value);
         elsif Tok_Text = "null" then
            Store.Push (Lith.Objects.Nil);
         elsif Tok_Text = "undefined" then
            Store.Push (Lith.Objects.Undefined);
         elsif Chaos.Dice.Is_Die_Roll (Tok_Text) then
            Store.Push (Chaos.Dice.To_Expression
                        (Chaos.Dice.Parse_Die_Roll (Tok_Text)));
         elsif Is_Number (Tok_Text) then
            Store.Push (Lith.Objects.To_Object
                        (Integer'Value (Tok_Text)));
         else
            Store.Push
              (Lith.Objects.To_Object
                 (Lith.Objects.Symbols.Get_Symbol (Tok_Text)));
         end if;
         Scan;
      elsif Tok = Tok_Quote then
         Scan;
         if Tok = Tok_Identifier then
            Store.Push (Lith.Objects.Symbols.Quote_Symbol);
            Store.Push (Lith.Objects.Symbols.Get_Symbol (Tok_Text));
            Store.Create_List (2);
            Scan;
         else
            Error ("missing identifier");
            Store.Push (Lith.Objects.False_Value);
         end if;
      elsif Tok = Tok_Left_Bracket then
         Scan;
         Store.Push (Chaos.Expressions.Vectors.Create);

         while At_Expression loop
            Parse_Expression;
            declare
               Value : constant Lith.Objects.Object :=
                         Store.Pop;
            begin
               Chaos.Expressions.Vectors.Append
                 (Store.Top, Value);
            end;

            if Tok = Tok_Comma then
               Scan;
            end if;
         end loop;

         if Tok = Tok_Right_Bracket then
            Scan;
         else
            Error ("missing ']'");
         end if;

      elsif Tok = Tok_Left_Brace then
         Scan;
         Store.Push (Chaos.Expressions.Maps.Create);

         while Tok = Tok_Identifier loop
            declare
               Key : constant String := Tok_Text;
            begin
               Scan;
               if Tok = Tok_Colon then
                  Scan;
               elsif At_Expression then
                  Warning ("missing ':'");
               elsif Tok = Tok_Right_Brace then
                  Error ("missing value");
               else
                  Error ("syntax error");
               end if;

               if At_Expression then
                  Parse_Expression;
                  declare
                     Value : constant Lith.Objects.Object := Store.Pop;
                  begin
                     Chaos.Expressions.Maps.Set (Store.Top, Key, Value);
                  end;
               end if;

               if Tok = Tok_Comma then
                  Scan;
               elsif not At_Expression and then Tok /= Tok_Right_Brace then
                  Error ("missing ','");
                  exit;
               elsif Tok /= Tok_Right_Brace then
                  Warning ("inserted ','");
               end if;
            end;
         end loop;

         if Tok = Tok_Right_Brace then
            Scan;
         else
            Error ("missing '}'");
         end if;

      elsif Tok = Tok_Left_Paren then
         Scan;
         Parse_Expression;
         if Tok = Tok_Right_Paren then
            Scan;
         else
            Error ("missing ')'");
         end if;
      else
         Error ("expected an expression");
         Store.Push (Lith.Objects.Undefined);
         Scan;
      end if;

      while Tok = Tok_Dot loop
         Scan;
         if Tok = Tok_Identifier then
            if Next_Tok = Tok_Assign then
               Store.Push
                 (Lith.Objects.Symbols.Get_Symbol ("chaos-set-property"));
               Store.Swap;
               Store.Push (Lith.Objects.Symbols.Quote_Symbol);
               Store.Push (Lith.Objects.Symbols.Get_Symbol (Tok_Text));
               Store.Create_List (2);

               Scan;
               Scan;
               Parse_Expression;

               Store.Push (Lith.Objects.Nil);
               Store.Cons;
               Store.Cons;
               Store.Cons;
               Store.Cons;
            else
               Store.Push
                 (Lith.Objects.Symbols.Get_Symbol ("chaos-get-property"));
               Store.Swap;
               Store.Push (Lith.Objects.Symbols.Quote_Symbol);
               Store.Push (Lith.Objects.Symbols.Get_Symbol (Tok_Text));
               Store.Create_List (2);
               Scan;

               Store.Push (Lith.Objects.Nil);
               Store.Cons;
               Store.Cons;
               Store.Cons;
            end if;
         else
            Error ("missing identifier");
            Scan;
            exit;
         end if;
      end loop;
   end Parse_Atomic_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   procedure Parse_Expression is
      use Chaos.Expressions;
   begin
      if Tok_Line > 0 then
         Store.Set_Context (Tok_File_Name, Tok_Line - 1);
      end if;

      if Tok = Tok_If then
         Store.Push (Lith.Objects.Symbols.Get_Symbol ("if"));
         Scan;
         Parse_Expression;

         if Tok = Tok_Then then
            Scan;
         else
            Error ("missing 'then'");
         end if;

         Parse_Expression;

         if Tok = Tok_Else then
            Scan;
            Parse_Expression;
         else
            Store.Push (0);
         end if;

         Store.Create_List (4);
      elsif Tok = Tok_Lambda then
         Scan;
         Parse_Lambda_Expression;
      else
         Parse_Operator_Expression (Precedence_Level'Last);
      end if;
   end Parse_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     (Text  : String)
      return Lith.Objects.Object
   is
   begin
      Parse_Expression (Text);
      return Chaos.Expressions.Store.Pop;
   end Parse_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   procedure Parse_Expression
     (Text  : String)
   is
   begin
      Open_String (Text);
      Parse_Expression;
      Close;
   end Parse_Expression;

   ------------------------------------
   -- Parse_Function_Call_Expression --
   ------------------------------------

   procedure Parse_Function_Call_Expression is
      use Chaos.Expressions;
      Indent    : constant Positive := Tok_Indent;
      Arguments : Natural := 0;
   begin
      Parse_Atomic_Expression;
      while Tok_Indent > Indent and then At_Expression loop
         Parse_Atomic_Expression;
         Arguments := Arguments + 1;
      end loop;

      if Arguments > 0 then
         Store.Push (Lith.Objects.Nil);
         for I in 1 .. Arguments loop
            Store.Cons;
         end loop;
         Store.Cons;
      end if;

   end Parse_Function_Call_Expression;

   -----------------------------
   -- Parse_Lambda_Expression --
   -----------------------------

   procedure Parse_Lambda_Expression is
      use Chaos.Expressions;
      Indent : constant Positive := Tok_Indent;
      Argument_Count : Natural := 0;
   begin
      Store.Push (Lith.Objects.Symbols.Lambda_Symbol);

      while Tok = Tok_Identifier loop
         Store.Push (Lith.Objects.Symbols.Get_Symbol (Tok_Text));
         Scan;
         Argument_Count := Argument_Count + 1;
      end loop;

      Store.Create_List (Argument_Count);

      if Tok = Tok_Arrow
        or else (At_Expression and then Tok_Indent >= Indent)
      then
         if Tok = Tok_Arrow then
            Scan;
         else
            Error ("inserted '=>'");
         end if;

         Parse_Expression;
         Store.Create_List (3);
      else
         Error ("expected an identifier or '=>'");
         Store.Push_Nil;
      end if;
   end Parse_Lambda_Expression;

   -------------------------------
   -- Parse_Operator_Expression --
   -------------------------------

   procedure Parse_Operator_Expression
     (Precedence : Precedence_Level)
   is
      use Chaos.Expressions;
   begin
      if Precedence = 1 then
         Parse_Function_Call_Expression;
      else
         Parse_Operator_Expression (Precedence - 1);
      end if;

--        if Tok /= Tok_End_Of_File then
--           Warning ("operator expression: precedence" & Precedence'Img
--                    & "; tok = " & Token'Image (Tok));
--        end if;

      while At_Operator and then Operator_Precedence (Tok) = Precedence loop
         Store.Push
           (Lith.Objects.Symbols.Get_Symbol
              (Ada.Strings.Fixed.Trim
                   (Operator_Name (Tok), Ada.Strings.Right)));
         Store.Swap;
         Scan;
         if Precedence = 1 then
            Parse_Function_Call_Expression;
         else
            Parse_Operator_Expression (Precedence - 1);
         end if;

         Store.Create_List (3);
      end loop;

   end Parse_Operator_Expression;

   -------------------
   -- Parse_Trigger --
   -------------------

   function Parse_Trigger (Text : String) return Lith.Objects.Object is
      Clean_Text : String (1 .. Text'Length);
      Length     : Natural := 0;
      Count      : Positive := 1;
   begin
      for Ch of Text loop
         if Character'Pos (Ch) not in 32 .. 126 then
            null;
         else
            Length := Length + 1;
            Clean_Text (Length) := Ch;
         end if;
      end loop;

      Open_String (Clean_Text (1 .. Length));

      Chaos.Expressions.Store.Push
        (Lith.Objects.Symbols.Get_Symbol ("and"));

      while Tok = Tok_Identifier or else Tok = Tok_Exclam loop
         declare
            Call : constant Chaos.Expressions.Import.Function_Call :=
                     Import_Inline_Function_Call;
         begin
            Chaos.Logging.Log
              ("TRIGGER", Clean_Text (1 .. Length));
            Chaos.Expressions.Import.Triggers.Import_Trigger (Call);
         end;

         Count := Count + 1;
      end loop;

      Close;
      Chaos.Expressions.Store.Create_List (Count);

      return Chaos.Expressions.Store.Pop;
   end Parse_Trigger;

end Chaos.Parser;
