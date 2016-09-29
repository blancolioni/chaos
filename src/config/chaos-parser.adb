with Ada.Characters.Handling;
with Ada.Directories;

with Chaos.Parser.Tokens;              use Chaos.Parser.Tokens;
with Chaos.Parser.Lexical;             use Chaos.Parser.Lexical;

with Chaos.Expressions.Conditional;
with Chaos.Expressions.Functions;
with Chaos.Expressions.Identifiers;
with Chaos.Expressions.Maps;
with Chaos.Expressions.Numbers;
with Chaos.Expressions.Sequences;
with Chaos.Expressions.Vectors;

with Chaos.Dice;

package body Chaos.Parser is

   use Set_Of_Tokens;

   type Precedence_Level is range 1 .. 9;

   type Precedence_Table is array (Token) of Precedence_Level;

   Operator_Precedence : constant Precedence_Table :=
                 (Tok_Asterisk => 3, Tok_Forward_Slash => 3,
                  Tok_Plus     => 4, Tok_Minus => 4,
                  Tok_Less     => 5, Tok_Less_Equal => 5,
                  Tok_Greater  => 5, Tok_Greater_Equal => 5,
                  Tok_Equal    => 6, Tok_Not_Equal => 6,
                  Tok_Arrow    => 8,
                  others       => 9);

   function Parse_Atomic_Expression
     return Chaos.Expressions.Chaos_Expression;

   function Parse_Expression return Chaos.Expressions.Chaos_Expression;

   function Parse_Operator_Expression
     (Precedence : Precedence_Level)
      return Chaos.Expressions.Chaos_Expression;

   function At_Expression return Boolean
   is (Tok = Tok_Left_Paren or else Tok = Tok_Left_Brace
       or else Tok = Tok_Left_Bracket or else Tok = Tok_Identifier);

   function At_Operator return Boolean is
     (Tok <= +(Tok_Asterisk, Tok_Forward_Slash, Tok_Plus, Tok_Minus,
               Tok_Less, Tok_Less_Equal, Tok_Greater, Tok_Greater_Equal,
               Tok_Equal, Tok_Not_Equal, Tok_Arrow));

   function Is_Number
     (Text : String)
      return Boolean;

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
     (Path : String;
      On_Setting : not null access
        procedure (Name  : String;
                   Value : Chaos.Expressions.Chaos_Expression))
   is
   begin
      Open (Path);
      while Tok /= Tok_End_Of_File loop
         if Tok = Tok_Identifier then
            declare
               Name  : constant String := Tok_Text;
               Value : Chaos.Expressions.Chaos_Expression;
            begin
               Scan;
               if Tok = Tok_Colon then
                  Scan;
                  Value := Parse_Expression;
                  if Tok = Tok_Semicolon then
                     Scan;
                  end if;
                  On_Setting (Name, Value);
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
     (Path : String)
      return Chaos.Expressions.Chaos_Expression
   is
      Result : constant Chaos.Expressions.Chaos_Expression :=
                 Chaos.Expressions.Sequences.Sequence_Expression;
   begin
      Open (Path);
      while Tok /= Tok_End_Of_File loop
         declare
            E : constant Chaos.Expressions.Chaos_Expression :=
                  Parse_Expression;
         begin
            Chaos.Expressions.Sequences.Append (Result, E);
         end;
      end loop;
      Close;
      return Result;
   end Load_Script;

   -----------------------------
   -- Parse_Atomic_Expression --
   -----------------------------

   function Parse_Atomic_Expression
     return Chaos.Expressions.Chaos_Expression
   is
      E : Chaos.Expressions.Chaos_Expression;
   begin
      if Tok = Tok_Identifier then
         if Tok_Text = "always" then
            E := Chaos.Expressions.Always;
         elsif Tok_Text = "never" then
            E := Chaos.Expressions.Never;
         elsif Tok_Text = "null" then
            E := Chaos.Expressions.Null_Value;
         elsif Tok_Text = "undefined" then
            E := Chaos.Expressions.Undefined_Value;
         elsif Chaos.Dice.Is_Die_Roll (Tok_Text) then
            E :=
              Chaos.Dice.To_Expression
                (Chaos.Dice.Parse_Die_Roll (Tok_Text));
         elsif Is_Number (Tok_Text) then
            E := Chaos.Expressions.Numbers.To_Expression
              (Integer'Value (Tok_Text));
         else
            E := Chaos.Expressions.Identifiers.To_Expression (Tok_Text);
         end if;
         Scan;
      elsif Tok = Tok_Left_Bracket then
         Scan;
         E := Chaos.Expressions.Vectors.Vector_Expression;

         while At_Expression loop
            Chaos.Expressions.Vectors.Append
              (E, Parse_Expression);
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
         E := Chaos.Expressions.Maps.Map_Expression;

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
                  Chaos.Expressions.Maps.Set
                    (E, Key, Parse_Expression);
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
         E := Parse_Expression;
         if Tok = Tok_Right_Paren then
            Scan;
         else
            Error ("missing ')'");
         end if;
      else
         Error ("expected an expression");
         E := Chaos.Expressions.Undefined_Value;
         Scan;
      end if;
      while Tok = Tok_Dot loop
         Scan;
         if Tok = Tok_Identifier then
            if Next_Tok = Tok_Assign then
               declare
                  Name : constant String := Tok_Text;
                  Value : Chaos.Expressions.Chaos_Expression;
               begin
                  Scan;
                  Scan;
                  Value := Parse_Expression;
                  E :=
                    Chaos.Expressions.Functions.Create_Assignment
                      (E, Name, Value);
               end;
            else
               E :=
                 Chaos.Expressions.Functions.Create_Method_Call
                   (E, Tok_Text, Chaos.Expressions.No_Array);
               Scan;
            end if;
         else
            Error ("missing identifier");
            Scan;
            exit;
         end if;
      end loop;

      return E;
   end Parse_Atomic_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression return Chaos.Expressions.Chaos_Expression is
   begin
      if Tok = Tok_If then
         declare
            Condition,
            True_Part,
            False_Part : Chaos.Expressions.Chaos_Expression;
         begin
            Scan;
            Condition := Parse_Expression;
            if Tok = Tok_Then then
               Scan;
            else
               Error ("missing 'then'");
            end if;
            True_Part := Parse_Expression;
            if Tok = Tok_Else then
               Scan;
               False_Part := Parse_Expression;
            else
               False_Part := Chaos.Expressions.Null_Value;
            end if;
            return Chaos.Expressions.Conditional.Create_Conditional
              (Condition, True_Part, False_Part);
         end;
      else
         return Parse_Operator_Expression (Precedence_Level'Last);
      end if;
   end Parse_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     (Text : String)
      return Chaos.Expressions.Chaos_Expression
   is
      Result : Chaos.Expressions.Chaos_Expression;
   begin
      Open_String (Text);
      Result := Parse_Expression;
      Close;
      return Result;
   end Parse_Expression;

   function Parse_Operator_Expression
     (Precedence : Precedence_Level)
      return Chaos.Expressions.Chaos_Expression
   is
      Result : Chaos.Expressions.Chaos_Expression;
   begin
      if Precedence = 1 then
         Result := Parse_Atomic_Expression;
      else
         Result := Parse_Operator_Expression (Precedence - 1);
      end if;

      while At_Operator and then Operator_Precedence (Tok) = Precedence loop
         declare
            Op    : constant Token := Tok;
            Name  : constant String := Tok_Text;
            Right : Chaos.Expressions.Chaos_Expression;
         begin
            Scan;
            if Precedence = 1 then
               Right := Parse_Atomic_Expression;
            else
               Right := Parse_Operator_Expression (Precedence - 1);
            end if;

            if Op = Tok_Arrow then
               Result :=
                 Chaos.Expressions.Conditional.Create_Conditional
                   (Result, Right, Chaos.Expressions.Null_Value);
            else
               Result :=
                 Chaos.Expressions.Functions.Create_Function_Call
                   (Name, (Result, Right));
            end if;
         end;
      end loop;

      return Result;

   end Parse_Operator_Expression;

end Chaos.Parser;
