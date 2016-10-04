with Ada.Characters.Handling;
with Ada.Text_IO;

with WL.Binary_IO;                     use WL.Binary_IO;

with Chaos.Expressions.Conditional;
with Chaos.Expressions.Identifiers;
with Chaos.Expressions.Numbers;
with Chaos.Expressions.Sequences;
with Chaos.Expressions.Vectors;

with Chaos.Expressions.Import.Actions;
with Chaos.Expressions.Import.Objects;
with Chaos.Expressions.Import.Triggers;

package body Chaos.Expressions.Import is

   procedure Error (Line    : Positive;
                    Message : String);

   procedure Skip_Line
     (Resource        : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Tag             : String;
      Line_Number     : in out Positive;
      End_Of_Line_Tag : Boolean := False);

   procedure Skip_Spaces
     (Line  : String;
      Index : in out Positive);

   function Scan_Word
     (Line  : String;
      Index : in out Positive)
      return Word_32;

   function Scan_Integer
     (Line  : String;
      Index : in out Positive)
      return Integer;

   function Scan_String
     (Line  : String;
      Index : in out Positive)
      return String;

   function Import_SC
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression;

   function Import_CR
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression;

   function Import_CO
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression;

   function Import_TR
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression;

   function Import_RS
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression;

   function Import_AC
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression;

   function Import_OB
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression;

   -----------
   -- Error --
   -----------

   procedure Error (Line    : Positive;
                    Message : String)
   is
   begin
      raise Constraint_Error with "Error on line" & Line'Img & ": " & Message;
   end Error;

   ---------------
   -- Import_AC --
   ---------------

   function Import_AC
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression
   is
      Action_Id : Word_32;
      Object_1  : Chaos_Expression;
      Object_2  : Chaos_Expression;
      Object_3  : Chaos_Expression;
      Integer_1  : Integer;
      Integer_2  : Integer;
      Integer_3  : Integer;
      X, Y       : Integer;
   begin
      Skip_Line (Resource, "AC", Index, End_Of_Line_Tag => True);
      declare
         Line_Index : Positive := 1;
      begin
         Action_Id := Scan_Word (Resource.Line (Index), Line_Index);
         Object_1 := Import_OB (Resource, Index);
         Object_2 := Import_OB (Resource, Index);
         Object_3 := Import_OB (Resource, Index);

         Line_Index := 1;
         Integer_1 := Scan_Integer (Resource.Line (Index), Line_Index);
         X := Scan_Integer (Resource.Line (Index), Line_Index);
         Y := Scan_Integer (Resource.Line (Index), Line_Index);
         Integer_2 := Scan_Integer (Resource.Line (Index), Line_Index);
         Integer_3 := Scan_Integer (Resource.Line (Index), Line_Index);
         declare
            String_1 : constant String :=
                         Scan_String (Resource.Line (Index), Line_Index);
            String_2 : constant String :=
                         Scan_String (Resource.Line (Index), Line_Index);
         begin
            Skip_Line (Resource, "AC", Index, End_Of_Line_Tag => True);
            return Chaos.Expressions.Import.Actions.Import_Action
              (Action_Id,
               Object_1, Object_2, Object_3,
               Integer_1, Integer_2, Integer_3,
               X, Y,
               String_1, String_2);
         end;
      end;
   end Import_AC;

   ---------------
   -- Import_CO --
   ---------------

   function Import_CO
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression
   is
      First  : Boolean := True;
      Result : Chaos_Expression := Null_Value;
   begin
      Skip_Line (Resource, "CO", Index);
      while Resource.Line (Index) = "TR" loop
         declare
            TR : constant Chaos_Expression := Import_TR (Resource, Index);
         begin
            if First then
               Result := TR;
               First := False;
            else
               Result :=
                 Chaos.Expressions.Apply
                   (Chaos.Expressions.Apply
                      (Chaos.Expressions.Identifiers.To_Expression ("and"),
                       Result),
                    TR);
            end if;
         end;
      end loop;
      Skip_Line (Resource, "CO", Index);
      return Result;
   end Import_CO;

   ---------------
   -- Import_CR --
   ---------------

   function Import_CR
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression
   is
      Condition : Chaos_Expression;
      Response  : Chaos_Expression;
   begin
      Skip_Line (Resource, "CR", Index);
      Condition := Import_CO (Resource, Index);
      Response  := Import_RS (Resource, Index);
      Skip_Line (Resource, "CR", Index);
      return Chaos.Expressions.Conditional.Create_Conditional
        (Condition, Response, Null_Value);
   end Import_CR;

   ---------------
   -- Import_OB --
   ---------------

   function Import_OB
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression
   is
      EA        : Natural;
      General   : Natural;
      Race      : Natural;
      Class     : Natural;
      Specific  : Natural;
      Gender    : Natural;
      Alignment : Natural;
      Id_1      : Natural;
      Id_2      : Natural;
      Id_3      : Natural;
      Id_4      : Natural;
      Id_5      : Natural;
   begin
      Skip_Line (Resource, "OB", Index, End_Of_Line_Tag => True);

      declare
         Line : constant String := Resource.Line (Index);
         Line_Index : Positive := 1;
      begin
         EA := Scan_Integer (Line, Line_Index);
         General := Scan_Integer (Line, Line_Index);
         Race := Scan_Integer (Line, Line_Index);
         Class := Scan_Integer (Line, Line_Index);
         Specific := Scan_Integer (Line, Line_Index);
         Gender := Scan_Integer (Line, Line_Index);
         Alignment := Scan_Integer (Line, Line_Index);
         Id_1 := Scan_Integer (Line, Line_Index);
         Id_2 := Scan_Integer (Line, Line_Index);
         Id_3 := Scan_Integer (Line, Line_Index);
         Id_4 := Scan_Integer (Line, Line_Index);
         Id_5 := Scan_Integer (Line, Line_Index);

         declare
            Name : constant String := Scan_String (Line, Line_Index);
         begin
            Skip_Line (Resource, "OB", Index, End_Of_Line_Tag => True);
            return Chaos.Expressions.Import.Objects.Import_Object
              (0, 0, EA, General, Race, Class, Specific, Gender, Alignment,
               Id_1, Id_2, Id_3, Id_4, Id_5, Name);
         end;
      end;

   end Import_OB;

   ---------------
   -- Import_RS --
   ---------------

   function Import_RS
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression
   is
      Max_Responses  : constant := 20;
      Probabilities  : array (1 .. Max_Responses) of Natural;
      Responses      : array (1 .. Max_Responses) of Chaos_Expression;
      Response_Count : Natural := 0;
   begin
      Skip_Line (Resource, "RS", Index);
      while Resource.Line (Index) = "RE" loop
         Skip_Line (Resource, "RE", Index);
         declare
            Line_Index  : Positive := 1;
            Probability : constant Integer :=
                            Scan_Integer (Resource.Line (Index), Line_Index);
            Actions     : constant Chaos_Expression :=
                            Chaos.Expressions.Sequences.Sequence_Expression;
         begin
            while Resource.Line (Index) /= "RE" loop
               Chaos.Expressions.Sequences.Append
                 (Actions, Import_AC (Resource, Index));
            end loop;
            Skip_Line (Resource, "RE", Index);
            Response_Count := Response_Count + 1;
            Probabilities (Response_Count) := Probability;
            Responses (Response_Count) := Actions;
         end;
      end loop;

      Skip_Line (Resource, "RS", Index);

      if Response_Count = 1 then
         return Responses (1);
      else
         declare
            Result : constant Chaos.Expressions.Chaos_Expression :=
                       Chaos.Expressions.Identifiers.To_Expression
                         ("random-choice");
            Args   : constant Chaos.Expressions.Chaos_Expression :=
                       Chaos.Expressions.Vectors.Vector_Expression;
         begin
            for I in 1 .. Response_Count loop
               Chaos.Expressions.Vectors.Append
                 (Args, Chaos.Expressions.Numbers.To_Expression
                    (Probabilities (I)));
               Chaos.Expressions.Vectors.Append
                 (Args, Responses (I));
            end loop;
            return Chaos.Expressions.Apply
              (Result, Args);
         end;
      end if;
   end Import_RS;

   ---------------
   -- Import_SC --
   ---------------

   function Import_SC
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression
   is
      Expr  : constant Chaos_Expression :=
                Chaos.Expressions.Sequences.Sequence_Expression;
   begin
      Skip_Line (Resource, "SC", Index);
      while Resource.Line (Index) = "CR" loop
         declare
            E : constant Chaos_Expression :=
                  Import_CR (Resource, Index);
         begin
            Chaos.Expressions.Sequences.Append (Expr, E);
         end;
      end loop;
      Skip_Line (Resource, "SC", Index);
      return Expr;
   end Import_SC;

   -------------------
   -- Import_Script --
   -------------------

   function Import_Script
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class)
      return Chaos_Expression
   is
      Start : Integer := 1;
   begin
      return Result : constant Chaos_Expression :=
        Import_SC (Resource, Start)
      do
         Ada.Text_IO.Put_Line (To_String (Result));
      end return;
   end Import_Script;

   ---------------
   -- Import_TR --
   ---------------

   function Import_TR
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
      return Chaos_Expression
   is
      Result : Chaos_Expression;
   begin
      Skip_Line (Resource, "TR", Index);
      declare
         Line_Index : Positive := 1;
         Line       : constant String := Resource.Line (Index);
         Trigger_Id : constant Word_32 :=
                        Scan_Word (Line, Line_Index);
         Integer_1  : constant Integer :=
                        Scan_Integer (Line, Line_Index);
         Flags      : constant Word_32 :=
                        Scan_Word (Line, Line_Index);
         Integer_2  : constant Integer :=
                        Scan_Integer (Line, Line_Index);
         Unknown    : constant Integer :=
                        Scan_Integer (Line, Line_Index);
         Text_1     : constant String :=
                        Scan_String (Line, Line_Index);
         Text_2     : constant String :=
                        Scan_String (Line, Line_Index);
      begin
         pragma Unreferenced (Unknown);
         Skip_Line (Resource, "OB", Index, End_Of_Line_Tag => True);
         Skip_Line (Resource, "OB", Index, End_Of_Line_Tag => True);
         Result :=
           Chaos.Expressions.Import.Triggers.Import_Trigger
             (Trigger_Id, Integer_1, Flags, Integer_2,
              Text_1, Text_2);
      end;
      Skip_Line (Resource, "TR", Index);
      return Result;
   end Import_TR;

   ------------------
   -- Scan_Integer --
   ------------------

   function Scan_Integer
     (Line  : String;
      Index : in out Positive)
      return Integer
   is
      Negative : Boolean := False;
      Word     : Word_32;
   begin
      Skip_Spaces (Line, Index);
      if Index < Line'Last and then Line (Index) = '-' then
         Negative := True;
         Index := Index + 1;
      end if;
      Word := Scan_Word (Line, Index);
      if Negative then
         return -Integer (Word);
      else
         return Integer (Word);
      end if;
   end Scan_Integer;

   -----------------
   -- Scan_String --
   -----------------

   function Scan_String
     (Line  : String;
      Index : in out Positive)
      return String
   is
      Start : Positive;
   begin
      Skip_Spaces (Line, Index);
      if Index > Line'Last or else Line (Index) /= '"' then
         return "";
      else
         Index := Index + 1;
         Start := Index;
         while Index <= Line'Last and then Line (Index) /= '"' loop
            Index := Index + 1;
         end loop;

         declare
            Result : constant String := Line (Start .. Index - 1);
         begin
            if Index <= Line'Last then
               Index := Index + 1;
            end if;
            return Result;
         end;
      end if;
   end Scan_String;

   ---------------
   -- Scan_Word --
   ---------------

   function Scan_Word
     (Line  : String;
      Index : in out Positive)
      return Word_32
   is
      use Ada.Characters.Handling;
      Result : Word_32 := 0;
   begin
      Skip_Spaces (Line, Index);
      while Index <= Line'Last and then Is_Digit (Line (Index)) loop
         Result := Result * 10 + Character'Pos (Line (Index)) - 48;
         Index := Index + 1;
      end loop;
      return Result;
   end Scan_Word;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line
     (Resource        : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Tag             : String;
      Line_Number     : in out Positive;
      End_Of_Line_Tag : Boolean := False)
   is
      Line : constant String := Resource.Line (Line_Number);
   begin
      if False then
         Ada.Text_IO.Put_Line
           (Line_Number'Img & ": " & Line);
      end if;

      if Line'Length < Tag'Length then
         Error (Line_Number, "expected tag: " & Tag);
      else
         if End_Of_Line_Tag then
            if Line (Line'Last - Tag'Length + 1 .. Line'Last) = Tag then
               Line_Number := Line_Number + 1;
            else
               Error (Line_Number, "expected end of line tag: " & Tag);
            end if;
         else
            if Line (Line'First .. Line'First + Tag'Length - 1) = Tag then
               Line_Number := Line_Number + 1;
            else
               Error (Line_Number, "expected tag: " & Tag);
            end if;
         end if;
      end if;
   end Skip_Line;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces
     (Line  : String;
      Index : in out Positive)
   is
   begin
      while Index <= Line'Last and then Line (Index) = ' ' loop
         Index := Index + 1;
      end loop;
   end Skip_Spaces;

end Chaos.Expressions.Import;
