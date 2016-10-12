with Ada.Characters.Handling;
with Ada.Text_IO;

with WL.Binary_IO;                     use WL.Binary_IO;

with Lith.Objects.Symbols;

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

   procedure Import_SC
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive);

   procedure Import_CR
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive);

   procedure Import_CO
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive);

   procedure Import_TR
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive);

   procedure Import_RS
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive);

   procedure Import_AC
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive);

   procedure Import_OB
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive);

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

   procedure Import_AC
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
   is
      Action_Id  : Word_32;
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
         Import_OB (Resource, Index);
         Import_OB (Resource, Index);
         Import_OB (Resource, Index);

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

            Chaos.Expressions.Import.Actions.Import_Action
              (Action_Id,
               Store.Top (3, Lith.Objects.Secondary),
               Store.Top (2, Lith.Objects.Secondary),
               Store.Top (1, Lith.Objects.Secondary),
               Integer_1, Integer_2, Integer_3,
               X, Y,
               String_1, String_2);

            Store.Drop (3, Lith.Objects.Secondary);
         end;
      end;
   end Import_AC;

   ---------------
   -- Import_CO --
   ---------------

   procedure Import_CO
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
   is
      Count  : Natural := 0;
   begin
      Skip_Line (Resource, "CO", Index);
      Store.Push (Lith.Objects.Symbols.Get_Symbol ("and"));
      while Resource.Line (Index) = "TR" loop
         Import_TR (Resource, Index);
         Count := Count + 1;
      end loop;

      if Count > 1 then
         Store.Create_List (Count + 1);
      elsif Count = 1 then
         Store.Swap;
         Store.Drop;
      else
         Store.Drop;
         Store.Push (Lith.Objects.True_Value);
      end if;
      Skip_Line (Resource, "CO", Index);
   end Import_CO;

   ---------------
   -- Import_CR --
   ---------------

   procedure Import_CR
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
   is
   begin
      Skip_Line (Resource, "CR", Index);
      Store.Push (Lith.Objects.Symbols.Get_Symbol ("if"));
      Import_CO (Resource, Index);
      Import_RS (Resource, Index);
      Skip_Line (Resource, "CR", Index);
      Store.Create_List (3);
   end Import_CR;

   ---------------
   -- Import_OB --
   ---------------

   procedure Import_OB
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
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
            Chaos.Expressions.Import.Objects.Import_Object
              (0, 0, EA, General, Race, Class, Specific, Gender, Alignment,
               Id_1, Id_2, Id_3, Id_4, Id_5, Name);
         end;
      end;

   end Import_OB;

   ---------------
   -- Import_RS --
   ---------------

   procedure Import_RS
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
   is
      Response_Count : Natural := 0;
   begin
      Skip_Line (Resource, "RS", Index);
      while Resource.Line (Index) = "RE" loop
         Skip_Line (Resource, "RE", Index);
         declare
            Line_Index  : Positive := 1;
            Action_Count : Natural := 0;
            Probability  : constant Integer :=
                             Scan_Integer (Resource.Line (Index), Line_Index);
         begin
            Store.Push (Probability);
            Store.Push (Lith.Objects.Symbols.Begin_Symbol);
            while Resource.Line (Index) /= "RE" loop
               Import_AC (Resource, Index);
               Action_Count := Action_Count + 1;
            end loop;
            if Action_Count > 1 then
               Store.Create_List (Action_Count + 1);
            elsif Action_Count = 1 then
               Store.Swap;
               Store.Drop;
            else
               raise Constraint_Error with "expected at least one action";
            end if;

            Skip_Line (Resource, "RE", Index);
            Response_Count := Response_Count + 1;
         end;
      end loop;

      Skip_Line (Resource, "RS", Index);

      if Response_Count = 1 then
         Store.Swap;
         Store.Drop;
      else
         Store.Push_Nil;
         for I in 1 .. Response_Count loop
            Store.Push (Store.Pop, Lith.Objects.Secondary);
            Store.Cons;
            Store.Push (Store.Pop (Lith.Objects.Secondary));
            Store.Cons;
         end loop;
         Store.Push (Lith.Objects.Symbols.Get_Symbol ("random-choice"));
         Store.Swap;
         Store.Cons;
      end if;
   end Import_RS;

   ---------------
   -- Import_SC --
   ---------------

   procedure Import_SC
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
   is
      Count : Natural := 0;
   begin
      Skip_Line (Resource, "SC", Index);
      Store.Push (Lith.Objects.Symbols.Get_Symbol ("begin"));
      while Resource.Line (Index) = "CR" loop
         Import_CR (Resource, Index);
         Count := Count + 1;
      end loop;
      Store.Create_List (Count + 1);
      Skip_Line (Resource, "SC", Index);
   end Import_SC;

   -------------------
   -- Import_Script --
   -------------------

   procedure Import_Script
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class)
   is
      Start : Integer := 1;
   begin
      Import_SC (Resource, Start);
      Ada.Text_IO.Put_Line
        (Store.Show (Store.Top));
   end Import_Script;

   ---------------
   -- Import_TR --
   ---------------

   procedure Import_TR
     (Resource : Chaos.Resources.Bcs.Bcs_Resource'Class;
      Index    : in out Positive)
   is
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
         Chaos.Expressions.Import.Triggers.Import_Trigger
           (Trigger_Id, Integer_1, Flags, Integer_2,
            Text_1, Text_2);
      end;
      Skip_Line (Resource, "TR", Index);
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
      Store.Set_Context (Resource.Name, Line_Number);
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
