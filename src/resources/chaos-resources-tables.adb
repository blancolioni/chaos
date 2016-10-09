with Ada.Strings.Fixed.Equal_Case_Insensitive;

package body Chaos.Resources.Tables is

   function Equal (X, Y : String) return Boolean
                   renames Ada.Strings.Fixed.Equal_Case_Insensitive;

   procedure Scan_String
     (Text        : String;
      Handle_Word : not null access
        procedure (Index : Positive;
                   Word : String));

   ------------------
   -- Column_Count --
   ------------------

   function Column_Count
     (Table : Table_Resource'Class)
      return Natural
   is
   begin
      return Table.Col_Headings.Last_Index;
   end Column_Count;

   --------------------
   -- Column_Heading --
   --------------------

   function Column_Heading
     (Table : Table_Resource'Class;
      Index : Positive)
      return String
   is
   begin
      return Table.Col_Headings (Index);
   end Column_Heading;

   ---------
   -- Get --
   ---------

   function Get
     (Table : Table_Resource'Class;
      Row   : Positive;
      Col   : Positive)
      return String
   is
   begin
      return Table.Cell_Data ((Row - 1) * Table.Column_Count + Col);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Table : Table_Resource'Class;
      Row   : String;
      Col   : String)
      return String
   is
      Row_Index, Col_Index : Natural := 0;
   begin
      for R in 1 .. Table.Row_Count loop
         if Equal (Table.Row_Heading (R), Row) then
            Row_Index := R;
            exit;
         end if;
      end loop;
      if Row_Index = 0 then
         return Ada.Strings.Unbounded.To_String (Table.Default_Value);
      end if;

      for C in 1 .. Table.Column_Count loop
         if Equal (Table.Column_Heading (C), Col) then
            Col_Index := C;
            exit;
         end if;
      end loop;

      if Col_Index = 0 then
         return Ada.Strings.Unbounded.To_String (Table.Default_Value);
      end if;

      return Table.Get (Row_Index, Col_Index);
   end Get;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Table : in out Table_Resource)
   is
      use Ada.Strings, Ada.Strings.Fixed;

      procedure Add_Column (Index : Positive;
                            Text  : String);

      procedure Add_Row (Index : Positive;
                         Text  : String);

      ----------------
      -- Add_Column --
      ----------------

      procedure Add_Column (Index : Positive;
                            Text  : String)
      is
         pragma Unreferenced (Index);
      begin
         Table.Col_Headings.Append (Text);
      end Add_Column;

      -------------
      -- Add_Row --
      -------------

      procedure Add_Row (Index : Positive;
                         Text  : String)
      is
      begin
         if Index = 1 then
            Table.Row_Headings.Append (Text);
         else
            Table.Cell_Data.Append (Text);
         end if;
      end Add_Row;

   begin
      Table.Set_Offset (0);
      Chaos.Resources.Text.Text_Resource (Table).Load;

      if Table.Line_Count < 4 then
         raise Constraint_Error with
           "invalid table: too few lines";
      end if;

      if Trim (Table.Line (1), Both) /= "2DA V1.0" then
         raise Constraint_Error with
           "invalid table: bad signature or unknown version: "
           & Table.Line (1);
      end if;

      Table.Default_Value :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Trim (Table.Line (2), Both));

      Scan_String (Table.Line (3), Add_Column'Access);
      for I in 4 .. Table.Line_Count loop
         Scan_String (Table.Line (I), Add_Row'Access);
         while Table.Cell_Data.Last_Index mod Table.Col_Headings.Last_Index
           /= 0
         loop
            Table.Cell_Data.Append
              (Ada.Strings.Unbounded.To_String (Table.Default_Value));
         end loop;
      end loop;

   end Load;

   ---------------
   -- Row_Count --
   ---------------

   function Row_Count
     (Table : Table_Resource'Class)
      return Natural
   is
   begin
      return Table.Row_Headings.Last_Index;
   end Row_Count;

   -----------------
   -- Row_Heading --
   -----------------

   function Row_Heading
     (Table : Table_Resource'Class;
      Index : Positive)
      return String
   is
   begin
      return Table.Row_Headings (Index);
   end Row_Heading;

   -----------------
   -- Scan_String --
   -----------------

   procedure Scan_String
     (Text        : String;
      Handle_Word : not null access
        procedure (Index : Positive;
                   Word : String))
   is
      Ext_Text : constant String := Text & ' ';
      Start    : Positive := Text'First;
      Skipping : Boolean := True;
      Count    : Natural := 0;
   begin
      for I in Ext_Text'Range loop
         if Ext_Text (I) = ' ' then
            if not Skipping then
               Count := Count + 1;
               Handle_Word (Count, Ext_Text (Start .. I - 1));
               Start := I + 1;
               Skipping := True;
            end if;
         else
            if Skipping then
               Start := I;
               Skipping := False;
            end if;
         end if;
      end loop;
   end Scan_String;

end Chaos.Resources.Tables;
