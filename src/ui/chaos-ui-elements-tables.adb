package body Chaos.UI.Elements.Tables is

   function New_Cell
     (Identity : String)
     return UI_Table_Cell;

   function New_Row
     (Identity : String)
     return UI_Table_Row;

   --------------
   -- New_Cell --
   --------------

   function New_Cell
     (Identity : String)
      return UI_Table_Cell
   is
      Cell : constant UI_Table_Cell := new Root_UI_Table_Cell;
   begin
      Cell.New_Container (Identity);
      return Cell;
   end New_Cell;

   --------------
   -- New_Cell --
   --------------

   function New_Cell
     (Row  : not null access Root_UI_Table_Row)
      return UI_Table_Cell
   is
      Cell : constant UI_Table_Cell :=
               New_Cell
                 (Identity (Row.all)
                  & "-cell"
                  & Integer'Image (-Row.Count + 1));
   begin
         Row.Add (Cell);
         return Cell;
   end New_Cell;

   -------------
   -- New_Row --
   -------------

   function New_Row
     (Identity : String)
      return UI_Table_Row
   is
      Row : constant UI_Table_Row := new Root_UI_Table_Row;
   begin
      Row.Create (Identity);
      return Row;
   end New_Row;

   -------------
   -- New_Row --
   -------------

   function New_Row
     (Table  : not null access Root_UI_Table)
      return UI_Table_Row
   is
      Row : constant UI_Table_Row :=
              New_Row
                (Identity (Table.all)
                 & "-row"
                 & Integer'Image (-Table.Count + 1));
   begin
      Table.Add (Row);
      return Row;
   end New_Row;

   ---------------
   -- New_Table --
   ---------------

   function New_Table
     (Identity : String)
      return UI_Table
   is
      Table : constant UI_Table := new Root_UI_Table;
   begin
      Table.Create (Identity);
      return Table;
   end New_Table;

   ---------------
   -- Row_Cells --
   ---------------

   overriding function Row_Cells
     (Element : Root_UI_Table_Row)
      return Css.Array_Of_Elements
   is
   begin
      return Element.Layout_Children;
   end Row_Cells;

   ----------------
   -- Table_Rows --
   ----------------

   overriding function Table_Rows
     (Element : Root_UI_Table)
      return Css.Array_Of_Elements
   is
   begin
      return Element.Layout_Children;
   end Table_Rows;

end Chaos.UI.Elements.Tables;
