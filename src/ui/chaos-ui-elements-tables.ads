private with Ada.Containers.Doubly_Linked_Lists;

with Chaos.UI.Elements.Containers;

package Chaos.UI.Elements.Tables is

   type Root_UI_Table_Cell is
     new Chaos.UI.Elements.Containers.Root_UI_Container with private;

   type UI_Table_Cell is access all Root_UI_Table_Cell'Class;

   type Root_UI_Table_Row is new Root_UI_Element with private;

   function New_Cell
     (Row  : not null access Root_UI_Table_Row)
      return UI_Table_Cell;

   type UI_Table_Row is access all Root_UI_Table_Row'Class;

   type Root_UI_Table is new Root_UI_Element with private;

   function New_Row
     (Table  : not null access Root_UI_Table)
      return UI_Table_Row;

   type UI_Table is access all Root_UI_Table'Class;

   function New_Table
     (Identity : String)
      return UI_Table;

private

   type Root_UI_Table_Cell is
     new Chaos.UI.Elements.Containers.Root_UI_Container with null record;

   package List_Of_Cells is
     new Ada.Containers.Doubly_Linked_Lists (UI_Table_Cell);

   type Root_UI_Table_Row is
     new Chaos.UI.Elements.Containers.Root_UI_Container with null record;

   overriding function Table_Row_Layout
     (Row : Root_UI_Table_Row)
      return Boolean
   is (True);

   overriding function Row_Cells
     (Element : Root_UI_Table_Row)
      return Css.Array_Of_Elements;

   type Root_UI_Table is
     new Chaos.UI.Elements.Containers.Root_UI_Container with null record;

   overriding function Table_Layout
     (Row : Root_UI_Table)
      return Boolean
   is (True);

   overriding function Table_Rows
     (Element : Root_UI_Table)
      return Css.Array_Of_Elements;

end Chaos.UI.Elements.Tables;
