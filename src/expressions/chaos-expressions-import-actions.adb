with Ada.Containers.Vectors;

with Lith.Environment;
with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.Logging;
with Chaos.Paths;

package body Chaos.Expressions.Import.Actions is

   package Action_Vectors is
     new Ada.Containers.Vectors (Natural, Lith.Objects.Symbol_Type,
                                 Lith.Objects."=");

   Actions : Action_Vectors.Vector;

   No_Action : Lith.Objects.Symbol_Type;

   function Evaluate_Chaos_Add_Action
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   --------------------------------
   -- Evaluate_Chaos_Add_Action --
   --------------------------------

   function Evaluate_Chaos_Add_Action
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects, Lith.Objects.Symbols;
      Index : constant Integer := To_Integer (Store.Argument (1));
      Action_Name : constant Symbol_Type := To_Symbol (Store.Argument (2));
      Full_Name   : constant Symbol_Type :=
                      Get_Symbol
                        ("chaos-action-"
                         & Get_Name (Action_Name));
   begin
      while Actions.Last_Index < Index loop
         Actions.Append (No_Action);
      end loop;

      Actions.Replace_Element (Index, Full_Name);

      Store.Push (Lambda_Symbol);
      Store.Push (Get_Symbol ("object-1"));
      Store.Push (Get_Symbol ("object-2"));
      Store.Push (Get_Symbol ("object-3"));
      Store.Push (Get_Symbol ("integer-1"));
      Store.Push (Get_Symbol ("point-x"));
      Store.Push (Get_Symbol ("point-y"));
      Store.Push (Get_Symbol ("integer-2"));
      Store.Push (Get_Symbol ("integer-3"));
      Store.Push (Get_Symbol ("text-1"));
      Store.Push (Get_Symbol ("text-2"));
      Store.Create_List (10);
      Store.Push (Store.Argument (3));
      Store.Create_List (3);
      Chaos.Logging.Log
        ("ACTION",
         Get_Name (Actions (Index)) & " = " & Store.Show (Store.Top));
      Lith.Environment.Define (Full_Name, Store.Pop);

      return Store.Argument (1);
   end Evaluate_Chaos_Add_Action;

   -------------------
   -- Import_Action --
   -------------------

   procedure Import_Action
     (Action_Id  : WL.Binary_IO.Word_32;
      Integer_1  : Integer;
      Integer_2  : Integer;
      Integer_3  : Integer;
      X, Y       : Integer;
      Text_1     : String;
      Text_2     : String)
   is
      use Lith.Objects, Lith.Objects.Symbols;
      Index : constant Natural := Natural (Action_Id);
   begin
      if Index > Actions.Last_Index
        or else Actions.Element (Index) = No_Action
      then
         Chaos.Logging.Log
           ("ACTION",
            "warning: bad action id: " & WL.Binary_IO.Hex_Image (Action_Id));
         Store.Push (False_Value);
         return;
      end if;

      Store.Push (Actions.Element (Index));
      Store.Push (Store.Top (3, Secondary));
      Store.Push (Store.Top (2, Secondary));
      Store.Push (Store.Top (1, Secondary));

      Store.Push (To_Object (Integer_1));
      Store.Push (To_Object (X));
      Store.Push (To_Object (Y));
      Store.Push (To_Object (Integer_2));
      Store.Push (To_Object (Integer_3));
      Store.Push (Quote_Symbol);
      if Text_1 /= "" then
         Store.Push (Get_Symbol (Text_1));
      else
         Store.Push_Nil;
      end if;
      Store.Create_List (2);
      Store.Push (Quote_Symbol);
      if Text_2 /= "" then
         Store.Push (Get_Symbol (Text_2));
      else
         Store.Push_Nil;
      end if;
      Store.Create_List (2);
      Store.Create_List (11);

   end Import_Action;

   ------------------
   -- Load_Actions --
   ------------------

   procedure Load_Actions is
   begin
      Lith.Objects.Interfaces.Define_Function
        ("chaos-add-action", 3, Evaluate_Chaos_Add_Action'Access);
      No_Action := Lith.Objects.Symbols.Get_Symbol ("no-action");

      if not Chaos.Expressions.Store.Load
        (Chaos.Paths.Config_File
           ("script/actions.scm"))
      then
         raise Constraint_Error with
           "cannot load action configuration";
      end if;
   end Load_Actions;

end Chaos.Expressions.Import.Actions;
