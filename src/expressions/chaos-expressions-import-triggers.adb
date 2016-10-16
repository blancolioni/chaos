with Ada.Containers.Vectors;

with Lith.Environment;
with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.Logging;
with Chaos.Paths;

package body Chaos.Expressions.Import.Triggers is

   use WL.Binary_IO;

   type Trigger_Info is
      record
         Name      : Lith.Objects.Symbol_Type;
      end record;

   package Trigger_Vectors is
     new Ada.Containers.Vectors (Positive, Trigger_Info);

   Triggers : Trigger_Vectors.Vector;

   No_Trigger : Lith.Objects.Symbol_Type;

   function Evaluate_Chaos_Add_Trigger
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   --------------------------------
   -- Evaluate_Chaos_Add_Trigger --
   --------------------------------

   function Evaluate_Chaos_Add_Trigger
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects, Lith.Objects.Symbols;
      Index : constant Integer := To_Integer (Store.Argument (1));
      Name  : constant Symbol_Type := To_Symbol (Store.Argument (2));
      Info  : Trigger_Info;

   begin
      while Triggers.Last_Index < Index loop
         Triggers.Append ((Name => No_Trigger));
      end loop;
      Info.Name :=
        Get_Symbol ("chaos-trigger-" & Get_Name (Name));

      Store.Push (Lambda_Symbol);
      Store.Push (Get_Symbol ("integer-1"));
      Store.Push (Get_Symbol ("flags"));
      Store.Push (Get_Symbol ("integer-2"));
      Store.Push (Get_Symbol ("text-1"));
      Store.Push (Get_Symbol ("text-2"));
      Store.Push (Get_Symbol ("object"));
      Store.Create_List (6);
      Store.Push (Store.Argument (3));
      Store.Create_List (3);
      Chaos.Logging.Log
        ("TRIGGER",
         Get_Name (Info.Name) & " = " & Store.Show (Store.Top));
      Lith.Environment.Define (Info.Name, Store.Pop);

      Triggers (Index) := Info;

      return Store.Argument (1);
   end Evaluate_Chaos_Add_Trigger;

   --------------------
   -- Import_Trigger --
   --------------------

   procedure Import_Trigger
     (Trigger_Id : WL.Binary_IO.Word_32;
      Integer_1  : Integer;
      Flags      : WL.Binary_IO.Word_32;
      Integer_2  : Integer;
      Text_1     : String;
      Text_2     : String)
   is
      use Lith.Objects, Lith.Objects.Symbols;
      Index : constant Positive := Positive (Trigger_Id) mod 16#4000#;
   begin
      if Index > Triggers.Last_Index
        or else Triggers.Element (Index).Name = No_Trigger
      then
         Chaos.Logging.Log
           ("TRIGGER",
            "warning: bad trigger id: " & WL.Binary_IO.Hex_Image (Trigger_Id));
         return;
      end if;

      Store.Push (Triggers.Element (Index).Name);
      Store.Push (To_Object (Integer_1));
      Store.Push (To_Object (Natural (Flags)));
      Store.Push (To_Object (Integer_2));
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
      Store.Push (Store.Pop (Secondary));
      Store.Create_List (7);

      if Flags mod 2 = 1 then
         Store.Push (Get_Symbol ("not"));
         Store.Swap;
         Store.Create_List (2);
      end if;

   end Import_Trigger;

   -------------------
   -- Load_Triggers --
   -------------------

   procedure Load_Triggers is
   begin
      Lith.Objects.Interfaces.Define_Function
        ("chaos-add-trigger", 3, Evaluate_Chaos_Add_Trigger'Access);
      No_Trigger := Lith.Objects.Symbols.Get_Symbol ("no-trigger");
      if not Chaos.Expressions.Store.Load
        (Chaos.Paths.Config_File
           ("script/triggers.scm"))
      then
         raise Constraint_Error with
           "cannot load trigger configuration";
      end if;
   end Load_Triggers;

end Chaos.Expressions.Import.Triggers;
