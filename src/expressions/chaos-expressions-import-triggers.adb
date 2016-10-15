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
         Name      : Lith.Objects.Object;
      end record;

   package Trigger_Vectors is
     new Ada.Containers.Vectors (Positive, Trigger_Info);

   Triggers : Trigger_Vectors.Vector;

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
      if Triggers.Last_Index < Index then
         Triggers.Set_Length (Ada.Containers.Count_Type (Index));
      end if;
      Info.Name :=
        To_Object (Get_Symbol ("chaos-trigger-" & Get_Name (Name)));

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
         Store.Show (Info.Name) & " = " & Store.Show (Store.Top));
      Lith.Environment.Define (To_Symbol (Info.Name), Store.Pop);

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
      Info : constant Trigger_Info :=
               Triggers (Positive (Trigger_Id) mod 16#4000#);
   begin
      if Info.Name = Nil then
         Chaos.Logging.Log
           ("TRIGGER",
            "warning: bad trigger id: " & WL.Binary_IO.Hex_Image (Trigger_Id));
         return;
      end if;

      Store.Push (Info.Name);
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

--
--        case Trigger_Id is
--           when Exists_Trigger =>
--              Store.Push
--                (Lith.Objects.Symbols.Get_Symbol
--                   ("chaos-object-exists"));
--              Store.Push
--                (Store.Top (1, Lith.Objects.Secondary));
--              Store.Create_List (2);
--           when Global_Trigger =>
--              if Text_1'Length > 6 then
--                 Chaos.Parser.Parse_Expression
--                   (Text_1 (Text_1'First .. Text_1'First + 5) & "."
--                    & Text_1 (Text_1'First + 6 .. Text_1'Last)
--                    & " = " & Integer'Image (Integer_1));
--              else
--                 Chaos.Parser.Parse_Expression
--                   (Text_1 & " = " & Integer'Image (Integer_1));
--              end if;
--           when Local_Timer_Expired =>
--              Chaos.Parser.Parse_Expression
--                ("chaos-timer-expired this " & Integer'Image (Integer_1));
--           when On_Creation_Trigger =>
--              Chaos.Parser.Parse_Expression
--                ("not (chaos-flag this 'script-executed)");
--           when Dead_Trigger =>
--              Chaos.Parser.Parse_Expression
--                ("chaos-flag this 'dead");
--           when others =>
--              Chaos.Logging.Log ("SCRIPT", "unknown trigger "
--                                 & WL.Binary_IO.Hex_Image
--                                   (WL.Binary_IO.Word_16 (Trigger_Id)));
--              if Flags mod 2 = 1 then
--                 Store.Push (Lith.Objects.True_Value);
--              else
--                 Store.Push (Lith.Objects.False_Value);
--              end if;
--        end case;

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
      if not Chaos.Expressions.Store.Load
        (Chaos.Paths.Config_File
           ("script/triggers.scm"))
      then
         raise Constraint_Error with
           "cannot load trigger configuration";
      end if;
   end Load_Triggers;

end Chaos.Expressions.Import.Triggers;
