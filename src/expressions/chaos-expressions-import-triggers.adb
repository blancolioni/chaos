with Ada.Containers.Vectors;

with WL.String_Maps;

with Lith.Environment;
with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.Logging;
with Chaos.Paths;

package body Chaos.Expressions.Import.Triggers is

   use WL.Binary_IO;

   Argument_Names : array (Trigger_Argument_Name) of Lith.Objects.Symbol_Type;
   Got_Argument_Names : Boolean := False;

   type Argument_Flags is array (Trigger_Argument_Name) of Boolean;

   type Trigger_Info is
      record
         Function_Name : Lith.Objects.Symbol_Type;
         Script_Name   : Lith.Objects.Symbol_Type;
         Arguments     : Argument_Flags := (others => False);
      end record;

   package Trigger_Vectors is
     new Ada.Containers.Vectors (Positive, Trigger_Info);

   package Trigger_Maps is
     new WL.String_Maps (Positive);

   Triggers : Trigger_Vectors.Vector;
   Trigger_Id_Map : Trigger_Maps.Map;

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
      Fn    : constant Object := Store.Argument (3);

      procedure Check_Arg (It : Object);

      ---------------
      -- Check_Arg --
      ---------------

      procedure Check_Arg (It : Object) is
      begin
         if Is_Symbol (It) then
            for Arg in Argument_Names'Range  loop
               if Argument_Names (Arg) = To_Symbol (It) then
                  Info.Arguments (Arg) := True;
                  exit;
               end if;
            end loop;
         elsif Is_Pair (It) then
            Check_Arg (Store.Car (It));
            Check_Arg (Store.Cdr (It));
         end if;
      end Check_Arg;

   begin
      if not Got_Argument_Names then
         Argument_Names :=
           (Integer_1 => Get_Symbol ("integer-1"),
            Integer_2 => Get_Symbol ("integer-2"),
            Flags     => Get_Symbol ("flags"),
            Text_1    => Get_Symbol ("text-1"),
            Text_2    => Get_Symbol ("text-2"),
            Object_Reference => Get_Symbol ("object"));
         Got_Argument_Names := True;
      end if;

      Chaos.Logging.Log ("TRIGGER", "checking arguments: "
                         & Store.Show (Fn));
      Check_Arg (Fn);

      while Triggers.Last_Index < Index loop
         Triggers.Append ((Function_Name => No_Trigger,
                           Script_Name   => No_Trigger,
                           Arguments     => (others => False)));
      end loop;

      Info.Script_Name := Name;
      Info.Function_Name :=
        Get_Symbol ("chaos-trigger-" & Get_Name (Name));

      Trigger_Id_Map.Insert (Get_Name (Name), Index);

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
         Get_Name (Info.Script_Name) & " = " & Store.Show (Store.Top));
      Lith.Environment.Define (Info.Function_Name, Store.Pop);

      Triggers (Index) := Info;

      return Store.Argument (1);
   end Evaluate_Chaos_Add_Trigger;

   ---------------------------
   -- Get_Trigger_Arguments --
   ---------------------------

   function Get_Trigger_Arguments
     (Id : Positive)
      return Trigger_Argument_Names
   is
      Result : Trigger_Argument_Names (1 .. 6);
      Count  : Natural := 0;
      Args   : Argument_Flags renames
                 Triggers (Id).Arguments;
   begin
      for Arg in Args'Range loop
         if Args (Arg) then
            Count := Count + 1;
            Result (Count) := Arg;
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Trigger_Arguments;

   --------------------
   -- Get_Trigger_Id --
   --------------------

   function Get_Trigger_Id
     (Name : String)
      return Natural
   is
   begin
      if Trigger_Id_Map.Contains (Name) then
         return Trigger_Id_Map.Element (Name);
      else
         return 0;
      end if;
   end Get_Trigger_Id;

   --------------------
   -- Import_Trigger --
   --------------------

   procedure Import_Trigger
     (Trigger_Id : Positive;
      Integer_1  : Integer;
      Flags      : WL.Binary_IO.Word_32;
      Integer_2  : Integer;
      Text_1     : String;
      Text_2     : String)
   is
      use Lith.Objects, Lith.Objects.Symbols;
      Index : constant Positive := Trigger_Id mod 16#4000#;
   begin
      if Index > Triggers.Last_Index
        or else Triggers.Element (Index).Function_Name = No_Trigger
      then
         Chaos.Logging.Log
           ("TRIGGER",
            "warning: bad trigger id: "
            & WL.Binary_IO.Hex_Image
              (WL.Binary_IO.Word_16 (Trigger_Id)));
         return;
      end if;

      Store.Push (Triggers.Element (Index).Function_Name);
      Store.Push (To_Object (Integer_1));
      Store.Push (To_Object (Natural (Flags)));
      Store.Push (To_Object (Integer_2));
      Store.Push (Quote_Symbol);
      if Text_1 /= "" then
         if Text_2 = ""
           and then Triggers.Element (Index).Arguments
           (Import.Triggers.Text_2)
         then
            Store.Push
              (Get_Symbol (Text_1 (Text_1'First + 6 .. Text_1'Last)));
         else
            Store.Push (Get_Symbol (Text_1));
         end if;
      else
         Store.Push_Nil;
      end if;
      Store.Create_List (2);
      Store.Push (Quote_Symbol);
      if Text_2 /= "" then
         Store.Push (Get_Symbol (Text_2));
      elsif Triggers.Element (Index).Arguments
        (Import.Triggers.Text_2)
      then
         Store.Push
           (Get_Symbol (Text_1 (Text_1'First .. Text_1'First + 5)));
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
