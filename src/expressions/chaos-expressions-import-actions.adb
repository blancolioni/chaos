with Ada.Containers.Vectors;

with WL.String_Maps;

with Lith.Environment;
with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.Logging;
with Chaos.Paths;

package body Chaos.Expressions.Import.Actions is

   Argument_Names     : array (Action_Argument_Name)
     of Lith.Objects.Symbol_Type;
   Got_Argument_Names : Boolean := False;

   type Argument_Flags is array (Action_Argument_Name) of Boolean;

   type Action_Info is
      record
         Function_Name : Lith.Objects.Symbol_Type;
         Script_Name   : Lith.Objects.Symbol_Type;
         Arguments     : Argument_Flags := (others => False);
      end record;

   package Action_Vectors is
     new Ada.Containers.Vectors (Natural, Action_Info);

   package Action_Maps is
     new WL.String_Maps (Natural);

   Actions       : Action_Vectors.Vector;
   Action_Id_Map : Action_Maps.Map;

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
      Info        : Action_Info;
      Fn          : constant Object := Store.Argument (3);

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
           (Integer_1        => Get_Symbol ("integer-1"),
            Integer_2        => Get_Symbol ("integer-2"),
            Integer_3        => Get_Symbol ("integer-3"),
            Point_X          => Get_Symbol ("x"),
            Point_Y          => Get_Symbol ("y"),
            Text_1           => Get_Symbol ("text-1"),
            Text_2           => Get_Symbol ("text-2"),
            Object_1         => Get_Symbol ("object-1"),
            Object_2         => Get_Symbol ("object-2"),
            Object_3         => Get_Symbol ("object-3"));

         Got_Argument_Names := True;
      end if;

      Check_Arg (Fn);

      while Actions.Last_Index < Index loop
         Actions.Append ((Function_Name => No_Action,
                           Script_Name   => No_Action,
                           Arguments     => (others => False)));
      end loop;

      Info.Script_Name := Action_Name;
      Info.Function_Name :=
        Get_Symbol ("chaos-action-" & Get_Name (Action_Name));

      Action_Id_Map.Insert (Get_Name (Action_Name), Index);

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
         Get_Name (Full_Name)
         & " = " & Store.Show (Store.Top));
      Lith.Environment.Define (Full_Name, Store.Pop);

      Actions (Index) := Info;

      return Store.Argument (1);
   end Evaluate_Chaos_Add_Action;

   --------------------------
   -- Get_Action_Arguments --
   --------------------------

   function Get_Action_Arguments
     (Id : Positive)
      return Action_Argument_Names
   is
      Result : Action_Argument_Names (1 .. 6);
      Count  : Natural := 0;
      Args   : Argument_Flags renames
                 Actions (Id).Arguments;
   begin
      for Arg in Args'Range loop
         if Args (Arg) then
            Count := Count + 1;
            Result (Count) := Arg;
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Action_Arguments;

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
        or else Actions.Element (Index).Function_Name = No_Action
      then
         Chaos.Logging.Log
           ("ACTION",
            "warning: bad action id:" & Natural'Image (Index));
         Store.Push (False_Value);
         return;
      end if;

      Store.Push (Actions.Element (Index).Function_Name);
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
         if Text_2 = ""
           and then Actions.Element (Index).Arguments
           (Import.Actions.Text_2)
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
      elsif Actions.Element (Index).Arguments
        (Import.Actions.Text_2)
      then
         Store.Push
           (Get_Symbol (Text_1 (Text_1'First .. Text_1'First + 5)));
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
