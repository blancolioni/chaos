with Ada.Containers.Vectors;

with WL.String_Maps;

with Lith.Objects.Interfaces;

with Chaos.Logging;
with Chaos.Paths;

with Chaos.Identifiers;

with Chaos.Expressions.Import.Objects;

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
      use Lith.Objects;
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

      Store.Push ("lambda");
      Store.Push ("object-1");
      Store.Push ("object-2");
      Store.Push ("object-3");
      Store.Push ("integer-1");
      Store.Push ("x");
      Store.Push ("y");
      Store.Push ("integer-2");
      Store.Push ("integer-3");
      Store.Push ("text-1");
      Store.Push ("text-2");
      Store.Create_List (10);
      Store.Push (Store.Argument (3));
      Store.Create_List (3);
      Chaos.Logging.Log
        ("ACTION",
         Get_Name (Action_Name)
         & " = " & Store.Show (Store.Top));
      Store.Define_Top_Level
        (Full_Name, Store.Pop);

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
   -- Get_Action_Id --
   -------------------

   function Get_Action_Id
     (Name : String)
      return Natural
   is
   begin
      if Action_Id_Map.Contains (Name) then
         return Action_Id_Map (Name);
      else
         return 0;
      end if;
   end Get_Action_Id;

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
      use Lith.Objects;
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

      Store.Push (To_Object (Actions.Element (Index).Function_Name));
      Store.Push (Store.Top (3, Secondary));
      Store.Push (Store.Top (2, Secondary));
      Store.Push (Store.Top (1, Secondary));

      Store.Push (To_Object (Integer_1));
      Store.Push (To_Object (X));
      Store.Push (To_Object (Y));
      Store.Push (To_Object (Integer_2));
      Store.Push (To_Object (Integer_3));
      Store.Push (Single_Quote);
      if Text_1 /= "" then
         if Text_2 = ""
           and then Actions.Element (Index).Arguments
           (Import.Actions.Text_2)
         then
            Store.Push
              (Text_1 (Text_1'First + 6 .. Text_1'Last));
         else
            Store.Push (Text_1);
         end if;
      else
         Store.Push_Nil;
      end if;
      Store.Create_List (2);
      Store.Push (Single_Quote);
      if Text_2 /= "" then
         Store.Push (Text_2);
      elsif Actions.Element (Index).Arguments
        (Import.Actions.Text_2)
      then
         Store.Push
           (Text_1 (Text_1'First .. Text_1'First + 5));
      else
         Store.Push_Nil;
      end if;
      Store.Create_List (2);
      Store.Create_List (11);

   end Import_Action;

   -------------------
   -- Import_Action --
   -------------------

   procedure Import_Action
     (Call : Function_Call)
   is
      use Ada.Strings.Unbounded;
      Action_Name     : constant String :=
                          To_String (Call.Name);
      Action_Id       : constant Natural :=
                          Get_Action_Id (Action_Name);
      Integer_1_Value : Integer := 0;
      Integer_2_Value : Integer := 0;
      Integer_3_Value : Integer := 0;
      Text_1_Value    : Unbounded_String;
      Text_2_Value    : Unbounded_String;
      X, Y            : Integer := 0;
      Available       : Argument_Flags;
      Have_Object     : array (Object_Argument) of Boolean :=
                          (others => False);
   begin

      if Action_Id = 0 then
         Chaos.Logging.Log
           ("Action", "unknown Action: " & Action_Name);
         Chaos.Expressions.Store.Push (Lith.Objects.False_Value);
         return;
      end if;

      Available :=
        Actions.Element (Action_Id).Arguments;

      for Arg of Call.Args loop
         case Arg.Arg_Type is
            when Integer_Argument =>
               if Available (Integer_1) then
                  Integer_1_Value := Arg.Integer_Value;
                  Available (Integer_1) := False;
               elsif Available (Integer_2) then
                  Integer_2_Value := Arg.Integer_Value;
                  Available (Integer_2) := False;
               elsif Available (Integer_3) then
                  Integer_3_Value := Arg.Integer_Value;
                  Available (Integer_3) := False;
               else
                  Chaos.Logging.Log
                    ("Action",
                     "warning: too many integer arguments in call to "
                     & Action_Name);
               end if;
            when Text_Argument =>
               if Available (Text_1) then
                  Text_1_Value := Arg.Text_Value;
                  Available (Text_1) := False;
               elsif Available (Text_2) then
                  Text_2_Value := Arg.Text_Value;
                  Available (Text_2) := True;
               elsif Available (Object_1) then
                  Objects.Import_Object_Name
                    (To_String (Arg.Text_Value));
                  Available (Object_1) := False;
                  Have_Object (Object_1) := True;
               elsif Available (Object_2) then
                  Objects.Import_Object_Name
                    (To_String (Arg.Text_Value));
                  Available (Object_2) := False;
                  Have_Object (Object_2) := True;
               elsif Available (Object_3) then
                  Objects.Import_Object_Name
                    (To_String (Arg.Text_Value));
                  Available (Object_3) := False;
                  Have_Object (Object_3) := True;
               else
                  Chaos.Logging.Log
                    ("Action",
                     "warning: too many string arguments in call to "
                     & Action_Name);
               end if;

            when Identifier_Argument =>
               declare
                  Id : constant String := To_String (Arg.Identifier_Name);
               begin
                  if Chaos.Identifiers.Exists ("object", Id) then
                     if Available (Object_1)
                       or else Available (Object_2)
                       or else Available (Object_3)
                     then
                        Objects.Import_Object_Identifier (Id);
                        if Available (Object_1) then
                           Available (Object_1) := False;
                           Have_Object (Object_1) := True;
                        elsif Available (Object_2) then
                           Available (Object_2) := False;
                           Have_Object (Object_2) := True;
                        else
                           Available (Object_3) := False;
                           Have_Object (Object_3) := True;
                        end if;
                     else
                        Chaos.Logging.Log
                          ("Action",
                           "warning: too many object arguments in call to "
                           & Action_Name);
                     end if;
                  else
                     Chaos.Logging.Log
                       ("Action",
                        "warning: undeclared object identifier '"
                        & Id
                        & " in call to "
                        & Action_Name);
                  end if;
               end;

            when Tuple_Argument =>
               if Available (Point_X)
                 and then Arg.Tuple.Element'Length = 2
               then
                  X := Arg.Tuple.Element (1);
                  Y := Arg.Tuple.Element (2);
                  Available (Point_X) := False;
                  Available (Point_Y) := False;
               elsif Available (Object_1)
                 or else Available (Object_2)
                 or else Available (Object_3)
               then
                  Objects.Import_Object_Tuple
                    (Arg.Tuple.Element);
                  if Available (Object_1) then
                     Available (Object_1) := False;
                     Have_Object (Object_1) := True;
                  elsif Available (Object_2) then
                     Available (Object_2) := False;
                     Have_Object (Object_2) := True;
                  else
                     Available (Object_3) := False;
                     Have_Object (Object_3) := True;
                  end if;
               else
                  Chaos.Logging.Log
                    ("Action",
                     "warning: extra point arguments in call to "
                     & Action_Name);
               end if;

         end case;
      end loop;

      for Obj in Object_Argument loop
         if not Have_Object (Obj) then
            Chaos.Expressions.Store.Push
              (Lith.Objects.Single_Quote);
            Chaos.Expressions.Store.Push_Nil;
            Chaos.Expressions.Store.Create_List (2);
            Chaos.Expressions.Store.Push
              (Chaos.Expressions.Store.Pop, Lith.Objects.Secondary);
         end if;
      end loop;

      Chaos.Expressions.Import.Actions.Import_Action
        (WL.Binary_IO.Word_32 (Action_Id),
         Integer_1_Value, Integer_2_Value, Integer_3_Value,
         X, Y,
         To_String (Text_1_Value), To_String (Text_2_Value));

      Store.Drop (3, Lith.Objects.Secondary);

   end Import_Action;

   ------------------
   -- Load_Actions --
   ------------------

   procedure Load_Actions is
   begin
      Lith.Objects.Interfaces.Define_Function
        (Chaos.Expressions.Store, "chaos-add-action",
         Evaluate_Chaos_Add_Action'Access);
      No_Action := Lith.Objects.Get_Symbol ("no-action");

      if not Chaos.Expressions.Store.Load
        (Chaos.Paths.Config_File
           ("script/actions.scm"))
      then
         raise Constraint_Error with
           "cannot load action configuration";
      end if;
   end Load_Actions;

end Chaos.Expressions.Import.Actions;
