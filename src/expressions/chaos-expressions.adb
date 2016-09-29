package body Chaos.Expressions is

   type Primitive_Constant_Expression is
     new Constant_Chaos_Expression_Record with
      record
         Name       : access String;
         Bool_Value : Boolean;
      end record;

   overriding function To_String
     (Primitive : Primitive_Constant_Expression)
      return String
   is (Primitive.Name.all);

   overriding function To_Boolean
     (Primitive : Primitive_Constant_Expression)
      return Boolean
   is (Primitive.Bool_Value);

   Local_Always_Value    : Chaos_Expression;
   Local_Never_Value      : Chaos_Expression;
--     Local_Null_Value      : Chaos_Expression;
   Local_Undefined_Value : Chaos_Expression;

   ------------
   -- Always --
   ------------

   function Always return Chaos_Expression is
   begin
      if Is_Null (Local_Always_Value) then
         declare
            Rec : constant Primitive_Constant_Expression :=
                    (Constant_Chaos_Expression_Record with
                     Name => new String'("always"), Bool_Value => True);
         begin
            Local_Always_Value := Create (Rec);
         end;
      end if;
      return Local_Always_Value;
   end Always;

   --------------
   -- Contains --
   --------------

   function Contains
     (Environment : Chaos_Environment;
      Name        : String)
      return Boolean
   is
   begin
      for Table of Environment.Tables loop
         if Get (Table).Map.Contains (Name) then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Environment : Chaos_Environment;
      Expression  : Chaos_Expression)
      return Chaos_Expression
   is
   begin
      if Is_Null (Expression) then
         return Expression;
      else
         return Get (Expression).Evaluate (Environment);
      end if;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Environment : Chaos_Environment;
      Expression  : Chaos_Expression;
      Name        : String;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression
   is
      Local : constant Chaos_Environment := Get (Expression).Local_Environment;
   begin
      if not Contains (Local, Name) then
         return Undefined_Value;
      else
         return Get (Find (Local, Name)).Apply
           (Environment, Expression & Arguments);
      end if;
   end Evaluate;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Environment : Chaos_Environment;
      Expression  : Chaos_Expression)
   is
      Ignored : constant Chaos_Expression :=
                  Evaluate (Environment, Expression);
      pragma Unreferenced (Ignored);
   begin
      null;
   end Execute;

   ----------
   -- Find --
   ----------

   function Find
     (Environment : Chaos_Environment;
      Name        : String)
      return Chaos_Expression
   is
   begin
      for Table of Environment.Tables loop
         if Get (Table).Map.Contains (Name) then
            return Get (Table).Map.Element (Name);
         end if;
      end loop;
      return Undefined_Value;
   end Find;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Environment : in out Chaos_Environment;
      Name        : String;
      Value       : Chaos_Expression)
   is
   begin
      if Environment.Tables.Is_Empty then
         null;
      else
         declare
            Table : Chaos_Environment_Table renames
                      Environment.Tables.First_Element;
         begin
            if Get (Table).Map.Contains (Name) then
               Set (Table).Map.Replace (Name, Value);
            else
               Set (Table).Map.Insert (Name, Value);
            end if;
         end;
      end if;
   end Insert;

   -----------------------
   -- Local_Environment --
   -----------------------

   function Local_Environment
     (Expression  : Root_Chaos_Expression_Record)
      return Chaos_Environment
   is
      pragma Unreferenced (Expression);
   begin
      return Env : Chaos_Environment do
         null;
      end return;
   end Local_Environment;

   -----------
   -- Never --
   -----------

   function Never return Chaos_Expression is
   begin
      if Is_Null (Local_Never_Value) then
         declare
            Rec : constant Primitive_Constant_Expression :=
                    (Constant_Chaos_Expression_Record with
                     Name => new String'("never"), Bool_Value => False);
         begin
            Local_Never_Value := Create (Rec);
         end;
      end if;
      return Local_Never_Value;
   end Never;

   ---------------------
   -- New_Environment --
   ---------------------

   function New_Environment
     (Base : Chaos_Environment)
      return Chaos_Environment
   is
   begin
      return Environment : Chaos_Environment := Base do
         Push_Table (Environment);
      end return;
   end New_Environment;

   --------------
   -- No_Array --
   --------------

   function No_Array return Array_Of_Expressions is
   begin
      return Result : Array_Of_Expressions (1 .. 0) do
         null;
      end return;
   end No_Array;

   ----------------
   -- Null_Value --
   ----------------

   function Null_Value return Chaos_Expression is
   begin
      return Chaos_Expression (Expression_Handles.Null_Handle);

--        if Is_Null (Local_Null_Value) then
--           declare
--              Rec : constant Primitive_Constant_Expression :=
--                      (Constant_Chaos_Expression_Record with
--                       Name => new String'("null"), Bool_Value => False);
--           begin
--              Local_Null_Value := Create (Rec);
--           end;
--        end if;
--        return Local_Null_Value;
   end Null_Value;

   ---------------
   -- Pop_Table --
   ---------------

   procedure Pop_Table
     (Env : in out Chaos_Environment)
   is
   begin
      Env.Tables.Delete_First;
   end Pop_Table;

   ----------------
   -- Push_Table --
   ----------------

   procedure Push_Table
     (Env : in out Chaos_Environment)
   is
      Table : Chaos_Environment_Table_Record;
   begin
      Env.Tables.Insert (Env.Tables.First, Create (Table));
   end Push_Table;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean
     (Expression  : Chaos_Expression)
      return Boolean
   is
   begin
      if Is_Null (Expression) then
         return False;
      else
         return Get (Expression).To_Boolean;
      end if;
   end To_Boolean;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer
     (Expression  : Chaos_Expression)
      return Chaos_Expression
   is
   begin
      return Get (Expression).To_Integer;
   end To_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer
     (Expression  : Root_Chaos_Expression_Record)
      return Chaos_Expression
   is
      Expr_Class : Root_Chaos_Expression_Record'Class renames
                     Root_Chaos_Expression_Record'Class (Expression);
      Env        : constant Chaos_Environment :=
                     Expr_Class.Local_Environment;
   begin
      if Contains (Env, "to_integer") then
         return Evaluate (Env, Create (Expr_Class), "to_integer",
                          No_Array);
      else
         return Undefined_Value;
      end if;
   end To_Integer;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Expression : Chaos_Expression)
      return String
   is
   begin
      if Is_Null (Expression) then
         return "null";
      else
         return Get (Expression).To_String;
      end if;
   end To_String;

   ---------------------
   -- Undefined_Value --
   ---------------------

   function Undefined_Value return Chaos_Expression is
   begin
      if Is_Null (Local_Undefined_Value) then
         declare
            Rec : constant Primitive_Constant_Expression :=
                    (Constant_Chaos_Expression_Record with
                     Name => new String'("undefined"), Bool_Value => False);
         begin
            Local_Undefined_Value := Create (Rec);
         end;
      end if;
      return Local_Undefined_Value;
   end Undefined_Value;

end Chaos.Expressions;
