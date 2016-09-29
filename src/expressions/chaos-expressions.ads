private with Ada.Containers.Doubly_Linked_Lists;

private with WL.Handles;
private with WL.String_Maps;

package Chaos.Expressions is

   type Chaos_Environment is private;

   function New_Environment
     (Base : Chaos_Environment)
      return Chaos_Environment;

   type Chaos_Expression is private;

   type Array_Of_Expressions is
     array (Positive range <>) of Chaos_Expression;

   function No_Array return Array_Of_Expressions;

   function Undefined_Value return Chaos_Expression;
   function Null_Value return Chaos_Expression;

   function Always return Chaos_Expression;
   function Never return Chaos_Expression;

   function To_Boolean
     (Expression  : Chaos_Expression)
      return Boolean;

   function To_Integer
     (Expression  : Chaos_Expression)
      return Chaos_Expression;

   procedure Execute
     (Expression  : Chaos_Expression;
      Environment : Chaos_Environment);

   function Evaluate
     (Expression  : Chaos_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   procedure Execute
     (Expression  : Chaos_Expression);

   function Evaluate
     (Expression  : Chaos_Expression)
      return Chaos_Expression;

   function Evaluate
     (Expression  : Chaos_Expression;
      Name        : String;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   function To_String
     (Expression : Chaos_Expression)
      return String;

   procedure Insert
     (Environment : in out Chaos_Environment;
      Name        : String;
      Value       : Chaos_Expression);

   function Find
     (Environment : Chaos_Environment;
      Name        : String)
      return Chaos_Expression;

   function Contains
     (Environment : Chaos_Environment;
      Name        : String)
      return Boolean;

private

   type Root_Chaos_Expression_Record is abstract tagged null record;

   function Is_Atom
     (Expression : Root_Chaos_Expression_Record)
      return Boolean
   is (True);

   function Is_Function
     (Expression : Root_Chaos_Expression_Record)
      return Boolean
   is (not (Root_Chaos_Expression_Record'Class (Expression).Is_Atom));

   function Evaluate
     (Expression  : Root_Chaos_Expression_Record;
      Environment : Chaos_Environment)
      return Chaos_Expression
      is abstract;

   function To_Boolean
     (Expression  : Root_Chaos_Expression_Record)
      return Boolean
      is abstract;

   function Apply
     (Expression  : Root_Chaos_Expression_Record;
      Argument    : Chaos_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
      is abstract;

   function Equal
     (Left  : Root_Chaos_Expression_Record;
      Right : Root_Chaos_Expression_Record'Class)
      return Boolean
   is (False);

   procedure Set
     (Expression  : in out Root_Chaos_Expression_Record;
      Name        : String;
      Value       : Chaos_Expression)
   is null;

   function Get
     (Expression  : in out Root_Chaos_Expression_Record;
      Name        : String)
      return Chaos_Expression
   is (Undefined_Value);

   function Local_Environment
     (Expression  : Root_Chaos_Expression_Record)
      return Chaos_Environment;

   function To_Integer
     (Expression  : Root_Chaos_Expression_Record)
      return Chaos_Expression;

   function To_String
     (Expression  : Root_Chaos_Expression_Record)
      return String
      is abstract;

   package Expression_Handles is
     new WL.Handles (Root_Chaos_Expression_Record'Class);

   type Chaos_Expression is
     new Expression_Handles.Handle_Type;

   function Express
     (From : Root_Chaos_Expression_Record'Class)
      return Chaos_Expression
   is (Chaos_Expression (Expression_Handles.Create (From)));

   package Expression_Maps is
     new WL.String_Maps (Chaos_Expression);

   type Chaos_Environment_Table_Record is
      record
         Map    : Expression_Maps.Map;
      end record;

   package Environment_Table_Handles is
     new WL.Handles (Chaos_Environment_Table_Record);

   type Chaos_Environment_Table is
     new Environment_Table_Handles.Handle_Type;

   package Table_Stacks is
     new Ada.Containers.Doubly_Linked_Lists (Chaos_Environment_Table);

   type Chaos_Environment is
      record
         Tables : Table_Stacks.List;
      end record;

   procedure Push_Table
     (Env : in out Chaos_Environment);

   procedure Pop_Table
     (Env : in out Chaos_Environment);

   type Constant_Chaos_Expression_Record is
     abstract new Root_Chaos_Expression_Record with null record;

   overriding function To_Boolean
     (Expression  : Constant_Chaos_Expression_Record)
      return Boolean
   is (True);

   overriding function Evaluate
     (Expression  : Constant_Chaos_Expression_Record;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is (Chaos_Expression (Expression_Handles.Create (Expression)));

   overriding function Apply
     (Expression  : Constant_Chaos_Expression_Record;
      Argument    : Chaos_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is (Chaos_Expression (Expression_Handles.Create (Expression)));

   Standard_Env : Chaos_Environment;

end Chaos.Expressions;
