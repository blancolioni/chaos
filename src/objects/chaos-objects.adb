with Ada.Characters.Handling;

with Lith.Environment;
with Lith.Objects.Symbols;

with Chaos.Expressions;

with Chaos.Localisation;

with Chaos.Logging;

package body Chaos.Objects is

   package Property_Maps is
     new WL.String_Maps (Property_Get_Function);

   package Class_Property_Maps is
     new WL.String_Maps (Property_Maps.Map, Property_Maps."=");

   Class_Properties : Class_Property_Maps.Map;

   function Get_Identifier_Property
     (Object : Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is (Chaos.Expressions.Store.To_Object (Object.Identifier));

   ------------------
   -- Add_Property --
   ------------------

   procedure Add_Property
     (Object         : Root_Chaos_Object_Record'Class;
      Name           : String;
      Get            : Property_Get_Function)
   is
      Class_Name : constant String :=
                     Object.Object_Database.Database_Class_Name;
   begin
      if not Class_Properties.Contains (Class_Name) then
         Object.Create_Property_Table;
      end if;

      Class_Properties (Class_Name).Insert (Name, Get);
   end Add_Property;

   ----------------
   -- Clear_Flag --
   ----------------

   procedure Clear_Flag
     (Object : in out Root_Chaos_Object_Record'Class;
      Name   : String)
   is
   begin
      if Object.Flags.Contains (Name) then
         Object.Flags.Delete (Name);
      end if;
   end Clear_Flag;

   ---------------------------
   -- Create_Property_Table --
   ---------------------------

   procedure Create_Property_Table
     (Object : Root_Chaos_Object_Record'Class)
   is
      Class_Name : constant String :=
                     Object.Object_Database.Database_Class_Name;
   begin
      Class_Properties.Insert (Class_Name, Property_Maps.Empty_Map);
      Class_Properties (Class_Name).Insert
        ("identifier", Get_Identifier_Property'Access);
   end Create_Property_Table;

   -------------------
   -- Define_Object --
   -------------------

   procedure Define_Object
     (Object : Root_Chaos_Object_Record'Class)
   is
   begin
      Lith.Environment.Define
        (Object.Object_Database.Database_Class_Name & "-" & Object.Identifier,
         Object.To_Expression);
   end Define_Object;

   ------------------
   -- Display_Name --
   ------------------

   overriding function Display_Name
     (Object : Root_Localised_Object_Record)
      return String
   is
   begin
      return Chaos.Localisation.Local_Text
        (Identifier (Object));
   end Display_Name;

   -----------
   -- Equal --
   -----------

   overriding function Equal
     (X, Y  : Object_Record_Interface;
      Store : Lith.Objects.Object_Store'Class)
      return Boolean
   is
      pragma Unreferenced (Store);
      use Memor;
   begin
      return X.Db = Y.Db and then X.Reference = Y.Reference;
   end Equal;

   --------------------
   -- Execute_Script --
   --------------------

   procedure Execute_Script
     (Object : Root_Chaos_Object_Record'Class)
   is
      use type Lith.Objects.Object;
      use Chaos.Expressions;

      procedure Set_Script_Executed
        (Entity : in out Memor.Root_Record_Type'Class);

      -------------------------
      -- Set_Script_Executed --
      -------------------------

      procedure Set_Script_Executed
        (Entity : in out Memor.Root_Record_Type'Class)
      is
      begin
         Root_Chaos_Object_Record'Class (Entity).Set_Flag ("script-executed");
      end Set_Script_Executed;

   begin
      if Object.Script /= Lith.Objects.Nil then
         Store.Evaluate (Object.Script,
                         Lith.Objects.Symbols.Get_Symbol ("this"),
                         Object.To_Expression);
         if not Object.Flag ("script_executed") then
            Object.Object_Database.Update
              (Object.Reference, Set_Script_Executed'Access);
         end if;
      end if;
   exception
      when others =>
         Chaos.Logging.Log
           (Object.Global_Setting_Name ("SCRIPT"),
            "Error executing script for " & Object.Identifier);
         Chaos.Logging.Log
           (Object.Global_Setting_Name ("SCRIPT"),
            Store.Show (Object.Script));
         raise;
   end Execute_Script;

   ----------
   -- Flag --
   ----------

   function Flag
     (Object : Root_Chaos_Object_Record'Class;
      Name   : String)
      return Boolean
   is
   begin
      return Object.Flags.Contains (Name)
        and then Object.Flags.Element (Name);
   end Flag;

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Object : Root_Chaos_Object_Record)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Object.Identity);
   end Identifier;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Object   : in out Root_Chaos_Object_Record'Class;
      Identity : String)
   is
   begin
      Object.Identity :=
        Ada.Strings.Unbounded.To_Unbounded_String (Identity);
   end Initialize;

   ---------------
   -- Is_Object --
   ---------------

   function Is_Object
     (Value : Lith.Objects.Object)
      return Boolean
   is
   begin
      return Lith.Objects.Is_External_Object (Value)
        and then Chaos.Expressions.Store.Get_External_Object (Value).all
      in Object_Record_Interface'Class;
   end Is_Object;

   ---------
   -- Log --
   ---------

   procedure Log
     (Object  : Root_Chaos_Object_Record'Class;
      Message : String)
   is
   begin
      Chaos.Logging.Log
        (Ada.Characters.Handling.To_Upper
           (Object.Object_Database.Database_Class_Name),
         Message);
   end Log;

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (Entity  : in out Object_Record_Interface;
      Store : in out Lith.Objects.Object_Store'Class;
      Mark  : not null access
        procedure (X : in out Lith.Objects.Object))
   is
      pragma Unreferenced (Store);

      procedure Mark_Object (Object : in out Memor.Root_Record_Type'Class);

      -----------------
      -- Mark_Object --
      -----------------

      procedure Mark_Object (Object : in out Memor.Root_Record_Type'Class) is
      begin
         Root_Chaos_Object_Record'Class (Object).Mark (Mark);
      end Mark_Object;

   begin
      Entity.Db.Update (Entity.Reference, Mark_Object'Access);
   end Mark;

   ----------
   -- Mark --
   ----------

   procedure Mark
     (Object     : in out Root_Chaos_Object_Record;
      Mark_Value : not null access
        procedure (Value : in out Lith.Objects.Object))
   is
   begin
      Mark_Value (Object.Script);
   end Mark;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Entity : Object_Record_Interface)
      return String
   is
   begin
      return Entity.Db.Database_Class_Name;
   end Name;

   -----------
   -- Print --
   -----------

   overriding function Print
     (Entity  : Object_Record_Interface;
      Store : in out Lith.Objects.Object_Store'Class)
      return String
   is
      pragma Unreferenced (Store);
   begin
      return Entity.Db.Database_Class_Name
        & "-" & Chaos_Object (Entity.Db.Element (Entity.Reference)).Identifier;
   end Print;

   --------------
   -- Property --
   --------------

   function Property
     (Object : Root_Chaos_Object_Record'Class;
      Name   : String)
      return Lith.Objects.Object
   is
      Class_Name : constant String :=
                     Object.Object_Database.Database_Class_Name;
      Empty_Map  : Property_Maps.Map;
   begin
      if not Class_Properties.Contains (Class_Name) then
         Class_Properties.Insert (Class_Name, Empty_Map);
         Class_Properties (Class_Name).Insert
           ("identifier", Get_Identifier_Property'Access);
      end if;

      if not Class_Properties (Class_Name).Contains (Name) then
         raise Constraint_Error with
           "no such property '" & Name & "' for class '" & Class_Name & "'";
      end if;

      declare
         Get : constant Property_Get_Function :=
                 Class_Properties.Element (Class_Name).Element (Name);
      begin
         return Get (Object);
      end;
   end Property;

   -----------------
   -- Save_Object --
   -----------------

   procedure Save_Object
     (Object : not null access constant Root_Chaos_Object_Record'Class)
   is
      use Chaos.Expressions;
      List_Name : constant String :=
                    "chaos-"
                    & Object.Object_Database.Database_Class_Name
                    & "-list";
      List_Symbol : constant Lith.Objects.Symbol_Type :=
                      Lith.Objects.Symbols.Get_Symbol (List_Name);
      List        : Lith.Objects.Object;
      Have_List   : Boolean;
   begin
      Lith.Environment.Get (List_Symbol, List, Have_List);
      if not Have_List then
         List := Lith.Objects.Nil;
         Lith.Environment.Define (List_Symbol, List);
         Object.Add_Properties;
      end if;

      Store.Push (Object.To_Expression);
      Store.Push (List);
      Store.Cons;
      Lith.Environment.Replace
        (List_Symbol, Store.Pop);
   end Save_Object;

   ------------
   -- Script --
   ------------

   function Script
     (Object : Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is
   begin
      return Object.Script;
   end Script;

   --------------
   -- Set_Flag --
   --------------

   procedure Set_Flag
     (Object : in out Root_Chaos_Object_Record'Class;
      Name   : String)
   is
   begin
      if Object.Flags.Contains (Name) then
         Object.Flags.Replace (Name, True);
      else
         Object.Flags.Insert (Name, True);
      end if;
   end Set_Flag;

   ----------------
   -- Set_Script --
   ----------------

   procedure Set_Script
     (Object : in out Root_Chaos_Object_Record'Class;
      Script : Lith.Objects.Object)
   is
   begin
      Object.Script := Script;
   end Set_Script;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression
     (Object : Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is
   begin
      return Chaos.Expressions.Store.Create_External_Reference
        (Object_Record_Interface'
           (Db        => Object.Object_Database,
            Reference => Object.Reference));
   end To_Expression;

   ---------------
   -- To_Object --
   ---------------

   function To_Object
     (Value : Lith.Objects.Object)
      return Chaos_Object
   is
      Object : Object_Record_Interface'Class renames
                 Object_Record_Interface'Class
                   (Chaos.Expressions.Store.Get_External_Object (Value).all);
   begin
      return Chaos_Object (Object.Db.Element (Object.Reference));
   end To_Object;

end Chaos.Objects;
