with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.Objects.Search;

with Chaos.Entities;
with Chaos.Items;

with Chaos.Logging;

------------------------------
-- Chaos.Objects.Primitives --
------------------------------

package body Chaos.Objects.Primitives is

   function Evaluate_Chaos_Clear_Flag
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Flag
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Has_Item
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Object_With_Code
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Set_Flag
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Set_Trigger
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is (Store.Argument (1));

   procedure Set_Flag_Value
     (Store : in out Lith.Objects.Object_Store'Class;
      Value : Boolean);

   --------------------
   -- Add_Primitives --
   --------------------

   procedure Add_Primitives is
   begin
      Lith.Objects.Interfaces.Define_Function
        ("chaos-flag", 2, Evaluate_Chaos_Flag'Access);
      Lith.Objects.Interfaces.Define_Function
        ("chaos-set-flag", 2, Evaluate_Chaos_Set_Flag'Access);
      Lith.Objects.Interfaces.Define_Function
        ("chaos-clear-flag", 2, Evaluate_Chaos_Clear_Flag'Access);
      Lith.Objects.Interfaces.Define_Function
        ("chaos-has-item", 2, Evaluate_Chaos_Has_Item'Access);
      Lith.Objects.Interfaces.Define_Function
        ("chaos-object-with-code", 1, Evaluate_Chaos_Object_With_Code'Access);
      Lith.Objects.Interfaces.Define_Function
        ("chaos-set-trigger", 2, Evaluate_Chaos_Set_Trigger'Access);
   end Add_Primitives;

   -------------------------------
   -- Evaluate_Chaos_Clear_Flag --
   -------------------------------

   function Evaluate_Chaos_Clear_Flag
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      Set_Flag_Value (Store, False);
      return Store.Argument (1);
   end Evaluate_Chaos_Clear_Flag;

   -------------------------
   -- Evaluate_Chaos_Flag --
   -------------------------

   function Evaluate_Chaos_Flag
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects, Lith.Objects.Symbols;
      This : constant Object := Store.Argument (1);
      Object : constant Chaos_Object :=
                 (if Is_Object (This)
                  then To_Object (This)
                  elsif Is_Symbol (This)
                  then Search.Find_Object
                    (Get_Name (To_Symbol (This)))
                  else raise Constraint_Error
                    with "expected a code or object, but found "
                  & Store.Show (This));
      Result : Boolean;
   begin
      if Object = null then
         Result := False;
      else
         Result := Object.Flag (Get_Name (To_Symbol (Store.Argument (2))));
      end if;
      return To_Object (Result);
   end Evaluate_Chaos_Flag;

   -----------------------------
   -- Evaluate_Chaos_Has_Item --
   -----------------------------

   function Evaluate_Chaos_Has_Item
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      if Is_Object (Store.Argument (1))
        and then To_Object (Store.Argument (1)).all in
          Chaos.Items.Inventory_Interface'Class
      then
         declare
            Inv : Chaos.Items.Inventory_Interface'Class renames
                    Chaos.Items.Inventory_Interface'Class
                      (To_Object (Store.Argument (1)).all);
            Item : constant Chaos_Object :=
                     Search.Find_Entity_Object
                       (Lith.Objects.Symbols.Get_Name
                          (Lith.Objects.To_Symbol
                             (Store.Argument (2))));
            Result : constant Boolean :=
                       Inv.Has_Entity (Chaos.Entities.Chaos_Entity (Item));
         begin
            return Lith.Objects.To_Object (Result);
         end;
      else
         raise Constraint_Error with
           "expected an object for chaos-has-item, but found '"
           & Store.Show (Store.Argument (1));
      end if;
   end Evaluate_Chaos_Has_Item;

   -------------------------------------
   -- Evaluate_Chaos_Object_With_Code --
   -------------------------------------

   function Evaluate_Chaos_Object_With_Code
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Code : constant String :=
               Lith.Objects.Symbols.Get_Name
                 (Lith.Objects.To_Symbol (Store.Argument (1)));
      Result : constant Chaos_Object :=
                 Chaos.Objects.Search.Find_Object (Code);
   begin
      if Result = null then
         Chaos.Logging.Log ("SCRIPT", "warning: object with code '"
                              & Code & "' cannot be found");
         return Lith.Objects.Nil;
      end if;
      return Result.To_Expression;
   end Evaluate_Chaos_Object_With_Code;

   -----------------------------
   -- Evaluate_Chaos_Set_Flag --
   -----------------------------

   function Evaluate_Chaos_Set_Flag
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      Set_Flag_Value (Store, True);
      return Store.Argument (1);
   end Evaluate_Chaos_Set_Flag;

   --------------------
   -- Set_Flag_Value --
   --------------------

   procedure Set_Flag_Value
     (Store : in out Lith.Objects.Object_Store'Class;
      Value : Boolean)
   is
      use Lith.Objects, Lith.Objects.Symbols;
      This   : constant Object := Store.Argument (1);
      Object : constant Chaos_Object :=
                 (if Is_Object (This)
                  then To_Object (This)
                  elsif Is_Symbol (This)
                  then Search.Find_Object
                    (Get_Name (To_Symbol (This)))
                  else raise Constraint_Error
                    with "expected a code or object, but found "
                  & Store.Show (This));
      Flag   : constant String :=
                 Lith.Objects.Symbols.Get_Name
                   (Lith.Objects.To_Symbol (Store.Argument (2)));

      procedure Update_Flag (Rec : in out Memor.Root_Record_Type'Class);

      -----------------
      -- Update_Flag --
      -----------------

      procedure Update_Flag (Rec : in out Memor.Root_Record_Type'Class) is
      begin
         if Value then
            Root_Chaos_Object_Record'Class (Rec).Set_Flag (Flag);
         else
            Root_Chaos_Object_Record'Class (Rec).Clear_Flag (Flag);
         end if;
      end Update_Flag;

   begin
      if Object /= null then
         Object.Object_Database.Update
           (Object.Reference, Update_Flag'Access);

      end if;
   end Set_Flag_Value;

end Chaos.Objects.Primitives;
