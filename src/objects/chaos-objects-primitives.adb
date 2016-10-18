with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.Objects.Search;

with Chaos.Entities;
with Chaos.Items;

package body Chaos.Objects.Primitives is

   function Evaluate_Chaos_Flag
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Has_Item
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Object_With_Code
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   --------------------
   -- Add_Primitives --
   --------------------

   procedure Add_Primitives is
   begin
      Lith.Objects.Interfaces.Define_Function
        ("chaos-flag", 2, Evaluate_Chaos_Flag'Access);
      Lith.Objects.Interfaces.Define_Function
        ("chaos-has-item", 2, Evaluate_Chaos_Has_Item'Access);
      Lith.Objects.Interfaces.Define_Function
        ("chaos-object-with-code", 1, Evaluate_Chaos_Object_With_Code'Access);
   end Add_Primitives;

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
         begin
            return Lith.Objects.To_Object
              (Inv.Has_Entity (Chaos.Entities.Chaos_Entity (Item)));
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
         raise Constraint_Error with
           "cannot find object with code '"
           & Code & "'";
      end if;
      return Result.To_Expression;
   end Evaluate_Chaos_Object_With_Code;

end Chaos.Objects.Primitives;
