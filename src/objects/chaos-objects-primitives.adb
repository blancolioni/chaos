with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.Objects.Search;

package body Chaos.Objects.Primitives is

   function Evaluate_Chaos_Flag
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   --------------------
   -- Add_Primitives --
   --------------------

   procedure Add_Primitives is
   begin
      Lith.Objects.Interfaces.Define_Function
        ("chaos-flag", 2, Evaluate_Chaos_Flag'Access);
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

end Chaos.Objects.Primitives;
