with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

package body Chaos.Game.Primitives is

   function Evaluate_Chaos_Script_Flag
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   --------------------
   -- Add_Primitives --
   --------------------

   procedure Add_Primitives is
      use Lith.Objects.Interfaces;
   begin
      Define_Function ("chaos-script-flag", 3,
                       Evaluate_Chaos_Script_Flag'Access);
   end Add_Primitives;

   --------------------------------
   -- Evaluate_Chaos_Script_Flag --
   --------------------------------

   function Evaluate_Chaos_Script_Flag
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Map    : constant Lith.Objects.Object :=
                 Store.Argument (1);
      Group  : constant String :=
                 Lith.Objects.Symbols.Get_Name
                   (Lith.Objects.To_Symbol (Store.Argument (2)));
      Name   : constant String :=
                 Lith.Objects.Symbols.Get_Name
                   (Lith.Objects.To_Symbol (Store.Argument (3)));
      Result : Boolean := False;
   begin
      if Chaos.Objects.Is_Object (Map) then
         Result :=
           Chaos.Game.Current_Game.Script_Flag
             (Chaos.Objects.To_Object (Map), Group, Name);
      else
         raise Constraint_Error with
           "chaos-get-property: expected an object, but found "
           & Store.Show (Map);
      end if;
      return Lith.Objects.To_Object (Result);
   end Evaluate_Chaos_Script_Flag;

end Chaos.Game.Primitives;
