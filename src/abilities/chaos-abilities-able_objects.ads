private with Lith.Objects;

with Chaos.Objects;

generic
   type Able_Object is new Chaos.Objects.Root_Chaos_Object_Record
     and Ability_Interface
   with private;
package Chaos.Abilities.Able_Objects is

   procedure Add_Ability_Properties
     (Object : Able_Object'Class);

private

   function Get_Str_Fn
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is (Lith.Objects.To_Object
       (Integer (Able_Object'Class (Object).Ability_Score (Str))));

   function Get_Con_Fn
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is (Lith.Objects.To_Object
       (Integer (Able_Object'Class (Object).Ability_Score (Con))));

   function Get_Dex_Fn
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is (Lith.Objects.To_Object
       (Integer (Able_Object'Class (Object).Ability_Score (Dex))));

   function Get_Int_Fn
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is (Lith.Objects.To_Object
       (Integer (Able_Object'Class (Object).Ability_Score (Int))));

   function Get_Wis_Fn
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is (Lith.Objects.To_Object
       (Integer (Able_Object'Class (Object).Ability_Score (Wis))));

   function Get_Cha_Fn
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is (Lith.Objects.To_Object
       (Integer (Able_Object'Class (Object).Ability_Score (Cha))));

   Get_Str : constant Chaos.Objects.Property_Get_Function :=
               Get_Str_Fn'Access;
   Get_Con : constant Chaos.Objects.Property_Get_Function :=
               Get_Con_Fn'Access;
   Get_Dex : constant Chaos.Objects.Property_Get_Function :=
               Get_Dex_Fn'Access;
   Get_Int : constant Chaos.Objects.Property_Get_Function :=
               Get_Int_Fn'Access;
   Get_Wis : constant Chaos.Objects.Property_Get_Function :=
               Get_Wis_Fn'Access;
   Get_Cha : constant Chaos.Objects.Property_Get_Function :=
               Get_Cha_Fn'Access;

end Chaos.Abilities.Able_Objects;
