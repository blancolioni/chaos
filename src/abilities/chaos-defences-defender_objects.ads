private with Lith.Objects;

with Chaos.Objects;

generic
   type Defender_Object is new Chaos.Objects.Root_Chaos_Object_Record
     and Defence_Interface
   with private;
package Chaos.Defences.Defender_Objects is

   procedure Add_Defence_Properties
     (Object : Defender_Object'Class);

private

   function Get_Fort_Fn
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is (Lith.Objects.To_Object
       (Integer (Defender_Object'Class (Object).Defence_Score (Fort))));

   function Get_Refl_Fn
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is (Lith.Objects.To_Object
       (Integer (Defender_Object'Class (Object).Defence_Score (Refl))));

   function Get_Will_Fn
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is (Lith.Objects.To_Object
       (Integer (Defender_Object'Class (Object).Defence_Score (Will))));

   function Get_AC_Fn
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is (Lith.Objects.To_Object
       (Integer (Defender_Object'Class (Object).Defence_Score (AC))));

   Get_Fort : constant Chaos.Objects.Property_Get_Function :=
               Get_Fort_Fn'Access;
   Get_Refl : constant Chaos.Objects.Property_Get_Function :=
               Get_Refl_Fn'Access;
   Get_Will : constant Chaos.Objects.Property_Get_Function :=
               Get_Will_Fn'Access;
   Get_AC   : constant Chaos.Objects.Property_Get_Function :=
               Get_AC_Fn'Access;

end Chaos.Defences.Defender_Objects;
