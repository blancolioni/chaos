generic
   type Object_Type is new Root_Chaos_Object_Record with private;
   type Property_Type is (<>);
   with function Get (Object : Object_Type'Class) return Property_Type;
package Chaos.Objects.Enum_Properties is

   function Get_Property
     (Object : Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object;

end Chaos.Objects.Enum_Properties;
