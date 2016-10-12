package body Chaos.Objects.Enum_Properties is

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is
      Value : constant Natural :=
                Property_Type'Pos (Get (Object_Type'Class (Object)));
   begin
      return Lith.Objects.To_Object (Value);
   end Get_Property;

end Chaos.Objects.Enum_Properties;
