package body Chaos.Objects.Number_Properties is

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is
   begin
      return Lith.Objects.To_Object
        (Integer (Get (Object_Type'Class (Object))));
   end Get_Property;

end Chaos.Objects.Number_Properties;
