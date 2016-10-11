package Chaos.Resources.Manager is

   function Load_Resource
     (Reference  : Resource_Reference;
      Res_Type   : Resource_Type)
      return access constant Chaos_Resource'Class;

   function Resource_Exists
     (Reference  : Resource_Reference;
      Res_Type   : Resource_Type)
      return Boolean;

end Chaos.Resources.Manager;
