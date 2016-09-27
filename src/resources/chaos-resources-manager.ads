package Chaos.Resources.Manager is

   function Load_Resource
     (Reference  : Resource_Reference;
      Res_Type   : Resource_Type)
      return access constant Chaos_Resource'Class;

end Chaos.Resources.Manager;
