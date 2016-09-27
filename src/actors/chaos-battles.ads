package Chaos.Battles is

   type Battle_Manager_Interface is interface;

   procedure Start_Battle
     (Battle : in out Battle_Manager_Interface)
   is null;

   procedure End_Battle
     (Battle : in out Battle_Manager_Interface)
   is null;

   function Active_Battle
     (Battle : Battle_Manager_Interface)
      return Boolean
      is abstract;

end Chaos.Battles;
