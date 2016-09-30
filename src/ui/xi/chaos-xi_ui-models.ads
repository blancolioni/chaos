with Xi.Scene;

private package Chaos.Xi_UI.Models is

   type Root_Chaos_Xi_Model is abstract tagged private;

   type Chaos_Xi_Model is access all Root_Chaos_Xi_Model'Class;

   function Scene
     (Model : Root_Chaos_Xi_Model)
      return Xi.Scene.Xi_Scene
      is abstract;

   function Show_Log
     (Model : Root_Chaos_Xi_Model)
      return Boolean
   is (True);

private

   type Root_Chaos_Xi_Model is abstract tagged
      record
         null;
      end record;

end Chaos.Xi_UI.Models;
