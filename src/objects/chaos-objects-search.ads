package Chaos.Objects.Search is

   function Find_Object
     (Code       : String;
      Creature   : Boolean := True;
      Entity     : Boolean := True;
      Feature    : Boolean := True)
      return Chaos_Object;

   function Find_Entity_Object
     (Code : String)
      return Chaos_Object;

   function Find_Creature_Object
     (Code : String)
      return Chaos_Object;

end Chaos.Objects.Search;
