package Chaos.Objects.Search is

   function Find_Object
     (Code     : String;
      Creature : Boolean := True;
      Entity     : Boolean := True)
      return Chaos_Object;

end Chaos.Objects.Search;
