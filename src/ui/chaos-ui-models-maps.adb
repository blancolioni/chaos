package body Chaos.UI.Models.Maps is

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Map  : Model_Map;
      Name : String)
      return Boolean
   is
   begin
      return Model_Maps.Map (Map).Contains (Name);
   end Contains;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Map  : Model_Map;
      Name : String)
      return UI_Model
   is
   begin
      return Model_Maps.Map (Map).Element (Name);
   end Element;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Map   : in out Model_Map;
      Name  : String;
      Model : UI_Model)
   is
   begin
      Model_Maps.Map (Map).Insert (Name, Model);
   end Insert;

end Chaos.UI.Models.Maps;
