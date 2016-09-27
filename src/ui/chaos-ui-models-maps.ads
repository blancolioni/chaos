private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash;

package Chaos.UI.Models.Maps is

   type Model_Map is tagged private;

   function Contains (Map  : Model_Map;
                      Name : String)
                      return Boolean;

   function Element (Map  : Model_Map;
                     Name : String)
                     return UI_Model;

   procedure Insert (Map   : in out Model_Map;
                     Name  : String;
                     Model : UI_Model);

private

   package Model_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => UI_Model,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   type Model_Map is new Model_Maps.Map with null record;

end Chaos.UI.Models.Maps;
