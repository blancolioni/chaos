with Chaos.Actors;
with Chaos.Areas;
with Chaos.Commands;

package Chaos.Players is

   type Party_Member_Index is range 1 .. 6;

   type Root_Party_Type is tagged private;

   function Get_Party_Member
     (Party    : Root_Party_Type'Class;
      Position : Party_Member_Index)
      return Chaos.Actors.Chaos_Actor;

   procedure Add_Party_Member
     (Party    : in out Root_Party_Type'Class;
      Actor    : Chaos.Actors.Chaos_Actor);

   procedure Remove_Party_Member
     (Party    : in out Root_Party_Type'Class;
      Actor    : Chaos.Actors.Chaos_Actor);

   procedure Clear
     (Party : in out Root_Party_Type'Class);

   type Party_Type is access all Root_Party_Type'Class;

private

   type Party_Member_Array is
     array (Party_Member_Index) of Chaos.Actors.Chaos_Actor;

   type Root_Party_Type is tagged
      record
         Members : Party_Member_Array;
         Area    : Chaos.Areas.Chaos_Area;
      end record;

end Chaos.Players;
