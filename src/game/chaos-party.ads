with Chaos.Actors;
with Chaos.Areas;
with Chaos.Commands;

package Chaos.Party is

   type Party_Member_Index is range 1 .. 6;

   type Chaos_Party_Record is tagged private;

   type Chaos_Party is access all Chaos_Party_Record'Class;

   function Party_Member
     (Party    : Chaos_Party_Record'Class;
      Position : Party_Member_Index)
      return Chaos.Actors.Chaos_Actor;

   procedure Add_Party_Member
     (Party    : in out Chaos_Party_Record'Class;
      Actor    : Chaos.Actors.Chaos_Actor);

   procedure Remove_Party_Member
     (Party    : in out Chaos_Party_Record'Class;
      Actor    : Chaos.Actors.Chaos_Actor);

   procedure Clear
     (Party : in out Chaos_Party_Record'Class);

   type Party_Type is access all Chaos_Party_Record'Class;

   function Create_Party return Party_Type;

private

   type Party_Member_Array is
     array (Party_Member_Index) of Chaos.Actors.Chaos_Actor;

   type Chaos_Party_Record is tagged
      record
         Members : Party_Member_Array;
         Area    : Chaos.Areas.Chaos_Area;
      end record;

end Chaos.Party;