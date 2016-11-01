private with Ada.Containers.Doubly_Linked_Lists;

with Chaos.Teams;

package Chaos.Actors.Visibility is

   function Can_See
     (Actor    : Chaos_Actor;
      Location : Chaos.Locations.Square_Location)
      return Boolean;

   function Can_See
     (Actor  : Chaos_Actor;
      Target : not null access constant
        Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Boolean;

   type Actor_Groups is private;

   procedure Clear (Groups : in out Actor_Groups);

   function Is_Empty
     (Groups   : Actor_Groups;
      Attitude : Chaos.Teams.Chaos_Attitude)
      return Boolean;

   function Contains
     (Groups    : Actor_Groups;
      Attitude  : Chaos.Teams.Chaos_Attitude;
      Reference : Chaos.Actors.Chaos_Actor)
      return Boolean;

   procedure Iterate
     (Groups   : Actor_Groups;
      Attitude : Chaos.Teams.Chaos_Attitude;
      Process  : not null access
        procedure (Reference : Chaos.Actors.Chaos_Actor));

   procedure Iterate
     (Groups : Actor_Groups;
      Process : not null access
        procedure (Reference : Chaos.Actors.Chaos_Actor));

   procedure Add_Actor_Can_See
     (Actor  : Chaos.Actors.Chaos_Actor;
      Groups : out Actor_Groups);

   procedure Add_Actor_Can_Be_Seen
     (Actor  : Chaos.Actors.Chaos_Actor;
      Groups : out Actor_Groups);

private

   package List_Of_Actors is
     new Ada.Containers.Doubly_Linked_Lists (Chaos_Actor);

   type Attitude_Array is
     array (Chaos.Teams.Chaos_Attitude) of List_Of_Actors.List;

   type Actor_Groups is
      record
         Gs : Attitude_Array;
      end record;

end Chaos.Actors.Visibility;
