private with Ada.Containers.Doubly_Linked_Lists;

private with Memor;

with Chaos.Objects;

package Chaos.Teams is

   type Chaos_Attitude is (Party, Friendly, Neutral, Hostile);

   type Chaos_Team_Record is
     new Chaos.Objects.Root_Localised_Object_Record with private;

   type Chaos_Team is access constant Chaos_Team_Record'Class;

   function Attitude (From, To : Chaos_Team) return Chaos_Attitude;

   function Get (Identifier : String) return Chaos_Team;

private

   package List_Of_Teams is
     new Ada.Containers.Doubly_Linked_Lists (Chaos_Team);

   type Attitude_Arrays is
     array (Chaos_Attitude) of List_Of_Teams.List;

   type Chaos_Team_Record is
     new Chaos.Objects.Root_Localised_Object_Record with
      record
         Attitudes : Attitude_Arrays;
      end record;

   overriding function Object_Database
     (Object : Chaos_Team_Record)
      return Memor.Memor_Database;

   overriding procedure Add_Properties
     (Team : Chaos_Team_Record)
   is null;

end Chaos.Teams;
