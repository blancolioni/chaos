private with Memor;

limited with Chaos.Areas;

with Chaos.Commands;
with Chaos.Creatures;
with Chaos.Locations;
with Chaos.Objects;
with Chaos.Powers;
with Chaos.Speed;
with Chaos.Teams;
with Chaos.Vision;

package Chaos.Actors is

   type Actor_Orientation is
     (South, South_West, West, North_West,
      North, North_East, East, South_East);

   type Chaos_Actor_Record is
     new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Speed.Chaos_Speed_Interface
     and Chaos.Vision.Chaos_Vision_Interface
   with private;

   overriding function Speed
     (Actor : Chaos_Actor_Record)
      return Chaos.Speed.Chaos_Speed;

   type Chaos_Actor is access constant Chaos_Actor_Record'Class;

   function Area
     (Actor : Chaos_Actor_Record'Class)
      return access constant Chaos.Areas.Chaos_Area_Record'Class;

   function Creature
     (Actor : Chaos_Actor_Record'Class)
      return Chaos.Creatures.Chaos_Creature;

   function Current_Hit_Points
     (Actor : Chaos_Actor_Record'Class)
      return Natural;

   function Location
     (Actor : Chaos_Actor_Record'Class)
     return Chaos.Locations.Square_Location;

   function Orientation
     (Actor : Chaos_Actor_Record'Class)
      return Actor_Orientation;

   overriding function Vision
     (Actor : Chaos_Actor_Record)
      return Chaos.Vision.Chaos_Vision
   is (Actor.Creature.Race.Vision);

   function Long_Name
     (Actor : Chaos_Actor_Record'Class)
      return String
   is (Actor.Creature.Long_Name);

   function Short_Name
     (Actor : Chaos_Actor_Record'Class)
      return String
   is (Actor.Creature.Short_Name);

   function Create_Actor
     (From_Creature : Chaos.Creatures.Chaos_Creature;
      Area          : not null access constant
        Chaos.Areas.Chaos_Area_Record'Class;
      Location      : Chaos.Locations.Square_Location;
      Orientation   : Actor_Orientation)
      return Chaos_Actor;

   function Commands
     (Actor : not null access constant Chaos_Actor_Record'Class;
      Environment : not null access constant
        Chaos.Commands.Command_Environment_Interface'Class)
      return Chaos.Commands.Command_Collection;

   function Has_Move_Action
     (Actor : Chaos_Actor_Record'Class)
      return Boolean;

   function Has_Minor_Action
     (Actor : Chaos_Actor_Record'Class)
      return Boolean;

   function Has_Standard_Action
     (Actor : Chaos_Actor_Record'Class)
      return Boolean;

   procedure Use_Move_Action
     (Actor : in out Chaos_Actor_Record'Class)
     with Pre => Actor.Has_Move_Action;

   procedure Use_Minor_Action
     (Actor : in out Chaos_Actor_Record'Class)
     with Pre => Actor.Has_Minor_Action;

   procedure Use_Standard_Action
     (Actor : in out Chaos_Actor_Record'Class)
     with Pre => Actor.Has_Standard_Action;

   function Maximum_Shift
     (Actor : Chaos_Actor_Record'Class)
      return Natural;

   function Team
     (Actor : Chaos_Actor_Record'Class)
      return Chaos.Teams.Chaos_Team
   is (Actor.Creature.Team);

   procedure Reset_Actions (Actor : in out Chaos_Actor_Record'Class);

   procedure Take_Damage
     (Actor  : Chaos_Actor_Record'Class;
      Form   : Chaos.Powers.Power_Damage_Type;
      Points : Positive)
     with Pre => Points <= Actor.Current_Hit_Points;

   procedure Kill
     (Actor  : Chaos_Actor_Record'Class)
     with Pre => Actor.Creature.Alive,
     Post => not Actor.Creature.Individual or else not Actor.Creature.Alive;

private

   type Chaos_Actor_Record is
     new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Speed.Chaos_Speed_Interface
     and Chaos.Vision.Chaos_Vision_Interface with
      record
         Area            : access constant
           Chaos.Areas.Chaos_Area_Record'Class;
         Creature        : Chaos.Creatures.Chaos_Creature;
         Location        : Chaos.Locations.Square_Location;
         Orientation     : Actor_Orientation;
         Alive           : Boolean := True;
         Move_Action     : Boolean := True;
         Minor_Action    : Boolean := True;
         Standard_Action : Boolean := True;
         Hit_Points      : Natural;
      end record;

   overriding function Object_Database
     (Object : Chaos_Actor_Record)
      return Memor.Root_Database_Type'Class;

end Chaos.Actors;
