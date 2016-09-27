private with Memor;

with Chaos.Abilities;
with Chaos.Defences;
with Chaos.Objects;
with Chaos.Sizes;
with Chaos.Speed;
with Chaos.Vision;

package Chaos.Races is

   type Chaos_Race_Record is
     new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Sizes.Chaos_Size_Interface
     and Chaos.Speed.Chaos_Speed_Interface
     and Chaos.Vision.Chaos_Vision_Interface
   with private;

   overriding function Size
     (Race : Chaos_Race_Record)
      return Chaos.Sizes.Chaos_Size;

   overriding function Speed
     (Race : Chaos_Race_Record)
      return Chaos.Speed.Chaos_Speed;

   overriding function Vision
     (Race : Chaos_Race_Record)
      return Chaos.Vision.Chaos_Vision;

   type Chaos_Race is access constant Chaos_Race_Record'Class;

   function Get (Identifier : String) return Chaos_Race;

   type Chaos_Race_Interface is limited interface;

   function Race (Item : Chaos_Race_Interface) return Chaos_Race
                  is abstract;

   function Size
     (Item : Chaos_Race_Interface'Class)
      return Chaos.Sizes.Chaos_Size
   is (Item.Race.Size);

   function Speed
     (Item : Chaos_Race_Interface'Class)
      return Chaos.Speed.Chaos_Speed
   is (Item.Race.Speed);

   function Vision
     (Item : Chaos_Race_Interface'Class)
      return Chaos.Vision.Chaos_Vision
   is (Item.Race.Vision);

private

   type Chaos_Race_Record is
     new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Sizes.Chaos_Size_Interface
     and Chaos.Speed.Chaos_Speed_Interface
     and Chaos.Vision.Chaos_Vision_Interface with
      record
         Abilities : Chaos.Abilities.Ability_Score_Changes := (others => 0);
         Defences  : Chaos.Defences.Defence_Score_Changes := (others => 0);
         Size      : Chaos.Sizes.Chaos_Size := Chaos.Sizes.Medium;
         Speed     : Chaos.Speed.Chaos_Speed := 6;
         Vision    : Chaos.Vision.Chaos_Vision := Chaos.Vision.Normal;
      end record;

   overriding function Object_Database
     (Object : Chaos_Race_Record)
      return Memor.Root_Database_Type'Class;

end Chaos.Races;
