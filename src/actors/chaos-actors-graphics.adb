with Chaos.Colours;
with Chaos.Infinity_Engine;

with Chaos.Db.Actor_Colour;

package body Chaos.Actors.Graphics is

   --------------------------
   -- Get_Creature_Colours --
   --------------------------

   function Get_Creature_Colours
     (Actor : Chaos.Db.Actor_Reference)
      return Chaos.Resources.Bam.Colour_Replacement_Array
   is
      Result : Chaos.Resources.Bam.Colour_Replacement_Array (1 .. 120);
      Count  : Natural := 0;
   begin
      for Actor_Colour of
        Chaos.Db.Actor_Colour.Select_By_Actor
          (Actor)
      loop
         Count := Count + 1;
         Result (Count) :=
           (Actor_Colour.Palette_Index + Actor_Colour.Brightness,
            Chaos.Infinity_Engine.Colour_Index_To_Colour
              (Chaos.Colours.Colour_Index (Actor_Colour.Colour_Index),
               Actor_Colour.Brightness));
      end loop;
      return Result (1 .. Count);
   end Get_Creature_Colours;

end Chaos.Actors.Graphics;
