with Ada.Text_IO;

package body Chaos.Creatures.Reports is

   ------------
   -- Report --
   ------------

   procedure Report (Creature : Chaos_Creature) is
      use Ada.Text_IO;
   begin
      Put_Line
        ("Name: " & Ada.Strings.Unbounded.To_String (Creature.Long_Name));
      Put_Line
        (Creature.Class.Display_Name & " level"
         & Chaos.Levels.Chaos_Level'Image (Creature.Level));
      Put_Line
        (Creature.Race.Display_Name);
      New_Line;

      for Ability in Chaos.Abilities.Ability loop
         Put_Line (Ability'Img
                   & Creature.Abilities (Ability)'Img);
      end loop;

      New_Line;

      for Defence in Chaos.Defences.Defence loop
         Put_Line (Defence'Img
                   & Chaos.Defences.Defence_Score_Range'Image
                     (Creature.Defence_Score (Defence)));
      end loop;

      New_Line;

      Put_Line
        ("HP max:" & Natural'Image (Creature.Max_Hit_Points)
         & " bloodied:"
         & Natural'Image (Creature.Max_Hit_Points / 2)
         & " current:"
         & Integer'Image (Creature.HP));

      New_Line;
      for I in 1 .. Creature.Power_Count loop
         Put_Line (Creature.Get_Power (I).Display_Name);
      end loop;

   end Report;

end Chaos.Creatures.Reports;
