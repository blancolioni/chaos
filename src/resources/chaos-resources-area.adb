with Chaos.Logging;

package body Chaos.Resources.Area is

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Area : in out Area_Resource)
   is
   begin
      Area.Set_Offset (8);
      Area.Get (Area.Wed_Resource);
      Area.Get (Area.Last_Saved);
      Area.Get (Area.Area_Flags);
      for I in Area.Neighbours'Range loop
         Area.Get (Area.Neighbours (I).Area_Resource);
         Area.Get (Area.Neighbours (I).Transition_Flags);
      end loop;

      Chaos.Logging.Log ("AREA", String (Area.Wed_Resource));
      Area.Get (Area.Area_Type_Flags);

      for I in Area.Weather_Probability'Range loop
         Area.Get (Area.Weather_Probability (I));
      end loop;

      Area.Get (Area.Actors_Offset);
      Area.Get (Area.Actors_Count);
      Area.Get (Area.Regions_Count);
      Area.Get (Area.Regions_Offset);
      Area.Get (Area.Spawn_Points_Offset);
      Area.Get (Area.Spawn_Points_Count);
      Area.Get (Area.Entrances_Offset);
      Area.Get (Area.Entrances_Count);
      Area.Get (Area.Containers_Offset);
      Area.Get (Area.Containers_Count);
      Area.Get (Area.Items_Count);
      Area.Get (Area.Items_Offset);
      Area.Get (Area.Vertices_Offset);
      Area.Get (Area.Vertices_Count);
      Area.Get (Area.Ambients_Count);
      Area.Get (Area.Ambients_Offset);
      Area.Get (Area.Variables_Offset);
      Area.Get (Area.Variables_Count);
      Area.Get (Area.Tiled_Object_Flags_Offset);
      Area.Get (Area.Tiled_Object_Flags_Count);
      Area.Get (Area.Area_Script);
      Area.Get (Area.Explored_Bitmask_Size);
      Area.Get (Area.Explored_Bitmask_Offset);
      Area.Get (Area.Doors_Count);
      Area.Get (Area.Doors_Offset);
      Area.Get (Area.Animations_Count);
      Area.Get (Area.Animations_Offset);
      Area.Get (Area.Tiled_Objects_Count);
      Area.Get (Area.Tiled_Objects_Offset);
      Area.Get (Area.Song_Entry_Offset);
      Area.Get (Area.Rest_Interruption_Offset);
      Area.Get (Area.Automap_Note_Offset);
      Area.Get (Area.Automap_Note_Count);
      Area.Get (Area.Projectile_Trap_Offset);
      Area.Get (Area.Rest_Movie_Day);
      Area.Get (Area.Rest_Movie_Night);
      Area.Get (Area.Unused_1);
      Area.Get (Area.Unused_2);
      Area.Get (Area.Unused_3);
      Area.Get (Area.Unused_4);
      Area.Get (Area.Unused_5);
      Area.Get (Area.Unused_6);
      Area.Get (Area.Unused_7);

      Area.Set_Offset (Area.Actors_Offset);
      for I in 1 .. Area.Actors_Count loop
         declare
            Actor : Actor_Entry;
            Name_Done : Boolean := False;
         begin
            Actor.Name := (others => ' ');
            for I in Actor.Name'Range loop
               declare
                  W : Word_8;
               begin
                  Area.Get (W);
                  Name_Done := Name_Done or else W = 0;
                  if not Name_Done then
                     Actor.Name (I) := Character'Val (W);
                  end if;
               end;
            end loop;
            Area.Get (Actor.Current_X);
            Area.Get (Actor.Current_Y);
            Area.Get (Actor.Dest_X);
            Area.Get (Actor.Dest_Y);
            Area.Get (Actor.Flags);
            Area.Get (Actor.Spawned);
            Area.Get (Actor.CRE_First);
            Area.Get (Actor.Unused_1);
            Area.Get (Actor.Animation);
            Area.Get (Actor.Orientation);
            Area.Get (Actor.Unused_2);
            Area.Get (Actor.Remove_Timer);
            Area.Get (Actor.Move_Restriction);
            Area.Get (Actor.Move_Object_Restriction);
            Area.Get (Actor.Appearance);
            Area.Get (Actor.Num_Times_Talked_To);
            Area.Get (Actor.Dialog);
            for I in Actor.Scripts'Range loop
               Area.Get (Actor.Scripts (I));
            end loop;
            Area.Get (Actor.CRE_File);
            Area.Get (Actor.CRE_Offset);
            Area.Get (Actor.CRE_Size);
            Area.Get (Actor.Unused);
            Area.Actors.Append (Actor);
         end;
      end loop;
   end Load;

end Chaos.Resources.Area;
