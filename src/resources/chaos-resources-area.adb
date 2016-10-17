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

      Chaos.Logging.Log ("AREA", To_String (Area.Wed_Resource));
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

      Area.Set_Offset (Area.Regions_Offset);
      for I in 1 .. Area.Regions_Count loop
         declare
            Region : Region_Entry;
         begin
            Area.Get (Region.Name);
            Area.Get (Region.Region_Type);
            Vertices.Get (Area, Region.Bounding_Box);
            Area.Get (Region.Vertex_Count);
            Area.Get (Region.First_Vertex);
            Area.Get (Region.Trigger_Value);
            Area.Get (Region.Cursor_Index);
            Area.Get (Region.Destination_Area);
            Area.Get (Region.Destination_Entrance);
            Area.Get (Region.Flags);
            Area.Get (Region.Information_Text);
            Area.Get (Region.Trap_Detection_Difficulty);
            Area.Get (Region.Trap_Removal_Difficulty);
            Area.Get (Region.Is_Trapped);
            Area.Get (Region.Trap_Detected);
            Area.Get (Region.Trap_Launch_X);
            Area.Get (Region.Trap_Launch_Y);
            Area.Get (Region.Key_Entity);
            Area.Get (Region.Region_Script);
            Area.Get (Region.Alternative_Use_X);
            Area.Get (Region.Alternative_Use_Y);
            Area.Get (Region.Unknown_1);
            Area.Get (Region.Unknown_2);
            Area.Get (Region.Sound);
            Area.Get (Region.Talk_Location_X);
            Area.Get (Region.Talk_Location_Y);
            Area.Get (Region.Name_Reference);
            Area.Get (Region.Dialog_File);

            Area.Regions.Append (Region);
         end;
      end loop;

      Area.Set_Offset (Area.Doors_Offset);
      for I in 1 .. Area.Doors_Count loop
         declare
            Door : Door_Entry;
         begin
            Area.Get (Door.Name);
            Area.Get (Door.Resource_Name);
            Area.Get (Door.Flags);
            Area.Get (Door.First_Open_Vertex);
            Area.Get (Door.Open_Vertex_Count);
            Area.Get (Door.Closed_Vertex_Count);
            Area.Get (Door.First_Closed_Vertex);
            Vertices.Get (Area, Door.Open_Bounding_Box);
            Vertices.Get (Area, Door.Closed_Bounding_Box);
            Area.Get (Door.First_Open_Impeded_Vertex);
            Area.Get (Door.Open_Impeded_Vertex_Count);
            Area.Get (Door.Closed_Impeded_Vertex_Count);
            Area.Get (Door.First_Closed_Impeded_Vertex);
            Area.Get (Door.HP);
            Area.Get (Door.AC);
            Area.Get (Door.Door_Open_Sound);
            Area.Get (Door.Door_Close_Sound);
            Area.Get (Door.Cursor_Index);
            Area.Get (Door.Trap_Detection_Difficulty);
            Area.Get (Door.Trap_Removal_Difficulty);
            Area.Get (Door.Is_Trapped);
            Area.Get (Door.Trap_Detected);
            Area.Get (Door.Trap_Launch_Target_X);
            Area.Get (Door.Trap_Launch_Target_Y);
            Area.Get (Door.Key_Entity);
            Area.Get (Door.Door_Script);
            Area.Get (Door.Secret_Door_Detection);
            Area.Get (Door.Lock_Difficulty);
            Vertices.Get (Area, Door.Toggle_Door_Open_Box);
            Area.Get (Door.Lockpick_String);
            Area.Get (Door.Travel_Trigger);
            Area.Get (Door.Dialog_Speaker_Name);
            Area.Get (Door.Dialog);
            Area.Get (Door.Unknown_1);
            Area.Get (Door.Unknown_2);

            Chaos.Logging.Log
              ("AREA", To_String (Door.Resource_Name) & " " & Door.Name);

            Area.Doors.Append (Door);
         end;
      end loop;

      Area.Set_Offset (Area.Containers_Offset);
      for I in 1 .. Area.Containers_Count loop
         declare
            Container : Container_Entry;
         begin
            Area.Get (Container.Name);
            Area.Get (Container.X);
            Area.Get (Container.Y);
            Area.Get (Container.Container_Type);
            Area.Get (Container.Lock_Difficulty);
            Area.Get (Container.Flags);
            Area.Get (Container.Trap_Detection_Difficulty);
            Area.Get (Container.Trap_Removal_Difficulty);
            Area.Get (Container.Is_Trapped);
            Area.Get (Container.Trap_Detected);
            Area.Get (Container.Trap_Launch_X);
            Area.Get (Container.Trap_Launch_Y);
            Vertices.Get (Area, Container.Bounding_Box);
            Area.Get (Container.First_Item_Index);
            Area.Get (Container.Item_Count);
            Area.Get (Container.Trap_Script);
            Area.Get (Container.First_Vertex);
            Area.Get (Container.Vertex_Count);
            Area.Get (Container.Trigger_Range);
            Area.Get (Container.Owner);
            Area.Get (Container.Key_Item);
            Area.Get (Container.Break_Difficulty);
            Area.Get (Container.Lockpick_String);
            Area.Get (Container.Unused);
            Area.Containers.Append (Container);
         end;
      end loop;

      Area.Set_Offset (Area.Items_Offset);
      for I in 1 .. Area.Items_Count loop
         declare
            Item : Item_Entry;
         begin
            Area.Get (Item.Resource);
            Area.Get (Item.Expiration_Time);
            Area.Get (Item.Quantity_1);
            Area.Get (Item.Quantity_2);
            Area.Get (Item.Quantity_3);
            Area.Get (Item.Flags);
            Area.Items.Append (Item);
         end;
      end loop;

      Area.Set_Offset (Area.Entrances_Offset);
      for I in 1 .. Area.Entrances_Count loop
         declare
            Entrance : Entrance_Entry;
         begin
            Area.Get (Entrance.Name);
            Area.Get (Entrance.X);
            Area.Get (Entrance.Y);
            Area.Get (Entrance.Orientation);
            Area.Get (Entrance.Unused);
            Chaos.Logging.Log
              ("AREA",
               Entrance.Name & Entrance.X'Img & Entrance.Y'Img
               & Entrance.Orientation'Img);
            Area.Entrances.Append (Entrance);
         end;
      end loop;

      Vertices.Get (Area, Area.Vertices_Offset,
                    Word_32 (Area.Vertices_Count),
                    Area.Area_Vertices);

   end Load;

end Chaos.Resources.Area;
