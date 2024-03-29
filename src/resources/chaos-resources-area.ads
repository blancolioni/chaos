with Ada.Containers.Vectors;

with Chaos.Resources.Vertices;

package Chaos.Resources.Area is

   use WL.Binary_IO;

   type Direction is (North, East, South, West);

   type Area_Neighbour is
      record
         Area_Resource : Resource_Reference;
         Transition_Flags : Word_32;
      end record;

   type Area_Neighbour_Array is array (Direction) of Area_Neighbour;

   type Area_Weather is (Rain, Snow, Fog, Lightning, Wind_Speed);

   type Area_Weather_Array is array (Area_Weather) of Word_16;

   type Actor_Script is (Overide, General, Class, Race, Default, Specific);

   type Script_Array is array (Actor_Script) of Resource_Reference;

   type Actor_Entry is
      record
         Name                    : String (1 .. 32);
         Current_X               : Word_16;
         Current_Y               : Word_16;
         Dest_X                  : Word_16;
         Dest_Y                  : Word_16;
         Flags                   : Word_32;
         Spawned                 : Word_16;
         CRE_First               : Word_8;
         Unused_1                : Word_8;
         Animation               : Word_32;
         Orientation             : Word_16;
         Unused_2                : Word_16;
         Remove_Timer            : Word_32;
         Move_Restriction        : Word_16;
         Move_Object_Restriction : Word_16;
         Appearance              : Word_32;
         Num_Times_Talked_To     : Word_32;
         Dialog                  : Resource_Reference;
         Scripts                 : Script_Array;
         CRE_File                : Resource_Reference;
         CRE_Offset              : Word_32;
         CRE_Size                : Word_32;
         Unused                  : Unused_Bytes (1 .. 128);
      end record;

   package Area_Actor_Vectors is
     new Ada.Containers.Vectors (Positive, Actor_Entry);

   type Door_Entry is
      record
         Name                        : String (1 .. 32);
         Resource_Name               : Resource_Reference;
         Flags                       : Word_32;
         First_Open_Vertex           : Word_32;
         Open_Vertex_Count           : Word_16;
         Closed_Vertex_Count         : Word_16;
         First_Closed_Vertex         : Word_32;
         Open_Bounding_Box           : Vertices.Rectangle;
         Closed_Bounding_Box         : Vertices.Rectangle;
         First_Open_Impeded_Vertex   : Word_32;
         Open_Impeded_Vertex_Count   : Word_16;
         Closed_Impeded_Vertex_Count : Word_16;
         First_Closed_Impeded_Vertex : Word_32;
         HP, AC                      : Word_16;
         Door_Open_Sound             : Resource_Reference;
         Door_Close_Sound            : Resource_Reference;
         Cursor_Index                : Word_32;
         Trap_Detection_Difficulty   : Word_16;
         Trap_Removal_Difficulty     : Word_16;
         Is_Trapped                  : Word_16;
         Trap_Detected               : Word_16;
         Trap_Launch_Target_X        : Word_16;
         Trap_Launch_Target_Y        : Word_16;
         Key_Entity                    : Resource_Reference;
         Door_Script                 : Resource_Reference;
         Secret_Door_Detection       : Word_32;
         Lock_Difficulty             : Word_32;
         Toggle_Door_Open_Box        : Vertices.Rectangle;
         Lockpick_String             : String (1 .. 4);
         Travel_Trigger              : String (1 .. 24);
         Dialog_Speaker_Name         : String (1 .. 4);
         Dialog                      : Resource_Reference;
         Unknown_1                   : Word_32;
         Unknown_2                   : Word_32;
      end record;

   package Area_Door_Vectors is
     new Ada.Containers.Vectors (Positive, Door_Entry);

   type Entrance_Entry is
      record
         Name        : String (1 .. 32);
         X, Y        : Word_16;
         Orientation : Word_16;
         Unused      : Unused_Bytes (1 .. 66);
      end record;

   package Area_Entrance_Vectors is
     new Ada.Containers.Vectors (Positive, Entrance_Entry);

   type Region_Entry is
      record
         Name                        : String (1 .. 32);
         Region_Type                 : Word_16;
         Bounding_Box                : Vertices.Rectangle;
         Vertex_Count                : Word_16;
         First_Vertex                : Word_32;
         Trigger_Value               : Word_32;
         Cursor_Index                : Word_32;
         Destination_Area            : Resource_Reference;
         Destination_Entrance        : String (1 .. 32);
         Flags                       : Word_32;
         Information_Text            : Word_32;
         Trap_Detection_Difficulty   : Word_16;
         Trap_Removal_Difficulty     : Word_16;
         Is_Trapped                  : Word_16;
         Trap_Detected               : Word_16;
         Trap_Launch_X               : Word_16;
         Trap_Launch_Y               : Word_16;
         Key_Entity                    : Resource_Reference;
         Region_Script               : Resource_Reference;
         Alternative_Use_X           : Word_16;
         Alternative_Use_Y           : Word_16;
         Unknown_1                   : Word_32;
         Unknown_2                   : Unused_Bytes (1 .. 32);
         Sound                       : Resource_Reference;
         Talk_Location_X             : Word_16;
         Talk_Location_Y             : Word_16;
         Name_Reference              : Word_32;
         Dialog_File                 : Resource_Reference;
      end record;

   package Area_Region_Vectors is
     new Ada.Containers.Vectors (Positive, Region_Entry);

   type Container_Entry is
      record
         Name                        : String (1 .. 32);
         X, Y                        : Word_16;
         Container_Type              : Word_16;
         Lock_Difficulty             : Word_16;
         Flags                       : Word_32;
         Trap_Detection_Difficulty   : Word_16;
         Trap_Removal_Difficulty     : Word_16;
         Is_Trapped                  : Word_16;
         Trap_Detected               : Word_16;
         Trap_Launch_X               : Word_16;
         Trap_Launch_Y               : Word_16;
         Bounding_Box                : Vertices.Rectangle;
         First_Item_Index            : Word_32;
         Item_Count                  : Word_32;
         Trap_Script                 : Resource_Reference;
         First_Vertex                : Word_32;
         Vertex_Count                : Word_16;
         Trigger_Range               : Word_16;
         Owner                       : String (1 .. 32);
         Key_Item                    : Resource_Reference;
         Break_Difficulty            : Word_32;
         Lockpick_String             : Word_32;
         Unused                      : Unused_Bytes (1 .. 56);
      end record;

   package Area_Container_Vectors is
     new Ada.Containers.Vectors (Positive, Container_Entry);

   type Item_Entry is
      record
         Resource        : Resource_Reference;
         Expiration_Time : Word_16;
         Quantity_1      : Word_16;
         Quantity_2      : Word_16;
         Quantity_3      : Word_16;
         Flags           : Word_32;
      end record;

   package Area_Item_Vectors is
     new Ada.Containers.Vectors (Positive, Item_Entry);

   type Area_Resource is
     new Chaos_Resource with
      record
         Wed_Resource              : Resource_Reference;
         Last_Saved                : Word_32;
         Area_Flags                : Word_32;
         Neighbours                : Area_Neighbour_Array;
         Area_Type_Flags           : Word_16;
         Weather_Probability       : Area_Weather_Array;
         Actors_Offset             : Word_32;
         Actors_Count              : Word_16;
         Regions_Count             : Word_16;
         Regions_Offset            : Word_32;
         Spawn_Points_Offset       : Word_32;
         Spawn_Points_Count        : Word_32;
         Entrances_Offset          : Word_32;
         Entrances_Count           : Word_32;
         Containers_Offset         : Word_32;
         Containers_Count          : Word_16;
         Items_Count               : Word_16;
         Items_Offset              : Word_32;
         Vertices_Offset           : Word_32;
         Vertices_Count            : Word_16;
         Ambients_Count            : Word_16;
         Ambients_Offset           : Word_32;
         Variables_Offset          : Word_32;
         Variables_Count           : Word_32;
         Tiled_Object_Flags_Offset : Word_16;
         Tiled_Object_Flags_Count  : Word_16;
         Area_Script               : Resource_Reference;
         Explored_Bitmask_Size     : Word_32;
         Explored_Bitmask_Offset   : Word_32;
         Doors_Count               : Word_32;
         Doors_Offset              : Word_32;
         Animations_Count          : Word_32;
         Animations_Offset         : Word_32;
         Tiled_Objects_Count       : Word_32;
         Tiled_Objects_Offset      : Word_32;
         Song_Entry_Offset         : Word_32;
         Rest_Interruption_Offset  : Word_32;
         Automap_Note_Offset       : Word_32;
         Automap_Note_Count        : Word_32;
         Projectile_Trap_Offset    : Word_32;
         Rest_Movie_Day            : Resource_Reference;
         Rest_Movie_Night          : Resource_Reference;
         Unused_1                  : Word_32;
         Unused_2                  : Word_32;
         Unused_3                  : Word_32;
         Unused_4                  : Word_32;
         Unused_5                  : Word_32;
         Unused_6                  : Word_32;
         Unused_7                  : Word_32;
         Actors                    : Area_Actor_Vectors.Vector;
         Doors                     : Area_Door_Vectors.Vector;
         Area_Vertices             : Vertices.Vertex_Vectors.Vector;
         Entrances                 : Area_Entrance_Vectors.Vector;
         Containers                : Area_Container_Vectors.Vector;
         Items                     : Area_Item_Vectors.Vector;
         Regions                   : Area_Region_Vectors.Vector;
      end record;

   overriding function Signature
     (Area : Area_Resource)
      return String
   is ("AREA");

   overriding procedure Load
     (Area : in out Area_Resource);

end Chaos.Resources.Area;
