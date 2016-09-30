with Ada.Containers.Vectors;

package Chaos.Resources.Wed is

   use WL.Binary_IO;

   package Tile_Index_Vectors is
     new Ada.Containers.Vectors (Positive, Word_16);

   type Tile_Map_Entry is
      record
         Start_Index     : Word_16;
         Tile_Count      : Word_16;
         Secondary_Index : Word_16;
         Overlay_Flags   : Word_8;
      end record;

   package Tile_Map_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Tile_Map_Entry);

   type Overlay_Entry is
      record
         Width             : Word_16;
         Height            : Word_16;
         Tileset_Name      : Resource_Reference;
         Unique_Tile_Count : Word_16;
         Movement_Type     : Word_16;
         Tilemap_Offset    : Word_32;
         Tile_Index_Lookup : Word_32;
         Tile_Map          : Tile_Map_Entry_Vectors.Vector;
         Tile_Indices      : Tile_Index_Vectors.Vector;
      end record;

   package Overlay_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Overlay_Entry);

   type Wed_Resource is
     new Chaos_Resource with
      record
         Overlay_Count           : Word_32;
         Door_Count              : Word_32;
         Overlay_Offset          : Word_32;
         Secondary_Header_Offset : Word_32;
         Doors_Offset            : Word_32;
         Door_Tile_Cell_Offset   : Word_32;
         Overlays                : Overlay_Entry_Vectors.Vector;
      end record;

   overriding function Signature
     (Wed : Wed_Resource)
      return String
   is ("WED ");

   overriding procedure Load
     (Wed : in out Wed_Resource);

end Chaos.Resources.Wed;
