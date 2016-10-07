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

   type Vertex is
      record
         X, Y : Word_16;
      end record;

   package Vertex_Vectors is
     new Ada.Containers.Vectors (Positive, Vertex);

   type Polygon_Entry is
      record
         Start_Vertex_Index : Word_32;
         Vertex_Count       : Word_32;
         Flags              : Word_8;
         Height             : Word_8;
         Min_X              : Word_16;
         Max_X              : Word_16;
         Min_Y              : Word_16;
         Max_Y              : Word_16;
      end record;

   package Polygon_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Polygon_Entry);

   type Door_Entry is
      record
         Name                  : Resource_Reference;
         Closed                : Word_16;
         First_Tile_Cell_Index : Word_16;
         Tile_Cell_Count       : Word_16;
         Polygon_Count_Open    : Word_16;
         Polygon_Count_Closed  : Word_16;
         Polygon_Offset_Open   : Word_32;
         Polygon_Offset_Closed : Word_32;
         Polygons_Open         : Polygon_Entry_Vectors.Vector;
         Polygons_Closed       : Polygon_Entry_Vectors.Vector;
      end record;

   package Door_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Door_Entry);

   type Wed_Resource is
     new Chaos_Resource with
      record
         Overlay_Count           : Word_32;
         Door_Count              : Word_32;
         Overlay_Offset          : Word_32;
         Secondary_Header_Offset : Word_32;
         Doors_Offset            : Word_32;
         Door_Tile_Cell_Offset   : Word_32;
         Wall_Polygon_Count      : Word_32;
         Polygons_Offset         : Word_32;
         Vertices_Offset         : Word_32;
         Wall_Groups_Offset      : Word_32;
         Polygon_Indices_Offset  : Word_32;
         Vertex_Count            : Natural := 0;
         Overlays                : Overlay_Entry_Vectors.Vector;
         Doors                   : Door_Entry_Vectors.Vector;
         Polygons                : Polygon_Entry_Vectors.Vector;
         Vertices                : Vertex_Vectors.Vector;
      end record;

   overriding function Signature
     (Wed : Wed_Resource)
      return String
   is ("WED ");

   overriding procedure Load
     (Wed : in out Wed_Resource);

end Chaos.Resources.Wed;
