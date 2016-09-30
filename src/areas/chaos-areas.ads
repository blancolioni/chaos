private with Ada.Containers.Vectors;

private with Memor;

with Chaos.Objects;
with Chaos.Locations;
with Chaos.Vision;

with Chaos.Expressions;

with Chaos.Actors;
with Chaos.Commands;
with Chaos.Images;

package Chaos.Areas is

   Pixels_Per_Square : constant := 32;

   type Chaos_Area_Record is
     new Chaos.Objects.Root_Chaos_Object_Record
     and Chaos.Commands.Command_Environment_Interface with private;

   procedure Create
     (Area                      : in out Chaos_Area_Record'Class;
      Identity                  : String;
      Pixel_Width, Pixel_Height : Natural);

   type Chaos_Area is access constant Chaos_Area_Record'Class;

   function Pixels_Across (Area : Chaos_Area_Record'Class) return Natural;
   function Pixels_Down (Area : Chaos_Area_Record'Class) return Natural;

   function Squares_Across (Area : Chaos_Area_Record'Class) return Natural;
   function Squares_Down (Area : Chaos_Area_Record'Class) return Natural;

   function Tiles_Across (Area : Chaos_Area_Record'Class) return Natural;
   function Tiles_Down (Area : Chaos_Area_Record'Class) return Natural;

   function Tile_Index
     (Area           : Chaos_Area_Record'Class;
      Tile_X, Tile_Y : Positive)
      return Positive;

   function Images
     (Area : Chaos_Area_Record'Class)
      return Chaos.Images.Chaos_Image_Container;

   function Contains_Point
     (Area : Chaos_Area_Record'Class;
      X, Y : Integer)
      return Boolean
   is (X in 0 .. Area.Squares_Across - 1
       and then Y in 0 .. Area.Squares_Down - 1);

   function To_Square
     (Area           : Chaos_Area_Record'Class;
      Pixel_Location : Chaos.Locations.Pixel_Location)
      return Chaos.Locations.Square_Location;

   function To_Pixels
     (Area            : Chaos_Area_Record'Class;
      Square_Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Pixel_Location;

   function Script
     (Area : Chaos_Area_Record'class)
      return Chaos.Expressions.Chaos_Expression;

   function Actor
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Actors.Chaos_Actor;

   procedure Add_Actor
     (Area     : Chaos_Area_Record'Class;
      Actor    : Chaos.Actors.Chaos_Actor);

   function Passable
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Boolean;

   function Transparent
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Boolean;

   function Neighbours
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path;

   function Part_Of_Battle
     (Area  : Chaos_Area_Record'Class;
      Actor : Chaos.Actors.Chaos_Actor)
      return Boolean
   is (False);

   function Visible
     (Area     : Chaos_Area_Record'Class;
      Square_1 : Chaos.Locations.Square_Location;
      Square_2 : Chaos.Locations.Square_Location)
      return Boolean;

   procedure Scan_Visible_Actors
     (Area    : Chaos_Area_Record'Class;
      Looker  : Chaos.Actors.Chaos_Actor;
      Process : not null access
        procedure (Actor : Chaos.Actors.Chaos_Actor));

   procedure Scan_Visible_To_Actors
     (Area    : Chaos_Area_Record'Class;
      Lookee  : Chaos.Actors.Chaos_Actor;
      Process : not null access
        procedure (Actor : Chaos.Actors.Chaos_Actor));

   overriding function Find_Path
     (Area   : Chaos_Area_Record;
      Start  : Chaos.Locations.Square_Location;
      Finish : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path;

private

   package Actor_Vectors is
     new Ada.Containers.Vectors (Positive, Chaos.Actors.Chaos_Actor,
                                 Chaos.Actors."=");

   type Square_Type is
      record
         Actor       : Chaos.Actors.Chaos_Actor;
         Passable    : Boolean := True;
         Transparent : Boolean := True;
      end record;

   package Square_Vectors is
     new Ada.Containers.Vectors (Positive, Square_Type);

   type Tileset_Map_Entry is
      record
         Tile_Index : Natural;
      end record;

   package Tileset_Map_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Tileset_Map_Entry);

   type Chaos_Area_Record is
     new Chaos.Objects.Root_Chaos_Object_Record
     and Chaos.Commands.Command_Environment_Interface with
      record
         Pixel_Width  : Natural;
         Pixel_Height : Natural;
         Squares      : Square_Vectors.Vector;
         Tiles        : Tileset_Map_Entry_Vectors.Vector;
         Actors       : Actor_Vectors.Vector;
         Visibility   : Chaos.Vision.Chaos_Vision;
         Script       : Chaos.Expressions.Chaos_Expression;
         Environment  : Chaos.Expressions.Chaos_Environment;
         Images       : Chaos.Images.Chaos_Image_Container;
      end record;

   overriding function Object_Database
     (Object : Chaos_Area_Record)
      return Memor.Root_Database_Type'Class;

   overriding function Current_Battle
     (Area : Chaos_Area_Record)
      return Boolean;

   function To_Square_Index
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Positive;

end Chaos.Areas;
