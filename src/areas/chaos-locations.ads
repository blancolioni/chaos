private with Ada.Containers.Vectors;

package Chaos.Locations is

   type Orientation is
     (South, South_West, West, North_West,
      North, North_East, East, South_East);

   type Pixel_Location is
      record
         X, Y : Integer;
      end record;

   type Square_Location is
      record
         X, Y : Natural;
      end record;

   function Get_Direction
     (From, To : Square_Location)
      return Orientation;

   function Adjacent
     (Square_1, Square_2 : Square_Location)
      return Boolean;

   type Square_Path is private;

   function "&"
     (Left : Square_Location;
      Right : Square_Path)
      return Square_Path;

   procedure Append
     (To     : in out Square_Path;
      Square : Square_Location);

   No_Path : constant Square_Path;

   function Minimum_Distance
     (From, To : Square_Location)
      return Natural
   is (Natural'Max (abs (From.X - To.X), abs (From.Y - To.Y)));

   function Length (Path : Square_Path) return Natural;

   function Square
     (Path  : Square_Path;
      Index : Positive)
      return Square_Location;

   function First_Square
     (Path : Square_Path)
      return Square_Location;

   function Drop_Last
     (Path : Square_Path)
      return Square_Path;

   function Drop_First
     (Path : Square_Path)
      return Square_Path;

   function Find_Path
     (Start  : Square_Location;
      Finish : Square_Location;
      Max_X  : Natural;
      Max_Y  : Natural;
      OK     : not null access
        function (Location : Square_Location) return Boolean)
      return Square_Path;

   function Straight_Line_Path
     (Start  : Square_Location;
      Finish : Square_Location;
      Max_X  : Natural;
      Max_Y  : Natural;
      OK     : not null access
        function (Location : Square_Location) return Boolean)
      return Square_Path;

private

   package Square_Location_Vectors is
     new Ada.Containers.Vectors (Positive, Square_Location);

   type Square_Path is
      record
         Path : Square_Location_Vectors.Vector;
      end record;

   No_Path : constant Square_Path :=
               (Path => Square_Location_Vectors.Empty_Vector);

end Chaos.Locations;
