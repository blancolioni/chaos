private with Ada.Containers.Vectors;

package Chaos.UI.Screens is

   type Coordinate is new Long_Float;

   type Point_Type is
      record
         X, Y : Coordinate;
      end record;

   procedure Get_Step
     (From, To  : Point_Type;
      Step_Size : Coordinate;
      DX, DY    : out Coordinate);

   type Path_Type is private;

   procedure Append (Path : in out Path_Type;
                     P    : Point_Type);

   function Length (Path : Path_Type) return Natural;
   function Point (Path  : Path_Type;
                   Index : Positive)
                   return Point_Type;

private

   package Point_Vectors is
     new Ada.Containers.Vectors (Positive, Point_Type);

   type Path_Type is
      record
         V : Point_Vectors.Vector;
      end record;

end Chaos.UI.Screens;
