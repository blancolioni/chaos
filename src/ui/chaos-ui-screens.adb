with Ada.Numerics.Generic_Elementary_Functions;

package body Chaos.UI.Screens is

   package Coordinate_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Coordinate);

   ------------
   -- Append --
   ------------

   procedure Append
     (Path : in out Path_Type;
      P    : Point_Type)
   is
   begin
      Path.V.Append (P);
   end Append;

   --------------
   -- Get_Step --
   --------------

   procedure Get_Step
     (From, To  : Point_Type;
      Step_Size : Coordinate;
      DX, DY    : out Coordinate)
   is
      use Coordinate_Functions;
      D : constant Coordinate :=
            Sqrt ((To.X - From.X) ** 2
                    + (To.Y - From.Y) ** 2);
   begin
      DX := (To.X - From.X) / D * Step_Size;
      DY := (To.Y - From.Y) / D * Step_Size;
   end Get_Step;

   ------------
   -- Length --
   ------------

   function Length (Path : Path_Type) return Natural is
   begin
      return Path.V.Last_Index;
   end Length;

   -----------
   -- Point --
   -----------

   function Point
     (Path  : Path_Type;
      Index : Positive)
      return Point_Type
   is
   begin
      return Path.V (Index);
   end Point;

end Chaos.UI.Screens;
