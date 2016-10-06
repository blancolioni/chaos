with Ada.Containers.Doubly_Linked_Lists;

package body Chaos.Locations is

   package List_Of_Squares is
     new Ada.Containers.Doubly_Linked_Lists (Square_Location);

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : Square_Location;
      Right : Square_Path)
      return Square_Path
   is
      Result : Square_Path := Right;
   begin
      Result.Path.Insert (Result.Path.First, Left);
      return Result;
   end "&";

   --------------
   -- Adjacent --
   --------------

   function Adjacent
     (Square_1, Square_2 : Square_Location)
      return Boolean
   is
      DX : constant Integer := abs (Square_2.X - Square_1.X);
      DY : constant Integer := abs (Square_2.Y - Square_1.Y);
   begin
      return DX = 1 or else DY = 1;
   end Adjacent;

   ------------
   -- Append --
   ------------

   procedure Append
     (To     : in out Square_Path;
      Square : Square_Location)
   is
   begin
      To.Path.Append (Square);
   end Append;

   ----------------
   -- Drop_First --
   ----------------

   function Drop_First
     (Path : Square_Path)
      return Square_Path
   is
   begin
      return Result : Square_Path do
         for I in 2 .. Path.Path.Last_Index loop
            Result.Path.Append (Square (Path, I));
         end loop;
      end return;
   end Drop_First;

   ---------------
   -- Drop_Last --
   ---------------

   function Drop_Last
     (Path : Square_Path)
      return Square_Path
   is
      Result : Square_Path := Path;
   begin
      Result.Path.Delete_Last;
      return Result;
   end Drop_Last;

   ---------------
   -- Find_Path --
   ---------------

   function Find_Path
     (Start  : Square_Location;
      Finish : Square_Location;
      Max_X  : Natural;
      Max_Y  : Natural;
      OK     : not null access
        function (Location : Square_Location) return Boolean)
      return Square_Path
   is

      type Partial_Path is
         record
            Current   : Square_Location;
            Previous  : Natural;
            Remaining : Natural;
         end record;

      package Queue_Of_Partials is
        new Ada.Containers.Doubly_Linked_Lists (Partial_Path);

      package Vector_Of_Partials is
        new Ada.Containers.Vectors (Positive, Partial_Path);

      Queue  : Queue_Of_Partials.List;
      Vector : Vector_Of_Partials.Vector;
      Tried  : List_Of_Squares.List;

      Result : Square_Path;

   begin

      Queue.Append ((Start, 0, 0));

      while not Queue.Is_Empty loop
         declare
            P    : constant Partial_Path := Queue.First_Element;
         begin
            Queue.Delete_First;
            if P.Current = Finish then
               declare
                  V      : Partial_Path := P;
               begin
                  while V.Previous > 0 loop
                     Result.Path.Append (V.Current);
                     V := Vector.Element (V.Previous);
                  end loop;
                  Result.Path.Reverse_Elements;
                  exit;
               end;
            end if;
            if not Tried.Contains (P.Current) then
               Tried.Append (P.Current);
               Vector.Append (P);

               for DX in -1 .. 1 loop
                  for DY in -1 .. 1 loop
                     declare
                        use Queue_Of_Partials;
                        X : constant Integer := P.Current.X + DX;
                        Y : constant Integer := P.Current.Y + DY;
                        R : constant Natural :=
                              (X - Finish.X) ** 2 + (Y - Finish.Y) ** 2;
                        Position : Cursor := Queue.First;
                     begin
                        if X in 0 .. Max_X and then Y in 0 .. Max_Y then
                           declare
                              New_P : constant Partial_Path :=
                                        ((X, Y), Vector.Last_Index, R);
                           begin
                              if OK (New_P.Current)
                                or else New_P.Current = Finish
                              then
                                 while Has_Element (Position)
                                   and then R > Element (Position).Remaining
                                 loop
                                    Next (Position);
                                 end loop;
                                 if Has_Element (Position) then
                                    Queue.Insert (Position, New_P);
                                 else
                                    Queue.Append (New_P);
                                 end if;
                              end if;
                           end;
                        end if;
                     end;
                  end loop;
               end loop;
            end if;
         end;
      end loop;

      return Result;

   end Find_Path;

   ------------------
   -- First_Square --
   ------------------

   function First_Square
     (Path : Square_Path)
      return Square_Location
   is
   begin
      return Square (Path, 1);
   end First_Square;

   -------------------
   -- Get_Direction --
   -------------------

   function Get_Direction
     (From, To : Square_Location)
      return Orientation
   is
      DX : constant Integer := To.X - From.X;
      DY : constant Integer := To.Y - From.Y;
   begin
      --  directions look funny because squares are
      --  rotated 45 degrees from pixels (and actor orientations)
      if abs DX > 2 * abs DY then
         if DX > 0 then
            return North_East;
         else
            return South_West;
         end if;
      elsif abs DY > 2 * abs DX then
         if DY > 0 then
            return South_East;
         else
            return North_West;
         end if;
      else
         if DX > 0 then
            if DY > 0 then
               return East;
            else
               return North;
            end if;
         else
            if DY > 0 then
               return South;
            else
               return West;
            end if;
         end if;
      end if;
   end Get_Direction;

   ------------
   -- Length --
   ------------

   function Length (Path : Square_Path) return Natural is
   begin
      return Path.Path.Last_Index;
   end Length;

   ------------
   -- Square --
   ------------

   function Square
     (Path  : Square_Path;
      Index : Positive)
      return Square_Location
   is
   begin
      return Path.Path.Element (Index);
   end Square;

   ------------------------
   -- Straight_Line_Path --
   ------------------------

   function Straight_Line_Path
     (Start  : Square_Location;
      Finish : Square_Location;
      Max_X  : Natural;
      Max_Y  : Natural;
      OK     : not null access
        function (Location : Square_Location) return Boolean)
      return Square_Path
   is
      X1     : constant Natural := Natural'Min (Start.X, Finish.X);
      X2     : constant Natural := Natural'Max (Start.X, Finish.X);
      Across : constant Natural := X2 - X1;
      Y1     : constant Natural := Natural'Min (Start.Y, Finish.Y);
      Y2     : constant Natural := Natural'Max (Start.Y, Finish.Y);
      Down   : constant Natural := Y2 - Y1;
      Result : Chaos.Locations.Square_Path;
   begin
      if Across > Down then
         for X in X1 .. X2 loop
            declare
               Mid_Y   : constant Natural := (X - X1) * (Y2 - Y1) / (X2 - X1);
            begin
               if OK ((X, Mid_Y)) then
                  Chaos.Locations.Append (Result, (X, Mid_Y));
               elsif Mid_Y > 0 and then OK ((X, Mid_Y - 1)) then
                  Chaos.Locations.Append (Result, (X, Mid_Y - 1));
               elsif Mid_Y < Max_Y and then OK ((X, Mid_Y + 1)) then
                  Chaos.Locations.Append (Result, (X, Mid_Y + 1));
               else
                  return Chaos.Locations.No_Path;
               end if;
            end;
         end loop;
      elsif Down > Across then
         for Y in Y1 .. Y2 loop
            declare
               Mid_X   : constant Natural := (Y - Y1) * (X2 - X1) / (Y2 - Y1);
            begin
               if OK ((Mid_X, Y)) then
                  Chaos.Locations.Append (Result, (Mid_X, Y));
               elsif Mid_X > 0 and then OK ((Mid_X - 1, Y)) then
                  Chaos.Locations.Append (Result, (Mid_X - 1, Y));
               elsif Mid_X < Max_X and then OK ((Mid_X + 1, Y)) then
                  Chaos.Locations.Append (Result, (Mid_X + 1, Y));
               else
                  return Chaos.Locations.No_Path;
               end if;
            end;
         end loop;
      else
         pragma Assert (Start = Finish);
         Append (Result, Start);
      end if;

      return Result;
   end Straight_Line_Path;

end Chaos.Locations;
