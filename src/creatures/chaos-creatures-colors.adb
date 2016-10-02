with WL.Binary_IO;
with WL.Bitmap_IO;

with Chaos.Paths;

package body Chaos.Creatures.Colors is

   Standard_Palette : array (0 .. 119, 0 .. 11)
     of Chaos.Resources.Resource_Color;

   Palette_Loaded   : Boolean := False;

   procedure Load_Palette;

   ----------------------
   -- Creature_Palette --
   ----------------------

   function Creature_Palette
     (Creature : Chaos.Creatures.Chaos_Creature)
      return Chaos.Resources.Resource_Palette
   is
      use WL.Binary_IO;
      Result : Chaos.Resources.Resource_Palette :=
                 (others => (0, 0, 0, 0));
   begin
      if not Palette_Loaded then
         Load_Palette;
      end if;

      for Part in Creature.Color_Map'Range loop
         declare
            Base_Index : constant Word_8 :=
                           Creature_Color_Part'Pos (Part) * 12 + 5;
         begin
            for I in Word_8 range 0 .. 11 loop
               Result (Base_Index + I) :=
                 Standard_Palette (Creature.Color_Map (Part), Natural (I));
               Result (Base_Index + I).A := 255;
            end loop;
         end;
      end loop;

      return Result;
   end Creature_Palette;

   ------------------
   -- Load_Palette --
   ------------------

   procedure Load_Palette is
      use WL.Binary_IO;
      use WL.Bitmap_IO;
      BM : Bitmap_Type;
   begin
      Read (BM, Chaos.Paths.Config_File ("infinity/palette.bmp"));
      for Y in 0 .. 119 loop
         for X in 0 .. 11 loop
            declare
               C : constant Colour_Type := Colour (BM, X, Y);
            begin
               Standard_Palette (Y, X) :=
                 (R => Word_8 (C.R), G => Word_8 (C.G),
                  B => Word_8 (C.B), A => Word_8 (C.Alpha));
            end;
         end loop;
      end loop;
      Palette_Loaded := True;
   end Load_Palette;

end Chaos.Creatures.Colors;
