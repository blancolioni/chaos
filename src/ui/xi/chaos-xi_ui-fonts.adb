with Ada.Text_IO;

with Chaos.Resources.Bam;
with Chaos.Resources.Manager;

package body Chaos.Xi_UI.Fonts is

   --------------------
   -- Interface_Font --
   --------------------

   function Interface_Font return Xi.Font.Xi_Font is
      Bam_Font : Chaos.Resources.Bam.Bam_Resource'Class renames
                   Chaos.Resources.Bam.Bam_Resource'Class
                     (Chaos.Resources.Manager.Load_Resource
                        (Reference => "TOOLFONT",
                         Res_Type  => Chaos.Resources.Bam_Resource).all);
   begin
      Ada.Text_IO.Put_Line ("font: cycles =" & Bam_Font.Cycle_Count'Img);
      return Xi.Font.Get_Font ("SegoeUI", 12.0);
   end Interface_Font;

end Chaos.Xi_UI.Fonts;
