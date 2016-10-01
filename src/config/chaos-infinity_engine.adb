with Tropos.Reader;
with Chaos.Paths;

package body Chaos.Infinity_Engine is

   Infinity_Config : Tropos.Configuration;

   --------------------
   -- Animation_Code --
   --------------------

   function Animation_Code
     (Animation_Id : Positive)
      return String
   is
   begin
      case Animation_Id is
         when 16#6402# =>
            return "CMNK";
         when others =>
            return "CHMT";
      end case;
   end Animation_Code;

   -------------------
   -- Infinity_Path --
   -------------------

   function Infinity_Path return String is
   begin
      return Infinity_Config.Get ("path");
   end Infinity_Path;

   --------------------------
   -- Read_Infinity_Config --
   --------------------------

   procedure Read_Infinity_Config is
   begin
      Infinity_Config :=
        Tropos.Reader.Read_Config
          (Chaos.Paths.Config_File ("infinity.txt"));
   end Read_Infinity_Config;

   ----------------
   -- Start_Area --
   ----------------

   function Start_Area return String is
   begin
      return "AR2600";
   end Start_Area;

   --------------------
   -- Start_Location --
   --------------------

   function Start_Location return Chaos.Locations.Pixel_Location is
   begin
      return (1080, 530);
   end Start_Location;

end Chaos.Infinity_Engine;
