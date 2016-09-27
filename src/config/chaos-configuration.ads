with Chaos.Locations;

package Chaos.Configuration is

   procedure Read_Configuration;

   function Infinity_Path return String;

   function Start_Area return String is ("AR2600");
   function Start_Location return Chaos.Locations.Pixel_Location
   is ((1080, 530));

end Chaos.Configuration;
