with Chaos.Locations;

package Chaos.Infinity_Engine is

   function Start_Area return String;

   function Start_Location return Chaos.Locations.Pixel_Location;

   function Animation_Code
     (Animation_Id : Positive)
      return String;

   function Infinity_Path return String;

   procedure Read_Infinity_Config;

end Chaos.Infinity_Engine;
