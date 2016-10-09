with Ada.IO_Exceptions;
with Ada.Text_IO;

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
          (Chaos.Paths.Config_File ("infinity/infinity.txt"));
   exception
      when Ada.IO_Exceptions.Status_Error =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Cannot find infinity engine config in "
            & Chaos.Paths.Config_File ("infinity/infinity.txt"));
            raise;
   end Read_Infinity_Config;

end Chaos.Infinity_Engine;
