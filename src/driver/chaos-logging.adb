with Ada.Text_IO;

package body Chaos.Logging is

   ---------
   -- Log --
   ---------

   procedure Log
     (Component : String;
      Message   : String)
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "[" & Component & "] "
         & Message);
   end Log;

end Chaos.Logging;
