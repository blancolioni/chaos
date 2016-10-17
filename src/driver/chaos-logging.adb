with Ada.Text_IO;

package body Chaos.Logging is

   ---------
   -- Log --
   ---------

   procedure Log
     (Component : String;
      Message   : String)
   is
      Clean : String := Message;
      Index : Natural := Clean'First - 1;
   begin
      for Ch of Message loop
         if Ch /= Character'Val (0) then
            Index := Index + 1;
            Clean (Index) := Ch;
         end if;
      end loop;

      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "[" & Component & "] "
         & (if True then Message
           else Clean (Clean'First .. Index)));
   end Log;

end Chaos.Logging;
