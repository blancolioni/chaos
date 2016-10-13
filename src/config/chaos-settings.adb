with WL.String_Maps;

with Chaos.Expressions;
with Chaos.Logging;
with Chaos.Parser;

package body Chaos.Settings is

   package Configure_Setting_Maps is
     new WL.String_Maps (Setting_Handler);

   Configure_Map : Configure_Setting_Maps.Map;

   -----------------
   -- Load_Object --
   -----------------

   procedure Load_Object
     (Object        : in out Object_Type'Class;
      Settings_Path : String)
   is
      procedure Set_Value
        (Name  : String;
         Value : Lith.Objects.Object);

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value
        (Name  : String;
         Value : Lith.Objects.Object)
      is
      begin
         if Configure_Map.Contains (Name) then
            Configure_Map.Element (Name) (Object, Value);
         else
            Chaos.Logging.Log
              ("CONFIG", "unknown setting: " & Name
               & " = "
               & Chaos.Expressions.Store.Show (Value));
         end if;
      end Set_Value;

   begin

      Chaos.Parser.Load_Configuration
        (Settings_Path, Set_Value'Access);

   end Load_Object;

   -------------
   -- Setting --
   -------------

   procedure Setting
     (Name    : String;
      Handler : Setting_Handler)
   is
   begin
      Configure_Map.Insert (Name, Handler);
   end Setting;

end Chaos.Settings;
