with Chaos.Expressions;
with Chaos.Objects;

generic
   type Object_Type is new Chaos.Objects.Root_Chaos_Object_Record with private;
package Chaos.Settings is

   type Setting_Handler is access
     procedure (Object : in out Object_Type'Class;
                Value  : Chaos.Expressions.Chaos_Expression);

   procedure Setting (Name : String;
                      Handler : Setting_Handler);

   procedure Load_Object
     (Object        : in out Object_Type'Class;
      Settings_Path : String);

end Chaos.Settings;
