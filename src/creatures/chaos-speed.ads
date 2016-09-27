package Chaos.Speed is

   subtype Chaos_Speed is Integer range 0 .. 20;

   type Chaos_Speed_Interface is limited interface;

   function Speed
     (Item : Chaos_Speed_Interface)
      return Chaos_Speed
      is abstract;

end Chaos.Speed;
