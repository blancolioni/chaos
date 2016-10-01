package Chaos.Animated is

   type Animated_Interface is limited interface;

   function Animation_Code
     (Animated : Animated_Interface)
      return Character
      is abstract;

end Chaos.Animated;
