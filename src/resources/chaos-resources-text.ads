private with Ada.Containers.Indefinite_Vectors;

package Chaos.Resources.Text is

   type Text_Resource is
     abstract new Chaos_Resource with private;

   overriding function Is_Text_Resource
     (Resource : Text_Resource)
      return Boolean
   is (True);

   overriding function Signature
     (Resource : Text_Resource)
      return String
   is ("");

   overriding procedure Load
     (Resource : in out Text_Resource);

   function Line_Count (Resource : Text_Resource) return Natural;
   function Line (Resource : Text_Resource;
                  Index    : Positive)
                  return String;

private

   package Line_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Text_Resource is
     abstract new Chaos_Resource with
      record
         Lines : Line_Vectors.Vector;
      end record;

end Chaos.Resources.Text;
