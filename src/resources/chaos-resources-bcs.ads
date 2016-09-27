with Chaos.Resources.Text;

package Chaos.Resources.Bcs is

   type Bcs_Resource is
     new Chaos.Resources.Text.Text_Resource with private;

private

   type Bcs_Resource is
     new Chaos.Resources.Text.Text_Resource with null record;

   overriding function Has_Header
     (Bcs : Bcs_Resource)
      return Boolean
   is (False);

end Chaos.Resources.Bcs;
