with Chaos.Resources.Text;

package Chaos.Resources.Bcs is

   type Bcs_Resource is
     new Chaos.Resources.Text.Text_Resource with private;

private

   type Bcs_Resource is
     new Chaos.Resources.Text.Text_Resource with null record;

end Chaos.Resources.Bcs;
