with Ada.Strings.Unbounded;

package Widget.Text is

   subtype Parent is Widget.Instance;

   type Instance is new Parent
   with record
      text          : su.Unbounded_String;
      text_color    : graphic.color := (graphic.color_val'last, graphic.color_val'last, graphic.color_val'last, 0.0);
      magnification : natural := 2;
   end record;
   
   subtype Class is Instance'Class;

   type Acc is access all Instance;
   type Any_Acc is access all Class;

   function Create (id            : string;
                    parent        : Widget.Any_Acc;
                    text          : string;
                    magnification : natural := 1;
                    self_flex     : flex_t  := default_flex;
                    child_flex    : flex_t  := default_flex;
                    bgd           : graphic.color) return Widget.Any_Acc;

   overriding
   procedure Event (This : in out Instance; Evt : Event_Kind);

   overriding 
   procedure Who_I_Am (This: in out Instance);

   overriding
   procedure Draw (This : in out Instance; img : in out graphic.image);

private
   
   subtype Dispatch is Instance'Class;
   
end Widget.Text;