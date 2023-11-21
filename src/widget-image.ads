with Ada.Strings.Unbounded;

with Ada.Finalization;

with graphic;
--with dui;

package Widget.Image is
    package asu renames Ada.Strings.Unbounded;

    subtype Parent is Widget.Instance;

    --type Instance is new Parent and 
    --                     dui.Loadable with 
    type Instance is new Parent with 
    record
        abs_filename : asu.Unbounded_String;
        image        : graphic.image_access;
    end record;

    subtype Class is Instance'Class;

    type Acc is access all Instance;
    type Any_Acc is access all Class;

    function Create (id            : string;
                     parent        : Widget.Any_Acc;
                     abs_filename  : string;
                     self_flex     : flex_t  := default_flex;
                     child_flex    : flex_t  := default_flex;
                     min_height, min_width : Natural := 0;
                     max_height, max_width : Natural := Natural'Last;
                     bgd           : graphic.color) return Widget.Any_Acc;

    --overriding procedure initialize (This : in out Instance);
    --overriding procedure adjust (This : in out Instance);
    overriding procedure finalize (This : in out Instance);

    overriding procedure Event (This : in out Instance; Evt : Event_Kind);
    overriding procedure Draw (This : in out Instance; img : in out graphic.image);
    overriding procedure Click (This : in out Instance);

    -- overriding procedure Load (This: Instance);

private

    subtype Dispatch is Instance'Class;

end Widget.Image;
