with Ada.Text_IO; use Ada.Text_IO;

with dui;

with namespaces; use namespaces;

package body Widget.Text is

    function Create (id            : string;
                     parent        : Widget.Any_Acc;
                     text          : string;
                     magnification : natural := 1;
                     self_flex     : flex_t  := default_flex;
                     child_flex    : flex_t  := default_flex;
                     bgd           : graphic.color) return Widget.Any_Acc is
        This : Widget.Any_Acc;
    begin
        This := new Instance' (af.Controlled with
                              id         => +id,
                              text       => +text,
                              self_flex  => self_flex,
                              child_flex => child_flex,
                              bgd        => bgd,
                              others     => <>);
        dui.add_to_LOT (This, Parent);
        return This;
    end;

    overriding procedure Event (This : in out Instance; Evt : Event_Kind) is
    begin
        null;
    end Event;

    overriding procedure Who_I_Am (This: in out Instance) is
    begin
        Put_Line("I am a Text Widget.");
    end;

    overriding procedure Draw (This : in out Instance; img : in out g.image) is
    begin
        Draw (Parent (This), img);
        --dui.draw_rect (img, this.x+1, this.y+1, this.w, this.h, g.red_1);
        dui.draw_text (img,
                       +this.text,
                       this.magnification,
                       this.x+1,
                       this.y+1,
                       this.text_color);
    end Draw;

end Widget.Text;