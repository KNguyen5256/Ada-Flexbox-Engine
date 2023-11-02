with dui;
with namespaces; use namespaces;
package body Widget is

    function Create (id         : string;
                     parent     : Widget.Any_Acc;
                     self_flex  : flex_t  := default_flex;
                     child_flex : flex_t  := default_flex;
                     bgd        : graphic.color) return Widget.Any_Acc is
        This : Widget.Any_Acc;
    begin
        This := new Instance' (af.Controlled with
                              id         => +id,
                              self_flex  => self_flex,
                              child_flex => child_flex,
                              bgd        => bgd,
                              kind       => Normal_Widget,
                              others     => <>);
        dui.add_to_LOT (This, Parent);
        return This;
    end;

    function Is_In_Bound (This : in out Instance; x_Input, y_Input : Natural) is
    begin
        return Boolean;
    end;

    procedure Event (This : in out Instance; Evt : Event_Kind) is
    begin
        null;
    end Event;

    procedure Draw (This : in out Instance; img : in out g.image) is
    use dui;
    begin
        dui.draw_rect (img, this.x+1, this.y+1, this.w, this.h, this.bgd);
    end Draw;

    procedure Who_I_Am (This : in out Instance) is
    begin
    Put_Line("I am a widget");
    end;

end Widget;
