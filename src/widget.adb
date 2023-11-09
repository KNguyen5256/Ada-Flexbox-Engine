with dui;
with namespaces; use namespaces;
with Ada.Text_IO;
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
                              others     => <>);
        dui.add_to_LOT (This, Parent);
        return This;
    end;

    function Is_In_Bound (This : in out Instance; x_Input : Natural; y_Input : Natural) return Boolean is
        returnval : Boolean;
    begin
        returnval := ((This.x <= x_Input) and ((This.x + This.w) >= x_Input)) and ((This.y <= y_Input) and ((This.y + This.h) >= y_Input));
        return returnval;
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
        Ada.Text_IO.Put_Line("I am a widget");
    end Who_I_Am;

end Widget;
